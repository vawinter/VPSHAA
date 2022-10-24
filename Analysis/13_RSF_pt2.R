#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##------------- November 2021 -----------X
##--- Last edited: October 10, 2022 -----X
#########################################X
##------- Last ran: oct 20, 2022 --------X
#########################################X

# Based on code Space Use Ecology: from 06_HSA_pt2.

# I am going to fit a set of simple resource selection
# functions (RSFs) for estimating relative habitat selection from
# Pronghorn GPS data. Here we're starting with a single year.
# In the next script, I will scale up to multiple years and
# getting population-level inference.

# Goals for this script:
#   1. Process our data for RSF fitting
#        a. get covar clip for each indiv in season/year

#clean up my R environment 
rm(list = ls())
gc()

source("Analysis/99_funs.R")

# Load packages ----
library(tidyverse)
library(lubridate)
library(amt)
library(DBI)
library(raster)
library(sf)
library(tidyr)
#library(parallel)

## Load data ---- 
### Load in landscape covariates ----
dir <- "../../../../Box/Projects/buffer/Covar_org/2021_covar/"

# List covariate files
landscapes <- list.files(dir, full.names = T)[list.files(dir) %in% c("landscape_202102.tif",
                                                                     "landscape_202104.tif",
                                                                     "landscape_202107.tif",
                                                                     "landscape_202111.tif")]
# Input into data frame
dates <- data.frame(filename = landscapes,
                    # find year in name
                    year = substr(stringr::word(landscapes, 4, 4, "_"), start = 1, stop = 4),
                    # find month in name
                    month = substr(stringr::word(landscapes, 4, 4, "_"), start = 5, stop = 6)) %>% 
  # create leap column for leap year
  mutate(leap = case_when(year == 2020 ~ TRUE,
                          TRUE ~ FALSE),
         # start date for loop
         start_date = "01") %>% 
  # end date depending on month
  mutate(end_date = case_when(
    month %in% c("04", "11") ~ "30",
    month == "07" ~ "31",
    month == "02" & leap ~ "29",
    month == "02" & !leap ~ "28"
  ))

# loop over covariates
for (l in landscapes) {
  
  # The landscape stack
  landscape <- stack(l)
  
  # Format naming for July and other months
  if(substr(l, start = 28, stop = 29) == "07") {
    names(landscape) <- c("elevation", "asp_sin", "asp_cos", "roughness", "bio",
                                                 "herb", "shrub", "tree")
  } else {
  
  names(landscape) <- c("elevation", "snd", "asp_sin", "asp_cos", "roughness", "bio",
                        "herb", "shrub", "tree") 
  }
  
 
  date_row <- dates[dates$filename == l, ]
  

  # Load and str data ----
  ph_dat <- readRDS("Data/Processed/comb_dat_20220524.rds")
  
  # Filter desired date range
  ph <- ph_dat %>%
    dplyr::select(ID,
                  dt,
                  x,
                  y) %>% 
    # start and end date based on above sf
    filter(dt >= paste(date_row$year, date_row$month, date_row$start_date, sep = "-") 
           & dt <= paste(date_row$year, date_row$month, date_row$end_date, sep = "-"))  %>% 
    arrange(dt) # new edit 1/12
  
  # Edit: 05/24/2022
  # # 1. convert to utm
  # dat_sf <- st_as_sf(ph2,
  #                    coords = c("x", "y"),
  #                    crs = 4326) %>%
  #   st_transform(32612)
  # 
  # # Convert from sf back to ordinary data.frame
  # ph <- dat_sf %>%
  #   mutate(x = st_coordinates(geometry)[, 1],
  #          y = st_coordinates(geometry)[, 2]) %>%
  #   st_drop_geometry()
  
  # Extent ----
  # New edit: 1/12
  # testing to get the mid point for each indiv
  indiv <- unique(ph$ID)
  ph$mid_x <- NA
  ph$mid_y <- NA
  ph3 <- data.frame()
  
  for(i in 1:length(indiv)){
    # Filter by individuals
    dat <- filter(ph, ID == indiv[i]) %>% 
      # find target date around mid month for each
#      mutate(target_date = ymd_hms(paste0(year(dt), "02-15 12:00:00"))) %>% 
      # Check if it needs an extra dash between year and month down here:
      mutate(target_date = ymd_hms(paste0(year(dt), date_row$month, "-15 12:00:00"))) %>% 
      # Denote how far dates are from mid month
      mutate(dif = abs(difftime(dt, target_date)))
    # save location point w/ least distance from mid month
    dat$mid_x <- dat[which.min(dat$dif), ]$x
    dat$mid_y <- dat[which.min(dat$dif), ]$y
    
    # Combine in new df
    ph3 <- rbind(dat, ph3)
  }
  
  # Looping over indiv rasters ----
  # 1. Create empty lists for storage
  clip <- list()
  locs <- list()
  indiv <- unique(ph3$ID)
  # New edit: 12/8 VW:
  locs_in <- list()
  
  # I want to create a raster of clipped covars available to each indiv
  #  within the extent to run RSF analysis 
  
  # 2. Loop
  for(k in 1:length(indiv)){
    
    # print status
    print(paste(k))
    
    # Subset individual data
    dat <- filter(ph3, ID == indiv[k])
    
    # Turn into a track_xyt
    locs[[k]] <- mk_track(dat, x, y, dt, crs = 32612) 
    
    # Now we can use the extent we created above to cut out a small piece of this
    # raster
    clip[[k]] <- crop(landscape, make_extent(dat$mid_x[1],dat$mid_y[1])) # Edit 1/12
    
    # Extract values at used points in buffer
    locs_in[[k]] <- locs[[k]] %>% 
      # BJS: use 'landscape' instead of 'clip[[i]]' for this
      extract_covariates(landscape)
    
    # Extract values for available points
    avail <- as.data.frame(values(clip[[k]]))  
    avail$case_ <- FALSE    
    
    # Combine used and available
    used <- locs_in[[k]] %>% 
      dplyr::select(-x_, -y_, -t_) %>% 
      mutate(case_ = TRUE)   
    
    # This is what you need for analysis
    locs[[k]] <- rbind(used, avail)
  }
  
  # 3.  Combine all the location data for individuals
  names(locs) <- indiv
  
  mod_dat <- locs %>% 
    # bind rows of df
    bind_rows(.id = "ID") %>% 
    # add weights column & input 1 for used and 5000 for available
    mutate(w = as.numeric(ifelse(case_ == "TRUE", 1, 5000))) %>% 
    # save month and year column
    mutate(month = date_row$month,
           year = date_row$year)
  
  # Save ----
  # BJS comment: I would end this script here. Save mod_dat as your "final"
  # model data, and fit RSFs in the next script
  ym <- stringr::word(stringr::word(l, 4, 4, "_"), 1, 1, "\\.")
  saveRDS(mod_dat, paste0("Data/Processed/2021_RSF_data/", ym, "_model_data.rds"))
    
}

# Read in test .rds
x <- readRDS("../RSF_data/202004_model_data.rds")
head(x)

# DONE!

# Sclae and center ----
# Load in data
m_sd <- read.csv("Data/Processed/Mean_sd/20221019_mean-sd_all.csv", header = T)

# Load in data
# Load in data
dir <- "Data/Processed/2021_RSF_data/"
# Read in 2021 data
files <- list.files(dir, full.names = T)

# merge into one file
# File lengths are different. Loading them in by season to investigate why
sum <- readRDS(files[3])
all_other <- do.call(rbind, lapply(files[c(1,2,4)], readRDS)) 

# Snow!
sum$SND <- "0"

# merge
pred_dat <- rbind(all_other, sum)
str(pred_dat)

# Scale and center
# Set SND NA values to 0
pred_dat$SND[is.na(pred_dat$SND)] <- 0

## scaled_log Elevation ----
# a. find mean
Elev_m <- m_sd$Elev[1]
# b. find sd
Elev_s <- m_sd$Elev[2]
#c. subtract and divide
pred_dat$scaled_Elev <- ((pred_dat$Elevation - Elev_m)/ Elev_s)

## Aspect(sin) ----
# a. find mean
a.sin_m <- m_sd$Asp_sin[1]
# b. find sd
a.sin_s <-  m_sd$Asp_sin[2]
#c. subtract and divide
pred_dat$scaled_Asp_sin <- ((pred_dat$Asp_sin - a.sin_m)/ a.sin_s)

## Aspect(cos) ----
# a. find mean
a.cos_m <- m_sd$Asp_cos[1]
# b. find sd
a.cos_s <-  m_sd$Asp_cos[2]
#c. subtract and divide
pred_dat$scaled_Asp_cos <- ((pred_dat$Asp_cos - a.cos_m)/ a.cos_s)

## Roughness ----
# a. find mean
Rough_m <- m_sd$Rough[1]
# b. find sd
Rough_s <- m_sd$Rough[2]
#c. subtract and divide
pred_dat$scaled_Rough <- ((pred_dat$Roughness - Rough_m)/ Rough_s)

## RAP (bio) ----
# a. find mean
bio_m <- m_sd$RAP_bio[1]
# b. find sd
bio_s <- m_sd$RAP_bio[2]
#c. subtract and divide
pred_dat$scaled_RAP_bio <- ((pred_dat$RAP_bio - bio_m)/ bio_s)

## RAP (cover) ----
# a. find mean
herb_m <- m_sd$Herb[1]
# b. find sd
herb_s <- m_sd$Herb[2]
#c. subtract and divide
pred_dat$scaled_Herb <- ((pred_dat$Herb - herb_m)/ herb_s)

## Shrub ----
# a. find mean
  Shrub_m <-  m_sd$Shrub[1]
# b. find sd
Shrub_s <- m_sd$Shrub[2]
#c. subtract and divide
pred_dat$scaled_Shrub <- ((pred_dat$Shrub - Shrub_m)/ Shrub_s)

## Tree ----
# a. find mean
Tree_m <-  m_sd$Tree[1]
# b. find sd
Tree_s <-  m_sd$Tree[2]
#c. subtract and divide
pred_dat$scaled_Tree <- ((pred_dat$Tree  - Tree_m)/ Tree_s)


# Save output ----
saveRDS(pred_dat, "Data/Processed/2021_RSF_data/20221024_2021_3rd-order_RSF-prep.rds")
