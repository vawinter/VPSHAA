#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- February 2022  -------------X
#########################################X
# ------- Modified from RBH -------------X
#####################################################X
# Code to extract average available attribute value #X
#####################################################X

# This code is adapted from my entire code for home range analysis, a lot of 
# which, is unnecessary for your work, so I tried to cut it down to just what
# you need. Hopefully it's not too hard to integrate it in the code process you
# already have. Let me know if you have questions!
rm(list = ls())
gc()

library(raster)
library(dplyr)
library(lubridate)
library(amt)

# Load environmental attribute ----
data_path <- "../../../Avgar Lab on WILD/UtahBarriers/Roads/roads_pa_proj.tif"
#attr <- raster::raster(data_path) 
# "attr" could be any environmental covariate you're working with. e.g. Roads
# "data_path" would the directory path and file name of the raster you want to
#     load in
# I'll let you fill this in with the attribute you need and the file path based
#     on your working directory

# create 'date_row' df

# List covariate files
dir <- "../Covar_org/2021_covar/"
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
  
  date_row <- dates[dates$filename == l, ]
  
  ph_dat <- readRDS("cleaned_data/20220620_cleaned-data.rds")
  
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
  
  # Make buffer ----
  # I'm assuming you already have a way of generating a buffer, so you could 
  #     incorporate that here. 
  source("99_funs.R")
  
  # Looping over indiv rasters ----
  # 1. Create empty lists for storage
  attr_buff_crop <- list()
  avg_avail <- data.frame(ID = NA,
                          m_road = NA,
                          month = NA,
                          year = NA)
  indiv <- unique(ph3$ID)
  
  # I want to create a raster of clipped covars available to each indiv
  #  within the extent to run RSF analysis 
  
  # 2. Loop
  for(k in 1:length(indiv)){
    
    # print status
    print(paste(k))
    
    # Subset individual data
    dat <- filter(ph3, ID == indiv[k])
    
    attr <- raster::raster(data_path)
    
    ## Crop buffer to the attribute raster ----
    # Now we can use the extent we created above to cut out a small piece of this
    # raster
    attr_buff_crop[[k]] <- raster::crop(attr, make_extent(dat$mid_x[1],dat$mid_y[1]))
    
    # Calculate Average Available Attribute ----
    # sum all the attribute data (clipped to the buffer) and divide by 
    # the total number of cells in the buffer
    temp <-  data.frame(ID = indiv[k],
                        m_road = (raster::cellStats(attr_buff_crop[[k]], stat = "sum", na.rm = T)) / 
                          (raster::ncell(attr_buff_crop[[k]])),
                        month = date_row$month,
                        year = date_row$year)
    
    # Combine together df's
    avg_avail <-  rbind(temp, avg_avail)
    
    # Save ----
    # BJS comment: I would end this script here. Save mod_dat as your "final"
    # model data, and fit RSFs in the next script
    ym <- stringr::word(stringr::word(l, 4, 4, "_"), 1, 1, "\\.")
    saveRDS(avg_avail, paste0("../RSF_data/final_outputs/road_years/", ym, "_road_data.rds"))
    
  }
}

# Read in and merge together
rm(list = ls())
gc()

library(tidyverse)

# read in
dir <- "../RSF_data/final_outputs/"

# list files
files <- list.files(path = dir, pattern = "road_years2021", full.names = TRUE)

# Load in data
dat <- do.call(rbind, lapply(files, readRDS))

# remove NA rows
dat_clean <- na.omit(dat) 
dat_clean$month <- as.integer(dat_clean$month)
dat_clean$year <- as.integer(dat_clean$year)

# # Load in MEM data
# mem <- read.csv( "../RSF_data/final_outputs/20220531-indiv_mod_final_comb-unit-sex_fix.csv",
#                 header = T, na.strings = c("", "N/A", "NA"))
# 
# x <- mem %>% 
#   left_join(dat_clean, by = c("ID", "month", "year")) %>% 
#   relocate(m_road, .before = m_Elev)

# Save
write.csv(dat_clean, "../RSF_data/final_outputs/20220714_2021_road-data_comb.csv", row.names = FALSE)

# DONE!