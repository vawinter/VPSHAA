#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- January 2022 ---------------X
#########################################X
#---- Organizing used vs avail pts ------X
#########################################X
##----- Last edited: 06/20/2022 ---------X
#########################################X

# Goals for this script:
#   1. Fins used pts per indiv
#         a. visualized used vs avail pts in and outside buffer
#          b. get # of each and combine into csv for that indiv in season/year


#clean up my R environment 
rm(list = ls())
gc()

# Load in libraries ----
library(dplyr)
library(purrr)
library(raster)
library(rgeos)
library(rgdal)
library(sf)
library(lubridate)

# functions
source("99_funs.R")

### Load in landscape covariates ----
dir <- "../../../../Box/Projects/buffer/Covar_org/"


# Create empty data frame to store results
output <- data.frame(ID = NA,
                     used_out = NA,
                     used_in = NA,
                     month = NA,
                     year = NA)

# List covariate files
landscapes <- list.files(dir, full.names = T)[!list.files(dir) %in% c("landscape_201801.tif",
                                                                      "landscape_201901.tif",
                                                                      "landscape_202001.tif",
                                                                      "202102_model_data.rds",
                                                                      "202104_model_data.rds",
                                                                      "202107_model_data.rds",
                                                                      "202111_model_data.rds",
                                                                      "2021_covar")]
# Input into data frame
dates <- data.frame(filename = landscapes,
                    # find year in name
                    year = substr(stringr::word(landscapes, 3, 3, "_"), start = 1, stop = 4),
                    # find month in name
                    month = substr(stringr::word(landscapes, 3, 3, "_"), start = 5, stop = 6)) %>% 
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
  
  # Load and str data ----
  ph_dat <- readRDS("Data/processed/comb_dat_20220524.rds")
  # Updated 06/20/2022 (cleaned data at 2 hr fixes)
  #ph_dat <- readRDS("cleaned_data/20220620_cleaned-data.rds")
  
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
  ext <- make_extent(ph$x[1], ph$y[1])
  
  # testing to get the mid point for each indiv
  indiv <- unique(ph$ID)
  ph$mid_x <- NA
  ph$mid_y <- NA
  ph3 <- data.frame()
  
  for(i in 1:length(indiv)){
    # Filter by individuals
    dat <- filter(ph, ID == indiv[i]) %>% 
      # find target date around mid month for each
      mutate(target_date = ymd_hms(paste0(year(dt), date_row$month, "-15 12:00:00"))) %>% 
      # Denote how far dates are from mid month
      mutate(dif = abs(difftime(dt, target_date)))
    # save location point w/ least distance from mid month
    dat$mid_x <- dat[which.min(dat$dif), ]$x
    dat$mid_y <- dat[which.min(dat$dif), ]$y
    
    # Combine in new df
    ph3 <- rbind(dat, ph3)
  }
  
  
  # Looping over indiv plots ----
  indiv <- unique(ph3$ID)
  p <- list()
  
  
  for(i in 1:length(indiv)){
    # Store in list
    print(paste(i, indiv[i]))
    # Subset individual data
    d <- filter(ph3, ID == indiv[i])
    # extent for each indiv
    ext <- make_extent(d$mid_x[1], d$mid_y[1])
    # plot extent
    plot(ext, main = indiv[i])
    points(x = d$x,
           y = d$y,
           pch = 16)
    # Make the extent a SpatialPolygon
    p <- as(ext, 'SpatialPolygons')
    # Assign CRS
    crs(p) <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"
    # Transform the extent into sf object
    p2 <- st_as_sf(p)
    # Transform the points into sf object
    pts.mp <- d %>%
      st_as_sf(coords = c("x", "y")) %>%
      st_cast("MULTIPOINT") %>%
      st_set_crs(32612)   
    # Identify points that fall outside the extent
    outside <- sapply(st_intersects(pts.mp, p2),function(x){length(x)==0})
    # Store results for the current individual
    res <- data.frame(ID = indiv[i],
                      used_out = nrow(pts.mp[outside,]),
                      used_in = nrow(pts.mp[!outside,]),
                      month = unique(month(pts.mp$dt)),
                      year = unique(year(pts.mp$dt)))
    
    # Bind to the general results table
    output <- rbind(output, res)
  }
}

# Remove first row of NAs
output <- output[-1, ]

# Check that all months/years are accounted for
table(output$month)
table(output$year)

# Save as .csv
outdir <- "../RSF_data/final_outputs/"
if(!dir.exists(outdir)){dir.create(outdir)}
dir.exists(outdir)

write.csv(output, paste0(outdir, "20220714_2021_used.csv"), row.names = FALSE)

# DONE!



### OLD CODE ----

# # Load in all seasons ----
# # # directory
# dir <- "../RSF_data/used_rds/"
# 
# # list files
# mod_files <- list.files(dir, full.names = T)
# 
# # merge into one file
# all <- do.call(rbind, lapply(mod_files, readRDS)) 
# 
# # Save
# write.csv(all, "../RSF_data/final_outputs/20220219_used.csv")
# 
# # Combine together w/ mean avail file ----
# new <- avail %>% 
#   left_join(output, by = "ID") 
# 
# #Save
# write.csv(new, "../RSF_data/used_rds/jul20_used_avail_all.csv")

# DONE!