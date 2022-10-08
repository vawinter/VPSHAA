#####################################################X
#---------Collecting and organizing covariates-------X
#-------------- Daymet & SNODAS data ----------------X
#------------ Created: September 2021 ---------------X
#----------- Last modified: 11/10/2021 --------------X
#--------------------   VAW  ------------------------X
#####################################################X

# I will be collecting and organizing my covariates to begin to 
# address potential drivers of pronghorn migration in Utah.

# I will be collecting snow, elevation,and RAP for now. I will be using code from
# a tutorial written by B. Smith:
# https://bsmity13.github.io/spatial_covars/03_Data/01_SWE/swe.html

# Download Brian's snow package if necessary:
# remotes::install_github("bsmity13/snowdl")

# In this script, let's get snow:

# General plan for downloading from a remote data source:
# i. Find the data
# ii. Figure out the pattern
# iii. Construct the URL
# iiii. Download the data

# Set opions ----
options(stringsAsFactors = FALSE)
#options(pkgType = "binary")

# Load in library ----
library(snowdl)
library(tidyverse)
library(raster)
library(lubridate)
library(sf)
library(snow)
library(parallel)

# 1. Find Spatial Extent ----
#load in template raster wih 100 km buffer around ut
ut <- raster("../raster_template_utm.tif")

# add in values
values(ut) <- 1:ncell(ut)
plot(ut)

# 2.Downloading SNODAS ----
#i. Test one date
test <- "2017-01-01"

# ii.#Download dates
  download_SNODAS(as.Date(test),
                  out_dir = "snow_rast")

# iii. Unpack ----
# This unpacks all tarballs in directory
unpack_SNODAS(tar_dir = "snow_rast",
              out_dir = "snow", # change back to 'temp'
              rm_tar = TRUE) 

# iv. Write to raster ----
rasterize_SNODAS(data_dir = "snow", 
                 out_dir = "snow_rast", 
                 rm_data = TRUE,
                 format = "GTiff",
                 reproject = ut,
                 method = "ngb", # nearest neighbor
                 verbose = TRUE)

# 3. Load in  ----
rast <- brick("snow_rast/SNODAS_20170101.tif")
plot(rast)


# Full download ----
# i. Download seasons
# Vector of dates in winter (January) in 17 - 21
winter <- c(seq(as.Date("2017-01-01"), as.Date("2017-01-31"), by = "days"),
            seq(as.Date("2018-01-01"), as.Date("2018-01-31"), by = "days"),
            seq(as.Date("2019-01-01"), as.Date("2019-01-31"), by = "days"),
            seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "days"))

# Vector of dates in spring (April) in 17 - 21
spring <- c(seq(as.Date("2017-04-01"), as.Date("2017-04-30"), by = "days"),
            seq(as.Date("2018-04-01"), as.Date("2018-04-30"), by = "days"),
            seq(as.Date("2019-04-01"), as.Date("2019-04-30"), by = "days"),
            seq(as.Date("2020-04-01"), as.Date("2020-04-30"), by = "days"))

# Vector of dates in summer (July) in 17 - 21
summer <- c(seq(as.Date("2017-07-01"), as.Date("2017-07-31"), by = "days"),
            seq(as.Date("2018-07-01"), as.Date("2018-07-31"), by = "days"),
            seq(as.Date("2019-07-01"), as.Date("2019-07-31"), by = "days"),
            seq(as.Date("2020-07-01"), as.Date("2020-07-31"), by = "days"))

# Vector of dates in fall (Nov) in 17 - 21
fall <- c(seq(as.Date("2017-11-01"), as.Date("2017-11-30"), by = "days"),
            seq(as.Date("2018-11-01"), as.Date("2018-11-30"), by = "days"),
            seq(as.Date("2019-11-01"), as.Date("2019-11-30"), by = "days"),
            seq(as.Date("2020-11-01"), as.Date("2020-11-30"), by = "days"))

# ii. Loop and download ----
# define season 
szn <- c(spring, winter, summer, fall)

for(i in 1:length(szn)){
  
  # progress
  print(paste(i, szn[i]))
  
  #Download dates
  download_SNODAS(as.Date(szn[i]),
                 out_dir = "snow_rast")
}

# iii. Unpack ----
# This unpacks all tarballs in directory
unpack_SNODAS(tar_dir = "snow_rast",
              out_dir = "temp", 
              rm_tar = TRUE) 

# iv. Write to raster ----
detectCores()

# Run in parallel
beginCluster(10)

rasterize_SNODAS(data_dir = "temp",
                 out_dir = "snow", 
                 rm_data = TRUE,
                 format = "GTiff",
                 reproject = ut,
                 method = "ngb", # bilinear or ngb?
                 verbose = TRUE)
# end cluster
endCluster()

# 3. Load in one example ----
tst <- brick("snow/SNODAS_20170401.tif")
plot(tst)

# Done!

# TEMP OPTION FOR MD ----                                                                                                                                                                                                                                                                                                             
# i. mule deer extra dates
md1 <- c(seq(as.Date("2012-07-01"), as.Date("2012-07-31"), by = "days"),
         seq(as.Date("2013-07-01"), as.Date("2013-07-31"), by = "days"),
         seq(as.Date("2014-07-01"), as.Date("2014-07-31"), by = "days"),
         seq(as.Date("2015-07-01"), as.Date("2015-07-31"), by = "days"),
         seq(as.Date("2016-07-01"), as.Date("2016-07-31"), by = "days"))

md2 <- c(seq(as.Date("2012-01-01"), as.Date("2012-01-31"), by = "days"),
         seq(as.Date("2013-01-01"), as.Date("2013-01-31"), by = "days"),
         seq(as.Date("2014-01-01"), as.Date("2014-01-31"), by = "days"),
         seq(as.Date("2015-01-01"), as.Date("2015-01-31"), by = "days"),
         seq(as.Date("2016-01-01"), as.Date("2016-01-31"), by = "days"))

md3 <- c(seq(as.Date("2012-04-01"), as.Date("2012-04-30"), by = "days"),
         seq(as.Date("2013-04-01"), as.Date("2013-04-30"), by = "days"),
         seq(as.Date("2014-04-01"), as.Date("2014-04-30"), by = "days"),
         seq(as.Date("2015-04-01"), as.Date("2015-04-30"), by = "days"),
         seq(as.Date("2016-04-01"), as.Date("2016-04-30"), by = "days"))

md3 <- c(seq(as.Date("2012-11-01"), as.Date("2012-11-30"), by = "days"),
         seq(as.Date("2013-11-01"), as.Date("2013-11-30"), by = "days"),
         seq(as.Date("2014-11-01"), as.Date("2014-11-30"), by = "days"),
         seq(as.Date("2015-11-01"), as.Date("2015-11-30"), by = "days"),
         seq(as.Date("2016-11-01"), as.Date("2016-11-30"), by = "days"))

winter <- md2


# ii. Loop and download ----
for(i in 1:length(winter)){
  
  # progress
  print(paste(i, winter[i]))
  
  #Download dates
  download_SNODAS(as.Date(winter[i]),
                  out_dir = "snow_rast")
}

# iii. Unpack ----
# This unpacks all tarballs in directory
unpack_SNODAS(tar_dir = "snow_rast",
              out_dir = "temp",
              rm_tar = TRUE) 

# iv. Write to raster ----
# directory to lab env covar file
pth <- "C:/Users/A02350534/Box/Avgar Lab on WILD/UtahEnvironmentalCovariates/SNODAS/snow/"

rasterize_SNODAS(data_dir = "../temp",
                 out_dir = pth,
                 rm_data = TRUE,
                 format = "GTiff",
                 reproject = ut,
                 method = "ngb",
                 verbose = TRUE)

# DONE! Upload these to Avgar lab file on box

# IF FAILS -----
# List files in snow
done <- list.files("snow/")

# Remaining that didn't download
winter <- c(seq(as.Date("2019-01-01"), as.Date("2019-01-31"), by = "days"),
            seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "days"))

# Vector of dates in spring (April) in 17 - 21
spring <- c(seq(as.Date("2018-04-09"), as.Date("2018-04-30"), by = "days"),
            seq(as.Date("2019-04-01"), as.Date("2019-04-30"), by = "days"),
            seq(as.Date("2020-04-01"), as.Date("2020-04-30"), by = "days"))

# Vector of dates in summer (July) in 17 - 21
summer <- c(seq(as.Date("2018-07-01"), as.Date("2018-07-31"), by = "days"),
            seq(as.Date("2019-07-01"), as.Date("2019-07-31"), by = "days"),
            seq(as.Date("2020-07-01"), as.Date("2020-07-31"), by = "days"))

# Vector of dates in fall (Nov) in 17 - 21
fall <- c(seq(as.Date("2018-11-01"), as.Date("2018-11-30"), by = "days"),
          seq(as.Date("2019-11-01"), as.Date("2019-11-30"), by = "days"),
          seq(as.Date("2020-11-01"), as.Date("2020-11-30"), by = "days"))

# (Unfinished)
# Daymet data download ----

# Find tiles I need
# Now download the tiles if necessary

# Create a vector of tiles needed for UT
utah_tiles <- c(11734, 11914)

get_daymet_swe(year = 2017:2020,
               tile = utah_tiles,
               out_dir = "snow")

