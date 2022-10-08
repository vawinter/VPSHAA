#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- October 2021 ---------------X
#########################################X
#---- Organizing covariates for RSF -----X
##------ Last edited: 01/18/2022 --------X
#########################################X

# Based on code Space Use Ecology: from 06_HSA_pt2.
# I am going to fit a set of simple resource selection
# functions (RSFs) for estimating relative habitat selection from
# Pronghorn GPS data. Here we're starting with creating 
# a raster stack for covariates in a single year.
# In the next script, I will crop the rasters to individuals in that month/year
# to prepare for GLMs.

# Goals for this script:
#   a. organize the covariates for RSF into a stack or brick
#   b. save output as "landscape_yearmonth.tif" per month/year

# 1/21/2022 changed file dir from ../ to buffer
# - working on lab comp in dif proj.

#clean up my R environment 
rm(list = ls())
gc()

# Load packages ----
library(raster)

# Load raster template
# Load in template raster
UT <- raster("../raster_template_utm.tif")

# 100km buffer
new_ext <- extent(UT)
new_ext

# Rasterize
ut_rast <- raster(crs = crs(UT),
                  res = c(10000, 10000), 
                  ext = new_ext)
ut_rast[] <- NA
ut_rast
plot(ut_rast)

## A. Load data ----
# landscape covariates ----
dir <- "../../../Avgar Lab on WILD/UtahEnvironmentalCovariates/"
snow <- paste0(dir, "SNODAS/monthly_aggregate/202011_mean_snd.tif")

#1. Aggregated snow *
snd <- raster(snow)
# Check projection
projection(snd)

# reproject
snd_prj <- projectRaster(snd, UT, method = 'ngb')

#2. RAP
# BJS: When you load RAP with raster(), you only get the first band. 
## You want to load that one with stack().

# RAP: bio
# Feb + Apr = prev year.
biomass <- paste0(dir, "RAP/v3/UTM/unstacked/RAP_biomass_2020_herb.tif")
bio <- raster(biomass)
# Check projection
projection(bio) 

# RAP: cover
# Feb + Apr = prev year.
#... aggregate bands 1 and 4
# Band 1 = Annual forbs & grasses
# Band 2 = Bare ground
# Band 3 = Litter
# Band 4 = Perennial forbs & grasses
# Band 5 = Shrubs
# Band 6 = Trees

# annual/periannual aggregated cover
herb <- paste0(dir, "RAP/v3/UTM/unstacked/RAP_cover_2020_herb.tif")
agg_cover <- raster(herb)

# tree and shrub
shrub_band <- paste0(dir, "RAP/v3/UTM/unstacked/RAP_cover_2020_shrub.tif")
tree_band <- paste0(dir, "RAP/v3/UTM/unstacked/RAP_cover_2020_tree.tif")

shrub <- raster(shrub_band)
tree <- raster(tree_band)

#3. Roughness *
rough_ut <- raster("../DEM/rough_ut.tif")

# Elevation
Elevation <- paste0(dir, "DEM/dem_utm.tif")
elev <- raster(Elevation)

# Check projection
projection(rough_ut) 

#4. Aspect (sin and cosine)
aspect_cos <- paste0(dir, "DEM/asp_cos_utm.tif")
asp_cos <- raster(aspect_cos)

aspect_sin <- paste0(dir, "DEM/asp_sin_utm.tif")
asp_sin <- raster(aspect_sin)

# Stack ---- 
# For July ----
land <- stack(elev, asp_sin, asp_cos, rough_ut, bio, agg_cover, shrub, tree)
# name the layers
names(land) <- c("Elevation", "Asp_sin", "Asp_cos", "Roughness", "RAP_bio",
                      "Herb", "Shrub", "Tree")
plot(land)

writeRaster(land, paste0(outdir, "landscape_202007.tif"),
            options ="INTERLEAVE = BAND", overwrite = TRUE)



rst <- stack(elev, snd, asp_sin, asp_cos, rough_ut, bio, agg_cover, shrub,
         tree)

# name the layers
names(rst) <- c("Elevation", "SND", "Asp_sin", "Asp_cos", "Roughness",
                      "RAP_bio", "Herb", "Shrub", "Tree")

# Save  ----
outdir <- "../Covar_org/2021_covar/"
#dir.create(outdir)
dir.exists(outdir)

writeRaster(rst, paste0(outdir, "landscape_202011.tif"),
            options ="INTERLEAVE = BAND", overwrite = TRUE)

# # Get 10x10 ----
# beginCluster(10)
# land <- resample(rst, ut_rast, method = 'bilinear')
# endCluster()
# 
# # name the layers
# names(land) <- c("Elevation", "SND", "Asp_sin", "Asp_cos", "Roughness", 
#                       "RAP_bio", "Herb", "Shrub", "Tree")
# plot(land)
# 
# # Save (w/ new res) ----
# outdir2 <- paste0(dir, "VW_Stacked_Covariates/")
# #dir.create(outdir)
# dir.exists(outdir2)
# 
# writeRaster(land, paste0(outdir2, "10x10_landscape_202107.tif"),
#             options ="INTERLEAVE = BAND", overwrite = TRUE)

# Save (w/out snow) ----
# writeRaster(land, "../Covar_org/landscape_201907.tif",
#             options="INTERLEAVE=BAND", overwrite = TRUE)

# DONE!

#---------- END ----------#
#---------- END ----------#
#---------- END ----------#
#---------- END ----------#

###...OLD CODE... -----
# # TO ALTER PROJECTIONS AND EXTENTS -----
# # Create new extent -------
# Load in template raster
# UT <- raster("buffer/raster_template_utm.tif")
# 
# # 1. Find Spatial Extent ----
# # i. make a map of utah
# utah <- maps::map("state", plot = F, fill = TRUE) %>%
#   # turn into sf obj
#   sf::st_as_sf() %>% 
#   # pull out utah
#   dplyr::filter(ID == "utah")
# 
# plot(utah)
# 
# # Extent in UTM & desired resolution
# utah_utm <- sf::st_transform(utah, 32612)
# 
# #ii. load in template raster wih 100 km buffer around ut
# ut <- raster("raster_template_utm.tif")
# plot(ut)
# 
# # add in values
# values(ut) <- 1:ncell(ut)
# plot(ut)
# plot(utah_utm$geom, add = T, lwd = 2)
# 
# projection(UT)
# extent(UT)
# 
# # BJS on RAP: The value 65535 is actually the NA value. So you want to actually 
# #set those to NA in R before you reproject, or it will average that big number 
# # with the percents. I.e.:
# beginCluster(10)
# 
# x <- values(rap)
# x[which(x == 65535)] <- NA
# values(rap) <- x
# 
# endCluster()
# 
# # Reproject ----
# # Use this raster to reproject the original rasters 
# # BJS: reproject in parallel!
# # Reproject to new extent, crs, and resolution 
# detectCores()
# 
# beginCluster(14)
# # Reproject to new extent, crs, and resolution 
# 
# rap_pr <- projectRaster(rap, UT)
# bio_pr <- projectRaster(rap_bio, UT)
# endCluster()
# 
# # For rough layer, change extent only and not proj.
# rough_new <- setExtent(rough_ut, UT, keepres = TRUE)
# 
# # Check extents
# extent(snow_pr)
# extent(rough_new)
# extent(rap_pr)
# extent(bio_pr)
# 
# # BJS: probably better of making this a stack
# # Bricks *must* come from a single file on disk, so you're actually asking
# # it to write the 3 layers to a temporary file on disk before you plot.
# # Stack will work the same without the single file requirement.
# 
# # Stack ---- 
# # landscape <- brick(rap_pr, rough_pr, snow_pr)
# landscape <- stack(swe, snd, asp_sin, asp_cos, rough_ut, bio, agg_cover,
#                    shrub, tree)
# # name the layers
# names(landscape) <- c("SWE", "SND", "Asp_sin", "Asp_cos", "Roughness", "RAP_bio", 
#                       "RAP_cover", "Srub", "Tree")
# plot(landscape)
# 
# # Save ----
# writeRaster(landscape, "Covar_org/landscape_2018_01.tif", 
#             options="INTERLEAVE=BAND", overwrite=FALSE)
# 
# # writeRaster(rap, "Covar_org/rap_2018.tif", 
# #             options="INTERLEAVE=BAND", overwrite=TRUE)
# 
# 
# # BJS: since you're writing to a single file, NOW you can use a brick when
# # you reload
# #landscape <- brick("Covar_org/landscape_201804.tif")
# 
#
# land <- lapply(rst, function(r)
#   resample(r, ut_rast, method = 'bilinear'))