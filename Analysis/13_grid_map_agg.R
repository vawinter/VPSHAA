#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- February 2022  -------------X
#########################################X
#---- Creating a map of UT w/ grid --- --X
#########################################X

# Clean env
rm(list = ls())
gc()

# Set Up ----
library(tidyverse)
library(dplyr)
library(raster)
library(ggplot2)
library(sf)
library("rnaturalearth")

# EPSG for CRS
wgs <- 4326
utm <- 32612 

## Base Map ----
# Load in shape of utah
dir <- "../../Analysis/VAW Study map/base"
utah <- st_read(dsn = dir,
                layer = "utah") %>% 
  st_transform(crs = st_crs(utm)) 
plot(utah)
crs(utah)
ext <- extent(utah)

# 100km buffer
buf <- 100 * 100
new_ext <- extent(ext[1] - buf, ext[2] + buf, ext[3] - buf, ext[4] + buf)
new_ext

# Rasterize template raster
ut_rast <- raster(crs = crs(utah),
                  res = c(10, 10),
                  ext = new_ext)

plot(ut_rast)

writeRaster(ut_rast, "../template_raster_100.tif", format = "GTIFF")

# test raster
land1 <- raster("../DEM/rough_ut.tif")
plot(land1)

agg_tst <- projectRaster(land1, ut_rast, method = "bilinear", res = 10)
plot(agg_tst)
df <-  as.data.frame(land1, xy = TRUE)
ext <- extent(land1)
writeRaster(agg_test, "../template_raster_100.tif", format = "GTIFF")


# placing a grid over ----
# 100km buffer
# Make 10x10 km grid ----
grid  <-  stars::st_as_stars(st_bbox(land1), dx = 10000, dy = 10000)
grid <-   st_as_sf(grid)
plot(grid)

# Make the hillshade raster into a dataframe so ggplot can handle it
grid_df <- as.data.frame(grid, xy = TRUE)
# 
# # Rasterize
# buff <- raster(crs = crs(land1),
#                   res = c(10, 10),
#                   ext = ext)

base <- ggplot() +
  geom_raster(data = df, 
              mapping = aes(x = x, y = y, fill = rough)) + 
  # # change the hillshade color gradient to grey
  # scale_fill_gradient(low = "snow", high = "palegreen",
  #                     # make NA values (like the ones masked out) transparent
  #                     na.value = "transparent") +
  geom_sf(data = grid, fill = NA, color = alpha("gray46", 0.2)) +
  xlim(c(ext@xmin, ext@xmax)) +
  ylim(c(ext@ymin, ext@ymax)) +
  theme(panel.background = element_rect(fill = NA, colour = "black")) +
  # this is so the axes coordinates are in UTM system and not latlon
  coord_sf(datum = st_crs(utm))

# iii. Load in layers ----
## a. Landscape stacks ----
dir1 <- "../Covar_org/"
# List covariate files
landscapes <- list.files(dir1, full.names = T)[!list.files(dir1) %in% c("landscape_201801.tif",
                                                                        "landscape_201901.tif",
                                                                        "landscape_202001.tif")]

## b. Roads ----
m_road <- raster("../../../Avgar Lab on WILD/UtahBarriers/Roads/roads_pa_proj.tif")

## c. Daymet ----
dir2 <- "../daymet_org/"
# List covariate files
daymet <- list.files(dir2, full.names = T)
day <-  stack(daymet[1])

## d. Stack and name all ----
all <- stack(land1, m_road, day)
names(all) <- c("Elevation", "SND", "Asp_sin", "Asp_cos", "Roughness", "RAP_bio",
                "RAP_cover", "Shrub", "Tree", "m_road", "prcp", "tmin", "tmax") 

plot(all)

# Plot ----
## Base Map ----
# Load in shape of utah
dir <- "../../Analysis/VAW Study map/base"
utah <- st_read(dsn = dir,
                layer = "utah") %>% 
  st_transform(crs = st_crs(utm)) 
plot(utah)
crs(utah)
ext <- extent(utah)

# 100km buffer
buf <- 100 * 100
new_ext <- extent(ext[1] - buf, ext[2] + buf, ext[3] - buf, ext[4] + buf)
new_ext

# Rasterize
ut_rast <- raster(crs = crs(utah),
                  res = c(10, 10),
                  ext = new_ext)

# I'm also going to add the Great Salt Lake (to make the map look nice)
lake <- st_read(dsn = dir, 
                layer = "salt_lake") %>% 
  st_transform(crs = st_crs(utm))
plot(lake)

# adding counties as well
count <-  st_read("../../Analysis/VAW Study map/VAW_Pronghorn/Utah_County_Boundaries-shp")

units <- c("CACHE", "BOX ELDER", "DAGGET", "RICH", "UINTA", "PIUTE")


# I also created a hillshade raster that makes a relief map and makes it look nice
#   (the raster resolution from the DEM I derived this from was really high
#   so I aggregated it by a factor of 10 to cut down unnecessary processing)
hill_low_res2 <- raster("../../Analysis/VAW Study map/base/hill_low_res.tif")

# testing soemthing
hill_low_res <- land1
plot(hill_low_res)
# Then mask/clip it to study area
hill_clip <- raster::mask(hill_low_res, as_Spatial(utah)) 
plot(hill_clip)

# Make the hillshade raster into a dataframe so ggplot can handle it
hill_df <- as.data.frame(hill_low_res, xy = TRUE)
hill_df2 <- as.data.frame(hill_low_res2, xy = TRUE)

# check what the "layer" column name is called (for ggplot later)
colnames(hill_df) # hill_low_res

# Make 10x10 km grid ----
grid  <-  stars::st_as_stars(st_bbox(land1), dx = 10000, dy = 10000)
grid <-  st_as_sf(grid)
plot(grid)

# Make the hillshade raster into a dataframe so ggplot can handle it
grid_df <- as.data.frame(grid, xy = TRUE)

# make the basemap into an object
base <- ggplot() +
  geom_raster(data = hill_df, 
              mapping = aes(x = x, y = y, fill = landscape_201802)) + 
  # # change the hillshade color gradient to grey
  # scale_fill_gradient(low = "snow", high = "palegreen",
  #                     # make NA values (like the ones masked out) transparent
  #                     na.value = "transparent") +
  geom_sf(data = utah, fill = NA) +
  xlim(c(new_ext@xmin, new_ext@xmax)) +
  ylim(c(new_ext@ymin, new_ext@ymax)) +
  theme(panel.background = element_rect(fill = NA, colour = "black")) +
  # this is so the axes coordinates are in UTM system and not latlon
  coord_sf(datum = st_crs(utm))

# # add in county boundaries
# base + 
#   geom_sf(data = count, fill = NA) +
#   geom_sf(data = lake, fill = "skyblue1", col = "skyblue1") +
#   # this is so the axes coordinates are in UTM system and not latlon
#   coord_sf(datum = st_crs(utm))

# add grid to map
grid_map <- base +
  geom_sf(data = grid, fill = NA, color = alpha("gray46", 0.2)) +
  theme(panel.background = element_rect(fill = NA, colour = "black")) #+
# this is so the axes coordinates are in UTM system and not latlon
# coord_sf(datum = st_crs(utm))
plot(grid_map)


# Save ----
ggsave("UT_base.tiff", 
       path = dir)

ggsave(plot = grid_map, 
       filename = "UT_grid.tiff", 
       path = dir,
       # you can change to png or tif or whatever you need
       device = "tiff")

# Done!

