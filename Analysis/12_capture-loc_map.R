#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- January 2022 ---------------X
#########################################X
#--- map of utah w/ capture locations ---X
##------ Last edited: 06/21/2022  -------X
#########################################X

# Clean env
rm(list = ls())
gc()

# load in tables from db
library(dplyr)
library(lubridate)
library(stringr)
library(sf)
library(raster)
library(DBI)
library(cowplot)
library("rnaturalearth")

options(scipen=999)

# EPSG for CRS
wgs <- 4326
utm <- 32612 

# Load in US map
data("us_states", package = "spData")

## Load in data ----
#capture <- read.csv("../../Data/PH_data/db_data/20220525_capture.csv", header = T)
#a. Query the db 
ph_db <- dbConnect(drv = RSQLite::SQLite(), # wherever I saved db
                   "../../Data/PH_data/pronghorn.db")

#b. Pull tables
prong <- dbGetQuery(ph_db, "SELECT * FROM pronghorn;")
tracking <- dbGetQuery(ph_db, "SELECT * FROM tracking;")

# need to get first location point for each individual
capture <- prong %>% 
  left_join(tracking, by = "ID") %>% 
  filter(sex == "F") %>% 
  dplyr::select(ID,
                x,
                y,
                dt,
                unit) %>% 
  group_by(ID) %>% 
  filter(row_number()==1) %>% 
  mutate(year = year(dt)) %>% 
  filter(year == "2021")

# Plot ----
## Base Map ----
# Load in US
us_states = st_transform(us_states, crs = 2163)

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
                  res = c(10000, 10000),
                  ext = new_ext)

# I'm also going to add the Great Salt Lake (to make the map look nice)
lake <- st_read(dsn = dir, 
                layer = "salt_lake") %>% 
  st_transform(crs = st_crs(utm))
plot(lake)


# I also created a hillshade raster that makes a relief map and makes it look nice
#   (the raster resolution from the DEM I derived this from was really high
#   so I aggregated it by a factor of 10 to cut down unnecessary processing)
hill_low_res <- raster("../../Analysis/VAW Study map/base/hill_low_res.tif")

# Then mask/clip it to study area
hill_clip <- raster::mask(hill_low_res, as_Spatial(utah)) 
plot(hill_clip)

# Make the hillshade raster into a dataframe so ggplot can handle it
hill_df <- as.data.frame(hill_low_res, xy = TRUE)

# check what the "layer" column name is called (for ggplot later)
colnames(hill_df) # hill_low_res

# Make 10x10 km grid ----
grid  <-  stars::st_as_stars(st_bbox(utah), dx = 10000, dy = 10000)
grid <-  st_as_sf(grid)
plot(grid)

# Make the hillshade raster into a dataframe so ggplot can handle it
grid_df <- as.data.frame(grid, xy = TRUE)

# make the basemap into an object
base <- ggplot() +
  geom_raster(data = hill_df, 
              mapping = aes(x = x, y = y, fill = hill_low_res)) + 
  # change the hillshade color gradient to grey
  scale_fill_gradient(low = "snow", high = "gray40",
                      # make NA values (like the ones masked out) transparent
                      na.value = "transparent",
                      guide = "none") +
  geom_sf(data = utah, fill = NA, color = "black") +
  geom_sf(data = lake, fill = "skyblue1", col = "skyblue1") +
  xlim(c(new_ext@xmin, new_ext@xmax)) +
  ylim(c(new_ext@ymin, new_ext@ymax)) +
  theme(panel.background = element_rect(fill = NA, colour = "black")) +
  # this is so the axes coordinates are in UTM system and not latlon
  coord_sf(datum = st_crs(utm)) 

# add grid to map
grid_map <- base +
  geom_sf(data = grid, fill = NA, color = alpha("gray46", 0.2)) +
  #geom_sf(data = grid, fill = NA, color = NA) +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1.2)) +
  oord_sf(xlim = c(-4300000, 45), ylim = c(30, 73), expand = FALSE)
# this is so the axes coordinates are in UTM system and not latlon
coord_sf(datum = st_crs(utm))

# add points per individual capture
map1 <- grid_map +
  # add in first point for PH
  geom_point(aes(capture$x, capture$y, col = as.factor(capture$year))) +
  # labels
  labs(col = "Year of Capture", fill = "", 
       y = "UTM Y",
       x = "UTM X") +
  ggtitle("Pronghorn Capture Locations") +
  # adjust title
  theme(plot.title = element_text(hjust = 0.5))  +
  ggspatial::annotation_scale()
  # theme(legend.position = c(0.4, 0.05),
  #      legend.direction = "horizontal",
  #      legend.key.width = unit(10, "mm"))

# add points per individual capture
map2 <- grid_map +
  # add in first point for PH
  geom_point(aes(capture$x, capture$y, col = as.factor(capture$year))) +
  # labels
  labs(col = "Year of Capture", fill = "", 
       y = "UTM Y",
       x = "UTM X") +
 # ggtitle("Pronghorn Capture Locations") +
  # adjust title
  theme(plot.title = element_text(hjust = 0.5))  +
  ggspatial::annotation_scale()
# theme(legend.position = c(0.4, 0.05),
#      legend.direction = "horizontal",
#      legend.key.width = unit(10, "mm"))

lake_inset <-  ggplot() +
  geom_sf(data = lake, fill = "skyblue1") +
  coord_sf(datum = st_crs(utm)) +
  theme_void() +
  labs(title = "Great Salt Lake") +
  theme(plot.title = element_text(hjust = -12, vjust = -11, size = 10))  

# inset map ----
# grid_ext  <-  stars::st_as_stars(st_bbox(utah), dx = 100000, dy = 100000)
# utah_bb = st_as_sfc(st_bbox(utah))
# 
# inset <-  ggplot() +
#   geom_sf(data = us_states, fill = "grey", color = "black") +
#   geom_sf(data = utah_bb, fill = "NA", color = "red", size = 1.2) +
#   # this is so the axes coordinates are in UTM system and not latlon
#   coord_sf(datum = st_crs(utm)) +
#   theme_void()
# 
# join maps together
gg_inset_map1 <-  ggdraw() +
  draw_plot(map1) +
 draw_plot(lake_inset, x = 0.7, y = 0.27, width = 0.1, height = 0.1)

gg_inset_map1

# join maps together
gg_inset_map2 <-  ggdraw() +
  draw_plot(map2) +
  draw_plot(lake_inset, x = 0.7, y = 0.27, width = 0.1, height = 0.1)

gg_inset_map2


# # Save ----
# ggsave("UT_base.tiff", 
#        path = dir)
# 

outdir <- "../../Figures_and_Results/"
maps <- paste0(outdir, "Maps")
if(!dir.exists(outdir)){dir.create(maps)}

ggsave(plot = gg_inset_map1,
       filename = "UT_capture-loc_map_lake.tiff",
       path = maps,
       # you can change to png or tif or whatever you need
       device = "tiff")

ggsave(plot = gg_inset_map2,
       filename = "UT_capture-loc_map-nt_lake.tiff",
       path = maps,
       # you can change to png or tif or whatever you need
       device = "tiff")
# Done!



# make the basemap into an object
base <- ggplot() +
  geom_raster(data = hill_df, 
              mapping = aes(x = x, y = y, fill = "")) + 
  # # change the hillshade color gradient to grey
  # scale_fill_gradient(low = "snow", high = "gray40",
  #                     # make NA values (like the ones masked out) transparent
  #                     na.value = "transparent",
  #                     guide = "none") +
 # geom_sf(data = utah, fill = NA, color = "black") +
  geom_sf(data = lake, fill = "skyblue1", col = "skyblue1") +
  xlim(c(new_ext@xmin, new_ext@xmax)) +
  ylim(c(new_ext@ymin, new_ext@ymax)) +
 # theme(panel.background = element_rect(fill = NA, colour = "black")) +
  # this is so the axes coordinates are in UTM system and not latlon
  coord_sf(datum = st_crs(utm)) 

# # add grid to map
# grid_map <- base +
#   geom_sf(data = grid, fill = NA, color = alpha("gray46", 0.2)) +
#   #geom_sf(data = grid, fill = NA, color = NA) +
#   theme(panel.background = element_rect(fill = NA, colour = "black", size = 1.2)) +
#   # this is so the axes coordinates are in UTM system and not latlon
#   coord_sf(datum = st_crs(utm))

# add points per individual capture
capture <- readRDS("cleaned_data/comb_dat_20220524.rds") 

cap <- capture %>% 
  mutate(year = year(dt),
         month = month(dt)) %>% 
  filter(year == "2021",
         Sex == "F")

# filter out summer gps points
capture2 <- cap %>% 
  filter(month == "7")

map1 <- base +
  # add in first point for PH
  geom_point(aes(capture2$x, capture2$y, col = as.factor(capture2$CaptureUnit))) +
  # labels
  labs(col = "", fill = "", 
       y = "",
       x = "") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# add points per individual capture
capture3 <- cap %>% 
  filter(month == "2")

map2 <- base +
  # add in first point for PH
  geom_point(aes(capture3$x, capture3$y, col = as.factor(capture3$CaptureUnit))) +
  # labels
  labs(col = "", fill = "", 
       y = "",
       x = "") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

lake_inset <-  ggplot() +
  geom_sf(data = lake, fill = "skyblue1") +
  coord_sf(datum = st_crs(utm)) +
  theme_void() +
  labs(title = "Great Salt Lake") +
  theme(plot.title = element_text(hjust = -12, vjust = -11, size = 10))  

# inset map ----
# grid_ext  <-  stars::st_as_stars(st_bbox(utah), dx = 100000, dy = 100000)
# utah_bb = st_as_sfc(st_bbox(utah))
# 
# inset <-  ggplot() +
#   geom_sf(data = us_states, fill = "grey", color = "black") +
#   geom_sf(data = utah_bb, fill = "NA", color = "red", size = 1.2) +
#   # this is so the axes coordinates are in UTM system and not latlon
#   coord_sf(datum = st_crs(utm)) +
#   theme_void()
# 
# join maps together
gg_inset_map1 <-  ggdraw() +
  draw_plot(map1) +
  draw_plot(lake_inset, x = 0.7, y = 0.27, width = 0.1, height = 0.1)

gg_inset_map1

# join maps together
gg_inset_map2 <-  ggdraw() +
  draw_plot(map2) +
  draw_plot(lake_inset, x = 0.7, y = 0.27, width = 0.1, height = 0.1)

gg_inset_map2


# # Save ----
# ggsave("UT_base.tiff", 
#        path = dir)
# 

outdir <- "../../Figures_and_Results/"
maps <- paste0(outdir, "Maps")
if(!dir.exists(outdir)){dir.create(maps)}

ggsave(plot = grid_map,
       filename = "UT_grid-map.tiff",
       path = maps,
       # you can change to png or tif or whatever you need
       device = "tiff")

ggsave(plot = map1,
       filename = "UT_summer_locs.png",
       path = maps,
       bg = "transparent",
       # you can change to png or tif or whatever you need
       device = "png")

ggsave(plot = map2,
       filename = "UT_winter_locs.png",
       path = maps,
       bg = "transparent",
       # you can change to png or tif or whatever you need
       device = "png")
# Done!


