# Creating 2nd order habitat selection maps
# Will multiply with 3rd order maps to hopefully get better estimates of 
# selection across UT


# Clean env
rm(list = ls())
gc()

# libraries
library(tidyverse)
library(lubridate)
library(raster)
library(rnaturalearth)
library(sf)

# Function to calculate harmonic mean
harm <- function(x, na.rm = FALSE) {
  return(1/mean(1/x, na.rm = na.rm))
}

# Data load in ----
## Load in ph data
ph_dat <- readRDS("Data/Processed/comb_dat_20220524.rds")

# Directories ----
## Load in shape of utah
dir1 <- "../../../../Box/Projects/Analysis/VAW Study map/base"
# EPSG for CRS
wgs <- 4326
utm <- 32612 
utah <- st_read(dsn = dir1,
                layer = "utah") %>% 
  st_transform(crs = st_crs(utm)) 
plot(utah)
crs(utah)
ext <- extent(utah)

## List covariate files ----
dir2 <- "../../../../Box/Avgar Lab on WILD/UtahEnvironmentalCovariates/VW_Stacked_Covariates/10x10_Covariate_stacks/2021_stacks/"
landscapes <- list.files(dir2, full.names = T)

# seperate per seson
winter <- stack(landscapes[1])
names(winter) <- c("elevation", "snd", "asp_sin", "asp_cos", "roughness", "bio",
                   "herb", "shrub", "tree") 


spring <- stack(landscapes[2])
names(spring) <- c("elevation", "snd", "asp_sin", "asp_cos", "roughness", "bio",
                   "herb", "shrub", "tree") 

summer <- stack(landscapes[3])
names(summer) <- c("elevation", "asp_sin", "asp_cos", "roughness", "bio",
                   "herb", "shrub", "tree") 

fall <- stack(landscapes[4])
names(fall) <- c("elevation", "snd", "asp_sin", "asp_cos", "roughness", "bio",
                 "herb", "shrub", "tree") 

# Set up loop ----
## Make a list of seasons
season <- c("fall", "winter", "spring")

## Make list of rasters
seas_rast <- c(fall, winter, spring)
names(seas_rast) <- season

# Season centroids ----
## Step 1: ----
# Centroid for each individual
cents <- ph_dat %>% 
  mutate(season = case_when(
    month(dt) == 2 ~ "winter",
    month(dt) == 4 ~ "spring",
    month(dt) == 7 ~ "summer",
    month(dt) == 11 ~ "fall",
    TRUE ~ NA_character_
  ),
  year = year(dt)) %>% 
  # Drop data not assigned a season
  filter(!is.na(season)) %>% 
  group_by(ID, year, season) %>% 
  summarize(n = n(),
            mean_x = mean(x),
            mean_y = mean(y),
            med_x = median(x),
            med_y = median(y),
            hm_x = harm(x),
            hm_y = harm(y))


# Set up list
season_plots <- list()
season_habsel <- list()

# Loop -----
for(s in 1:length(season)){
  
# Filter out appropriate season
cents_split <- cents %>% 
  filter(season == s)

# # Format model for summer missing snd
# if(!(seas == 2)) {
#   rsf_model <- case ~ elevation + I(elevation^2) + snd + asp_sin + asp_cos + 
#     roughness + bio + herb + shrub + tree
#   
# } else {
#   
#   rsf_model <- case ~ elevation + I(elevation^2) + asp_sin + asp_cos + 
#     roughness + bio + herb + shrub + tree
# }


## Step 2: ----
# get available data
avail <- as.data.frame(seas_rast[[s]], xy = TRUE) %>% 
  mutate(case = 0,
         cell = 1:nrow(.)) %>% 
  filter(!if_any(everything(), is.na))

## Step 3: ----
# Get used data
used_cells <- cellFromXY(seas_rast[[s]], cbind(cents_split$hm_x, cents_split$hm_y))
used <- avail %>%
  filter(cell %in% used_cells) %>% 
  mutate(case = 1) %>% 
  distinct()

## Step 4: ----
# Combine into one df
rsf_dat <- rbind(used, avail) %>% 
  # give weights to available points
  mutate(weight = case_when(
    case == 0 ~ 1e5,
    case == 1 ~ 1
  ))

## Step 5: ----
# Fit RSF
rsf <- glm(data = rsf_dat, formula = case ~ elevation + I(elevation^2) + snd + asp_sin + asp_cos + 
           roughness + bio + herb + shrub + tree, weights = weight, family = binomial)
summary(rsf)

## Step 6: ----
# Predict with RSF
map <- avail
# Linear predictor
map$lp <- predict(rsf, newdata = map, type = "link")
# Exponential Habitat Selection Function
map$ehsf <- exp(map$lp)
# Normalize
map$ehsf <- map$ehsf/sum(map$ehsf, na.rm = TRUE)

# Plot
season_plots[[s]] <- ggplot(map, aes(x = x, y = y, fill = log(ehsf))) +
  geom_raster() +
  geom_sf(data = utah, fill = NA, color = "red", inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_viridis_c() +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(s) +
  theme_bw()

# end loop
}

# Done!