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

# Load in shape of utah
dir <- "../../Analysis/VAW Study map/base"
# EPSG for CRS
wgs <- 4326
utm <- 32612 
utah <- st_read(dsn = dir,
                layer = "utah") %>% 
  st_transform(crs = st_crs(utm)) 
plot(utah)
crs(utah)
ext <- extent(utah)

# List covariate files
#dir <- "../../../Avgar Lab on WILD/UtahEnvironmentalCovariates/VW_Stacked_Covariates/10x10_Covariate_stacks/2021_stacks/"
dir <- "https://usu.app.box.com/folder/165641313465"
landscapes <- list.files(dir, full.names = T)

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

fall <- stack(landscapes[3])
names(fall) <- c("elevation", "snd", "asp_sin", "asp_cos", "roughness", "bio",
                 "herb", "shrub", "tree") 


# Function to calculate harmonic mean
harm <- function(x, na.rm = FALSE) {
  return(1/mean(1/x, na.rm = na.rm))
}

# Load in ph data
ph_dat <- readRDS("Data/Processed/comb_dat_20220524.rds")

# Winter ----
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

# Filter out apprpriate season
cents_wint <- cents %>% 
  filter(season == "winter")

## Step 2: ----
# get available data
avail <- as.data.frame(winter, xy = TRUE) %>% 
  mutate(case = 0,
         cell = 1:nrow(.)) %>% 
  filter(!if_any(everything(), is.na))

## Step 3: ----
# Get used data
used_cells <- cellFromXY(winter, cbind(cents$hm_x, cents$hm_y))
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
wint_rsf <- glm(case ~ elevation + I(elevation^2) + snd + asp_sin + asp_cos + roughness + bio + herb + shrub + tree,
                data = rsf_dat, family = binomial, weights = weight)
summary(wint_rsf)

## Step 6: ----
# Predict with RSF
map <- avail
# Linear predictor
map$lp <- predict(m, newdata = map, type = "link")
# Exponential Habitat Selection Function
map$ehsf <- exp(map$lp)
# Normalize
map$ehsf <- map$ehsf/sum(map$ehsf, na.rm = TRUE)

# Plot
ggplot(map, aes(x = x, y = y, fill = log(ehsf))) +
  geom_raster() +
  geom_sf(data = utah, fill = NA, color = "red", inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_viridis_c() +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw()

# Summer ----
# Treating 'summer' as an example
cents_summ <- cents %>% 
  filter(season == "summer")

# Available data
avail <- as.data.frame(summer, xy = TRUE) %>% 
  mutate(case = 0,
         cell = 1:nrow(.)) #%>% 
# filter(!if_any(everything(), is.na))

# Used data
used_cells <- cellFromXY(summer, cbind(cents$hm_x, cents$hm_y))
used <- avail %>%
  filter(cell %in% used_cells) %>% 
  mutate(case = 1)

used <- avail[used_cells,] %>% 
  mutate(case = 1)

# Combine
rsf_dat <- rbind(used, avail) %>% 
  mutate(weight = case_when(
    case == 0 ~ 1e5,
    case == 1 ~ 1
  ))

# Fit RSF
m <- glm(case ~ elevation + I(elevation^2) + asp_sin + asp_cos + roughness + herb + shrub + tree,
         data = rsf_dat, family = binomial, weights = weight)
summary(m)

# Predict with RSF
map <- avail
# Linear predictor
map$lp <- predict(m, newdata = map, type = "link")
# Exponential Habitat Selection Function
map$ehsf <- exp(map$lp)
# Normalize
map$ehsf <- map$ehsf/sum(map$ehsf, na.rm = TRUE)

# Plot
ggplot(map, aes(x = x, y = y, fill = log(ehsf))) +
  geom_raster() +
  geom_sf(data = utah, fill = NA, color = "red", inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_viridis_c() +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw()