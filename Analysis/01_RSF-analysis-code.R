# RSF preparation and analysis code for Winter et al. XXXX

# "Forecasting Animal Distribution through Individual
#  Habitat Selection: Insights for Population Inference and
#  Transferable Predictions"

# Clean up R environment 
rm(list = ls())
gc()

set.seed(123)

# Load packages and functions
source("Analysis/xx_funs.R")

# pronghorn data
prong <- readRDS("Data/Processed/20220813_cleaned-data.rds") 

# sample a few individuals
indiv <- prong %>% 
  dplyr::select(ID) %>% 
  distinct() %>% 
  sample_n(size = 20)

ph_dat <- prong %>% 
  dplyr::filter(ID %in% indiv$ID)

rm(prong)

# covariates
dir <- "../../../../Box/Avgar Lab on WILD/UtahEnvironmentalCovariates/VW_Stacked_Covariates/30X30_covariates/"
landscapes <- list.files(dir, full.names = TRUE)

# Additional covaraites ----
# Road data path
roads <- "../../../../Box/Avgar Lab on WILD/UtahBarriers/Roads/roads_pa_proj.tif"

# Drought ----
box_dir <- "../../../../Box/Avgar Lab on WILD/UtahEnvironmentalCovariates/"
drought_dir <- paste0(box_dir, "Drought/drought_PDSI/monthly_composite/")
daymet <- list.files(drought_dir, full.names = TRUE, pattern = "mean")
daymet <- daymet[13:28]

# Input into data frame to get dates for looping
dates <- data.frame(
  filename = landscapes,
  year = substr(stringr::word(landscapes, 5, 5, "_"), start = 1, stop = 4),
  month = substr(stringr::word(landscapes, 5, 5, "_"), start = 5, stop = 6)
) %>%
  mutate(
    leap = case_when(year == 2020 ~ TRUE, TRUE ~ FALSE),
    start_date = "01"
  ) %>%
  mutate(
    end_date = case_when(
      month %in% c("04", "11") ~ "30",
      month == "07" ~ "31",
      month == "02" & leap ~ "29",
      month == "02" & !leap ~ "28"
    )
  )

# Initialize an empty list to store the processed data
processed_data_list <- list()

# Loop over covariates
for (l in 1:length(landscapes)) {
  cat("Processing landscape:", l, "\n")
  
  # Load in landscape raster
  landscape <- stack(landscapes[l])
  
  # load in corresponding drought
  attr <- raster::raster(daymet[l])
  
  if ("07" %in% substr(names(landscape), start = 15, stop = 16)) {
    names(landscape) <- c(
      "elevation", "asp_sin", "asp_cos", "roughness", "bio",
      "herb", "shrub", "tree"
    )
  } else {
    names(landscape) <- c(
      "elevation", "snd", "asp_sin", "asp_cos", "roughness", "bio",
      "herb", "shrub", "tree"
    )
  }
  
  # create an object of desired dates for filtering gps data
  date_row <- dates[dates$filename == landscapes[l], ]
  
  # add pronghorn gps information
  ph <- ph_dat %>%
    dplyr::select(ID, dt, x, y) %>%
    filter(
      dt >= paste(date_row$year, date_row$month, date_row$start_date, sep = "-") &
        dt <= paste(date_row$year, date_row$month, date_row$end_date, sep = "-")
    ) %>%
    arrange(dt)
  
  # Extent
  # Get the mid point for each indiv in each 'season'
  indiv <- unique(ph$ID)
  ph$mid_x <- NA
  ph$mid_y <- NA
  ph3 <- data.frame()
  
  for (i in 1:length(indiv)) {
    # Filter by individuals
    dat <- filter(ph, ID == indiv[i]) %>%
      mutate(
        # find target date around mid month for each
        target_date = ymd_hms(paste0(year(dt), date_row$month, "-15 12:00:00"))
      ) %>%
      # Denote how far dates are from mid month
      mutate(dif = abs(difftime(dt, target_date)))
    # save location point w/ least distance from mid month
    dat$mid_x <- dat[which.min(dat$dif), ]$x
    dat$mid_y <- dat[which.min(dat$dif), ]$y
    
    ph3 <- rbind(dat, ph3)
  }
  
  # Looping over indiv rasters
  # 1. Create empty lists for storage
  clip <- list()
  locs <- list()
  indiv <- unique(ph3$ID)
  locs_in <- list()
  
  # I want to create a raster of clipped covars available to each indiv
  #  within the extent to run RSF analysis 
  
  for (k in 1:length(indiv)) {
    # Subset individual data
    dat <- filter(ph3, ID == indiv[k])
    
    # Turn into a track_xyt
    locs[[k]] <- mk_track(dat, x, y, dt, crs = 32612)
    
    # Now we can use the extent we created above to cut out a small piece of this
    # raster
    clip[[k]] <- crop(landscape, make_extent(dat$mid_x[1], dat$mid_y[1]))
    
    # Extract values at used points in buffer
    locs_in[[k]] <- locs[[k]] %>%
      extract_covariates(landscape)
    
    # Extract values for available points
    avail <- as.data.frame(values(clip[[k]]))
    avail$case_ <- FALSE
    
    # Combine used and available
    used <- locs_in[[k]] %>%
      dplyr::select(-x_, -y_, -t_) %>%
      mutate(case_ = TRUE)
    
    # This is what you need for RSF analysis
    locs[[k]] <- rbind(used, avail)
    
    # Additional covaraites ----
    # Read the road raster data
    road_attr <- raster::raster(roads)
    
    # Crop road attribute raster to the extent of the individual's location
    road_attr_crop <- raster::crop(road_attr, make_extent(dat$mid_x[1], dat$mid_y[1]))
    
    # Calculate the average road attribute value for the individual
    locs[[k]]$m_road <- raster::cellStats(road_attr_crop, stat = "mean", na.rm = TRUE)
    
    # Calculate Average PDSI  ----
    # List covariate files
    attr_buff_crop <- raster::crop(attr, make_extent(dat$mid_x[1],dat$mid_y[1]))
    locs[[k]]$m_PDSI <- raster::cellStats(attr_buff_crop, stat = "mean", na.rm = TRUE)
  }
  
  names(locs) <- indiv
  
  mod_dat <- locs %>% 
    # bind rows of df
    bind_rows(.id = "ID") %>% 
    # add weights column & input 1 for used and 5000 for available
    mutate(w = as.numeric(ifelse(case_ == "TRUE", 1, 5000))) %>% 
    # save month and year column
    mutate(month = date_row$month,
           year = date_row$year)
  
 #  Store processed data in the list
  processed_data_list[[l]] <- mod_dat
}

# Combine all processed dataframes into one
mod_dat2 <- do.call(rbind, processed_data_list)

# Save
saveRDS(mod_dat2, "Data/DataProcessing.rds")

# Need a clean up
rm(list = ls())
gc()

# Load packages and functions
source("Analysis/xx_funs.R")

# Preparing data for RSF
mod_dat <- readRDS("Data/DataProcessing.rds")

# Scaling covaraites ----
# Specify the columns to be scaled
cols_to_scale <- c("elevation", "asp_sin", "asp_cos", "roughness",
                   "bio", "herb", "shrub", "tree", "snd")

# Apply scaling transformation
RSF_dat <- mod_dat %>%
  mutate(across(all_of(cols_to_scale), list(scaled = ~ as.vector(scale(.))), 
                .names = "scaled_{.col}"))

head(RSF_dat)

# Save output
saveRDS(RSF_dat, "Data/RSF_prep.rds")

# RSF ----
# Need a clean up
rm(list = ls())
gc()

# Load packages and functions
source("Analysis/xx_funs.R")

# Run RSF -----
RSF_dat <- readRDS("Data/RSF_prep.rds")

# Set up loop ----
# iterate over unique individual/month/year combinations
months <- unique(RSF_dat$month)
years <- unique(RSF_dat$year)

# DF for output ----
glm_df <- NULL
# glm_df <- data.frame(ID = NA,
#                      # Fill with betas
#                      Intercept_beta = NA,
#                      Elev_beta = NA,
#                      Asp_sin_beta = NA,
#                      Asp_cos_beta = NA,
#                      Rough_beta = NA,
#                      Herb_beta = NA,
#                      Shrub_beta = NA,
#                      Tree_beta = NA,
#                      # Fill with st. errors
#                      Intercept_stder = NA,
#                      Elev_stder = NA,
#                      Asp_sin_stder = NA,
#                      Asp_cos_stder = NA,
#                      Rough_stder = NA,
#                      Herb_stder = NA,
#                      Shrub_stder = NA,
#                      Tree_stder = NA,
#                      # month and year of GLM
#                      month = NA,
#                      year = NA,
#                      # Available points
#                      avail_pts = NA,
#                      m_elev = NA,
#                      m_snd = NA,
#                      m_a.sin = NA,
#                      m_a.cos = NA,
#                      m_rough = NA,
#                      m_bio = NA,
#                      m_herb = NA,
#                      m_shrub = NA,
#                      m_tree = NA,
#                      m_road = NA,
#                      m_pdsi = NA,
#                      row.names = NULL)

# forumla

base <- case_ ~ scaled_elevation + scaled_roughness + scaled_herb + scaled_shrub + 
  scaled_tree + scaled_asp_sin + scaled_asp_cos

# Loop over years
for (y in years) {
  
  # print status
  print(paste(y))
  
  # filter by year
  x <- RSF_dat %>% 
    filter(year == y)
  
  # Loop over months
  for (m in months) {
    
    # print status
    print(paste(m))
    
    # filter by month
    dat <- x %>% 
      filter(month == m)
    
    indiv <- unique(dat$ID)
    
    for (i in indiv) {
      # Subset individual data
      unq <- dat %>% 
        filter(ID == i)
      
      mod <- glm(data = unq, formula = base, weights = w, family = binomial)
      
      # save outputs from model
      temp <- data.frame(ID = i,
                         Intercept_beta = coef(summary(mod))[, "Estimate"]["(Intercept)"],
                         Elev_beta = coef(summary(mod))[, "Estimate"]["scaled_elevation"],
                         Asp_sin_beta = coef(summary(mod))[, "Estimate"]["scaled_asp_sin"],
                         Asp_cos_beta = coef(summary(mod))[, "Estimate"]["scaled_asp_cos"],
                         Rough_beta = coef(summary(mod))[, "Estimate"]["scaled_roughness"],
                         Herb_beta = coef(summary(mod))[, "Estimate"]["scaled_herb"],
                         Shrub_beta = coef(summary(mod))[, "Estimate"]["scaled_shrub"],
                         Tree_beta = coef(summary(mod))[, "Estimate"]["scaled_tree"],
                         Intercept_stder = coef(summary(mod))[, "Std. Error"]["(Intercept)"],
                         Elev_stder = coef(summary(mod))[, "Std. Error"]["scaled_elevation"],
                         Asp_sin_stder = coef(summary(mod))[, "Std. Error"]["scaled_asp_sin"],
                         Asp_cos_stder = coef(summary(mod))[, "Std. Error"]["scaled_asp_cos"],
                         Rough_stder = coef(summary(mod))[, "Std. Error"]["scaled_roughness"],
                         Herb_stder = coef(summary(mod))[, "Std. Error"]["scaled_herb"],
                         Shrub_stder = coef(summary(mod))[, "Std. Error"]["scaled_shrub"],
                         Tree_stder = coef(summary(mod))[, "Std. Error"]["scaled_tree"],
                         month = unique(unq$month),
                         year = unique(unq$year),
                         avail_pts = sum(unq$case_ == "FALSE"),
                         m_elev = mean(unq$elevation),
                         m_snd = mean(unq$snd),
                         m_a.sin = mean(unq$asp_sin),
                         m_a.cos = mean(unq$asp_cos),
                         m_rough = mean(unq$roughness),
                         m_bio = mean(unq$bio),
                         m_herb = mean(unq$herb),
                         m_shrub = mean(unq$shrub),
                         m_tree = mean(unq$tree),
                         m_road = mean(unq$m_road),
                         m_pdsi = mean(unq$m_pdsi),
                         row.names = NULL)
      
      # Combine together df's
      glm_df <- rbind(temp, glm_df)
    }
  }
}

# Save output ----
# Move columns
glm_df <- glm_df %>% 
  relocate(month, year, .before = Intercept_beta) 

# remove last row of NA's
last_row_index <- nrow(glm_df)
glm_df <-  glm_df %>%
  slice(-last_row_index)

# Adding in movement behavior to data frame ----
# Join in movement behaviors
#a. Query the db 
ph_db <- dbConnect(drv = RSQLite::SQLite(), # wherever I saved db
                   "Data/Processed/pronghorn.db")

#b. Pull tables
prong <- dbGetQuery(ph_db, "SELECT * FROM pronghorn;")
status <- dbGetQuery(ph_db, "SELECT * FROM status;") 

# intersect the IDs to limit to only indiv in fin
combo <-  intersect(glm_df$ID, prong$ID)

# edit ph table
ph <-  prong  %>% 
  # filter IDs
  filter(ID %in% combo) %>% 
  # Join w/ status table
  left_join(status, by = "ID") %>% 
  # Select only needed columns
  dplyr::select(- CollarID)  

# modify column classifications for joining
glm_df$year <- as.character(glm_df$year)
glm_df$month <- as.character(glm_df$month)
ph$tendency <- as.character(ph$tendency)
ph$ID <- as.factor(ph$ID)

# Create a 'summer' and 'winter' movement class
fin <- glm_df %>%
  mutate(
    mig_season = case_when(
      month %in% c("04", "07") ~ "S",
      month %in% c("02", "11") ~ "W",
      TRUE ~ NA_character_
    )
  ) %>%
  # join data frame with behvaiors
  left_join(ph, by = c("ID", "year", "mig_season")) %>%
  # create columns for MEM
  mutate(
    is.mig = as.numeric(tendency == "mig"),
    is.res = as.numeric(tendency == "res"),
    is.unk_mig = as.numeric(tendency == "unk"),
    is.Adult = as.numeric(age_class == "adult"),
    is.age_unk = as.numeric(age_class == "U"),
    is.Male = as.numeric(sex == "M"),
    is.Female = as.numeric(sex == "F"),
    is.unk = as.numeric(sex == "U"),
    is.AntelopeIsland = as.numeric(unit == "Antelope Island"),
    is.Dugway = as.numeric(unit %in% c("Dugway", "West Desert")),
    is.NorthSlope = as.numeric(unit == "North Slope"),
    is.Plateau = as.numeric(unit == "Plateau"),
    is.SouthwestDesert = as.numeric(unit == "Southwest Desert"),
    is.WestDesert = as.numeric(unit == "West Desert"),
    is.Woodruff = as.numeric(unit %in% c("Cache", "Woodruff"))
  ) %>%
  mutate(
    is.Male = as.numeric(sex == "M"),
    is.Female = as.numeric(sex == "F"),
    is.unk = as.numeric(sex == "U"),
    is.AntelopeIsland = as.numeric(unit == "Antelope Island"),
    is.Woodruff = as.numeric(unit %in% c("Cache", "Woodruff")),
    is.Dugway = as.numeric(unit %in% c("Dugway", "West Desert")),
    is.NorthSlope = as.numeric(unit == "North Slope"),
    is.Plateau = as.numeric(unit == "Plateau"),
    is.SouthwestDesert = as.numeric(unit == "Southwest Desert"),
    clim.reg = case_when(
      unit == "Antelope Island" ~ "North Central",
      unit == "Book Cliffs" ~ "Uinta Basin",
      unit == "Cache" ~ "North Central",
      unit == "Dugway" ~ "Western",
      unit == "North Slope" ~ "Northern Mountains",
      unit == "Plateau" ~ "South Central",
      unit == "Southwest Desert" ~ "Western",
      unit == "West Desert" ~ "Western",
      unit == "Woodruff" ~ "Northern Mountains"
    ),
    is.NorthCentral = as.numeric(unit == "Antelope Island"),
    is.NorthMnt = as.numeric(unit %in% c("North Slope", "Woodruff")),
    is.Western = as.numeric(unit %in% c("Southwest Desert", "West Desert")),
    is.SouthCentral = as.numeric(unit == "Plateau"),
    is.mountain = as.numeric(unit %in% c("Antelope Island", "Book Cliffs", "Cache", "North Slope", "Woodruff"))
  ) %>%
  # create columns
  mutate(is.Winter = case_when(month == "02" ~ 1, TRUE ~ 0),
         is.Spring = case_when(month == "04" ~ 1, TRUE ~ 0),
         is.Summer = case_when(month == "07" ~ 1, TRUE ~ 0),
         is.Fall = case_when(month == "11" ~ 1, TRUE ~ 0),
         is.2018 = case_when(year == "2018"~ 1, TRUE ~ 0),
         is.2019 = case_when(year == "2019" ~ 1, TRUE ~ 0),
         is.2020 = case_when(year == "2020" ~ 1, TRUE ~ 0),
         is.2021 = case_when(year == "2021" ~ 1, TRUE ~ 0)) %>% 
  relocate(sex, unit, .before = Intercept_beta) %>%
  dplyr::select(-mig_season) %>%
  distinct()

# Check
head(fin)

# Save output 
saveRDS(fin, "Data/RSF_data.rds")

# Done! Data now prepared for step 2: Mixed effect models
