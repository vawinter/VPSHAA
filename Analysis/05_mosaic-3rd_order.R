
rm(list = ls())
gc()

# Load packages ----
library(raster)

# Source functions
source("Analysis/xx_funs_predicting.R")

# Seasonal files ----
# Each file is a stack with layers:
# fem_res, fem_mig, male_res, male_mig
winter <- list.files("pred", pattern = glob2rx("*_winter_*.tif"),
                     full.names = TRUE)
spring <- list.files("pred", pattern = glob2rx("*_spring_*.tif"),
                     full.names = TRUE)
summer <- list.files("pred", pattern = glob2rx("*_summer_*.tif"),
                     full.names = TRUE)
fall <- list.files("pred", pattern = glob2rx("*_fall_*.tif"),
                   full.names = TRUE)

# Mosaic ----
mos_df <- expand.grid(sex = c("f", "m"),
                      status = c("res", "mig"),
                      season = c("winter", "spring", "summer", "fall"),
                      stringsAsFactors = FALSE)

# # Mosaic ---- # 9/11 rerun
# mos_df <- expand.grid(sex = c("f"),
#                       status = c("res"),
#                       season = c("winter"),
#                       stringsAsFactors = FALSE)


# Get rid of winter migrants
mos_df <- mos_df[which(mos_df$status != "mig" | mos_df$season!= "winter"), ]

# Took 10481.49 s =
10481.49/60/60 #hours

# Took 9167.77 s =
9167.77/60/60 #hours

beginCluster(10)
system.time({
  lapply(1:nrow(mos_df), function(r) {
    cat("Map", r, "of", nrow(mos_df), "\n")
    mosaic_map(sex = mos_df$sex[r],
               status = mos_df$status[r],
               season = mos_df$season[r])
  })
})

# Null ----
# Load packages ----
rm(list = ls())
gc()
library(raster)

# Source functions
source("OtherScripts/00_fun_VAW.R")

# Seasonal files ----
# Each file is a stack with layers:
# fem_res, fem_mig, male_res, male_mig
winter <- list.files("pred/", pattern = glob2rx("*pnull_winter_*.tif"),
                     full.names = TRUE)
spring <- list.files("pred/", pattern = glob2rx("*pnull_spring_*.tif"),
                     full.names = TRUE)
summer <- list.files("pred/", pattern = glob2rx("*pnull_summer_*.tif"),
                     full.names = TRUE)
fall <- list.files("pred/", pattern = glob2rx("*pnull_fall_*.tif"),
                   full.names = TRUE)

# Mosaic ----
mos_df <- expand.grid(sex = c("f", "m"),
                      status = c("res", "mig"),
                      season = c("winter", "spring", "summer", "fall"),
                      stringsAsFactors = FALSE)


# Get rid of winter migrants
mos_df <- mos_df[which(mos_df$status != "mig" | mos_df$season!= "winter"), ]

# Took 10481.49 s =
10481.49/60/60 #hours

# Took 9167.77 s =
9167.77/60/60 #hours

beginCluster(10)
system.time({
  lapply(1:nrow(mos_df), function(r) {
    cat("Null_map", r, "of", nrow(mos_df), "\n")
    mosaic_map(sex = mos_df$sex[r],
               status = mos_df$status[r],
               season = mos_df$season[r])
  })
})
