# Spatial model predictions code for Winter et al. XXXX

# "Forecasting Animal Distribution through Individual
#  Habitat Selection: Insights for Population Inference and
#  Transferable Predictions"

# V. Winter fit pronghorn HSFs with selection strength as a function
# of availability in a 10km x 10km window. Now we want to create
# habitat selection map at 30m x 30m resolution, where the betas
# vary within each 10km x 10km pixel across Utah.

# We want predictions for 4 different seasons:
#   - Winter 2021 (represented by February)
#   - Spring 2021 (represented by April)
#   - Summer 2021 (represented by July)
#   - Fall 2021 (represented by November)

# And predictions for both sexes and both migratory statuses.

# Workflow:
# For each season:
#   1. Predict betas for a 10km cell
#   2. Use betas to predict selection strength in 30m cells within 10km cell


# define 30x30 directory
dir30 <- "../../../../Box/Avgar Lab on WILD/UtahEnvironmentalCovariates/VW_Stacked_Covariates/30X30_covariates/"
dir_dr <- "../../../../Box/Avgar Lab on WILD/UtahEnvironmentalCovariates/Drought/drought_PDSI/monthly_composite/"

# Translate season to number ----
season2num <- function(season) {
  s <- data.frame(season = c("winter", "spring", "summer", "fall"),
                  number = c("02", "04", "07", "11"))
  return(s$number[which(s$season == season)])
}

# Scale and center ----
# x is a data.frame of habitat covariates to scale
scale_data <- function(x) {
  # Load scaling and centering parameters
  sc <- read.csv("20221019_mean-sd_all.csv")
  row.names(sc) <- sc$X
  sc <- as.data.frame(t(sc[, 2:ncol(sc)]))
  sc$name <- row.names(sc)

  # data.frame key to how to scale/center
  key <- data.frame(raster = c("Elevation", "SND", "Asp_sin", "Asp_cos",
                               "Roughness", "RAP_bio", "Herb", "Shrub",
                               "Tree", "PDSI", "Road"),
                    # VW edited to updated names ----
                    # need to change this to match sc df -----
                    scale = c("Elev", "log_SND", "Asp_sin",
                              "Asp_cos", "Rough",
                              "RAP_bio", "Herb",
                              "Shrub", "Tree",
                              "PDSI", "log_Road"),
                    model = c("m_SC_elev", "scaled_log_SND",
                              "m_SC_a.sin", "m_SC_a.cos",
                              "m_SC_rough", "m_SC_bio",
                              "m_SC_herb", "m_SC_shrub",
                              "m_SC_tree", "scaled_PDSI",
                              "scaled_log_Road"),
                    log = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
                            FALSE, FALSE, FALSE, FALSE, TRUE),
                    add = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1))

  # Loop over rows of data.frame
  for (i in 1:nrow(key)) {
    # If SND doesn't exist, make it all 0s.
    if (is.null(x[[key$raster[i]]])) {
      if (key$raster[i] == "SND") {
        x[["SND"]] <- 0
      }
    }
    # Scaling data index
    ind <- which(sc$name == key$scale[i])
    if (key$log[i]) {
      # Take log first, then scale
      x[[key$model[i]]] <- (log(x[[key$raster[i]]] + key$add[i]) -
                              sc$mean[ind])/sc$sd[ind]
    } else {
      # Just scale
      x[[key$model[i]]] <- (x[[key$raster[i]]] - sc$mean[ind])/sc$sd[ind]
    }
  }
  # Return
  return(x)
}

# Attach region variables ----
region <- function(x) {
  # Load region key
  reg_key <- read.csv("CONUS/CONUS_key.csv")
  # Join to x
  x.merge <- dplyr::left_join(x, reg_key, by = "region")
  # Assign regions
  # SOUTHEAST is intercept
  # UINTA BASIN is missing and will get captured by intercept
  # DIXIE is missing, but we want it captured by SOUTH CENTRAL
  x.merge$is.NorthCentral <- as.numeric(x.merge$NAME == "NORTH CENTRAL")
  x.merge$is.NorthMnt <- as.numeric(x.merge$NAME == "NORTHERN MOUNTAINS")
  x.merge$is.SouthCentral <- as.numeric(x.merge$NAME == "SOUTH CENTRAL" | x.merge$NAME == "DIXIE")
  x.merge$is.Western <- as.numeric(x.merge$NAME == "WESTERN")

  # Return
  return(x)
}

# Attach sex and migration status variables ----
sex_status <- function(x, sex, status) {
  # Sex
  x$is.Male <- as.numeric(sex == "male")
  # Status
  x$is.res <- as.numeric(status == "res")
  # Return
  return(x)
}

# Attach season variables
attach_season <- function(x, season) {
  x$is.Winter <- as.numeric(season == "winter")
  x$is.Spring <- as.numeric(season == "spring")
  x$is.Summer <- as.numeric(season == "summer")
  x$is.Fall <- as.numeric(season == "fall")
  return(x)
}

# Predict betas in a 10km cell ----
# cell: the cell number from the 10x10 raster
# season: the season -- either "winter", "spring", "summer", "fall"

predict_10k <- function(cell, season, mods, new_PDSI = NA){
  # Load packages
  library(dplyr)
  library(raster)

  # Report cell
  cat("\nCell:", cell, "Season:", season, "\n")
  # Write cell to file (for understanding status when running in parallel)
  status_file <- paste0("status/cell_", cell, "_season_", season, ".txt")
  cat("Working on cell", cell, "; season", season, "\n", file = status_file)
  # Check inputs
  if (!season %in% c("winter", "spring", "summer", "fall")) {
    stop("Argument 'season' should be one of 'winter', 'spring',",
         "'summer', 'fall'.")
  }

  # Get 10x10 raster stack
  if (season == "summer") {
    fn10 <- paste0("2021_stacks/10x10_landscape_2021",
                   season2num(season), ".tif")
    r10 <- raster::stack(fn10)
    names(r10) <- c("Elevation", "Asp_sin", "Asp_cos", "Roughness",
                    "RAP_bio", "Herb", "Shrub", "Tree")
  } else {
    fn10 <- paste0("2021_stacks/10x10_landscape_2021",
                   season2num(season), ".tif")
    r10 <- raster::stack(fn10)
    names(r10) <- c("Elevation", "SND", "Asp_sin", "Asp_cos", "Roughness",
                    "RAP_bio", "Herb", "Shrub", "Tree")
  }

  # Get 10x10 drought
  fn10_dr <- paste0("2021_drought/10x10_PDSI_2021",
                    season2num(season), ".tif")
  r10_dr <- raster::raster(fn10_dr)
  names(r10_dr) <- "PDSI"

  # Get 10x10 roads
  r10_rd <- raster::raster("roads/roads_10k.tif")
  names(r10_rd) <- "Road"

  # Get 10x10 region
  r10_reg <- raster::raster("CONUS/conus_raster.tif")
  names(r10_reg) <- "region"

  # Combine
  s10 <- raster::stack(r10, r10_dr, r10_rd, r10_reg)
  # Remove components
  rm(list = c("fn10", "r10", "fn10_dr", "r10_dr", "r10_rd", "r10_reg"))

  # Get 30x30 raster stack
  if (season == "summer") {
    fn30 <- paste0(dir30, "landscape_2021",
                   season2num(season), ".tif")
    r30 <- raster::stack(fn30)
    names(r30) <- c("Elevation", "Asp_sin", "Asp_cos", "Roughness",
                    "RAP_bio", "Herb", "Shrub", "Tree")
  } else {
    fn30 <- paste0(dir30, "landscape_2021",
                   season2num(season), ".tif")
    r30 <- raster::stack(fn30)
    names(r30) <- c("Elevation", "SND", "Asp_sin", "Asp_cos", "Roughness",
                    "RAP_bio", "Herb", "Shrub", "Tree")
  }

  # Get 30x30 drought
  fn30_dr <- paste0(dir_dr, "2021",
                    season2num(season), "_mean.tif")
  r30_dr <- raster::raster(fn30_dr)
  names(r30_dr) <- "PDSI"

  # Combine
  s30 <- raster::stack(r30, r30_dr)
  # Remove components
  rm(list = c("fn30", "r30", "fn30_dr", "r30_dr"))

  # Get extent for cell of interest
  ext <- raster::extentFromCells(s10, cells = cell)

  # Crop 30x30 raster
  c30 <- raster::crop(s30, ext, snap = "out")
  # Remove large raster
  rm(s30)

  # Good time for a garbage cleanup
  gc()

  if (is.na(new_PDSI)) {
    # Get data in data.frame
    dat <- as.data.frame(s10) %>%
      # Add cell numbers
      dplyr::mutate(cell_num = 1:nrow(.)) %>%
      dplyr::filter(cell_num == cell) %>%
      # Scale and center
      scale_data() %>%
      # Create region columns
      region() %>%
      # Create season columns
      attach_season(season)
  } else {
    # Get data in data.frame
    dat <- as.data.frame(s10) %>%
      # Add cell numbers
      dplyr::mutate(cell_num = 1:nrow(.)) %>%
      dplyr::filter(cell_num == cell) %>%
      dplyr::mutate(PDSI = new_PDSI) %>%
      # Scale and center
      scale_data() %>%
      # Create region columns
      region() %>%
      # Create season columns
      attach_season(season)
  }

  # Create sex/mig specific data
  DAT <- dplyr::bind_rows(
    "fem_res" = dat %>%
      sex_status("female", "res"),
    "fem_mig" = dat %>%
      sex_status("female", "mig"),
    "male_res" = dat %>%
      sex_status("male", "res"),
    "male_mig" = dat %>%
      sex_status("male", "mig"),
    .id = "sex_status")

  # Predict elevation
  # Set intercept_beta_scaled to 0
  DAT$Intercept_beta_scale <- 0

  DAT$Elev.mod.prediction.full <- predict(object = mods$Elev.mod.full,
                                          newdata = DAT,
                                          re.form = NA)


  # Predict roughness
  # Include predicted elevation
  DAT$Rough.mod.prediction.full <- predict(object = mods$Rough.mod.full,
                                           newdata = DAT,
                                           re.form = NA)

  # Predict herbaceous
  # Include predicted elevation and roughness
  DAT$Herb.mod.prediction.full <- predict(object = mods$Herb.mod.full,
                                          newdata = DAT,
                                          re.form = NA)

  # Predict shrub
  DAT$Shrub.mod.prediction.full <- predict(object = mods$Shrub.mod.full,
                                           newdata = DAT,
                                           re.form = NA)

  # Predict tree
  DAT$Tree.mod.prediction.full <- predict(object = mods$Tree.mod.full,
                                          newdata = DAT,
                                          re.form = NA)

  # Predict asp_sin
  DAT$Asp_sin.mod.prediction.full <- predict(object = mods$Asp_sin.mod.full,
                                             newdata = DAT,
                                             re.form = NA)

  # Predict asp_cos
  DAT$Asp_cos.mod.prediction.full <- predict(object = mods$Asp_cos.mod.full,
                                             newdata = DAT,
                                             re.form = NA)

  # Now that we have all betas, we can predict eHSF
  betas <- DAT %>%
    dplyr::select(Elev.mod.prediction.full:Asp_cos.mod.prediction.full) %>%
    as.matrix()

   scaled_df30 <- as.data.frame(c30) %>%
    # add columns so I can use scale fun
    mutate(Road = NA) %>%
    # scale fun
    scale_data() %>%
    # Remove and rename scaled columns
    dplyr::select(-c(Road, scaled_log_Road,
                  Elevation, Roughness, Herb, Shrub,
                  Tree, Asp_sin, Asp_cos)) %>%
    rename(Elevation = m_SC_elev,
           Roughness= m_SC_rough,
           Herb = m_SC_herb,
           Shrub = m_SC_shrub,
           Tree = m_SC_tree,
           Asp_sin = m_SC_a.sin,
           Asp_cos = m_SC_a.cos)

  X <- model.matrix(~ 0 + Elevation + Roughness + Herb + Shrub +
                      Tree + Asp_sin + Asp_cos,
                    data = scaled_df30)

  # Predict for each beta
  rs <- raster::stack(
    lapply(1:nrow(betas), function(i) {
       # w <- exp((X %*% betas[i,])[,1])
      log.w <- (X %*% betas[i,])[,1]
     # Place values into raster
      r <- c30[[1]]
      values(r) <- log.w
      return(r)
    })
  )

  names(rs) <- DAT$sex_status


  cat("Done.", file = status_file, append = TRUE)

  # Return
  return(rs)
}

# #TEST ----
# r.10 <- predict_10k(cell = readRDS("out/cells_good.rds")[600],
#                  season = "spring",
#                  mods = readRDS("20230321_model_outputs.rds"))


# Parallelize prediction ----
predict_parallel <- function(i, comb, new_PDSI = NA) {
  # Load packages
  library(raster)
  library(sf)
  library(lme4)
  library(lmerTest)
  # Self source
  source("Analysis/xx_funs_predicting.R")
  # Get cell
  c <- comb$cell[i]
  s <- comb$season[i]
  # Predict
  r <- predict_10k(cell = c,
                   season = s,
                   mods = readRDS("20230321_model_outputs.rds"),
                   new_PDSI = new_PDSI)

  # Save
  if(is.na(new_PDSI)){
    writeRaster(r, paste0("pred/pred30m_", s, "_cell", c, ".tif"),
                overwrite = TRUE)
  } else {
    fold <- paste0("pred/PDSI_", new_PDSI)
    dir.create(fold, showWarnings = FALSE)
    writeRaster(r, paste0(fold, "/pred30m_", s, "_cell", c, ".tif"),
                overwrite = TRUE)
  }

  # Return NULL
  return(invisible(NULL))
}

# Make a file by sex, mig status, season
mosaic_map <- function(sex, status, season, new_PDSI = NA) {
  # Order of layers
  ord <- c("fem_res", "fem_mig", "male_res", "male_mig")

  # Decide which layer to load
  lyr_nm <- paste0(ifelse(sex == "f", "fem", "male"), "_", status)
  lyr <- which(lyr_nm == ord)

  # Load all the files from a season into a list
  l <- lapply(get(season), function(s) {
    x <- raster(s, band = lyr)
    return(x)
  })

  # Add mosaic arguments
  names(l)[1:2] <- c("x", "y")
  l$fun <- mean
  l$na.rm <- TRUE

  # Mosaic
  m <- do.call(mosaic, l)

  # Construct filename for save
  if(is.na(new_PDSI)) {
    fn <- paste0("out/geo/", season, "_", sex, "_", status, ".tif")
  } else {
    fold <- paste0("out/geo/PDSI_", new_PDSI, "/")
    dir.create(fold, showWarnings = FALSE)
    fn <- paste0(fold, season, "_", sex, "_", status, ".tif")
  }


  # Save
  writeRaster(m, fn, overwrite = TRUE)

  # Return invisible null
  return(invisible(NULL))
}

