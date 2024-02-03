# Some 10k cells fall outside of Utah and are missing some state-specific
# covariates (e.g., roads, climate regions).

# ID these cells once so that we can skip them later.

library(raster)

# Get 10x10 climate
conus <- raster::raster("CONUS/conus_raster.tif")
names(conus) <- "conus"

# Get data.frame, identify NAs
dat <- as.data.frame(conus)
na_10k <- which(is.na(dat$conus))
good <- which(!is.na(dat$conus))

# Save
saveRDS(na_10k, "out/cells_NA.rds")
saveRDS(good, "out/cells_good.rds")
