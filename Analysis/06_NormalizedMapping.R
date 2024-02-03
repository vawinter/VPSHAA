# Step 1, combine 3rd- & 2nd-order maps
# Scale 3rd order habitat selection so that each 10-km pixel sums to 1
#   [i.e., Pr(3rd-order use | 2nd-order use)]
# Scale 2nd order habitat selection so that entire raster sums to 1
#   [i.e., Pr(2nd-order use)]
# Combine by multiplying
#   [i.e., Pr(3rd-order use | 2nd-order use) * Pr(2nd-order use)
#                                                         = Pr(3rd-order use)]
#
# Step 2, subset to pronghorn GPS locations
# Keep just those 10-km pixels with at least 1 pronghorn location
#
# Step 3, bin 30-m cells
#   Sort by combined eHSF, break into ~ 30 bins w/ equal area
#     (= equal # of raster cells)
#   Count GPS locations (30 m)/individual (10 km) in each bin
#   Calculate average eHSF in each bin
#
# Step 4, calculate Spearman's R

rm(list = ls())
gc()

# Load packages ----
library(lubridate)
library(terra)
library(dplyr)
library(matrixStats)
library(ggplot2)
library(sf)

# Combinations of maps ----
combs <- expand.grid(sex = c("f", "m"),
                     stat = c("res", "mig"),
                     seas = c("winter", "spring", "summer", "fall"),
                     stringsAsFactors = FALSE)

# Get rid of winter migrants
combs <- combs[which(combs$stat != "mig" | combs$seas!= "winter"), ]

# Make map ----
lapply(1:nrow(combs), function(c) {
  # Cluster
  beginCluster(15)

  # Print status
  cat("\nCombination", c, "of", nrow(combs), "\n")
  # Get season, sex, status
  seas <- combs$seas[c]
  #seas <- "summer"
  sex <- combs$sex[c]
  stat <- combs$stat[c]

  # Create filename
  fn <- paste0("out/NULL/", seas,"_", sex,"_", stat,".tif")
  r3 <- rast(fn)
  # 2nd-order raster (ON LOG SCALE)
  r2 <- rast(paste0("../Winter_etal_map/out/2nd-order_map/", seas, "_2nd-order_map.tif"))

  # Step 1 -- combine ----
  # Normalize 3rd-order by 10 km cell
  # Put data in data.frame
  d3 <- as.data.frame(r3, xy = TRUE, cells = TRUE, na.rm = FALSE)
  #d3$cell_3rd <- row.names(d3)
  names(d3)[1] <- "cell_3rd" # this as renaming x column, which I don't think was the intention
  names(d3)[4] <- "value"

  # Add cell number from 2nd-order raster
  d3$cell_2nd <- cellFromXY(r2, d3[, c("x", "y")])

  # Normalizing constant is sum by cell
  norm <- d3 %>%
    group_by(cell_2nd) %>%
    summarize(norm_const = logSumExp(value, na.rm = TRUE),
              n = n())

  # Step 2 ---- Normalizing
  # Join normalizing constant
  d3 <- d3 %>%
    left_join(norm, by = "cell_2nd") %>%
    # and normalize 3rd order
    mutate(pr3_2 = value - norm_const)

  # Re normalizing 2nd order per cell
  d3$pr2 <- values(r2)[d3$cell_2nd]

  # Put into raster (3rd order)
  r3_2 <- setValues(r3, d3$pr3_2)

  # # CHECK - sum to 1?
  ck <- d3 %>%
    group_by(cell_2nd) %>%
    summarize(check_vals = sum(exp(pr3_2), na.rm = TRUE), # this should now sum to 1 w. changes
              n = n())

  # YES!!

  # Okay, continue
  # Calculate combined eHSF
  d3$log.eHSF <- d3$pr3_2 + d3$pr2 # b.c both are on log scale
  # Re normalize
  d3$log.eHSF <- d3$log.eHSF - logSumExp(d3$log.eHSF, na.rm = TRUE) # normalizing combined order

  # Place in raster (combined)
  log.comb <- setValues(r3, d3$log.eHSF)
  names(log.comb) <- "log.eHSF"

  # Save ----
  writeRaster(r3_2, paste0("out/NULL/",seas,"_", sex,"_", stat,"_3rd_norm.tif"), overwrite = TRUE)
  writeRaster(log.comb, paste0("out/NULL/", seas,"_", sex,"_", stat,"_combined.tif"), overwrite = TRUE)
  saveRDS(d3, paste0("out/NULLBoyce_d3-",seas,"-", sex,"-", stat,".rds"))

  # Return
  return(invisible(NULL))

})
endCluster()

# Done!
