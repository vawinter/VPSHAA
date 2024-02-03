# Created 9/12/2023
# Trying to seperate out plotting for RAM

rm(list = ls())
gc()

# Load packages ----
library(ggplot2)
library(terra)
library(sf)

# Load data ----
# Utah
gsl <- st_read("geo/salt_lake.shp") %>%
  st_transform(32612)
ut <- st_read("geo/utah.shp") %>%
  st_transform(32612)

# Read in rasters
# Load season sex status ----
seas <-  "summer"
sex <- "m"
stat <-  "mig"
labela <-  "Summer - Male"
label1 <- "A. Third-order "
label1b <- "C. Combined-order"
label2 <- "Migrant"

for(i in 1:length(maps)){# 3rd-order raster
r3 <- rast(paste0("out/geo/",seas,"_", sex,"_", stat,"_3rd_norm.tif"))
# 2nd-order raster
r2 <- rast(paste0("../Winter_etal_map/out/2nd-order_map/", seas, "_2nd-order_map.tif"))
# Combined order
comb <- rast(paste0("out/geo/",seas, "_", sex,"_",stat,"_combined.tif"))

# Step 2: plot maps ----#
names(r3) <- "fill"

g <- r3 %>%
  crop(ut) %>%
  mask(ut) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot(aes(x = x, y = y, fill = fill)) +
  geom_raster() +
  geom_sf(data = ut, inherit.aes = FALSE,
          color = "black", fill = NA, size = 8) +
  geom_sf(data = gsl, inherit.aes = FALSE,
          color = "skyblue", fill = "skyblue", size = 1) +
  coord_sf() +
  scale_fill_viridis_c(name = "Relative Pr(use)", option = "magma") +
  xlab(NULL) +
  ylab(NULL) +
  # ggtitle(label = label1,
  #         subtitle = paste(labela, label2)) +
  theme_bw() +
  theme(text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(size = 15, color = "black"),
        axis.text.y = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"))

# Combined orders  -----#
names(comb) <- "fill"
gc <- comb %>%
  crop(ut) %>%
  mask(ut) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot(aes(x = x, y = y, fill = fill)) +
  geom_raster() +
  geom_sf(data = ut, inherit.aes = FALSE,
          color = "black", fill = NA, size = 8) +
  geom_sf(data = gsl, inherit.aes = FALSE,
          color = "skyblue", fill = "skyblue", size = 1) +
  coord_sf() +
  scale_fill_viridis_c(name = "Relative Pr(use)", option = "magma") +
  xlab(NULL) +
  ylab(NULL) +
  # ggtitle(label = label1b,
  #         subtitle = paste(labela, label2)) +
  theme_bw() +
  theme(text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(size = 15, color = "black"),
        axis.text.y = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"))


# Step 3 --- save plot ---
# Create filename
fn <- paste0(seas, "_", sex, "_", stat, ".tif")
# Save plot
beginCluster(10)
ggsave(file.path("out/map/", fn), plot = g,
       device = "png", width = 8.5, height = 11, units = "in",
       dpi = 300)
endCluster()

## Save plot (combined)
# Create filename
fnl <- paste0("log_", seas, "_", sex, "_", stat, ".tif")
# Save plot
ggsave(file.path("out/map/", fnl), plot = gc,
       device = "png", width = 8.5, height = 11, units = "in",
       dpi = 300)
}


 # END!


# # Zoom in
# # Can see some detail
# plot(crop(r3, ext(6e05 + c(-30000, 30000), 4390000 + c(-30000, 30000))))
#
# # Can still see detail, but every cell is the same on average
# plot(crop(log(r3_2), ext(6e05 + c(-30000, 30000), 4390000 + c(-30000, 30000))))
#
# # Can barely see any detail
# plot(crop(log(comb), ext(6e05 + c(-30000, 30000), 4390000 + c(-30000, 30000))))
