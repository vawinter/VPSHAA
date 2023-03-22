# Cross validation 
# Quantifying predictive capacity of these models


# Need to scale and center based on scaling and centering done for 2018-2020 
# data

rm(list = ls())
gc()

# Libraries
library(tidyverse)
library(wCorr)

source("Analysis/11_MEM-full.R")

options(scipen = 999)

# Load in data
m_sd <- read.csv("Data/Processed/Mean_sd/20221019_mean-sd_all.csv", header = T)

# Load in data
pred_dat <- read.csv("Data/Outputs/2021_pred/20221024_2021-10.6.csv", header = T)
pred_dat <- pred_dat[(pred_dat$used_in + pred_dat$used_out) > 70,]

# Scale and center
# Set SND NA values to 0
pred_dat$m_snd[is.na(pred_dat$m_snd)] <- 0

## PDSI ----
# a. find mean
pdsi_m <- m_sd$PDSI[1]
# b. find sd
pdsi_s <- m_sd$PDSI[2]
#c. subtract and divide
pred_dat$scaled_PDSI <- ((pred_dat$m_PDSI - pdsi_m)/ pdsi_s)

## road ----
pred_dat$m_road_no_0 <- pred_dat$m_road
pred_dat$m_road_no_0[pred_dat$m_road == 0] <- min(pred_dat$m_road[pred_dat$m_road > 0])
# a. find mean
road_m <- m_sd$log_Road[1]
# b. find sd
road_s <-m_sd$log_Road[2]
#c. subtract and divide
pred_dat$scaled_log_Road <- ((log(pred_dat$m_road_no_0) - road_m)/ road_s)

## Intercept -----
# need to multiply by SE for this 
pred_dat$Intercept_beta_scale <- 0

# ## scaled_log Elevation ----
# pred_dat$Elev_log <- log(pred_dat$m_elev)
# # a. find mean
# Elev_m <- m_sd$scaled_log_Elev[1]
# # b. find sd
# Elev_s <- m_sd$scaled_log_Elev[2]
# #c. subtract and divide
# pred_dat$scaled_log_Elev <- ((pred_dat$Elev_log - Elev_m)/ Elev_s)

## SND ----
pred_dat$m_snd_no_0 <- pred_dat$m_snd
pred_dat$m_snd_no_0[pred_dat$m_snd == 0] <- min(pred_dat$m_snd[pred_dat$m_snd > 0]) 
# a. find mean
SND_m <- m_sd$log_SND[1]
# b. find sd
SND_s <- m_sd$log_SND[2]
#c. subtract and divide
pred_dat$scaled_log_SND <- ((log(pred_dat$m_snd_no_0) - SND_m)/ SND_s)

# ## Aspect(sin) ----
# # a. find mean
# a.sin_m <- m_sd$Asp_sin[1]
# # b. find sd
# a.sin_s <-  m_sd$Asp_sin[2]
# #c. subtract and divide
# pred_dat$scaled_Asp_sin <- ((pred_dat$m_a.sin - a.sin_m)/ a.sin_s)
# 
# ## Aspect(cos) ----
# # a. find mean
# a.cos_m <- m_sd$Asp_cos[1]
# # b. find sd
# a.cos_s <-  m_sd$Asp_cos[2]
# #c. subtract and divide
# pred_dat$scaled_Asp_cos <- ((pred_dat$m_a.cos - a.cos_m)/ a.cos_s)

# ## Roughness ----
# pred_dat$Rough_log <- log(pred_dat$m_rough)
# # a. find mean
# Rough_m <- m_sd$scaled_log_Rough[1]
# # b. find sd
# Rough_s <- m_sd$scaled_log_Rough[2]
# #c. subtract and divide
# pred_dat$scaled_log_Rough <- ((pred_dat$Rough_log - Rough_m)/ Rough_s)

# ## RAP (bio) ----
# pred_dat$RAP_bio_log <- log(pred_dat$m_bio)
# # a. find mean
# bio_m <- m_sd$scaled_log_RAP_bio[1]
# # b. find sd
# bio_s <- m_sd$scaled_log_RAP_bio[2]
# #c. subtract and divide
# pred_dat$scaled_log_RAP_bio <- ((pred_dat$RAP_bio_log - bio_m)/ bio_s)

# ## RAP (cover) ----
# pred_dat$Shrub_log <- log(pred_dat$m_herb)
# # a. find mean
# herb_m <- m_sd$scaled_log_Herb[1]
# # b. find sd
# herb_s <- m_sd$scaled_log_Herb[2]
# #c. subtract and divide
# pred_dat$scaled_log_Herb <- ((pred_dat$Shrub_log - herb_m)/ herb_s)
# 
# ## Shrub ----
# pred_dat$Shrub_log <- log(pred_dat$m_shrub)
# # a. find mean
# Shrub_m <-  m_sd$scaled_log_Shrub[1]
# # b. find sd
# Shrub_s <- m_sd$scaled_log_Shrub[2]
# #c. subtract and divide
# pred_dat$scaled_log_Shrub <- ((pred_dat$Shrub_log - Shrub_m)/ Shrub_s)
# 
# ## Tree ----
# pred_dat$Tree_log <- log(pred_dat$m_tree)
# # a. find mean
# Tree_m <-  m_sd$scaled_log_Tree[1]
# # b. find sd
# Tree_s <-  m_sd$scaled_log_Tree[2]
# #c. subtract and divide
# pred_dat$scaled_log_Tree <- ((pred_dat$Tree_log  - Tree_m)/ Tree_s)
# 
############################################################################################

# Elevation

pred_dat$Elev.mod.prediction.full.m <- predict(Elev.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Elev.mod.prediction.null.m <- predict(Elev.mod.null, newdata = pred_dat, re.form = NA)

temp <- 1 / (pred_dat$Elev_stder ^ 2)
observed.weights <- temp / sum(temp, na.rm = TRUE)

cor.full <- weightedCorr(pred_dat$Elev_beta,
                         pred_dat$Elev.mod.prediction.full.m,
                         method = "Pearson",
                         weights = observed.weights)
cor.null <- weightedCorr(pred_dat$Elev_beta,
                         pred_dat$Elev.mod.prediction.null.m,
                         method = "Pearson",
                         weights = observed.weights)
(cor.full - cor.null) / 2

plot(x = pred_dat$Elev.mod.prediction.full.m, y = pred_dat$Elev_beta,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     xlim = c(-5, 7), ylim = c(-40, 40), pch = 19, cex = 100 * observed.weights,
     main = "Elevation")
points(x = pred_dat$Elev.mod.prediction.null.m, y = pred_dat$Elev_beta, col = "blue", 
       pch = 19, cex = 100 * observed.weights)
abline(0,1, col = "red")
tiff("Figures_and_Results/Manuscript/Elevation.tif")
dev.off()


# Roughness
pred_dat$Elev.mod.prediction.full.m.scl <- scale(pred_dat$Elev.mod.prediction.full.m)

pred_dat$Rough.mod.prediction.full.m <- predict(Rough.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Rough.mod.prediction.null.m <- predict(Rough.mod.null, newdata = pred_dat, re.form = NA)

temp <- 1 / (pred_dat$Rough_stder ^ 2)
observed.weights <- temp / sum(temp, na.rm = TRUE)

cor.full <- weightedCorr(pred_dat$Rough_beta,
                         pred_dat$Rough.mod.prediction.full.m,
                         method = "Pearson",
                         weights = observed.weights)
cor.null <- weightedCorr(pred_dat$Rough_beta,
                         pred_dat$Rough.mod.prediction.null.m,
                         method = "Pearson",
                         weights = observed.weights)
(cor.full - cor.null) / 2

plot(x = pred_dat$Rough.mod.prediction.full.m, y = pred_dat$Rough_beta,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     xlim = c(-1, 2), ylim = c(-40, 40), pch = 19, cex = 100 * observed.weights,
     main = "Roughness")
points(x = pred_dat$Rough.mod.prediction.null.m, y = pred_dat$Rough_beta, col = "blue", 
       pch = 19, cex = 100 * observed.weights)
abline(0,1, col = "red")
tiff("Figures_and_Results/Manuscript/Rough.tif")
dev.off()

# Herbaceous
pred_dat$Rough.mod.prediction.full.m.scl <- scale(pred_dat$Rough.mod.prediction.full.m)

pred_dat$Herb.mod.prediction.full.m <- predict(Herb.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Herb.mod.prediction.null.m <- predict(Herb.mod.null, newdata = pred_dat, re.form = NA)

temp <- 1 / (pred_dat$Herb_stder ^ 2)
observed.weights <- temp / sum(temp, na.rm = TRUE)

cor.full <- weightedCorr(pred_dat$Herb_beta,
                         pred_dat$Herb.mod.prediction.full.m,
                         method = "Pearson",
                         weights = observed.weights)
cor.null <- weightedCorr(pred_dat$Herb_beta,
                         pred_dat$Herb.mod.prediction.null.m,
                         method = "Pearson",
                         weights = observed.weights)
(cor.full - cor.null) / 2

plot(x = pred_dat$Herb.mod.prediction.full.m, y = pred_dat$Herb_beta,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     xlim = c(-1, 2), ylim = c(-40, 40), pch = 19, cex = 100 * observed.weights,
     main = "Herbaceous")
points(x = pred_dat$Herb.mod.prediction.null.m, y = pred_dat$Herb_beta, col = "blue", 
       pch = 19, cex = 100 * observed.weights)
abline(0,1, col = "red")
tiff("Figures_and_Results/Manuscript/Herb.tif")
dev.off()

# Shrub
pred_dat$Herb.mod.prediction.full.m.scl <- scale(pred_dat$Herb.mod.prediction.full.m)

pred_dat$Shrub.mod.prediction.full.m <- predict(Shrub.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Shrub.mod.prediction.null.m <- predict(Shrub.mod.null, newdata = pred_dat, re.form = NA)

temp <- 1 / (pred_dat$Shrub_stder ^ 2)
observed.weights <- temp / sum(temp, na.rm = TRUE)

cor.full <- weightedCorr(pred_dat$Shrub_beta,
                         pred_dat$Shrub.mod.prediction.full.m,
                         method = "Pearson",
                         weights = observed.weights)
cor.null <- weightedCorr(pred_dat$Shrub_beta,
                         pred_dat$Shrub.mod.prediction.null.m,
                         method = "Pearson",
                         weights = observed.weights)
(cor.full - cor.null) / 2

plot(x = pred_dat$Shrub.mod.prediction.full.m, y = pred_dat$Shrub_beta,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     xlim = c(-1, 2), ylim = c(-40, 40), pch = 19, cex = 100 * observed.weights,
     main = "Shrub")
points(x = pred_dat$Shrub.mod.prediction.null.m, y = pred_dat$Shrub_beta, col = "blue", 
       pch = 19, cex = 100 * observed.weights)
abline(0,1, col = "red")
tiff("Figures_and_Results/Manuscript/Shrub.tif")
dev.off()

# Tree
pred_dat$Shrub.mod.prediction.full.m.scl <- scale(pred_dat$Shrub.mod.prediction.full.m)

pred_dat$Tree.mod.prediction.full.m <- predict(Tree.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Tree.mod.prediction.null.m <- predict(Tree.mod.null, newdata = pred_dat, re.form = NA)

temp <- 1 / (pred_dat$Tree_stder ^ 2)
observed.weights <- temp / sum(temp, na.rm = TRUE)

cor.full <- weightedCorr(pred_dat$Tree_beta,
                         pred_dat$Tree.mod.prediction.full.m,
                         method = "Pearson",
                         weights = observed.weights)
cor.null <- weightedCorr(pred_dat$Tree_beta,
                         pred_dat$Tree.mod.prediction.null.m,
                         method = "Pearson",
                         weights = observed.weights)
(cor.full - cor.null) / 2

plot(x = pred_dat$Tree.mod.prediction.full.m, y = pred_dat$Tree_beta,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     xlim = c(-1, 2), ylim = c(-40, 40), pch = 19, cex = 100 * observed.weights,
     main = "Tree")
points(x = pred_dat$Tree.mod.prediction.null.m, y = pred_dat$Tree_beta, col = "blue", 
       pch = 19, cex = 100 * observed.weights)
abline(0,1, col = "red")
tiff("Figures_and_Results/Manuscript/Tree.tif")
dev.off()

# Aspect - sin
pred_dat$Tree.mod.prediction.full.m.scl <- scale(pred_dat$Tree.mod.prediction.full.m)

pred_dat$Asp_sin.mod.prediction.full.m <- predict(Asp_sin.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Asp_sin.mod.prediction.null.m <- predict(Asp_sin.mod.null, newdata = pred_dat, re.form = NA)

temp <- 1 / (pred_dat$Asp_sin_stder ^ 2)
observed.weights <- temp / sum(temp, na.rm = TRUE)

cor.full <- weightedCorr(pred_dat$Asp_sin_beta,
                         pred_dat$Asp_sin.mod.prediction.full.m,
                         method = "Pearson",
                         weights = observed.weights)
cor.null <- weightedCorr(pred_dat$Asp_sin_beta,
                         pred_dat$Asp_sin.mod.prediction.null.m,
                         method = "Pearson",
                         weights = observed.weights)
(cor.full - cor.null) / 2

plot(x = pred_dat$Asp_sin.mod.prediction.full.m, y = pred_dat$Asp_sin_beta,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     xlim = c(-1, 2), ylim = c(-40, 40), pch = 19, cex = 100 * observed.weights,
     main = "Easting")
points(x = pred_dat$Asp_sin.mod.prediction.null.m, y = pred_dat$Asp_sin_beta, col = "blue", 
       pch = 19, cex = 100 * observed.weights)
abline(0,1, col = "red")
tiff("Figures_and_Results/Manuscript/Easting.tif")
dev.off()


# Aspect - cos
pred_dat$Asp_sin.mod.prediction.full.m.scl <- scale(pred_dat$Asp_sin.mod.prediction.full.m)

pred_dat$Asp_cos.mod.prediction.full.m <- predict(Asp_cos.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Asp_cos.mod.prediction.null.m <- predict(Asp_cos.mod.null, newdata = pred_dat, re.form = NA)

temp <- 1 / (pred_dat$Asp_cos_stder ^ 2)
observed.weights <- temp / sum(temp, na.rm = TRUE)

cor.full <- weightedCorr(pred_dat$Asp_cos_beta,
                         pred_dat$Asp_cos.mod.prediction.full.m,
                         method = "Pearson",
                         weights = observed.weights)
cor.null <- weightedCorr(pred_dat$Asp_cos_beta,
                         pred_dat$Asp_cos.mod.prediction.null.m,
                         method = "Pearson",
                         weights = observed.weights)
(cor.full - cor.null) / 2

plot(x = pred_dat$Asp_cos.mod.prediction.full.m, y = pred_dat$Asp_cos_beta,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     xlim = c(-1, 2), ylim = c(-40, 40), pch = 19, cex = 100 * observed.weights,
     main = "Northing")
points(x = pred_dat$Asp_cos.mod.prediction.null.m, y = pred_dat$Asp_cos_beta, col = "blue", 
       pch = 19, cex = 100 * observed.weights)
abline(0,1, col = "red")
tiff("Figures_and_Results/Manuscript/Northing.tif")
dev.off()