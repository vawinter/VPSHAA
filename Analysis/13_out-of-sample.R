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

## SND ----
pred_dat$m_snd_no_0 <- pred_dat$m_snd
pred_dat$m_snd_no_0[pred_dat$m_snd == 0] <- min(pred_dat$m_snd[pred_dat$m_snd > 0]) 
# a. find mean
SND_m <- m_sd$log_SND[1]
# b. find sd
SND_s <- m_sd$log_SND[2]
#c. subtract and divide
pred_dat$scaled_log_SND <- ((log(pred_dat$m_snd_no_0) - SND_m)/ SND_s)

# Elevation

pred_dat$Elev.mod.prediction.full.m <- predict(Elev.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Elev.mod.prediction.null.m <- predict(Elev.mod.null, newdata = pred_dat, re.form = NA)
pred_dat$Elev.mod.prediction.seas.m <- predict(Elev.mod.seas, newdata = pred_dat, re.form = NA)
pred_dat$Elev.mod.prediction.avail.m <- predict(Elev.mod.avail, newdata = pred_dat, re.form = NA)

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



cor.seas <- weightedCorr(pred_dat$Elev_beta,
                         pred_dat$Elev.mod.prediction.seas.m,
                         method = "Pearson",
                         weights = observed.weights)

cor.avail <- weightedCorr(pred_dat$Elev_beta,
                         pred_dat$Elev.mod.prediction.avail.m,
                         method = "Pearson",
                         weights = observed.weights)

(cor.full - cor.seas) / 2
(cor.full - cor.avail) / 2

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



# whats the story? avail only null model shows that compared to full we need 
# other factors of the individual

# seas model tells us we need availability

# null model tells us that compared to assumed even dist our model is more transferable than nothing

# overall, model transferability is important and we are showing that, for pronghorn, this is a more transferable model
# than otherwise would've been used?