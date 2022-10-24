# Cross validation 
# Quantifying predictive capacity of these models


# Need to scale and center based on scaling and centering done for 2018-2020 
# data

rm(list = ls())
gc()

# Libraries
library(tidyverse)

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

## Aspect(sin) ----
# a. find mean
a.sin_m <- m_sd$Asp_sin[1]
# b. find sd
a.sin_s <-  m_sd$Asp_sin[2]
#c. subtract and divide
pred_dat$scaled_Asp_sin <- ((pred_dat$m_a.sin - a.sin_m)/ a.sin_s)

## Aspect(cos) ----
# a. find mean
a.cos_m <- m_sd$Asp_cos[1]
# b. find sd
a.cos_s <-  m_sd$Asp_cos[2]
#c. subtract and divide
pred_dat$scaled_Asp_cos <- ((pred_dat$m_a.cos - a.cos_m)/ a.cos_s)

## Roughness ----
pred_dat$Rough_log <- log(pred_dat$m_rough)
# a. find mean
Rough_m <- m_sd$scaled_log_Rough[1]
# b. find sd
Rough_s <- m_sd$scaled_log_Rough[2]
#c. subtract and divide
pred_dat$scaled_log_Rough <- ((pred_dat$Rough_log - Rough_m)/ Rough_s)

## RAP (bio) ----
pred_dat$RAP_bio_log <- log(pred_dat$m_bio)
# a. find mean
bio_m <- m_sd$scaled_log_RAP_bio[1]
# b. find sd
bio_s <- m_sd$scaled_log_RAP_bio[2]
#c. subtract and divide
pred_dat$scaled_log_RAP_bio <- ((pred_dat$RAP_bio_log - bio_m)/ bio_s)

## RAP (cover) ----
pred_dat$Shrub_log <- log(pred_dat$m_herb)
# a. find mean
herb_m <- m_sd$scaled_log_Herb[1]
# b. find sd
herb_s <- m_sd$scaled_log_Herb[2]
#c. subtract and divide
pred_dat$scaled_log_Herb <- ((pred_dat$Shrub_log - herb_m)/ herb_s)

## Shrub ----
pred_dat$Shrub_log <- log(pred_dat$m_shrub)
# a. find mean
Shrub_m <-  m_sd$scaled_log_Shrub[1]
# b. find sd
Shrub_s <- m_sd$scaled_log_Shrub[2]
#c. subtract and divide
pred_dat$scaled_log_Shrub <- ((pred_dat$Shrub_log - Shrub_m)/ Shrub_s)

## Tree ----
pred_dat$Tree_log <- log(pred_dat$m_tree)
# a. find mean
Tree_m <-  m_sd$scaled_log_Tree[1]
# b. find sd
Tree_s <-  m_sd$scaled_log_Tree[2]
#c. subtract and divide
pred_dat$scaled_log_Tree <- ((pred_dat$Tree_log  - Tree_m)/ Tree_s)

############################################################################################
# create df for the MAE outputs
mae <- data.frame()
# Elevation:
pred_dat$Elev.mod.prediction.full <- predict(Elev.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Elev.mod.prediction.null <- predict(Elev.mod.null, newdata = pred_dat, re.form = NA)

plot(y = pred_dat$Elev_beta, x = pred_dat$Elev.mod.prediction.full,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     main = "Elevation")
points(pred_dat$Elev_beta, pred_dat$Elev.mod.prediction.null, col = "blue")
abline(0,1, col = "red")
tiff("Figures_and_Results/TWS/predictions/Elevation.tif")


observed.weights <- pred_dat$Elev_stder ^ -2
MSE.full <- sum(((pred_dat$Elev_beta - pred_dat$Elev.mod.prediction.full) ^ 2) *
                  observed.weights) / sum(observed.weights)
MSE.null <- sum(((pred_dat$Elev_beta - pred_dat$Elev.mod.prediction.null) ^ 2) *
                  observed.weights) / sum(observed.weights)
1 - (MSE.full / MSE.null) 

MAE.full <- sum((abs(pred_dat$Elev_beta - pred_dat$Elev.mod.prediction.full)) *
                  observed.weights) / sum(observed.weights)
MAE.null <- sum((abs(pred_dat$Elev_beta - pred_dat$Elev.mod.prediction.null)) *
                  observed.weights) / sum(observed.weights)
1 - (MAE.full / MAE.null) 

# Roughness
pred_dat$Rough.mod.prediction.full <- predict(Rough.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Rough.mod.prediction.null <- predict(Rough.mod.null, newdata = pred_dat, re.form = NA)

plot(pred_dat$Rough_beta, pred_dat$Rough.mod.prediction.full,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     main = "Roughness")
points(pred_dat$Rough_beta, pred_dat$Rough.mod.prediction.null, col = "blue")
abline(0,1, col = "red")
tiff("Figures_and_Results/TWS/predictions/Rough.tif")
dev.off()

observed.weights <- pred_dat$Rough_stder ^ -2
MSE.full <- sum(((pred_dat$Rough_beta - pred_dat$Rough.mod.prediction.full) ^ 2) *
                  observed.weights) / sum(observed.weights)
MSE.null <- sum(((pred_dat$Rough_beta - pred_dat$Rough.mod.prediction.null) ^ 2) *
                  observed.weights) / sum(observed.weights)
1 - (MSE.full / MSE.null) 

MAE.full <- sum((abs(pred_dat$Rough_beta - pred_dat$Rough.mod.prediction.full)) *
                  observed.weights) / sum(observed.weights)
MAE.null <- sum((abs(pred_dat$Rough_beta - pred_dat$Rough.mod.prediction.null)) *
                  observed.weights) / sum(observed.weights)
1 - (MAE.full / MAE.null) 

# Herb
pred_dat$Herb.mod.prediction.full <- predict(Herb.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Herb.mod.prediction.null <- predict(Herb.mod.null, newdata = pred_dat, re.form = NA)

plot(y = pred_dat$Herb_beta, x = pred_dat$Herb.mod.prediction.full,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     main = "Herbaceous cover")
points(pred_dat$Herb_beta, pred_dat$Herb.mod.prediction.null, col = "blue")
abline(0,1, col = "red")
tiff("Figures_and_Results/TWS/predictions/Herb.tif")
dev.off()

observed.weights <- pred_dat$Herb_stder ^ -2
MSE.full <- sum(((pred_dat$Herb_beta - pred_dat$Herb.mod.prediction.full) ^ 2) *
                  observed.weights) / sum(observed.weights)
MSE.null <- sum(((pred_dat$Herb_beta - pred_dat$Herb.mod.prediction.null) ^ 2) *
                  observed.weights) / sum(observed.weights)
1 - (MSE.full / MSE.null) 

MAE.full <- sum((abs(pred_dat$Herb_beta - pred_dat$Herb.mod.prediction.full)) *
                  observed.weights) / sum(observed.weights)
MAE.null <- sum((abs(pred_dat$Herb_beta - pred_dat$Herb.mod.prediction.null)) *
                  observed.weights) / sum(observed.weights)
1 - (MAE.full / MAE.null) 

# Shrub
pred_dat$Shrub.mod.prediction.full <- predict(Shrub.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Shrub.mod.prediction.null <- predict(Shrub.mod.null, newdata = pred_dat, re.form = NA)

plot(pred_dat$Shrub_beta, pred_dat$Shrub.mod.prediction.full,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     main = "Shrub")
points(pred_dat$Shrub_beta, pred_dat$Shrub.mod.prediction.null, col = "blue")
abline(0,1, col = "red")
tiff("Figures_and_Results/TWS/predictions/Shrub.tif")
dev.off()


observed.weights <- pred_dat$Shrub_stder ^ -2
MSE.full <- sum(((pred_dat$Shrub_beta - pred_dat$Shrub.mod.prediction.full) ^ 2) *
                  observed.weights) / sum(observed.weights)
MSE.null <- sum(((pred_dat$Shrub_beta - pred_dat$Shrub.mod.prediction.null) ^ 2) *
                  observed.weights) / sum(observed.weights)
1 - (MSE.full / MSE.null) 

MAE.full <- sum((abs(pred_dat$Shrub_beta - pred_dat$Shrub.mod.prediction.full)) *
                  observed.weights) / sum(observed.weights)
MAE.null <- sum((abs(pred_dat$Shrub_beta - pred_dat$Shrub.mod.prediction.null)) *
                  observed.weights) / sum(observed.weights)
1 - (MAE.full / MAE.null) 

# Tree
pred_dat$Tree.mod.prediction.full <- predict(Tree.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Tree.mod.prediction.null <- predict(Tree.mod.null, newdata = pred_dat, re.form = NA)

plot(pred_dat$Tree_beta, pred_dat$Tree.mod.prediction.full,
     ylab = "Observed Selection Coefficients", xlab = "Predicted Selection Coefficients",
     main = "Shrub")
points(pred_dat$Tree_beta, pred_dat$Tree.mod.prediction.null, col = "blue")
abline(0,1, col = "red")
tiff("Figures_and_Results/TWS/predictions/Tree.tif")
dev.off()

observed.weights <- pred_dat$Tree_stder ^ -2
MSE.full <- sum(((pred_dat$Tree_beta - pred_dat$Tree.mod.prediction.full) ^ 2) *
                  observed.weights) / sum(observed.weights)
MSE.null <- sum(((pred_dat$Tree_beta - pred_dat$Tree.mod.prediction.null) ^ 2) *
                  observed.weights) / sum(observed.weights)
1 - (MSE.full / MSE.null) 

MAE.full <- sum((abs(pred_dat$Tree_beta - pred_dat$Tree.mod.prediction.full)) *
                  observed.weights) / sum(observed.weights)
MAE.null <- sum((abs(pred_dat$Tree_beta - pred_dat$Tree.mod.prediction.null)) *
                  observed.weights) / sum(observed.weights)
1 - (MAE.full / MAE.null) 

# sin
pred_dat$Asp_sin.mod.prediction.full <- predict(Asp_sin.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Asp_sin.mod.prediction.null <- predict(Asp_sin.mod.null, newdata = pred_dat, re.form = NA)

plot(pred_dat$Asp_sin_beta, pred_dat$Asp_sin.mod.prediction.full)
points(pred_dat$Asp_sin_beta, pred_dat$Asp_sin.mod.prediction.null, col = "red")
abline(0,1)

observed.weights <- pred_dat$Asp_sin_stder ^ -2
MSE.full <- sum(((pred_dat$Asp_sin_beta - pred_dat$Asp_sin.mod.prediction.full) ^ 2) *
                  observed.weights) / sum(observed.weights)
MSE.null <- sum(((pred_dat$Asp_sin_beta - pred_dat$Asp_sin.mod.prediction.null) ^ 2) *
                  observed.weights) / sum(observed.weights)
1 - (MSE.full / MSE.null) 

MAE.full <- sum((abs(pred_dat$Asp_sin_beta - pred_dat$Asp_sin.mod.prediction.full)) *
                  observed.weights) / sum(observed.weights)
MAE.null <- sum((abs(pred_dat$Asp_sin_beta - pred_dat$Asp_sin.mod.prediction.null)) *
                  observed.weights) / sum(observed.weights)
1 - (MAE.full / MAE.null) 

# cosin
pred_dat$Asp_cos.mod.prediction.full <- predict(Asp_cos.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Asp_cos.mod.prediction.null <- predict(Asp_cos.mod.null, newdata = pred_dat, re.form = NA)

plot(pred_dat$Asp_cos_beta, pred_dat$Asp_cos.mod.prediction.full)
points(pred_dat$Asp_cos_beta, pred_dat$Asp_cos.mod.prediction.null, col = "red")
abline(0,1)

observed.weights <- pred_dat$Asp_cos_stder ^ -2
MSE.full <- sum(((pred_dat$Asp_cos_beta - pred_dat$Asp_cos.mod.prediction.full) ^ 2) *
                  observed.weights) / sum(observed.weights)
MSE.null <- sum(((pred_dat$Asp_cos_beta - pred_dat$Asp_cos.mod.prediction.null) ^ 2) *
                  observed.weights) / sum(observed.weights)
1 - (MSE.full / MSE.null) 

MAE.full <- sum((abs(pred_dat$Asp_cos_beta - pred_dat$Asp_cos.mod.prediction.full)) *
                  observed.weights) / sum(observed.weights)
MAE.null <- sum((abs(pred_dat$Asp_cos_beta - pred_dat$Asp_cos.mod.prediction.null)) *
                  observed.weights) / sum(observed.weights)
1 - (MAE.full / MAE.null) 

################################################################################
# Predictive intervals: ----
lmer.response <- pred_dat$Elev_beta
lmer.model <- Elev.mod.full
lmer.weights <- dat$Elev_stder

# Now, let's throw in mixed effects and weights:
weighted.sd.residuals.fixed <- sqrt(sum(((lmer.response - predict(lmer.model, re.form = ~0)) ^ 2) *
                                          lmer.weights) / sum(lmer.weights))
upper95 <- predict(lmer.model, re.form = ~0) + (weighted.sd.residuals.fixed * 1.96)
lower95 <- predict(lmer.model, re.form = ~0) - (weighted.sd.residuals.fixed * 1.96)





