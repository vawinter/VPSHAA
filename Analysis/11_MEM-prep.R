#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- February 2022  -------------X
#########################################X
#-Prepping for and executing a series of-X
#------ linear mixed effects models -----X
#---Scaling and centering of covariates--X
#########################################X
#--------------- 10/17/2022 -------------X
#########################################X

# 1. Need to replace prcp with drought and add that to this final csv
# 2. Find new base MEM format

# clean env
rm(list = ls())
gc()

# Libraries
library(dplyr)
library(MuMIn)
library(lme4)
library(MASS)
library(lmerTest)

# Load in data
dat <- read.csv( "Data/Outputs/RSF_outputs/20221017-10.6.csv",
                header = T, na.strings = c("", "N/A", "NA"))

# Removing poor data folds
dat <- dat[(dat$used_in + dat$used_out) > 70, ] 


# Re-position some covariates
dat <- dat %>% 
  relocate(unit, sex, age_class, mig_tend, .before = is.Fall) 

str(dat)

## Change class = integer to factor
dat[, "year"] <- as.factor(dat[, "year"])
dat[, "month"] <- as.factor(dat[, "month"])

head(dat)

# Set SND NA values to 0
dat$m_snd[is.na(dat$m_snd)] <- 0
## SND ----
# a. find mean
SND_m <- mean(dat$m_snd, na.rm = T)
# b. find sd
SND_s <- sd(dat$m_snd, na.rm = T)
#c. subtract and divide
dat$scaled_SND <- ((dat$m_snd - SND_m)/ SND_s)

## PDSI ----
# a. find mean
p_m <- mean(dat$m_PDSI, na.rm = T)
# b. find sd
p_s <- sd(dat$m_PDSI, na.rm = T)
#c. subtract and divide
dat$scaled_PDSI <- ((dat$m_PDSI - p_m)/ p_s)

## road ----
# a. find mean
r_m <- mean(dat$m_road, na.rm = T)
# b. find sd
r_s <- sd(dat$m_road, na.rm = T)
#c. subtract and divide
dat$scaled_Road <- ((dat$m_road - r_m)/ r_s)

## Intercept -----
# need to multiply by SE for this 
dat$Intercept_beta_scale <- scale(dat$Intercept_beta * (dat$Intercept_stder ^ -2))

## Elevation ----
# a. find mean
Elev_m <- mean(dat$m_elev, na.rm = T)
# b. find sd
Elev_s <- sd(dat$m_elev, na.rm = T)
#c. subtract and divide
dat$scaled_Elev <- ((dat$m_elev - Elev_m)/ Elev_s)

## Aspect(sin) ----
# a. find mean
a.sin_m <- mean(dat$m_a.sin, na.rm = T)
# b. find sd
a.sin_s <- sd(dat$m_a.sin, na.rm = T)
#c. subtract and divide
dat$scaled_Asp_sin <- ((dat$m_a.sin - a.sin_m)/ a.sin_s)

## Aspect(cos) ----
# a. find mean
a.cos_m <- mean(dat$m_a.cos, na.rm = T)
# b. find sd
a.cos_s <- sd(dat$m_a.cos, na.rm = T)
#c. subtract and divide
dat$scaled_Asp_cos <- ((dat$m_a.cos - a.cos_m)/ a.cos_s)

## Roughness ----
# a. find mean
Rough_m <- mean(dat$m_rough, na.rm = T)
# b. find sd
Rough_s <- sd(dat$m_rough, na.rm = T)
#c. subtract and divide
dat$scaled_Rough <- ((dat$m_rough - Rough_m)/ Rough_s)

## RAP (bio) ----
# a. find mean
bio_m <- mean(dat$m_bio, na.rm = T)
# b. find sd
bio_s <- sd(dat$m_bio, na.rm = T)
#c. subtract and divide
dat$scaled_RAP_bio <- ((dat$m_bio - bio_m)/ bio_s)

## RAP (cover) ----
# a. find mean
Herb_m <- mean(dat$m_herb, na.rm = T)
# b. find sd
Herb_s <- sd(dat$m_herb, na.rm = T)
#c. subtract and divide
dat$scaled_Herb <- ((dat$m_herb - Herb_m)/ Herb_s)

## Shrub ----
# a. find mean
Shrub_m <- mean(dat$m_shrub, na.rm = T)
# b. find sd
Shrub_s <- sd(dat$m_shrub, na.rm = T)
#c. subtract and divide
dat$scaled_Shrub <- ((dat$m_shrub - Shrub_m)/ Shrub_s)

## Tree ----
# a. find mean
Tree_m <- mean(dat$m_tree, na.rm = T)
# b. find sd
Tree_s <- sd(dat$m_tree, na.rm = T)
#c. subtract and divide
dat$scaled_Tree <- ((dat$m_tree - Tree_m)/ Tree_s)

## weights
# Elevation ---
temp <- 1 / (dat$Elev_stder ^ 2)
dat$weight_Elev <- temp / sum(temp, na.rm = TRUE)

# Aspect (sin) ---
temp <- 1 / (dat$Asp_sin_stder ^ 2)
dat$weight_Asp_sin <- temp / sum(temp, na.rm = TRUE)

# Aspect (cos) ---
temp <- 1 / (dat$Asp_cos_stder ^ 2)
dat$weight_Asp_cos <- temp / sum(temp, na.rm = TRUE)

# RAP (cover) ---
temp <- 1 / (dat$Herb_stder ^ 2)
dat$weight_Herb <- temp / sum(temp, na.rm = TRUE)

# Roughness  ---
temp <- 1 / (dat$Rough_stder ^ 2)
dat$weight_Rough <- temp / sum(temp, na.rm = TRUE)

# Shrub  ---
temp <- 1 / (dat$Shrub_stder ^ 2)
dat$weight_Shrub <- temp / sum(temp, na.rm = TRUE)

# Tree  ---
temp <- 1 / (dat$Tree_stder ^ 2)
dat$weight_Tree <- temp / sum(temp, na.rm = TRUE)


# Set logs
#Scale and center
## RAP (bio) ----
# # a. find mean
# log_Bio_m <- mean(log(dat$m_bio), na.rm = T)
# # b. find sd
# log_Bio_sd <- sd(log(dat$m_bio), na.rm = T)
# #c. subtract and divide
# dat$scaled_log_RAP_bio <- ((log(dat$m_bio) - log_Bio_m)/ log_Bio_sd)
dat$scaled_log_RAP_bio <- log(dat$m__SC_bio)
## Shrub ----
# a. find mean
log_Shrub_m <- mean(log(dat$m_shrub), na.rm = T)
# b. find sd
log_Shrub_sd <- sd(log(dat$m_shrub), na.rm = T)
#c. subtract and divide
dat$scaled_log_Shrub <- ((log(dat$m_shrub) - log_Shrub_m)/ log_Shrub_sd)
dat$scaled_log_Shrub <- log(dat$m__SC_shrub)
## Tree ----
# a. find mean
log_Tree_m <- mean(log(dat$m_tree), na.rm = T)
# b. find sd
log_Tree_sd <- sd(log(dat$m_tree), na.rm = T)
#c. subtract and divide
dat$scaled_log_Tree <- ((log(dat$m_tree) - log_Tree_m)/ log_Tree_sd)

## Herb ----
# a. find mean
log_Herb_m <- mean(log(dat$m_herb), na.rm = T)
# b. find sd
log_Herb_sd <- sd(log(dat$m_herb), na.rm = T)
#c. subtract and divide
dat$scaled_log_Herb <- ((log(dat$m_herb) - log_Herb_m)/ log_Herb_sd)

## Rough ----
# a. find mean
log_Rough_m <- mean(log(dat$m_rough), na.rm = T)
# b. find sd
log_Rough_sd <- sd(log(dat$m_rough), na.rm = T)
#c. subtract and divide
dat$scaled_log_Rough <- ((log(dat$m_rough) - log_Rough_m)/ log_Rough_sd)

## SND ----
dat$m_snd_no_0 <- dat$m_snd
dat$m_snd_no_0[dat$m_snd == 0] <- min(dat$m_snd[dat$m_snd > 0]) 
# a. find mean
log_SND_m <- mean(log(dat$m_snd_no_0), na.rm = T)
# b. find sd
log_SND_sd <- sd(log(dat$m_snd_no_0), na.rm = T)
#c. subtract and divide
dat$scaled_log_SND <- ((log(dat$m_snd_no_0) - log_SND_m) / log_SND_sd)

## Elevation ----
# a. find mean
log_Elev_m <- mean(log(dat$m_elev), na.rm = T)
# b. find sd
log_Elev_sd <- sd(log(dat$m_elev), na.rm = T)
#c. subtract and divide
dat$scaled_log_Elev <- ((log(dat$m_elev) - log_Elev_m)/ log_Elev_sd)

## Road ----
dat$m_road_no_0 <- dat$m_road
dat$m_road_no_0[dat$m_road == 0] <- min(dat$m_road[dat$m_road > 0]) 
# a. find mean
log_Road_m <- mean(log(dat$m_road_no_0), na.rm = T)
# b. find sd
log_Road_sd <- sd(log(dat$m_road_no_0), na.rm = T)
#c. subtract and divide
dat$scaled_log_Road <- ((log(dat$m_road_no_0) - log_Road_m)/ log_Road_sd)

# Save output ----
write.csv(dat, "Data/Outputs/RSF_outputs/20221017_mem-prep.csv",
          row.names = FALSE)

# Create data frame
models_means <- data.frame(mean = c(log_Bio_m, log_Herb_m, log_Shrub_m, log_Tree_m,
                                    log_SND_m, log_Elev_m, log_Rough_m, log_Road_m,
                                    Elev_m,  Rough_m,  Herb_m, bio_m,
                                    Shrub_m,  Tree_m,  SND_m, p_m,
                                    a.sin_m, a.cos_m),
                           sd = c(log_Bio_sd,log_Herb_sd, log_Shrub_sd, log_Tree_sd,
                                  log_SND_sd, log_Elev_sd,log_Rough_sd, log_Road_sd,
                                  Elev_s,  Rough_s,  Herb_s, bio_s,
                                  Shrub_s,  Tree_s,  SND_s, p_s,
                                  a.sin_s, a.cos_s))

# transpose df fro calling columns later
mod_fin <- data.frame(t(models_means))
names(mod_fin) <- c("log_RAP_bio", "log_Herb", "log_Shrub", "log_Tree",
                         "log_SND", "log_Elev", "log_Rough",
                         "log_Road",  "Elev", "Rough", "Herb", "RAP_bio",
                         "Shrub", "Tree","SND", "PDSI",
                         "Asp_sin", "Asp_cos")

write.csv(mod_fin, "20220718_mean-sd-t.csv", row.names = F)
