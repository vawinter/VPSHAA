#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
#########################################X
#------ Prepping vartiables for RSF -----X
#---Scaling and centering of covariates--X
#########################################X
#--------------- 10/11/2022 -------------X
#########################################X
#### ---- Last edited: 10/18/2022 -------X
#########################################X

# clean env
rm(list = ls())
gc()

# Libraries
library(dplyr)

# Load in data
dir <- "Data/Processed/RSF_data/"
# only read in 18-20 files
files <- list.files(dir, full.names = T)[!list.files(dir) %in% c("202102_model_data.rds",
                                                                 "202104_model_data.rds",
                                                                  "202107_model_data.rds",
                                                                  "202111_model_data.rds")]

# merge into one file
# File lengths are different. Loading them in by season to investigate why
all_sum <- do.call(rbind, lapply(files[c(3,7,11)], readRDS)) 
all_win <- do.call(rbind, lapply(files[c(1,5,9)], readRDS)) 
all_spr <- do.call(rbind, lapply(files[c(2,6,10)], readRDS)) 
all_fall <- do.call(rbind, lapply(files[c(4,8,12)], readRDS)) 

# Snow!
all_sum$SND <- "0"

# merge
dat <- rbind(all_fall, all_win, all_sum, all_spr)
str(dat)

## Change class = character to factor
dat[, "year"] <- as.factor(dat[, "year"])
dat[, "month"] <- as.factor(dat[, "month"])

head(dat)

# Filter only available locations for finding mean and sd
avail <- dat %>% 
  filter(case_ == "FALSE")

## Elevation ----
# a. find mean
Elev_m <- mean(avail$Elevation, na.rm = T)
# b. find sd
Elev_s <- sd(avail$Elevation, na.rm = T)
#c. subtract and divide
dat$scaled_Elev <- ((dat$Elevation - Elev_m)/ Elev_s)

## Aspect(sin) ----
# a. find mean
a.sin_m <- mean(avail$Asp_sin, na.rm = T)
# b. find sd
a.sin_s <- sd(avail$Asp_sin, na.rm = T)
#c. subtract and divide
dat$scaled_Asp_sin <- ((dat$Asp_sin - a.sin_m)/ a.sin_s)

## Aspect(cos) ----
# a. find mean
a.cos_m <- mean(avail$Asp_cos, na.rm = T)
# b. find sd
a.cos_s <- sd(avail$Asp_cos, na.rm = T)
#c. subtract and divide
dat$scaled_Asp_cos <- ((dat$Asp_cos - a.cos_m)/ a.cos_s)

## Roughness ----
# a. find mean
Rough_m <- mean(avail$Roughness, na.rm = T)
# b. find sd
Rough_s <- sd(avail$Roughness, na.rm = T)
#c. subtract and divide
dat$scaled_Rough <- ((dat$Roughness - Rough_m)/ Rough_s)

## RAP (bio) ----
# a. find mean
bio_m <- mean(avail$RAP_bio, na.rm = T)
# b. find sd
bio_s <- sd(avail$RAP_bio, na.rm = T)
#c. subtract and divide
dat$scaled_RAP_bio <- ((dat$RAP_bio - bio_m)/ bio_s)

## RAP (cover) ----
# a. find mean
Herb_m <- mean(avail$Herb, na.rm = T)
# b. find sd
Herb_s <- sd(avail$Herb, na.rm = T)
#c. subtract and divide
dat$scaled_Herb <- ((dat$Herb - Herb_m)/ Herb_s)

## Shrub ----
# a. find mean
Shrub_m <- mean(avail$Shrub, na.rm = T)
# b. find sd
Shrub_s <- sd(avail$Shrub, na.rm = T)
#c. subtract and divide
dat$scaled_Shrub <- ((dat$Shrub - Shrub_m)/ Shrub_s)

## Tree ----
# a. find mean
Tree_m <- mean(avail$Tree, na.rm = T)
# b. find sd
Tree_s <- sd(avail$Tree, na.rm = T)
#c. subtract and divide
dat$scaled_Tree <- ((dat$Tree - Tree_m)/ Tree_s)

## Not working ---
# Set SND NA values to 0
avail$SND[is.na(avail$SND)] <- 0
dat$SND[is.na(dat$SND)] <- 0
## SND ----
# a. find mean
SND_m <- mean(avail$SND, na.rm = T)
# b. find sd
SND_s <- sd(avail$SND)
#c. subtract and divide
dat$scaled_SND <- ((dat$SND - SND_m)/ SND_s)


# Save output ----
saveRDS(dat, "Data/Processed/RSF_data/20221018_3rd-order_RSF-prep.rds")

# Create data frame
models_means <- data.frame(mean = c(Elev_m,  Rough_m,  Herb_m, bio_m,
                                    Shrub_m,  Tree_m, a.sin_m, a.cos_m),
                           sd = c(Elev_s,  Rough_s,  Herb_s, bio_s,
                                  Shrub_s,  Tree_s, a.sin_s, a.cos_s))

# transpose df for calling columns later
mod_fin <- data.frame(t(models_means))
names(mod_fin) <- c("Elev", "Rough", "Herb", "RAP_bio",
                    "Shrub", "Tree", "Asp_sin", "Asp_cos")

write.csv(mod_fin, "Data/Processed/RSF_data/20221018_mean-sd-preRSF.csv", row.names = F)
