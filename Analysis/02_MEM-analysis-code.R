# MEM preparation and analysis code for Winter et al. XXXX

# "Forecasting Animal Distribution through Individual
#  Habitat Selection: Insights for Population Inference and
#  Transferable Predictions"


rm(list = ls())
gc()

source("Analysis/xx_funs.R")

# Load in data
dat <- readRDS("Data/RSF_data.rds")

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
## SND ----
dat$m_snd_no_0 <- dat$m_snd
dat$m_snd_no_0[dat$m_snd == 0] <- min(dat$m_snd[dat$m_snd > 0]) 
# a. find mean
log_SND_m <- mean(log(dat$m_snd_no_0), na.rm = T)
# b. find sd
log_SND_sd <- sd(log(dat$m_snd_no_0), na.rm = T)
#c. subtract and divide
dat$scaled_log_SND <- ((log(dat$m_snd_no_0) - log_SND_m) / log_SND_sd)

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
saveRDS(dat, "Data/Mem-prep.rds")

# Create data frame
models_means <- data.frame(mean = c(log_SND_m, log_Road_m, SND_m, p_m),
                           
                           sd = c(log_SND_sd, log_Road_sd, SND_s, p_s))

# transpose df for calling columns later
mod_fin <- data.frame(t(models_means))
names(mod_fin) <- c("log_SND", "log_Road","SND", "PDSI")

# save model outputs for predicting
write.csv(mod_fin, "Data/Mean-sd_snow-road.csv", row.names = F)

# MEM model run ----
# 2021 data is excluded from this analysis and done later on
## Elevation ----
# ... Elevation ----
summary(Elev.mod.full <- lmer(Elev_beta ~ 0 + Intercept_beta_scale + 
                                # Availability
                                m_SC_bio +
                                scaled_log_SND +
                                scaled_log_Road +
                                m_SC_elev +
                                # Winter
                                is.Winter + 
                                I(is.Winter * is.Male) +
                                I(is.Winter * scaled_PDSI) +
                                # Spring
                                is.Spring +
                                I(is.Spring * is.res) +
                                I(is.Spring * scaled_PDSI) +
                                # Summer
                                is.Summer + 
                                I(is.Summer * is.res) +
                                I(is.Summer * scaled_PDSI) +
                                # Fall
                                is.Fall + 
                                I(is.Fall * is.res) +
                                I(is.Fall * scaled_PDSI) +
                                (1 | unit:year) + (1 | ID),
                              data = dat, REML = T, weights = weight_Elev))
summary(Elev.mod.seas <- lmer(Elev_beta ~ 1 +
                                # Winter
                                is.Winter +
                                # Spring
                                is.Spring +
                                # Summer
                                is.Summer +
                                # Fall
                                is.Fall +
                                (1 | unit:year) +
                                (1 | ID),
                              weights = weight_Elev, data = dat, REML = T))

summary(Elev.mod.null <- lm(dat$Elev_beta ~ 1))
r.squaredGLMM(Elev.mod.full)
r.squaredGLMM(Elev.mod.seas)


# ... Roughness ----
summary(Rough.mod.full <- lmer(Rough_beta ~ 0 +
                                 # Availability
                                 m_SC_bio +
                                 scaled_log_SND +
                                 scaled_log_Road +
                                 m_SC_rough +
                                 # Winter
                                 is.Winter + 
                                 I(is.Winter * is.Male) +
                                 I(is.Winter * scaled_PDSI) +
                                 # Spring
                                 is.Spring + 
                                 I(is.Spring * is.res) +
                                 I(is.Spring * scaled_PDSI) +
                                 # Summer
                                 is.Summer + 
                                 I(is.Summer * is.res) +
                                 I(is.Summer * scaled_PDSI) +
                                 # Fall
                                 is.Fall + 
                                 I(is.Fall * is.res) +
                                 I(is.Fall * scaled_PDSI) +
                                 (1 | unit:year) + (1 | ID),
                               weights = weight_Rough, data = dat, REML = T))
summary(Rough.mod.seas <- lmer(Rough_beta ~ 0 +
                                 # Winter
                                 is.Winter + 
                                 # Spring
                                 is.Spring + 
                                 # Summer
                                 is.Summer + 
                                 # Fall
                                 is.Fall + 
                                 (1 | unit:year) + (1 | ID),
                               weights = weight_Rough, data = dat, REML = T))
summary(Rough.mod.null <- lm(dat$Rough_beta ~ 1))
r.squaredGLMM(Rough.mod.full)
r.squaredGLMM(Rough.mod.null)


# ... Herb ----
summary(Herb.mod.full <- lmer(Herb_beta ~ 0 +
                                # Availability
                                m_SC_bio +
                                scaled_log_SND +
                                scaled_log_Road +
                                m_SC_herb +
                                # Winter
                                is.Winter + 
                                I(is.Winter * is.Male) +
                                I(is.Winter * scaled_PDSI) +
                                # Spring
                                is.Spring + 
                                I(is.Spring * is.res) +
                                I(is.Spring * scaled_PDSI) +
                                # Summer
                                is.Summer + 
                                I(is.Summer * is.res) +
                                I(is.Summer * scaled_PDSI) +
                                # Fall
                                is.Fall + 
                                I(is.Fall * is.res) +
                                I(is.Fall * scaled_PDSI) +
                                (1 | unit:year) + (1 | ID),
                              weights = weight_Herb, data = dat, REML = T))
summary(Herb.mod.seas <- lmer(Herb_beta ~ 0 +
                                # Winter
                                is.Winter + 
                                # Spring
                                is.Spring + 
                                # Summer
                                is.Summer + 
                                # Fall
                                is.Fall + 
                                (1 | unit:year) + (1 | ID),
                              weights = weight_Herb, data = dat, REML = T))
summary(Herb.mod.null <- lm(dat$Herb_beta ~ 1))
r.squaredGLMM(Herb.mod.full)
r.squaredGLMM(Herb.mod.null)


# ... Shrub ----
summary(Shrub.mod.full <- lmer(Shrub_beta ~ 0 +
                                 # Availability
                                 m_SC_bio +
                                 scaled_log_SND +
                                 scaled_log_Road +
                                 m_SC_shrub +
                                 # Winter
                                 is.Winter + 
                                 I(is.Winter * is.Male) +
                                 I(is.Winter * scaled_PDSI) +
                                 # Spring
                                 is.Spring + 
                                 I(is.Spring * is.res) +
                                 I(is.Spring * scaled_PDSI) +
                                 # Summer
                                 is.Summer + 
                                 I(is.Summer * is.res) +
                                 I(is.Summer * scaled_PDSI) +
                                 # Fall
                                 is.Fall + 
                                 I(is.Fall * is.res) +
                                 I(is.Fall * scaled_PDSI) +
                                 (1 | unit:year) + (1 | ID),
                               weights = weight_Shrub, data = dat, REML = T))
summary(Shrub.mod.seas <- lmer(Shrub_beta ~ 0 +
                                 # Winter
                                 is.Winter + 
                                 # Spring
                                 is.Spring + 
                                 # Summer
                                 is.Summer + 
                                 # Fall
                                 is.Fall + 
                                 (1 | unit:year) + (1 | ID),
                               weights = weight_Shrub, data = dat, REML = T))
summary(Shrub.mod.null <- lm(dat$Shrub_beta ~ 1))
r.squaredGLMM(Shrub.mod.full)
r.squaredGLMM(Shrub.mod.null)

# ... Tree ----
summary(Tree.mod.full <- lmer(Tree_beta ~ 0 +
                                # Availability
                                m_SC_bio +
                                scaled_log_SND +
                                scaled_log_Road +
                                m_SC_tree + 
                                # Winter
                                is.Winter + 
                                I(is.Winter * is.Male) +
                                I(is.Winter * scaled_PDSI) +
                                # Spring
                                is.Spring + 
                                I(is.Spring * is.res) +
                                I(is.Spring * scaled_PDSI) +
                                # Summer
                                is.Summer + 
                                I(is.Summer * is.res) +
                                I(is.Summer * scaled_PDSI) +
                                # Fall
                                is.Fall + 
                                I(is.Fall * is.res) + I(is.Fall * scaled_PDSI) +
                                (1 | unit:year) + (1 | ID),
                              weights = weight_Tree, data = dat, REML = T))
summary(Tree.mod.seas <- lmer(Tree_beta ~ 0 +
                                # Winter
                                is.Winter + 
                                # Spring
                                is.Spring + 
                                # Summer
                                is.Summer + 
                                # Fall
                                is.Fall + 
                                (1 | unit:year) + (1 | ID),
                              weights = weight_Tree, data = dat, REML = T))
summary(Tree.mod.null <- lm(dat$Tree_beta ~ 1))
r.squaredGLMM(Tree.mod.full)
r.squaredGLMM(Tree.mod.null)

# ... ASP sin ----
summary(Asp_sin.mod.full <- lmer(Asp_sin_beta ~ 0 +
                                   # Availability
                                   m_SC_bio +
                                   scaled_log_SND +
                                   scaled_log_Road +
                                   m_SC_a.sin +
                                   # Winter
                                   is.Winter + 
                                   I(is.Winter * is.Male) +
                                   I(is.Winter * scaled_PDSI) +
                                   # Spring
                                   is.Spring + 
                                   I(is.Spring * is.res) +
                                   I(is.Spring * scaled_PDSI) +
                                   # Summer
                                   is.Summer + 
                                   I(is.Summer * is.res) +
                                   I(is.Summer * scaled_PDSI) +
                                   # Fall
                                   is.Fall + 
                                   I(is.Fall * is.res) +
                                   I(is.Fall * scaled_PDSI) +
                                   (1 | unit:year) + (1 | ID),
                                 weights = weight_Asp_sin, data = dat, REML = T))
summary(Asp_sin.mod.seas <- lmer(Asp_sin_beta ~ 0 +
                                   ## Winter
                                   is.Winter + 
                                   # Spring
                                   is.Spring + 
                                   # Summer
                                   is.Summer + 
                                   # Fall
                                   is.Fall + 
                                   (1 | unit:year) + (1 | ID),
                                 weights = weight_Asp_sin, data = dat, REML = T))
summary(Asp_sin.mod.null <- lm(dat$Asp_sin_beta ~ 1))
r.squaredGLMM(Asp_sin.mod.full)
r.squaredGLMM(Asp_sin.mod.null)


# ... ASP cos ----
summary(Asp_cos.mod.full <- lmer(Asp_cos_beta ~ 0 +
                                  # Availability
                                   m_SC_bio +
                                   scaled_log_SND +
                                   scaled_log_Road +
                                   m_SC_a.cos +
                                   # Winter
                                   is.Winter + 
                                   I(is.Winter * is.Male) +
                                   I(is.Winter * scaled_PDSI) +
                                   # Spring
                                   is.Spring + 
                                   I(is.Spring * is.res) +
                                   I(is.Spring * scaled_PDSI) +
                                   # Summer
                                   is.Summer + 
                                   I(is.Summer * is.res) +
                                   I(is.Summer * scaled_PDSI) +
                                   # Fall
                                   is.Fall + 
                                   I(is.Fall * is.res) +
                                   I(is.Fall * scaled_PDSI) +
                                   (1 | unit:year) + (1 | ID),
                                 weights = weight_Asp_cos, data = dat, REML = T))
summary(Asp_cos.mod.seas <- lmer(Asp_cos_beta ~ 0 +
                                   # Winter
                                   is.Winter + 
                                   # Spring
                                   is.Spring + 
                                   # Summer
                                   is.Summer + 
                                   # Fall
                                   is.Fall + 
                                   (1 | unit:year) + (1 | ID),
                                 weights = weight_Asp_cos, data = dat, REML = T))
summary(Asp_cos.mod.null <- lm(dat$Asp_cos_beta ~ 1))
r.squaredGLMM(Asp_cos.mod.full)
r.squaredGLMM(Asp_cos.mod.null)


# Save outputs
# Save model outputs ----
models <- list(Elev.mod.full, Rough.mod.full, Herb.mod.full, Shrub.mod.full,
               Tree.mod.full, Asp_sin.mod.full, Asp_cos.mod.full)

names(models) <- c("Elev.mod.full", "Rough.mod.full", "Herb.mod.full", "Shrub.mod.full",
                   "Tree.mod.full", "Asp_sin.mod.full", "Asp_cos.mod.full")


saveRDS(models, "Data/MEM_data.rds")

# Done! Now can plot in script 03 and/or move onto script 04 for predictive mapping
