#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- February 2022  -------------X
#########################################X
#---- Running a series of linear mixed --X
# ----------- effects models ------------X
#########################################X
#--------------- 06/21/2022 -------------X
#########################################X

# clean env
rm(list = ls())
gc()

# Libraries
library(dplyr)
library(MuMIn)
library(lme4)
library(MASS)
library(lmerTest)
library(effects)

# Load in null.data
null.dat <- read.csv("20220719_outputs/20220718_MEM_prep.csv", header = T)

## Elevation ----

summary(null.Elev.mod <- lmer(Elev_beta ~ 0 + Intercept_beta_scale + 
                           # # Availabilities
                           # scaled_log_RAP_bio + scaled_log_SND +
                           # scaled_log_Elev +
                           # scaled_log_Road +
                           # Winter
                           is.Winter + I(is.Winter * is.Male) + #I(is.Winter * scaled_PDSI) +
                           I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                           I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                           # Spring
                           is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + #I(is.Spring * scaled_PDSI) +
                           I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                           I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                           # Summer
                           is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + #I(is.Summer * scaled_PDSI) +
                           I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                           I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                           # Fall
                           is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + #I(is.Fall * scaled_PDSI) +
                           I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                           I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                           (1 | ID), weights = weight_Elev, data = null.dat, REML = T))

r.squaredGLMM(null.Elev.mod)

## Roughness ----

null.dat$pred.Elev <- predict(null.Elev.mod, re.form = NA)

summary(null.Rough.mod <- lmer(Rough_beta ~ 0 +
                             pred.Elev + 
                            # # Availabilities
                            # scaled_log_RAP_bio + scaled_log_SND +
                            # scaled_log_Elev + scaled_log_Road +
                            # scaled_log_Rough +
                            # Winter
                            is.Winter + I(is.Winter * is.Male) + #I(is.Winter * scaled_PDSI) +
                            I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                            I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                            # Spring
                            is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + #I(is.Spring * scaled_PDSI) +
                            I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                            I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                            # Summer
                            is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + #I(is.Summer * scaled_PDSI) +
                            I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                            I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                            # Fall
                            is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + #I(is.Fall * scaled_PDSI) +
                            I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                            I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                            (1 | ID), weights = weight_Rough, data = null.dat, REML = T))

r.squaredGLMM(null.Rough.mod)

## Herb ----

null.dat$pred.Rough <- predict(null.Rough.mod, re.form = NA) 

summary(null.Herb.mod <- lmer(Herb_beta ~ 1 +
                           pred.Elev + pred.Rough +
                           # Availabilities
                           scaled_log_RAP_bio + scaled_log_SND +
                           scaled_log_Elev + scaled_log_Road +
                           scaled_log_Herb +
                           # Winter
                           is.Winter + I(is.Winter * is.Male) + #I(is.Winter * scaled_PDSI) +
                           I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                           I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                           # Spring
                           is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + #I(is.Spring * scaled_PDSI) +
                           I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                           I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                           # Summer
                           is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + #I(is.Summer * scaled_PDSI) +
                           I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                           I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                           # Fall
                           is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + #$I(is.Fall * scaled_PDSI) +
                           I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                           I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                           (1 | ID), weights = weight_Herb, data = null.dat, REML = T))

r.squaredGLMM(null.Herb.mod)

## Shrub ----

null.dat$pred.Herb <- predict(null.Herb.mod, re.form = NA) 

summary(null.Shrub.mod <- lmer(Shrub_beta ~ 0 +
                           pred.Elev + pred.Herb + pred.Rough + 
                            # # Availabilities
                            # scaled_log_RAP_bio + scaled_log_SND +
                            # scaled_log_Elev + scaled_log_Road +
                            # scaled_log_Shrub +
                            # Winter
                            is.Winter + I(is.Winter * is.Male) + #I(is.Winter * scaled_PDSI) +
                            I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                            I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                            # Spring
                            is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + #I(is.Spring * scaled_PDSI) +
                            I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                            I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                            # Summer
                            is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + #I(is.Summer * scaled_PDSI) +
                            I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                            I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                            # Fall
                            is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + #I(is.Fall * scaled_PDSI) +
                            I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                            I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                            (1 | ID), weights = weight_Shrub, data = null.dat, REML = T))

r.squaredGLMM(null.Shrub.mod)

## Tree ----

null.dat$pred.Shrub <- predict(null.Shrub.mod, re.form = NA) 

summary(null.Tree.mod <- lmer(Tree_beta ~ 0 +
                           pred.Elev + pred.Shrub +
                           pred.Herb + pred.Rough +
                           # # Availabilities
                           # scaled_log_RAP_bio + scaled_log_SND +
                           # scaled_log_Elev + scaled_log_Road +
                           # scaled_log_Tree + 
                           # Winter
                           is.Winter + I(is.Winter * is.Male) + #I(is.Winter * scaled_PDSI) +
                           I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                           I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                           # Spring
                           is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + #I(is.Spring * scaled_PDSI) +
                           I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                           I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                           # Summer
                           is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + #I(is.Summer * scaled_PDSI) +
                           I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                           I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                           # Fall
                           is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + #I(is.Fall * scaled_PDSI) +
                           I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                           I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                           (1 | ID), weights = weight_Tree, data = null.dat, REML = T))

r.squaredGLMM(null.Tree.mod)

# ASP sin ----

null.dat$pred.Tree <- predict(null.Tree.mod, re.form = NA) 

summary(null.Asp_sin.mod <- lmer(Asp_sin_beta ~ 0 +
                              pred.Elev + pred.Rough + pred.Herb +
                              pred.Shrub + pred.Tree +
                              # # Availabilities
                              # scaled_log_RAP_bio + scaled_log_SND +
                              # scaled_log_Elev + scaled_log_Road +
                              # scaled_Asp_sin +
                              # Winter
                              is.Winter + I(is.Winter * is.Male) + #I(is.Winter * scaled_PDSI) +
                              I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                              I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                              # Spring
                              is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + #I(is.Spring * scaled_PDSI) +
                              I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                              I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                              # Summer
                              is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + #I(is.Summer * scaled_PDSI) +
                              I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                              I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                              # Fall
                              is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + #I(is.Fall * scaled_PDSI) +
                              I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                              I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                              (1 | ID), weights = weight_Asp_sin, data = null.dat, REML = T))

r.squaredGLMM(null.Asp_sin.mod)

# ASP cos ----

null.dat$pred.Asp_sin <- predict(null.Asp_sin.mod, re.form = NA) 

summary(null.Asp_cos.mod <- lmer(Asp_cos_beta ~ 0 +
                              pred.Elev + pred.Rough + pred.Herb +
                              pred.Shrub + pred.Tree + pred.Asp_sin +
                              # # Availabilities
                              # scaled_log_RAP_bio + scaled_log_SND +
                              # scaled_log_Elev + scaled_log_Road +
                              # scaled_Asp_cos +
                              # Winter
                              is.Winter + I(is.Winter * is.Male) + #I(is.Winter * scaled_PDSI) +
                              I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                              I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                              # Spring
                              is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + #I(is.Spring * scaled_PDSI) +
                              I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                              I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                              # Summer
                              is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + #I(is.Summer * scaled_PDSI) +
                              I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                              I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                              # Fall
                              is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + #I(is.Fall * scaled_PDSI) +
                              I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                              I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                              (1 | ID), weights = weight_Asp_cos, data = null.dat, REML = T))

r.squaredGLMM(null.Asp_cos.mod)

# Save model outputs ----
# null_models <- c(Elev.mod, Rough.mod, Herb.mod, Shrub.mod,
#             Tree.mod, Asp_sin.mod, Asp_cos.mod)
# 
# names(null_models) <- c("elevation", "roughness", "herbaceous", "shrub",
#                    "tree", "asp_sin", "asp_cos")

#saveRDS(null_models, "20220719_outputs/20220721_null_model_outputs.rds")

