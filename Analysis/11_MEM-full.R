#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- February 2022  -------------X
#########################################X
#---- Running a series of linear mixed --X
# ----------- effects models ------------X
#########################################X
#--------------- 07/22/2022 -------------X
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

# Load in data
dat <- read.csv("Data/Outputs/RSF_outputs/20221017_mem-prep-v2.csv", header = T)

## Elevation ----

summary(Elev.mod.full <- lmer(Elev_beta ~ 0 + Intercept_beta_scale + 
                                # Availabilities
                                m_SC_bio + scaled_SND +
                                m_SC_elev +
                                scaled_log_Road +
                                # Winter
                                is.Winter + I(is.Winter * is.Male) + I(is.Winter * scaled_PDSI) +
                                I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                # Spring
                                is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + I(is.Spring * scaled_PDSI) +
                                I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                # Summer
                                is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + I(is.Summer * scaled_PDSI) +
                                I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                # Fall
                                is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + I(is.Fall * scaled_PDSI) +
                                I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                (1 | ID), weights = weight_Elev, data = dat, REML = T))
summary(Elev.mod.null <- lmer(Elev_beta ~ 0 + Intercept_beta_scale + 
                                # Winter
                                is.Winter + I(is.Winter * is.Male) +
                                I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                # Spring
                                is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) +
                                I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                # Summer
                                is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) +
                                I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                # Fall
                                is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) +
                                I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                (1 | ID), weights = weight_Elev, data = dat, REML = T))

## Roughness ----
dat$Elev.mod.prediction.full <- predict(Elev.mod.full, re.form = NA) 
summary(Rough.mod.full <- lmer(Rough_beta ~ 0 +
                                 Elev.mod.prediction.full + 
                                 # Availabilities
                                 m_SC_bio + scaled_log_SND +
                                 m_SC_elev + scaled_log_Road +
                                 m_SC_rough +
                                 # Winter
                                 is.Winter + I(is.Winter * is.Male) + I(is.Winter * scaled_PDSI) +
                                 I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                 I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                 # Spring
                                 is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + I(is.Spring * scaled_PDSI) +
                                 I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                 I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                 # Summer
                                 is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + I(is.Summer * scaled_PDSI) +
                                 I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                 I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                 # Fall
                                 is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + I(is.Fall * scaled_PDSI) +
                                 I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                 I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                 (1 | ID), weights = weight_Rough, data = dat, REML = T))

dat$Elev.mod.prediction.null <- predict(Elev.mod.null, re.form = NA) 
summary(Rough.mod.null <- lmer(Rough_beta ~ 0 +
                                 Elev.mod.prediction.null +
                                 # Winter
                                 is.Winter + I(is.Winter * is.Male) +
                                 I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                 I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                 # Spring
                                 is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) +
                                 I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                 I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                 # Summer
                                 is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) +
                                 I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                 I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                 # Fall
                                 is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) +
                                 I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                 I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                 (1 | ID), weights = weight_Rough, data = dat, REML = T))

## Herb ----
dat$Rough.mod.prediction.full <- predict(Rough.mod.full, re.form = NA) 
summary(Herb.mod.full <- lmer(Herb_beta ~ 0 +
                                Elev.mod.prediction.full +
                                Rough.mod.prediction.full + 
                                # Availabilities
                                m_SC_bio + scaled_log_SND +
                                m_SC_elev + scaled_log_Road +
                                m_SC_herb +
                                # Winter
                                is.Winter + I(is.Winter * is.Male) + I(is.Winter * scaled_PDSI) +
                                I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                # Spring
                                is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + I(is.Spring * scaled_PDSI) +
                                I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                # Summer
                                is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + I(is.Summer * scaled_PDSI) +
                                I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                # Fall
                                is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + I(is.Fall * scaled_PDSI) +
                                I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                (1 | ID), weights = weight_Herb, data = dat, REML = T))

dat$Rough.mod.prediction.null <- predict(Rough.mod.null, re.form = NA) 
summary(Herb.mod.null <- lmer(Herb_beta ~ 0 +
                                Elev.mod.prediction.null +
                                Rough.mod.prediction.null +
                                # Winter
                                is.Winter + I(is.Winter * is.Male) +
                                I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                # Spring
                                is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) +
                                I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                # Summer
                                is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) +
                                I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                # Fall
                                is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) +
                                I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                (1 | ID), weights = weight_Herb, data = dat, REML = T))

## Shrub ----
dat$Herb.mod.prediction.full <- predict(Herb.mod.full, re.form = NA) 
summary(Shrub.mod.full <- lmer(Shrub_beta ~ 0 +
                                 Elev.mod.prediction.full +
                                 Rough.mod.prediction.full +
                                 Herb.mod.prediction.full +
                                 # Availabilities
                                 m_SC_bio + scaled_log_SND +
                                 m_SC_elev + scaled_log_Road +
                                 m_SC_shrub +
                                 # Winter
                                 is.Winter + I(is.Winter * is.Male) + I(is.Winter * scaled_PDSI) +
                                 I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                 I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                 # Spring
                                 is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + I(is.Spring * scaled_PDSI) +
                                 I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                 I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                 # Summer
                                 is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + I(is.Summer * scaled_PDSI) +
                                 I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                 I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                 # Fall
                                 is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + I(is.Fall * scaled_PDSI) +
                                 I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                 I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                 (1 | ID), weights = weight_Shrub, data = dat, REML = T))

dat$Herb.mod.prediction.null <- predict(Herb.mod.null, re.form = NA) 
summary(Shrub.mod.null <- lmer(Shrub_beta ~ 0 +
                                 Elev.mod.prediction.null +
                                 Rough.mod.prediction.null +
                                 Herb.mod.prediction.null +
                                 # Winter
                                 is.Winter + I(is.Winter * is.Male) +
                                 I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                 I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                 # Spring
                                 is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) +
                                 I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                 I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                 # Summer
                                 is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) +
                                 I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                 I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                 # Fall
                                 is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) +
                                 I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                 I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                 (1 | ID), weights = weight_Shrub, data = dat, REML = T))

## Tree ----
dat$Shrub.mod.prediction.full <- predict(Shrub.mod.full, re.form = NA) 
summary(Tree.mod.full <- lmer(Tree_beta ~ 0 +
                                Elev.mod.prediction.full +
                                Rough.mod.prediction.full +
                                Herb.mod.prediction.full +
                                Shrub.mod.prediction.full +
                                # Availabilities
                                m_SC_bio + scaled_log_SND +
                                m_SC_elev + scaled_log_Road +
                                m_SC_tree + 
                                # Winter
                                is.Winter + I(is.Winter * is.Male) + I(is.Winter * scaled_PDSI) +
                                I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                # Spring
                                is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + I(is.Spring * scaled_PDSI) +
                                I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                # Summer
                                is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + I(is.Summer * scaled_PDSI) +
                                I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                # Fall
                                is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + I(is.Fall * scaled_PDSI) +
                                I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                (1 | ID), weights = weight_Tree, data = dat, REML = T))

dat$Shrub.mod.prediction.null <- predict(Shrub.mod.null, re.form = NA) 
summary(Tree.mod.null <- lmer(Tree_beta ~ 0 +
                                Elev.mod.prediction.null +
                                Rough.mod.prediction.null +
                                Herb.mod.prediction.null +
                                Shrub.mod.prediction.null +
                                # Winter
                                is.Winter + I(is.Winter * is.Male) + 
                                I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                # Spring
                                is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + 
                                I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                # Summer
                                is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + 
                                I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                # Fall
                                is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + 
                                I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                (1 | ID), weights = weight_Tree, data = dat, REML = T))

# ASP sin ----
dat$Tree.mod.prediction.full <- predict(Tree.mod.full, re.form = NA) 
summary(Asp_sin.mod.full <- lmer(Asp_sin_beta ~ 0 +
                                   Elev.mod.prediction.full +
                                   Rough.mod.prediction.full +
                                   Herb.mod.prediction.full +
                                   Shrub.mod.prediction.full +
                                   Tree.mod.prediction.full +
                                   # Availabilities
                                   m_SC_bio + scaled_log_SND +
                                   m_SC_elev + scaled_log_Road +
                                   m_SC_a.sin +
                                   # Winter
                                   is.Winter + I(is.Winter * is.Male) + I(is.Winter * scaled_PDSI) +
                                   I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                   I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                   # Spring
                                   is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + I(is.Spring * scaled_PDSI) +
                                   I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                   I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                   # Summer
                                   is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + I(is.Summer * scaled_PDSI) +
                                   I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                   I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                   # Fall
                                   is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + I(is.Fall * scaled_PDSI) +
                                   I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                   I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                   (1 | ID), weights = weight_Asp_sin, data = dat, REML = T))

dat$Tree.mod.prediction.null <- predict(Tree.mod.null, re.form = NA) 
summary(Asp_sin.mod.null <- lmer(Asp_sin_beta ~ 0 +
                                   Elev.mod.prediction.null +
                                   Rough.mod.prediction.null +
                                   Herb.mod.prediction.null +
                                   Shrub.mod.prediction.null +
                                   Tree.mod.prediction.null +
                                   # Winter
                                   is.Winter + I(is.Winter * is.Male) + 
                                   I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                   I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                   # Spring
                                   is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + 
                                   I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                   I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                   # Summer
                                   is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + 
                                   I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                   I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                   # Fall
                                   is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + 
                                   I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                   I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                   (1 | ID), weights = weight_Asp_sin, data = dat, REML = T))


# ASP cos ----
dat$Asp_sin.mod.prediction.full <- predict(Asp_sin.mod.full, re.form = NA) 
summary(Asp_cos.mod.full <- lmer(Asp_cos_beta ~ 0 +
                                   Elev.mod.prediction.full +
                                   Rough.mod.prediction.full +
                                   Herb.mod.prediction.full +
                                   Shrub.mod.prediction.full +
                                   Tree.mod.prediction.full +
                                   Asp_sin.mod.prediction.full +
                                   # Availabilities
                                   m_SC_bio + scaled_log_SND +
                                   m_SC_elev + scaled_log_Road +
                                   m_SC_a.cos +
                                   # Winter
                                   is.Winter + I(is.Winter * is.Male) + I(is.Winter * scaled_PDSI) +
                                   I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                   I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                   # Spring
                                   is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + I(is.Spring * scaled_PDSI) +
                                   I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                   I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                   # Summer
                                   is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + I(is.Summer * scaled_PDSI) +
                                   I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                   I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                   # Fall
                                   is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + I(is.Fall * scaled_PDSI) +
                                   I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                   I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                   (1 | ID), weights = weight_Asp_cos, data = dat, REML = T))

dat$Asp_sin.mod.prediction.null <- predict(Asp_sin.mod.null, re.form = NA) 
summary(Asp_cos.mod.null <- lmer(Asp_cos_beta ~ 0 +
                                   Elev.mod.prediction.null +
                                   Rough.mod.prediction.null +
                                   Herb.mod.prediction.null +
                                   Shrub.mod.prediction.null +
                                   Tree.mod.prediction.null +
                                   Asp_sin.mod.prediction.null +
                                   # Winter
                                   is.Winter + I(is.Winter * is.Male) + 
                                   I(is.Winter * is.NorthCentral) + I(is.Winter * is.NorthMnt) +
                                   I(is.Winter * is.Western) + I(is.Winter * is.SouthCentral) +
                                   # Spring
                                   is.Spring + I(is.Spring * is.Male) + I(is.Spring * is.res) + 
                                   I(is.Spring * is.NorthCentral) + I(is.Spring * is.NorthMnt) +
                                   I(is.Spring * is.Western) + I(is.Spring * is.SouthCentral) +
                                   # Summer
                                   is.Summer + I(is.Summer * is.Male) + I(is.Summer * is.res) + 
                                   I(is.Summer * is.NorthCentral) + I(is.Summer * is.NorthMnt) +
                                   I(is.Summer * is.Western) + I(is.Summer * is.SouthCentral) +
                                   # Fall
                                   is.Fall + I(is.Fall * is.Male) + I(is.Fall * is.res) + 
                                   I(is.Fall * is.NorthCentral) + I(is.Fall * is.NorthMnt) +
                                   I(is.Fall * is.Western) + I(is.Fall * is.SouthCentral) +
                                   (1 | ID), weights = weight_Asp_cos, data = dat, REML = T))

