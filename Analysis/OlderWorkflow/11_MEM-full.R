#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- February 2022  -------------X
#########################################X
#---- Running a series of linear mixed --X
# ----------- effects models ------------X
#########################################X
#--------------- 10/31/2022 -------------X
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
dat <- read.csv("Data/Outputs/RSF_outputs/20221018_mem-prep.csv", header = T)

## Elevation ----
# ... Elevation ----
summary(Elev.mod.full <- lmer(Elev_beta ~ 0 + Intercept_beta_scale + 
                                # Availabilities
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
summary(Elev.mod.null <- lmer(Elev_beta ~ 1 +
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

#summary(Elev.mod.null <- lm(dat$Elev_beta ~ 1))
r.squaredGLMM(Elev.mod.full)
r.squaredGLMM(Elev.mod.null)


# ... Roughness ----
#dat$Elev.mod.prediction.full.m.scl <- scale(predict(Elev.mod.full, re.form = NA)) 
summary(Rough.mod.full <- lmer(Rough_beta ~ 0 +
                                # Elev.mod.prediction.full.m.scl + 
                                 # Availabilities
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
summary(Rough.mod.null <- lmer(Rough_beta ~ 0 +
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
r.squaredGLMM(Rough.mod.full)
r.squaredGLMM(Rough.mod.null)


# ... Herb ----
#dat$Rough.mod.prediction.full.m.scl <- scale(predict(Rough.mod.full, re.form = NA)) 
summary(Herb.mod.full <- lmer(Herb_beta ~ 0 +
                                # Elev.mod.prediction.full.m.scl +
                                # Rough.mod.prediction.full.m.scl + 
                                # Availabilities
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
summary(Herb.mod.null <- lmer(Herb_beta ~ 0 +
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
r.squaredGLMM(Herb.mod.full)
r.squaredGLMM(Herb.mod.null)


# ... Shrub ----
#dat$Herb.mod.prediction.full.m.scl <- scale(predict(Herb.mod.full, re.form = NA)) 
summary(Shrub.mod.full <- lmer(Shrub_beta ~ 0 +
                                 # Elev.mod.prediction.full.m.scl +
                                 # Rough.mod.prediction.full.m.scl +
                                 # Herb.mod.prediction.full.m.scl +
                                 # Availabilities
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
summary(Shrub.mod.null <- lmer(Shrub_beta ~ 0 +
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
r.squaredGLMM(Shrub.mod.full)
r.squaredGLMM(Shrub.mod.null)

# ... Tree ----
#dat$Shrub.mod.prediction.full.m.scl <- scale(predict(Shrub.mod.full, re.form = NA)) 
summary(Tree.mod.full <- lmer(Tree_beta ~ 0 +
                                # Elev.mod.prediction.full.m.scl +
                                # Rough.mod.prediction.full.m.scl +
                                # Herb.mod.prediction.full.m.scl +
                                # Shrub.mod.prediction.full.m.scl +
                                # Availabilities
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
summary(Tree.mod.null <- lmer(Tree_beta ~ 0 +
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
r.squaredGLMM(Tree.mod.full)
r.squaredGLMM(Tree.mod.null)

# ... ASP sin ----
#dat$Tree.mod.prediction.full.m.scl <- scale(predict(Tree.mod.full, re.form = NA)) 
summary(Asp_sin.mod.full <- lmer(Asp_sin_beta ~ 0 +
                                   # Elev.mod.prediction.full.m.scl +
                                   # Rough.mod.prediction.full.m.scl +
                                   # Herb.mod.prediction.full.m.scl +
                                   # Shrub.mod.prediction.full.m.scl +
                                   # Tree.mod.prediction.full.m.scl +
                                   # Availabilities
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
summary(Asp_sin.mod.null <- lmer(Asp_sin_beta ~ 0 +
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
r.squaredGLMM(Asp_sin.mod.full)
r.squaredGLMM(Asp_sin.mod.null)


# ... ASP cos ----
#dat$Asp_sin.mod.prediction.full.m.scl <- scale(predict(Asp_sin.mod.full, re.form = NA)) 
summary(Asp_cos.mod.full <- lmer(Asp_cos_beta ~ 0 +
                                   # Elev.mod.prediction.full.m.scl +
                                   # Rough.mod.prediction.full.m.scl +
                                   # Herb.mod.prediction.full.m.scl +
                                   # Shrub.mod.prediction.full.m.scl +
                                   # Tree.mod.prediction.full.m.scl +
                                   # Asp_sin.mod.prediction.full.m.scl +
                                   # Availabilities
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
summary(Asp_cos.mod.null <- lmer(Asp_cos_beta ~ 0 +
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
r.squaredGLMM(Asp_cos.mod.full)
r.squaredGLMM(Asp_cos.mod.null)


# Save outputs
# Save model outputs ----
models <- list(Elev.mod.full, Rough.mod.full, Herb.mod.full, Shrub.mod.full,
               Tree.mod.full, Asp_sin.mod.full, Asp_cos.mod.full)

names(models) <- c("Elev.mod.full", "Rough.mod.full", "Herb.mod.full", "Shrub.mod.full",
                   "Tree.mod.full", "Asp_sin.mod.full", "Asp_cos.mod.full")

# saveRDS(models, "Data/Outputs/MEM_outputs/20221031_model_outputs.rds")
saveRDS(models, "Data/Outputs/MEM_outputs/20230321_model_outputs.rds")
# Save in winter et al also
# saveRDS(models, "../Winter_etal_map/20221031_model_outputs.rds")
saveRDS(models, "../Winter_etal_map/20230321_model_outputs.rds")
