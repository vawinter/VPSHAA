#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- February 2022  -------------X
#########################################X
#---- Running a series of linear mixed --X
# ----------- effects models ------------X
#########################################X
#--------------- 05/09/2023 -------------X
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

## Treeation ----
# ... Treeation ----
summary(Tree.mod.full <- lmer(Tree_beta ~ 0 + Intercept_beta_scale + 
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
                                I(is.Fall * is.res) +
                                I(is.Fall * scaled_PDSI) +
                                (1 | unit:year) + (1 | ID),
                              data = dat, REML = T, weights = weight_Tree))


summary(Tree.mod.seas <- lmer(Tree_beta ~ 1 +
                                # Winter
                              #  is.Winter +
                                # Spring
                                is.Spring +
                                # Summer
                                is.Summer +
                                # Fall
                                is.Fall +
                                #(1 | unit:year) + 
                                (1 | ID),
                              weights = weight_Tree, data = dat, REML = T))

summary(Tree.mod.avail <- lmer(Tree_beta ~ 1 +
                                 # Availabilities
                                 m_SC_bio +
                                 scaled_log_SND +
                                 scaled_log_Road +
                                 m_SC_tree + 
                                (1 | ID),
                              weights = weight_Tree, data = dat, REML = T))

summary(Tree.mod.null <- lmer(Tree_beta ~ 1 +
                                # # Winter
                                # is.Winter +
                                # # Spring
                                # is.Spring +
                                # # Summer
                                # is.Summer +
                                # # Fall
                                # is.Fall +
                                #(1 | unit:year) + 
                                (1 | ID),
                              weights = weight_Tree, data = dat, REML = T))

r.squaredGLMM(Tree.mod.full)
r.squaredGLMM(Tree.mod.seas)
r.squaredGLMM(Tree.mod.avail)
r.squaredGLMM(Tree.mod.null)

pred_dat$Tree.mod.prediction.full.m <- predict(Tree.mod.full, newdata = pred_dat, re.form = NA)
pred_dat$Tree.mod.prediction.null.m <- predict(Tree.mod.null, newdata = pred_dat, re.form = NA)
pred_dat$Tree.mod.prediction.seas.m <- predict(Tree.mod.seas, newdata = pred_dat, re.form = NA)
pred_dat$Tree.mod.prediction.avail.m <- predict(Tree.mod.avail, newdata = pred_dat, re.form = NA)

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
     main = "Treeness")
points(x = pred_dat$Tree.mod.prediction.null.m, y = pred_dat$Tree_beta, col = "blue", 
       pch = 19, cex = 100 * observed.weights)
abline(0,1, col = "red")
tiff("Figures_and_Results/Manuscript/Tree.tif")
dev.off()


cor.seas <- weightedCorr(pred_dat$Tree_beta,
                         pred_dat$Tree.mod.prediction.seas.m,
                         method = "Pearson",
                         weights = observed.weights)

cor.avail <- weightedCorr(pred_dat$Tree_beta,
                          pred_dat$Tree.mod.prediction.avail.m,
                          method = "Pearson",
                          weights = observed.weights)

(cor.full - cor.seas) / 2
(cor.full - cor.avail) / 2


