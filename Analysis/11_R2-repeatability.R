
rm(lists = ls())
gc()

# Libraries
library(lme4)
library(lmerTest)

# Source script
source("Analysis/11_MEM-full.R")

# Define model
lmer.model <- Asp_cos.mod.full
# Get fixed effects
lmer.response  <- dat$Asp_cos_beta
# get weights
lmer.weights <- dat$weight_Asp_cos

## calculate conditional R^2
weighted.mean.response <- sum(lmer.response * lmer.weights) / sum(lmer.weights)
weighted.variance.response <- sum(((lmer.response - weighted.mean.response) ^ 2) * lmer.weights) /
  sum(lmer.weights)
weighted.variance.residuals.full <- sum(((lmer.response - predict(lmer.model)) ^ 2) * lmer.weights) /
  sum(lmer.weights)
lmer.R2c <- 1 - (weighted.variance.residuals.full / weighted.variance.response)

## calculate marginal R^2
weighted.variance.residuals.fixed <- sum(((lmer.response - predict(lmer.model, re.form = ~0)) ^ 2) * lmer.weights) /
  sum(lmer.weights)
lmer.R2m <- 1 - (weighted.variance.residuals.fixed / weighted.variance.response)

## calculate conservative ID repeatability assuming the between-individual variance is captured by
## the difference in variances between the partial model and the full one
individual.repeatability <- (weighted.variance.residuals.fixed - weighted.variance.residuals.full) /
  (weighted.variance.residuals.fixed)



# mylm <- Shrub.mod
# 
# # standard error of coefficient
# 
# days_se <- sqrt(diag(vcov(mylm)))[8]
# 
# # estimated coefficient
# 
# days_coef <- fixef(mylm)[8]
# 
# upperCI <-  days_coef + 1.96*days_se
# lowerCI <-  days_coef  - 1.96*days_se
