#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##-------------- May 2022  --------------X
#########################################X
#---------- Figures and graphs ----------X
#########################################X
#--------------- 05/31/2022 -------------X
#########################################X

# trying to figure out best graphical representation of data 

# 06/06/2022:
# From Ronan: I updated the my_predict() function so that you can input specifically
# which intercepts you'd like to knock out. For both "target_predictor" 
# (i.e. the predictor you'd like to knock out for your partial residuals) and 
# "target_intercept" (i.e. the intercept(s) you'd like to knock out), you need to 
# make sure you spell them correctly so they match exactly how you put them in the 
# model. Also for the intercepts don't forget to also include (Intercept) 
#                     (the overall intercept of the model).

# clean env
rm(list = ls())
gc()

# Libraries
library(dplyr)
library(gridExtra)
library(ggplot2)
library(effects)
library(tidyverse)
library(lubridate)
library(MuMIn)
library(lme4)
library(MASS)
library(lmerTest)

# Source MEM script and pred function
source("Analysis/11_MEM-full.R")

# create a directory for the plots
dir <- "Figures_and_Results/TWS/"
plot_dir <- paste0(dir, "partial_residuals/PDSI/")
if(!dir.exists(plot_dir)){dir.create(plot_dir, recursive = T)}

# Get fit line ----
# List seasons
all_groups <- c("is.Winter", "is.Spring", "is.Summer", "is.Fall")
# List models
all_models <- c(Shrub.mod.full, Elev.mod.full, Herb.mod.full, Rough.mod.full, 
                Tree.mod.full, Asp_sin.mod.full, Asp_cos.mod.full)

# Empty df for storage
fit_line <- data.frame()


# Loop over models ----
for(m in 1:length(all_models)){
  
  # Select single model
  mod <- all_models[[m]]
  
  ## Loop over seasons ----
  for(i in 1:length(all_groups)){
    # Select single season
    group <- all_groups[i]
    
    # Grab coeff information
    beta <- summary(mod)$coefficients %>%
      as.data.frame()
    beta <- beta %>%
      dplyr::mutate(Fixed_Effects = row.names(beta)) %>%
      dplyr::relocate(Fixed_Effects, .before = Estimate)
    row.names(beta) <- NULL
    
    # Find intercept
    int <- beta$Estimate[which(beta$Fixed_Effects %in% group)]
    # Find slope
    slope <- beta$Estimate[which(beta$Fixed_Effects %in% paste0("I(", group, " * scaled_PDSI)"))]
    
    # If Winter, no distinction between movement classification
    if(group == "is.Winter"){
      int_adjust = 0
    } else{
      # If not Winter, intercept adjustment for movement classification
      int_adjust <- beta$Estimate[which(beta$Fixed_Effects %in% paste0("I(", group, " * is.res)"))]
    }
    
    # Temporary df to store results
    temp <- data.frame(pdsi = c(-4, 4),
                       # Season
                       group = group,
                       # Movement
                       is.res = c(0, 1),
                       # Model
                       model = m) %>%
      expand.grid() %>%
      distinct() %>%
      # Calculate intercept/intercept adjustment
      mutate(intercept = ifelse(is.res == 0, int, int + int_adjust),
             # slope = slope since PDSI does not directly interact w/ movement
             slope = slope) %>%
      # y axis for fit line
      mutate(y = (slope * pdsi) + intercept)
    
    # Combine df together
    fit_line <- rbind(fit_line, temp)
  }
  
}

# Change numbers to model names
fit_line <- fit_line %>% 
  mutate(model = case_when(.$model == "1" ~ "Shrub.mod",
                           .$model == "2" ~ "Elev.mod",
                           .$model == "3" ~ "Herb.mod",
                           .$model == "4" ~ "Rough.mod",
                           .$model == "5" ~ "Tree.mod",
                           .$model == "6" ~ "Asp_sin.mod",
                           .$model == "7" ~ "Asp_cos.mod"))

# Check
table(fit_line$model)

# DONE!

#PLOTTING ----
# Subset data for predicting
sub.dat <- dat %>% 
  filter(is.Female == 1)

# Subset fit line data
elev_fit <- fit_line %>% 
  filter(model == "Elev.mod")

# Partial residuals --------------
elev.wint <- sub.dat %>% 
  filter(is.Winter == 1)
elev.spr <- sub.dat %>% 
  filter(is.Spring == 1)
elev.sum <- sub.dat %>% 
  filter(is.Summer == 1)
elev.fall <- sub.dat %>% 
  filter(is.Fall == 1)

# ex: Elev model ----
# calculate predication with no road effects and no intercepts
# questions 6/6: How to perform this over a list of predictors/intercepts?

# Winter
elev_no.int_wint <- my_predict(model = Elev.mod.full, data = elev.wint, ranef = F, 
                          partial_resid = TRUE, intercept = TRUE, 
                          target_predictor = "I(is.Winter * scaled_PDSI)", 
  target_intercept = c("is.Winter"))
# Spring
elev_no.int_spr <- my_predict(model = Elev.mod.full, data = elev.spr, ranef = F, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Spring * scaled_PDSI)", 
                               target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
elev_no.int_sum<- my_predict(model = Elev.mod.full, data = elev.sum, ranef = F, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Summer * scaled_PDSI)", 
                               target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
elev_no.int_fall <- my_predict(model = Elev.mod.full, data = elev.fall, ranef = F, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Fall * scaled_PDSI)", 
                               target_intercept = c("is.Fall", "I(is.Fall * is.res)"))

# abline
# Winter
wint_line <- elev_fit %>%
  mutate(mig.tend = ifelse(group == "is.Winter", "Range", is.res)) %>%
  filter(group == "is.Winter") %>% 
  distinct()
# Spring
spr_line <- elev_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- elev_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                            is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- elev_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                            is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- elev.wint %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Elev_beta - elev_no.int_wint) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
 # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Elev)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y, color = "Range"), data = wint_line) +
  scale_color_manual(values = c("Range" = "#00BA38")) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Elevation",
       x = "PDSI (scaled & centered)") +
  ggtitle("February") +
  ylim(c(-20, 20)) +
  #xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")


## Spring ----
spr <- elev.spr %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Elev_beta - elev_no.int_spr) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Elev)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = spr_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Elevation",
       x = "PDSI (scaled & centered)") +
  ggtitle("April") +
  ylim(c(-20, 20)) +
 # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

## Summer ----
sum <- elev.sum %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Elev_beta - elev_no.int_sum) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Elev)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = sum_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Elevation",
       x = "PDSI (scaled & centered)") +
  ggtitle("July") +
  ylim(c(-20, 20)) +
 # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Fall ----
fall <- elev.fall %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Elev_beta - elev_no.int_fall) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Elev)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = fall_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
      y = "Elevation",
       x = "PDSI (scaled & centered)") +
  ggtitle("November") +
  ylim(c(-20, 20)) +
  xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))#+
  #scale_alpha(guide = 'none')

# Arrange plots ----
all.elev <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("elev_part_resid-transp.png", all.elev, path = plot_dir,
       width = 6, height = 4, unit = "in")


# ex: Rough model -----
# Partial residuals --------------
# Seperate the seasons
rough.wint <- sub.dat %>% 
  filter(is.Winter == 1)
rough.spr <- sub.dat %>% 
  filter(is.Spring == 1)
rough.sum <- sub.dat %>% 
  filter(is.Summer == 1)
rough.fall <- sub.dat %>% 
  filter(is.Fall == 1)

rough_fit <- fit_line %>% 
  filter(model == "Rough.mod")

# Winter
rough_no.int_wint <- my_predict(model = Rough.mod, data = rough.wint, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Winter * scaled_PDSI)", 
                               target_intercept = c("is.Winter"))
# Spring
rough_no.int_spr <- my_predict(model = Rough.mod, data = rough.spr, ranef = TRUE, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "I(is.Spring * scaled_PDSI)", 
                              target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
rough_no.int_sum<- my_predict(model = Rough.mod, data = rough.sum, ranef = TRUE, 
                             partial_resid = TRUE, intercept = TRUE, 
                             target_predictor = "I(is.Summer * scaled_PDSI)", 
                             target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
rough_no.int_fall <- my_predict(model = Rough.mod, data = rough.fall, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Fall * scaled_PDSI)", 
                               target_intercept = c("is.Fall", "I(is.Fall * is.res)"))

# abline
# Winter
wint_line <- rough_fit %>%
  mutate(mig.tend = ifelse(group == "is.Winter", "Range", is.res)) %>%
  filter(group == "is.Winter") %>% 
  distinct()
# Spring
spr_line <- rough_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- rough_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- rough_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- rough.wint %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Rough_beta - rough_no.int_wint) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Rough), col = "#00BA38") +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y, color = "Range"), data = wint_line) +
  scale_color_manual(values = c("Range" = "#00BA38")) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Roughness",
       x = "PDSI (scaled & centered)") +
  ggtitle("February") +
  ylim(c(-0.75, 0.75)) +
#  xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")# +
#  scale_alpha(guide = 'none')
  

## Spring ----
spr <- rough.spr%>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Rough_beta - rough_no.int_spr) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Rough)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = spr_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Roughness",
       x = "PDSI (scaled & centered)") +
  ggtitle("April") +
  ylim(c(-0.75, 0.75)) +
  #xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))#+
  #scale_alpha(guide = 'none')

## Summer ----
sum <- rough.sum %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Rough_beta - rough_no.int_sum) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Rough)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = sum_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Roughness",
       x = "PDSI (scaled & centered)") +
  ggtitle("July") +
  ylim(c(-0.75, 0.75)) +
 # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))#+
 # scale_alpha(guide = 'none')

# Fall ----
fall <- rough.fall %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Rough_beta - rough_no.int_fall) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Rough)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = fall_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Roughness",
       x = "PDSI (scaled & centered)") +
  ggtitle("November") +
  ylim(c(-0.75, 0.75)) +
 # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))#+
 # scale_alpha(guide = 'none')

# Arrange plots
all.rough <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("rough_part_resid-transp.png", all.rough, path = plot_dir,
       width = 6, height = 4, unit = "in")


# ex: Herb model -----
# Partial residuals --------------
# Separate the seasons
herb.wint <- sub.dat %>% 
  filter(is.Winter == 1)
herb.spr <- sub.dat %>% 
  filter(is.Spring == 1)
herb.sum <- sub.dat %>% 
  filter(is.Summer == 1)
herb.fall <- sub.dat %>% 
  filter(is.Fall == 1)

# filter for fit line
herb_fit <- fit_line %>% 
  filter(model == "Herb.mod")

# Winter
herb_no.int_wint <- my_predict(model = Herb.mod, data = herb.wint, ranef = TRUE, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "I(is.Winter * scaled_PDSI)", 
                                target_intercept = c("is.Winter"))
# Spring
herb_no.int_spr <- my_predict(model = Herb.mod, data = herb.spr, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Spring * scaled_PDSI)", 
                               target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
herb_no.int_sum<- my_predict(model = Herb.mod, data = herb.sum, ranef = TRUE, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "I(is.Summer * scaled_PDSI)", 
                              target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
herb_no.int_fall <- my_predict(model = Herb.mod, data = herb.fall, ranef = TRUE, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "I(is.Fall * scaled_PDSI)", 
                                target_intercept = c("is.Fall", "I(is.Fall * is.res)"))

# abline
# Winter
wint_line <- herb_fit %>%
  mutate(mig.tend = ifelse(group == "is.Winter", "Range", is.res)) %>%
  filter(group == "is.Winter") %>% 
  distinct()
# Spring
spr_line <- herb_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- herb_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- herb_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- herb.wint %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = RAP_cover_beta - herb_no.int_wint) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_RAP_cover), col = "#00BA38") +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y, color = "Range"), data = wint_line) +
  scale_color_manual(values = c("Range" = "#00BA38")) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Herbaceous",
       x = "PDSI (scaled & centered)") +
  ggtitle("February") +
  ylim(c(-0.75, 0.75)) +
  #xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right") #+
  #scale_alpha(guide = 'none')


## Spring ----
spr <- herb.spr%>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  RAP_cover_beta - herb_no.int_spr) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_RAP_cover)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = spr_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Herbaceous",
       x = "PDSI (scaled & centered)") +
  ggtitle("April") +
  ylim(c(-0.75, 0.75)) +
#  xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) #+
  #scale_alpha(guide = 'none')

## Summer ----
sum <- herb.sum %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  RAP_cover_beta - herb_no.int_sum) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_RAP_cover)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = sum_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Herbaceous",
       x = "PDSI (scaled & centered)") +
  ggtitle("July") +
  ylim(c(-0.75, 0.75)) +
 # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) #+
 # scale_alpha(guide = 'none')

# Fall ----
fall <- herb.fall %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =RAP_cover_beta - herb_no.int_fall) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_RAP_cover)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = fall_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Herbaceous",
       x = "PDSI (scaled & centered)") +
  ggtitle("November") +
  ylim(c(-0.75, 0.75)) +
#  xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) #+
#  scale_alpha(guide = 'none')

# Arrange plots
all.herb <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("herb_part_resid-transp.png", all.herb, path = plot_dir,
       width = 6, height = 4, unit = "in")

# ex: Shrub model -----
# Separate the seasons
shrub.wint <- sub.dat %>% 
  filter(is.Winter == 1)
shrub.spr <- sub.dat %>% 
  filter(is.Spring == 1)
shrub.sum <- sub.dat %>% 
  filter(is.Summer == 1)
shrub.fall <- sub.dat %>% 
  filter(is.Fall == 1)

# filter for fit line
shrub_fit <- fit_line %>% 
  filter(model == "Shrub.mod")

# Winter
shrub_no.int_wint <- my_predict(model = Shrub.mod, data = shrub.wint, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Winter * scaled_PDSI)", 
                               target_intercept = c("is.Winter"))
# Spring
shrub_no.int_spr <- my_predict(model = Shrub.mod, data = shrub.spr, ranef = TRUE, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "I(is.Spring * scaled_PDSI)", 
                              target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
shrub_no.int_sum<- my_predict(model = Shrub.mod, data = shrub.sum, ranef = TRUE, 
                             partial_resid = TRUE, intercept = TRUE, 
                             target_predictor = "I(is.Summer * scaled_PDSI)", 
                             target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
shrub_no.int_fall <- my_predict(model = Shrub.mod, data = shrub.fall, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Fall * scaled_PDSI)", 
                               target_intercept = c("is.Fall", "I(is.Fall * is.res)"))

# abline
# Winter
wint_line <- shrub_fit %>%
  mutate(mig.tend = ifelse(group == "is.Winter", "Range", is.res)) %>%
  filter(group == "is.Winter") %>% 
  distinct()
# Spring
spr_line <- shrub_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- shrub_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- shrub_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- shrub.wint %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Shrub_beta - shrub_no.int_wint) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Shrub), col = "#00BA38") +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y, color = "Range"), data = wint_line) +
  scale_color_manual(values = c("Range" = "#00BA38")) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Shrub",
       x = "PDSI (scaled & centered)") +
  ggtitle("February") +
  ylim(c(-0.75, 0.75)) +
#  xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right") 


## Spring ----
spr <- shrub.spr%>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  Shrub_beta - shrub_no.int_spr) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Shrub)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = spr_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Shrub",
       x = "PDSI (scaled & centered)") +
  ggtitle("April") +
  ylim(c(-0.75, 0.75)) +
 # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## Summer ----
sum <- shrub.sum %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  Shrub_beta - shrub_no.int_sum) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Shrub)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = sum_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Shrub",
       x = "PDSI (scaled & centered)") +
  ggtitle("July") +
  ylim(c(-0.75, 0.75)) +
 # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Fall ----
fall <- shrub.fall %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Shrub_beta - shrub_no.int_fall) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Shrub)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = fall_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Shrub",
       x = "PDSI (scaled & centered)") +
  ggtitle("November") +
  ylim(c(-0.75, 0.75)) +
 # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange plots
all.shrub <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("shrub_part_resid-trasnp.png", all.shrub, path = plot_dir,
       width = 6, height = 4, unit = "in")


# ex: Tree model -----
# Separate the seasons
tree.wint <- sub.dat %>% 
  filter(is.Winter == 1)
tree.spr <- sub.dat %>% 
  filter(is.Spring == 1)
tree.sum <- sub.dat %>% 
  filter(is.Summer == 1)
tree.fall <- sub.dat %>% 
  filter(is.Fall == 1)

# filter for fit line
tree_fit <- fit_line %>% 
  filter(model == "Tree.mod")

# Winter
tree_no.int_wint <- my_predict(model = Tree.mod, data = tree.wint, ranef = TRUE, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "I(is.Winter * scaled_PDSI)", 
                                target_intercept = c("is.Winter"))
# Spring
tree_no.int_spr <- my_predict(model = Tree.mod, data = tree.spr, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Spring * scaled_PDSI)", 
                               target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
tree_no.int_sum<- my_predict(model = Tree.mod, data = tree.sum, ranef = TRUE, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "I(is.Summer * scaled_PDSI)", 
                              target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
tree_no.int_fall <- my_predict(model = Tree.mod, data = tree.fall, ranef = TRUE, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "I(is.Fall * scaled_PDSI)", 
                                target_intercept = c("is.Fall", "I(is.Fall * is.res)"))

# abline
# Winter
wint_line <- tree_fit %>%
  mutate(mig.tend = ifelse(group == "is.Winter", "Range", is.res)) %>%
  filter(group == "is.Winter") %>% 
  distinct()
# Spring
spr_line <- tree_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- tree_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- tree_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- tree.wint %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Tree_beta - tree_no.int_wint) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Tree), col = "#00BA38") +
  # scale_alpha(range = c(0.002, 0.008)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y, color = "Range"), data = wint_line) +
  scale_color_manual(values = c("Range" = "#00BA38")) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Tree",
       x = "PDSI (scaled & centered)") +
  ggtitle("February") +
  ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right") 


## Spring ----
spr <- tree.spr%>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  Tree_beta - tree_no.int_spr) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Tree)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = spr_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "tree",
       x = "PDSI (scaled & centered)") +
  ggtitle("April") +
  ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## Summer ----
sum <- tree.sum %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  Tree_beta - tree_no.int_sum) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Tree)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = sum_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "tree",
       x = "PDSI (scaled & centered)") +
  ggtitle("July") +
  ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Fall ----
fall <- tree.fall %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Tree_beta - tree_no.int_fall) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Tree)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = fall_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "tree",
       x = "PDSI (scaled & centered)") +
  ggtitle("November") +
  ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange plots
all.tree <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("tree_part_resid-trasnp.png", all.tree, path = plot_dir,
       width = 6, height = 4, unit = "in")


# ex: Aspect (sin) -----
# Separate the seasons
Asp_sin.wint <- sub.dat %>% 
  filter(is.Winter == 1)
Asp_sin.spr <- sub.dat %>% 
  filter(is.Spring == 1)
Asp_sin.sum <- sub.dat %>% 
  filter(is.Summer == 1)
Asp_sin.fall <- sub.dat %>% 
  filter(is.Fall == 1)

# filter for fit line
Asp_sin_fit <- fit_line %>% 
  filter(model == "Asp_sin.mod")

# Winter
Asp_sin_no.int_wint <- my_predict(model = Asp_sin.mod, data = Asp_sin.wint, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Winter * scaled_PDSI)", 
                               target_intercept = c("is.Winter"))
# Spring
Asp_sin_no.int_spr <- my_predict(model = Asp_sin.mod, data = Asp_sin.spr, ranef = TRUE, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "I(is.Spring * scaled_PDSI)", 
                              target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
Asp_sin_no.int_sum<- my_predict(model = Asp_sin.mod, data = Asp_sin.sum, ranef = TRUE, 
                             partial_resid = TRUE, intercept = TRUE, 
                             target_predictor = "I(is.Summer * scaled_PDSI)", 
                             target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
Asp_sin_no.int_fall <- my_predict(model = Asp_sin.mod, data = Asp_sin.fall, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Fall * scaled_PDSI)", 
                               target_intercept = c("is.Fall", "I(is.Fall * is.res)"))


# abline
# Winter
wint_line <- Asp_sin_fit %>%
  mutate(mig.tend = ifelse(group == "is.Winter", "Range", is.res)) %>%
  filter(group == "is.Winter") %>% 
  distinct()
# Spring
spr_line <- Asp_sin_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- Asp_sin_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- Asp_sin_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- Asp_sin.wint %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Asp_sin_beta - Asp_sin_no.int_wint) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Asp_sin), col = "#00BA38") +
  # scale_alpha(range = c(0.002, 0.008)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y, color = "Range"), data = wint_line) +
  scale_color_manual(values = c("Range" = "#00BA38")) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Aspest (sin)",
       x = "PDSI (scaled & centered)") +
  ggtitle("February") +
  # # ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right") 


## Spring ----
spr <- Asp_sin.spr%>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  Asp_sin_beta - Asp_sin_no.int_spr) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Asp_sin)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = spr_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Aspest (sin)",
       x = "PSDI (scaled & centered)") +
  ggtitle("April") +
  # ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## Summer ----
sum <- Asp_sin.sum %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  Asp_sin_beta - Asp_sin_no.int_sum) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Asp_sin)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = sum_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Aspest (sin)",
       x = "PDSI (scaled & centered)") +
  ggtitle("July") +
  # ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Fall ----
fall <- Asp_sin.fall %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Asp_sin_beta - Asp_sin_no.int_fall) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Asp_sin)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = fall_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Aspest (sin)",
       x = "PDSI (scaled & centered)") +
  ggtitle("November") +
  # ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange plots
all.Asp_sin <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("Asp_sin_part_resid-transp.png", all.Asp_sin, path = plot_dir,
       width = 6, height = 4, unit = "in")

# ex: Aspect (cos) -----
# Separate the seasons
Asp_cos.wint <- sub.dat %>% 
  filter(is.Winter == 1)
Asp_cos.spr <- sub.dat %>% 
  filter(is.Spring == 1)
Asp_cos.sum <- sub.dat %>% 
  filter(is.Summer == 1)
Asp_cos.fall <- sub.dat %>% 
  filter(is.Fall == 1)

# filter for fit line
Asp_cos_fit <- fit_line %>% 
  filter(model == "Asp_cos.mod")

# Winter
# Winter
Asp_cos_no.int_wint <- my_predict(model = Asp_cos.mod, data = Asp_cos.wint, ranef = TRUE, 
                                  partial_resid = TRUE, intercept = TRUE, 
                                  target_predictor = "I(is.Winter * scaled_PDSI)", 
                                  target_intercept = c("is.Winter"))
# Spring
Asp_cos_no.int_spr <- my_predict(model = Asp_cos.mod, data = Asp_cos.spr, ranef = TRUE, 
                                 partial_resid = TRUE, intercept = TRUE, 
                                 target_predictor = "I(is.Spring * scaled_PDSI)", 
                                 target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
Asp_cos_no.int_sum<- my_predict(model = Asp_cos.mod, data = Asp_cos.sum, ranef = TRUE, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "I(is.Summer * scaled_PDSI)", 
                                target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
Asp_cos_no.int_fall <- my_predict(model = Asp_cos.mod, data = Asp_cos.fall, ranef = TRUE, 
                                  partial_resid = TRUE, intercept = TRUE, 
                                  target_predictor = "I(is.Fall * scaled_PDSI)", 
                                  target_intercept = c("is.Fall", "I(is.Fall * is.res)"))


# abline
# Winter
wint_line <- Asp_cos_fit %>%
  mutate(mig.tend = ifelse(group == "is.Winter", "Range", is.res)) %>%
  filter(group == "is.Winter") %>% 
  distinct()
# Spring
spr_line <- Asp_cos_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- Asp_cos_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- Asp_cos_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Migrant",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- Asp_cos.wint %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Asp_cos_beta - Asp_cos_no.int_wint) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Asp_cos), col = "#00BA38") +
  # scale_alpha(range = c(0.002, 0.008)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y, color = "Range"), data = wint_line) +
  scale_color_manual(values = c("Range" = "#00BA38")) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Aspest (cos)",
       x = "PDSI (scaled & centered)") +
  ggtitle("February") +
  ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right") 


## Spring ----
spr <- Asp_cos.spr%>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  Asp_cos_beta - Asp_cos_no.int_spr) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Asp_cos)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = spr_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Aspest (cos)",
       x = "PDSI (scaled & centered)") +
  ggtitle("April") +
  ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## Summer ----
sum <- Asp_cos.sum %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  Asp_cos_beta - Asp_cos_no.int_sum) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Asp_cos)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = sum_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Aspest (cos)",
       x = "PDSI (scaled & centered)") +
  ggtitle("July") +
  ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Fall ----
fall <- Asp_cos.fall %>%
  # Bin non migrants into migrants (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Migrant",
                              (.$tendency == "unk") & (.$month != "2") ~ "Migrant",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Migrant",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Migrant",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Asp_cos_beta - Asp_cos_no.int_fall) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed, alpha = weight_Asp_cos)) +
  # this needs to be changed to abline
  geom_line(aes(x = pdsi, y = y), data = fall_line) +
  #geom_smooth(aes(y = pred_observed), method = "lm") +
  #facet_wrap(~ month) +
  labs(col = "", fill = "", alpha = "Weight",
       y = "Aspest (cos)",
       x = "PDSI (scaled & centered)") +
  ggtitle("November") +
  ylim(c(-0.75, 0.75)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange plots
all.Asp_cos <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("Asp_cos_part_resid-transp.png", all.Asp_cos, path = plot_dir,
       width = 6, height = 4, unit = "in")


# W/ scaled betas ----
dat %>% 
ggplot(aes(x = scaled_bio, y = RAP_cover_beta, col = as.factor(tendency))) +
        # geom_boxplot(size = as.factor(dat$sex)) +
  geom_point() + 
  facet_wrap(~month_long) +
  theme_minimal()

dat %>% 
  ggplot(aes(x = scaled_bio, y = Beta_s_Shrub, shape = as.factor(tendency), col = as.factor(sex))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_point() + 
  facet_wrap(~month_long) +
  theme_minimal()

dat %>% 
  ggplot(aes(x = scaled_prcp, y = Beta_s_Elev, col = as.factor(tendency), fill = as.factor(sex))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~month_long) +
  theme_minimal()
dat %>% 
  ggplot(aes(x = scaled_prcp, y = Beta_s_Rough, col = as.factor(tendency))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~month_long) +
  theme_minimal()


# W/ regular betas  ----
dat %>% 
  ggplot(aes(x = scaled_prcp, y = RAP_cover_beta, col = as.factor(tendency))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~month_long) +
  theme_minimal()
dat %>% 
  ggplot(aes(x = scaled_prcp, y = Tree_beta, col = as.factor(tendency))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~month_long) +
  theme_minimal()
dat %>% 
  ggplot(aes(x = scaled_prcp, y = Shrub_beta, col = as.factor(tendency))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~month_long) +
  theme_minimal()
dat %>% 
  ggplot(aes(x = scaled_prcp, y = Elev_beta, col = as.factor(tendency))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~month_long) +
  theme_minimal()
dat %>% 
  ggplot(aes(x = scaled_prcp, y = Rough_beta, col = as.factor(tendency))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~month_long) +
  theme_minimal()

# W/ sex wrap ----
dat %>% 
  filter(!is.na(tendency)) %>% 
  ggplot(aes(x = scaled_prcp, y = Beta_s_cover, col = as.factor(tendency))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~month_long + sex) +
  theme_minimal()
dat %>% 
  filter(!is.na(tendency)) %>% 
  ggplot(aes(x = scaled_prcp, y = Beta_s_Tree, col = as.factor(tendency))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~month_long + sex) +
  theme_minimal()
dat %>% 
  filter(!is.na(tendency)) %>% 
  ggplot(aes(x = scaled_prcp, y = Beta_s_Shrub, col = as.factor(tendency))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~month_long + sex) +
  theme_minimal()
dat %>% 
  ggplot(aes(x = scaled_prcp, y = Beta_s_Elev, col = as.factor(tendency))) +
  # geom_boxplot(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~month_long + sex) +
  theme_minimal()
dat %>% 
  filter(!is.na(tendency)) %>% 
  ggplot(aes(x = scaled_prcp, y = Beta_s_Rough, col = as.factor(tendency))) +
 # geom_point(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~ month_long + sex) +
  theme_minimal()

# W/ sex and tendency wrap ----
dat %>% 
  filter(!is.na(tendency)) %>% 
  ggplot(aes(x = scaled_prcp, y = Beta_s_cover, col = as.factor(month))) +
  # geom_point(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~ sex + tendency) +
  theme_minimal()
dat %>% 
  filter(!is.na(tendency)) %>% 
  ggplot(aes(x = scaled_prcp, y = Beta_s_Tree, col = as.factor(month))) +
  # geom_point(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~ sex + tendency) +
  theme_minimal()
 dat %>%
  filter(!is.na(tendency)) %>% 
  ggplot(aes(x = scaled_prcp, y = Beta_s_Shrub, col = as.factor(month))) +
  # geom_point(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~ sex + tendency) +
  theme_minimal()

 dat %>%
  ggplot(aes(x = scaled_prcp, y = Beta_s_Elev, col = as.factor(month))) +
  # geom_point(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~ sex + tendency) +
  theme_minimal()

dat %>% 
  filter(!is.na(tendency)) %>% 
  ggplot(aes(x = scaled_prcp, y = Beta_s_Rough, col = as.factor(month))) +
  # geom_point(size = as.factor(dat$sex)) +
  geom_boxplot() + 
  facet_wrap(~ sex + tendency) +
  theme_minimal()
