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

# Line function
# List seasons
groups <- c("is.Winter", "is.Spring", "is.Summer", "is.Fall")

line <- function(model = NULL, variable = NULL, xlim = c(-4, 4), ci_level = 0.95){
  pred_dat <- expand.grid(x = seq(xlim[1], xlim[2], length.out = 50),
                          group = c("is.Winter", "is.Spring", "is.Summer", "is.Fall"),
                          is.res = c(0, 1))
  
  # Get betas
  b <- fixef(model)
  
  # Split rows into list elements
  pred_list <- split(pred_dat, 1:nrow(pred_dat))
  
  # For every row (element of the list) make the prediction with CI
  pred <- do.call(rbind, lapply(pred_list, function(r) {
    # Get intercept index
    int_ind <- which(r$group == names(b))
    # Get slope index
    slope_ind <- which(variable == names(b))
    # Get interaction index
    if (r$group != "is.Winter") {
      x_ind <- which(paste0("I(", r$group, " * is.res)") == names(b))
      # Combine intercept index and slope indices
      ind <- c(int_ind, slope_ind, x_ind)
    } else {
      # Combine intercept index and slope index
      ind <- c(int_ind, slope_ind)
    }
    # Get betas
    B <- b[ind]
    # Create data matrix
    if (r$group != "is.Winter") {
      X <- as.matrix(data.frame(Int = 1,
                                X = r$x,
                                res = r$is.res))
    } else {
      X <- as.matrix(data.frame(Int = 1,
                                X = r$x))
    }
    
    # Get vcov matrix
    S <- vcov(model)[ind, ind]
    
    # Mean prediction
    y_hat <- (X %*% B)[1,1]
    
    # SE
    se <- sqrt(diag(X %*% S %*% t(X)))
    
    # alpha for confidence interval
    alpha <- 1 - ci_level
    
    # Quantiles for confidence interval
    q <- c(alpha/2, 1 - alpha/2)
    
    # Critical values
    cv <- qnorm(q)
    
    # Bound of CI
    ci <- y_hat + cv * se
    
    # Construct data.frame to return
    r$y <- y_hat
    r$lwr <- ci[1]
    r$upr <- ci[2]
    
    # Return
    return(r)
  }))
  
}


# Subset data for predicting
# Subset data for predicting
sub.dat <- dat %>% 
 # filter(is.Female == 1) %>% 
  mutate(tendency = case_when(mig_tend == "1" ~ "mig",
                              mig_tend == "0" ~ "res",
                              mig_tend == "0.5" ~ "unk")) %>% 
  mutate(year = as.character(year))

# Subset fit line data
elev_fit <- lapply(groups, function(g) {
  line(model = Elev.mod.full, variable = paste0("I(", g, " * scaled_PDSI)")) %>% 
    filter(group == g)
}) %>% 
  bind_rows()


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
elev_no.int_wint <- my_predict(model = Elev.mod.full, data = elev.wint, ranef = TRUE, 
                          partial_resid = TRUE, intercept = TRUE, 
                          target_predictor = "I(is.Winter * scaled_PDSI)", 
  target_intercept = c("is.Winter"))
# Spring
elev_no.int_spr <- my_predict(model = Elev.mod.full, data = elev.spr, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Spring * scaled_PDSI)", 
                               target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
elev_no.int_sum<- my_predict(model = Elev.mod.full, data = elev.sum, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Summer * scaled_PDSI)", 
                               target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
elev_no.int_fall <- my_predict(model = Elev.mod.full, data = elev.fall, ranef = TRUE, 
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
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- elev_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                            is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- elev_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                            is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- elev.wint %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Elev_beta - elev_no.int_wint,
         sex_fix = case_when(sex == "M" ~ "Male",
                             sex == "F" ~ "Female",
                             sex == "U" ~ "Female")) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(sex_fix))) +
 # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
 # scale_color_manual(values = c("Range" = "#00BA38")) +
  scale_color_manual(values = c("Female" = "#00BFC4", "Male" = "#C77CFF", "Range" = "#00BA38")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "",
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Winter") +
  ggtitle("(a) Elevation") +
  coord_cartesian(ylim = c(-20, 20),  xlim = c(-2, 2)) +
  theme(text = element_text(size = 15))  +
  theme_bw() +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


## Spring ----
spr <- elev.spr %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  labs(col = "", fill = "",
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Spring") +
  ggtitle("(b) Elevation") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-20, 20),  xlim = c(-2, 2)) +
  theme_bw() +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))
   

## Summer ----
sum <- elev.sum %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
   # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  labs(col = "", fill = "",
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Summer") +
  ggtitle("(c) Elevation") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-20, 20),  xlim = c(-2, 2)) +
  theme_bw() +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))
   

# Fall ----
fall <- elev.fall %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  labs(col = "", fill = "", 
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Fall") +
  ggtitle("(d) Elevation") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-20, 20),  xlim = c(-2, 2)) +
  theme_bw() +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Arrange plots ----
all.elev <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("elev_part_resid-seas.png", all.elev, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("elev_part_resid-fall.png", fall, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("elev_part_resid-wint.png", wint, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("elev_part_resid-sum.png", sum, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("elev_part_resid-spr.png", spr, path = plot_dir,
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

rough_fit <- lapply(groups, function(g) {
  line(model = Rough.mod.full, variable = paste0("I(", g, " * scaled_PDSI)")) %>% 
    filter(group == g)
}) %>% 
  bind_rows()


# Winter
rough_no.int_wint <- my_predict(model = Rough.mod.full, data = rough.wint, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Winter * scaled_PDSI)", 
                               target_intercept = c("is.Winter"))
# Spring
rough_no.int_spr <- my_predict(model = Rough.mod.full, data = rough.spr, ranef = TRUE, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "I(is.Spring * scaled_PDSI)", 
                              target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
rough_no.int_sum<- my_predict(model = Rough.mod.full, data = rough.sum, ranef = TRUE, 
                             partial_resid = TRUE, intercept = TRUE, 
                             target_predictor = "I(is.Summer * scaled_PDSI)", 
                             target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
rough_no.int_fall <- my_predict(model = Rough.mod.full, data = rough.fall, ranef = TRUE, 
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
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- rough_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- rough_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- rough.wint %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Rough_beta - rough_no.int_wint,
         sex_fix = case_when(sex == "M" ~ "Male",
                             sex == "F" ~ "Female",
                             sex == "U" ~ "Female")) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(sex_fix))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
   # scale_color_manual(values = c("Range" = "#00BA38")) +
  scale_color_manual(values = c("Female" = "#00BFC4", "Male" = "#C77CFF", "Range" = "#00BA38")) +
  labs(col = "", fill = "", 
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Winter") +
  ggtitle("(a) Roughness") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-0.75, 0.75),  xlim = c(-2, 2)) +
  theme_bw() +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

   
  

## Spring ----
spr <- rough.spr %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  scale_size_continuous(range = c(0.01, 1.2), 
                        labels = ~scales::comma(x = -.x),
                        name = "Weighted") +
  labs(col = "", fill = "", size = "Weighted",
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Spring") +
  ggtitle("(b) Roughness") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-0.75, 0.75),  xlim = c(-2, 2)) +
  theme_bw() +
 # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

## Summer ----
sum <- rough.sum %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  labs(col = "", fill = "",
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Summer") +
  ggtitle("(c) Roughness") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-0.75, 0.75),  xlim = c(-2, 2)) +
  theme_bw() +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))
   

# Fall ----
fall <- rough.fall %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  labs(col = "", fill = "",
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Fall") +
  ggtitle("(d) Roughness") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-0.75, 0.75),  xlim = c(-2, 2)) +
  theme_bw() +
 # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))
   

# Arrange plots
all.rough <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("rough_part_resid-seas.png", all.rough, path = plot_dir,
       width = 6, height = 4, unit = "in")


ggsave("rough_part_resid-fall.png", fall, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("rough_part_resid-wint.png", wint, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("rough_part_resid-sum.png", sum, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("rough_part_resid-spr.png", spr, path = plot_dir,
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
herb_fit <- lapply(groups, function(g) {
  line(model = Herb.mod.full, variable = paste0("I(", g, " * scaled_PDSI)")) %>% 
    filter(group == g)
}) %>% 
  bind_rows()


# Winter
herb_no.int_wint <- my_predict(model = Herb.mod.full, data = herb.wint, ranef = TRUE, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "I(is.Winter * scaled_PDSI)", 
                                target_intercept = c("is.Winter"))
# Spring
herb_no.int_spr <- my_predict(model = Herb.mod.full, data = herb.spr, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Spring * scaled_PDSI)", 
                               target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
herb_no.int_sum<- my_predict(model = Herb.mod.full, data = herb.sum, ranef = TRUE, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "I(is.Summer * scaled_PDSI)", 
                              target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
herb_no.int_fall <- my_predict(model = Herb.mod.full, data = herb.fall, ranef = TRUE, 
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
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- herb_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- herb_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- herb.wint %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Herb_beta - herb_no.int_wint,
         sex_fix = case_when(sex == "M" ~ "Male",
                    sex == "F" ~ "Female",
                    sex == "U" ~ "Female")) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(sex_fix))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Female" = "#00BFC4", "Male" = "#C77CFF", "Range" = "#00BA38")) +
  labs(col = "", fill = "",# size = "Weighted",
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Winter") +
  ggtitle("(a) Herbaceous") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-0.2, 0.2),  xlim = c(-2, 2)) +
  theme_bw() +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

## Spring ----
spr <- herb.spr%>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  Herb_beta - herb_no.int_spr) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  labs(col = "", fill = "", #size = "Weighted",
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Spring") +
  ggtitle("(b) Herbaceous") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-0.2, 0.2),  xlim = c(-2, 2)) +
  theme_bw() +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))
   

## Summer ----
sum <- herb.sum %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =  Herb_beta - herb_no.int_sum) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  labs(col = "", fill = "",# size = "Weighted",
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Summer") +
  ggtitle("(c) Herbaceous") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-0.2, 0.2),  xlim = c(-2, 2)) +
  theme_bw() +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))
   

# Fall ----
fall <- herb.fall %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed =Herb_beta - herb_no.int_fall) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  labs(col = "", fill = "", #size = "Weighted",
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Fall") +
  ggtitle("(d) Herbaceous") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-0.2, 0.2),  xlim = c(-2, 2)) +
  theme_bw() +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))
   

# Arrange plots
all.herb <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("herb_part_resid-seas.png", all.herb, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("herb_part_resid-fall.png", fall, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("herb_part_resid-wint.png", wint, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("herb_part_resid-sum.png", sum, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("herb_part_resid-spr.png", spr, path = plot_dir,
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
shrub_fit <- lapply(groups, function(g) {
  line(model = Shrub.mod.full, variable = paste0("I(", g, " * scaled_PDSI)")) %>% 
    filter(group == g)
}) %>% 
  bind_rows()


# Winter
shrub_no.int_wint <- my_predict(model = Shrub.mod.full, data = shrub.wint, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Winter * scaled_PDSI)", 
                               target_intercept = c("is.Winter"))
# Spring
shrub_no.int_spr <- my_predict(model = Shrub.mod.full, data = shrub.spr, ranef = TRUE, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "I(is.Spring * scaled_PDSI)", 
                              target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
shrub_no.int_sum<- my_predict(model = Shrub.mod.full, data = shrub.sum, ranef = TRUE, 
                             partial_resid = TRUE, intercept = TRUE, 
                             target_predictor = "I(is.Summer * scaled_PDSI)", 
                             target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
shrub_no.int_fall <- my_predict(model = Shrub.mod.full, data = shrub.fall, ranef = TRUE, 
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
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- shrub_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- shrub_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- shrub.wint %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Shrub_beta - shrub_no.int_wint,
         sex_fix = case_when(sex == "M" ~ "Male",
                             sex == "F" ~ "Female",
                             sex == "U" ~ "Female")) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(sex_fix))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Female" = "#00BFC4", "Male" = "#C77CFF", "Range" = "#00BA38")) +
  labs(col = "", fill = "", 
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Winter") +
  ggtitle("(a) Shrub") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-0.5, 0.5),  xlim = c(-2, 2)) +
  theme_bw() +
 # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


## Spring ----
spr <- shrub.spr%>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  labs(col = "", fill = "", 
       y = "",
       x = "Drought (PDSI)",
       subtitle = "Spring") +
  ggtitle("(b) Shrub") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-0.5, 0.5),  xlim = c(-2, 2)) +
  theme_bw() +
  #theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


## Summer ----
sum <- shrub.sum %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  labs(col = "", fill = "", 
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Summer") +
  ggtitle("(c) Shrub") +
  ylim(c(-0.5, 0.5)) +
  theme(text = element_text(size = 15))  +
  # xlim(c(-2, 4)) +
  theme_bw() +
  coord_cartesian(ylim = c(-0.5, 0.5),  xlim = c(-2, 2)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Fall ----
fall <- shrub.fall %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  labs(col = "", fill = "", 
       y = "",
       x = "Drought (PDSI)",
       subtitle = "Fall") +
  ggtitle("(d) Shrub") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-0.5, 0.5),  xlim = c(-2, 2)) +
  theme_bw() +
 # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


# Arrange plots
all.shrub <- grid.arrange(wint, spr, sum, fall)
dev.off()
# save output graph
ggsave("shrub_part_resid-seas.png", all.shrub, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("shrub_part_resid-fall.png", fall, path = plot_dir,
width = 6, height = 4, unit = "in")

ggsave("shrub_part_resid-wint.png", wint, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("shrub_part_resid-sum.png", sum, path = plot_dir,
       width = 6, height = 4, unit = "in")

ggsave("shrub_part_resid-spr.png", spr, path = plot_dir,
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
tree_fit <- lapply(groups, function(g) {
  line(model = Tree.mod.full, variable = paste0("I(", g, " * scaled_PDSI)")) %>% 
    filter(group == g)
}) %>% 
  bind_rows()

# Winter
tree_no.int_wint <- my_predict(model = Tree.mod.full, data = tree.wint, ranef = TRUE, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "I(is.Winter * scaled_PDSI)", 
                                target_intercept = c("is.Winter"))
# Spring
tree_no.int_spr <- my_predict(model = Tree.mod.full, data = tree.spr, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Spring * scaled_PDSI)", 
                               target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
tree_no.int_sum<- my_predict(model = Tree.mod.full, data = tree.sum, ranef = TRUE, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "I(is.Summer * scaled_PDSI)", 
                              target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
tree_no.int_fall <- my_predict(model = Tree.mod.full, data = tree.fall, ranef = TRUE, 
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
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- tree_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- tree_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- tree.wint %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
                              (.$tendency == "res") & (.$month !=  "2") ~ "Resident",
                              (.$tendency == "mig") & (.$month == "2") ~ "Range",
                              (.$tendency == "res") & (.$month == "2") ~ "Range",
                              (.$tendency == "unk") & (.$month == "2") ~ "Range",
                              (.$tendency == "NA") & (.$month == "2") ~ "Range",
                              (is.na(.$tendency)) & (.$month == "2") ~ "Range"),
         # Find y-axis: observed - predictions 
         pred_observed = Shrub_beta - shrub_no.int_wint,
         sex_fix = case_when(sex == "M" ~ "Male",
                             sex == "F" ~ "Female",
                             sex == "U" ~ "Female")) %>%
  # Plot against PDSI
  ggplot(aes(x = scaled_PDSI, col = as.factor(sex_fix))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # scale_size(range = c(0.002, 0.008)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Female" = "#00BFC4", "Male" = "#C77CFF", "Range" = "#00BA38")) +
  labs(col = "", fill = "",
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Winter") +
  ggtitle("(i) Tree")  +
  coord_cartesian(ylim = c(-3, 3),  xlim = c(-2, 2)) +
  theme_bw() +
  #theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

## Spring ----
spr <- tree.spr%>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  labs(col = "", fill = "", 
       y = "",
       x = "Drought (PDSI)",
       subtitle = "Spring") +
  ggtitle("(j) Tree") +
  coord_cartesian(ylim = c(-3, 3),  xlim = c(-2, 2)) +
  theme_bw() +
 # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


## Summer ----
sum <- tree.sum %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  labs(col = "", fill = "", 
       y = "Selection Strength",
       x = "Drought (PDSI)",
       subtitle = "Summer") +
  ggtitle("(k) Tree") +
  coord_cartesian(ylim = c(-3, 3),  xlim = c(-2, 2)) +
  theme_bw() +
  #theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


# Fall ----
fall <- tree.fall %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  labs(col = "", fill = "", 
       y = "",
       x = "Drought (PDSI)",
       subtitle = "Fall") +
  ggtitle("(l) Tree") +
  coord_cartesian(ylim = c(-3, 3),  xlim = c(-2, 2)) +
  theme_bw() +
 # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Arrange plots
all.tree <- grid.arrange(wint, spr, sum, fall)
dev.off()
# save output graph
ggsave("tree_part_resid-seas.png", all.tree, path = plot_dir,
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
Asp_sin_fit <- lapply(groups, function(g) {
  line(model = Asp_sin.mod.full, variable = paste0("I(", g, " * scaled_PDSI)")) %>% 
    filter(group == g)
}) %>% 
  bind_rows()


# Winter
Asp_sin_no.int_wint <- my_predict(model = Asp_sin.mod.full, data = Asp_sin.wint, ranef = TRUE, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "I(is.Winter * scaled_PDSI)", 
                               target_intercept = c("is.Winter"))
# Spring
Asp_sin_no.int_spr <- my_predict(model = Asp_sin.mod.full, data = Asp_sin.spr, ranef = TRUE, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "I(is.Spring * scaled_PDSI)", 
                              target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
Asp_sin_no.int_sum<- my_predict(model = Asp_sin.mod.full, data = Asp_sin.sum, ranef = TRUE, 
                             partial_resid = TRUE, intercept = TRUE, 
                             target_predictor = "I(is.Summer * scaled_PDSI)", 
                             target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
Asp_sin_no.int_fall <- my_predict(model = Asp_sin.mod.full, data = Asp_sin.fall, ranef = TRUE, 
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
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- Asp_sin_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- Asp_sin_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- Asp_sin.wint %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed), col = "#00BA38") +
  # scale_size(range = c(0.002, 0.008)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"),linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Range" = "#00BA38")) +
  labs(col = "", fill = "", 
       y = "Eastness",
       x = "Drought (PDSI)") +
  ggtitle("Winter") +
  ylim(c(-6, 6)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


## Spring ----
spr <- Asp_sin.spr%>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = lwr),linetype = "dashed", data = spr_line) +
  labs(col = "", fill = "",
       y = "Eastness",
       x = "Drought (PDSI)") +
  ggtitle("Spring") +
  ylim(c(-6, 6)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))
## Summer ----
sum <- Asp_sin.sum %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = lwr),linetype = "dashed", data = sum_line) +
  labs(col = "", fill = "", 
       y = "Eastness",
       x = "Drought (PDSI)") +
  ggtitle("Summer") +
  ylim(c(-6, 6)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Fall ----
fall <- Asp_sin.fall %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = lwr),linetype = "dashed", data = fall_line) +
  labs(col = "", fill = "", 
       y = "Eastness",
       x = "Drought (PDSI)") +
  ggtitle("Fall") +
  ylim(c(-6, 6)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))
# Arrange plots
all.Asp_sin <- grid.arrange(wint, spr, sum, fall)



# save output graph
ggsave("Asp_sin_part_resid-seas.png", all.Asp_sin, path = plot_dir,
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
Asp_cos_fit <- lapply(groups, function(g) {
  line(model = Asp_cos.mod.full, variable = paste0("I(", g, " * scaled_PDSI)")) %>% 
    filter(group == g)
}) %>% 
  bind_rows()

# Winter
# Winter
Asp_cos_no.int_wint <- my_predict(model = Asp_cos.mod.full, data = Asp_cos.wint, ranef = TRUE, 
                                  partial_resid = TRUE, intercept = TRUE, 
                                  target_predictor = "I(is.Winter * scaled_PDSI)", 
                                  target_intercept = c("is.Winter"))
# Spring
Asp_cos_no.int_spr <- my_predict(model = Asp_cos.mod.full, data = Asp_cos.spr, ranef = TRUE, 
                                 partial_resid = TRUE, intercept = TRUE, 
                                 target_predictor = "I(is.Spring * scaled_PDSI)", 
                                 target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
Asp_cos_no.int_sum<- my_predict(model = Asp_cos.mod.full, data = Asp_cos.sum, ranef = TRUE, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "I(is.Summer * scaled_PDSI)", 
                                target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
Asp_cos_no.int_fall <- my_predict(model = Asp_cos.mod.full, data = Asp_cos.fall, ranef = TRUE, 
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
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- Asp_cos_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- Asp_cos_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- Asp_cos.wint %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed), col = "#00BA38") +
  # scale_size(range = c(0.002, 0.008)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = lwr),linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Range" = "#00BA38")) +
  labs(col = "", fill = "", 
       y = "Northness",
       x = "Drought (PDSI)") +
  ggtitle("Winter") +
  ylim(c(-8, 8)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

## Spring ----
spr <- Asp_cos.spr%>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = lwr),linetype = "dashed", data = spr_line) +
  labs(col = "", fill = "", 
       y = "Northness",
       x = "Drought (PDSI)") +
  ggtitle("Spring") +
  ylim(c(-8, 8)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


## Summer ----
sum <- Asp_cos.sum %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = lwr),linetype = "dashed", data = sum_line) +
  labs(col = "", fill = "", 
       y = "Northness",
       x = "Drought (PDSI)") +
  ggtitle("Summer") +
  ylim(c(-8, 8)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


# Fall ----
fall <- Asp_cos.fall %>%
  # Bin non Movers into Movers (based on MEM)
  # Winter = not accounting for migration status
  mutate(mig.tend = case_when((.$tendency == "mig" ) & (.$month !=  "2") ~ "Mover",
                              (.$tendency == "unk") & (.$month != "2") ~ "Mover",
                              (.$tendency == "NA")  & (.$month !=  "2") ~ "Mover",
                              (is.na(.$tendency)) & (.$month != "2") ~ "Mover",
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
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = lwr),linetype = "dashed", data = fall_line) +
  labs(col = "", fill = "", 
       y = "Northness",
       x = "Drought (PDSI)") +
  ggtitle("Fall") +
  ylim(c(-8, 8)) +
  # xlim(c(-2, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Arrange plots
all.Asp_cos <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("Asp_cos_part_resid-seas.png", all.Asp_cos, path = plot_dir,
       width = 6, height = 4, unit = "in")


# static ----
plot_static <- gridExtra::grid.arrange(all.elev, all.rough)
dev.off()
# save output graph
ggsave("Static_part_resid.png", plot_static, path = plot_dir,
       width = 12, height = 10, unit = "in")

# Dynamic -----
plot_dy2 <- gridExtra::grid.arrange(all.herb, all.shrub, all.tree)
dev.off()
# save output graph
ggsave("Dynamic_part_resid-seas.png", plot_dy2, path = plot_dir,
       width = 12, height = 14, unit = "in")

