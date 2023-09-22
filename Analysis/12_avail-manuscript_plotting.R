#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##-------------- May 2022  --------------X
#########################################X
#---------- Figures and graphs ----------X
#########################################X
#--------------- 10/18/2022 -------------X
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
library(patchwork)
library(ggplot2)
library(effects)
library(tidyverse)
library(lubridate)
library(MuMIn)
library(lme4)
library(MASS)
library(lmerTest)
library(gridExtra)

# Source MEM script 
source("Analysis/11_MEM-full.R")
source("Analysis/999_RBH-partial_resid.R")
# Run script '999_RBH-partial_resid.R' for prep fun

# create a directory for the plots
dir <- "Figures_and_Results/Manuscript/"
plot_dir <- paste0(dir, "partial_residuals/Availability/")
if(!dir.exists(plot_dir)){dir.create(plot_dir, recursive = T)}

# Line fun ----
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
 sub.dat <- dat# %>% 
#   # filter(is.Female == 1) %>% 
#   mutate(tendency = case_when(mig_tend == "1" ~ "mig",
#                               mig_tend == "0" ~ "res",
#                               mig_tend == "0.5" ~ "unk")) %>% 
#   mutate(year = as.character(year))


# Subset fit line data
elev_fit <- line(model = Elev.mod.full, variable = "m_SC_elev")


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
# Issue: 10/18: Does random effect NEED to be true? getting an error in this case
# -- works when ranef = F

# Winter
elev_no.int_wint <- my_predict(model = Elev.mod.full, data = elev.wint, ranef = F, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "m_SC_elev", 
                               target_intercept = c("is.Winter"))
# Spring
elev_no.int_spr <- my_predict(model = Elev.mod.full, data = elev.spr, ranef = F, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "m_SC_elev", 
                              target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
elev_no.int_sum <- my_predict(model = Elev.mod.full, data = elev.sum, ranef = F, 
                             partial_resid = TRUE, intercept = TRUE, 
                             target_predictor = "m_SC_elev", 
                             target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
elev_no.int_fall <- my_predict(model = Elev.mod.full, data = elev.fall, ranef = F, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "m_SC_elev", 
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
wint_elev <- elev.wint %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_elev, col = as.factor(sex_fix))) +
  # add in predictions
  #  geom_point(aes(y = pred_observed, size = -weight_Elev)) +
  geom_point(aes(y = pred_observed )) +
  # scale_size_continuous(range = c(0.01, 1),
  #                       labels = ~scales::comma(x = .x)) +
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Female" = "darkorange", "Male" = "darkorchid3", "Range" = "chartreuse3")) +
  # scale_size_continuous(range = c(0.01, 1.2),
  #                         labels = ~scales::comma(x = -.x)) +
  labs(col = "", fill = "",
       y = "",
       x = "" ,
       caption = "")+
  ggtitle("Elevation") +
  coord_cartesian(ylim = c(-50, 50),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
    text = element_text(size = 15),
    legend.position = "top",
    plot.caption.position = "panel") +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

 ## Spring ----
spr_elev <- elev.spr %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_elev, col = as.factor(mig.tend))) +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  labs(col = "", fill = "",
       x = "",
       y = "Selection Strength",
       caption = "") +
  ggtitle("") +
  coord_cartesian(ylim = c(-50, 50),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(#plot.title = element_text(hjust = 0.5),
    text = element_text(size = 15),
    legend.position = "top",
    plot.caption.position = "plot") +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

## Summer ----
sum_elev <- elev.sum %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_elev, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  labs(col = "", fill = "",
       y = "",
       x = "",
       caption = "") +
  ggtitle("") +
  coord_cartesian(ylim = c(-50, 50),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(#plot.title = element_text(hjust = 0.5),
    text = element_text(size = 15),
    legend.position = "",
    plot.caption.position = "plot") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Fall ----
fall_elev <- elev.fall %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_elev, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # scale_size_continuous(range = c(0.001, 0.009),
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  labs(col = "", fill = "", 
       y = "",
       x = "Availablilty",
       caption = "") +
  ggtitle("") +
  coord_cartesian(ylim = c(-50, 50),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(vjust=2),
    text = element_text(size = 15),
    legend.position = "",
    plot.caption.position = "plot") +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Arrange plots ----
all.elev <- wint_elev/ spr_elev/ sum_elev/ fall_elev
dev.off()
# # save output graph
ggsave("elev2.png", all.elev, path = plot_dir,
       width = 5, height = 12,
       unit = "in")

# 
# ggsave("elev-fall.png", fall, path = plot_dir,
#        width = 6, height = 4, unit = "in")
# 
# ggsave("elev-wint.png", wint, path = plot_dir,
#        width = 6, height = 4, unit = "in")
# 
# ggsave("elev-sum.png", sum, path = plot_dir,
#        width = 6, height = 4, unit = "in")
# 
# ggsave("elev-spr.png", spr, path = plot_dir,
#        width = 6, height = 4, unit = "in")

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

# Get line fit
rough_fit <- line(model = Rough.mod.full, variable = "m_SC_rough")

# Winter
rough_no.int_wint <- my_predict(model = Rough.mod.full, data = rough.wint, ranef = F, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "m_SC_rough", 
                                target_intercept = "is.Winter")
# Spring
rough_no.int_spr <- my_predict(model = Rough.mod.full, data = rough.spr, ranef = F, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "m_SC_rough", 
                               target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
rough_no.int_sum<- my_predict(model = Rough.mod.full, data = rough.sum, ranef = F, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "m_SC_rough", 
                              target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
rough_no.int_fall <- my_predict(model = Rough.mod.full, data = rough.fall, ranef = F, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "m_SC_rough", 
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
wint_r <- rough.wint %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_rough, col = as.factor(sex_fix))) +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Female" = "#00BFC4", "Male" = "#C77CFF", "Range" = "#00BA38")) +
  labs(col = "", fill = "", 
       y = "",
       x = "",
       caption = "(a) Winter") +
  ggtitle("Roughness") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "top",
        plot.caption.position = "plot") +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


## Spring ----
spr_r <- rough.spr %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_rough, col = as.factor(mig.tend))) +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  labs(col = "", fill = "", 
       y = "",
       x = "",
       caption = "(b) Spring") +
  ggtitle("") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(#plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "top",
        plot.caption.position = "plot") +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))
## Summer ----
sum_r <- rough.sum %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_rough, col = as.factor(mig.tend))) +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  labs(col = "", fill = "", 
       y = "",
       x = "",
       caption = "(c) Summer") +
  ggtitle("") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "",
        plot.caption.position = "plot") +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Fall ----
fall_r <- rough.fall %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_rough, col = as.factor(mig.tend))) +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  labs(col = "", fill = "", 
       y = "",
       x = "Availablilty",
       caption = "(d) Fall") +
  ggtitle("") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "",
        plot.caption.position = "plot") +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Arrange plots
all.rough <- wint_r/spr_r/sum_r/fall_r
dev.off()

# # save output graph
ggsave("rough.png", all.rough, path = plot_dir,
       width = 5, height = 12,
       unit = "in")

# # save output graph
# ggsave("rough_part_resid.png", all.rough, path = plot_dir,
#        width = 6, height = 4, unit = "in")
# 
# ggsave("rough_part_resid-fall.png", fall, path = plot_dir,
#        width = 6, height = 4, unit = "in")
# 
# ggsave("rough_part_resid-wint.png", wint, path = plot_dir,
#        width = 6, height = 4, unit = "in")
# 
# ggsave("rough_part_resid-sum.png", sum, path = plot_dir,
#        width = 6, height = 4, unit = "in")
# 
# ggsave("rough_part_resid-spr.png", spr, path = plot_dir,
#        width = 6, height = 4, unit = "in")



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

# calculate for fit line
herb_fit <- line(model = Herb.mod.full, variable = "m_SC_herb")

# Winter
herb_no.int_wint <- my_predict(model = Herb.mod.full, data = herb.wint, ranef = F, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "m_SC_herb", 
                               target_intercept = c("is.Winter"))
# Spring
herb_no.int_spr <- my_predict(model = Herb.mod.full, data = herb.spr, ranef = F, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "m_SC_herb", 
                              target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
herb_no.int_sum<- my_predict(model = Herb.mod.full, data = herb.sum, ranef = F, 
                             partial_resid = TRUE, intercept = TRUE, 
                             target_predictor = "m_SC_herb", 
                             target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
herb_no.int_fall <- my_predict(model = Herb.mod.full, data = herb.fall, ranef = F, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "m_SC_herb", 
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
wint_herb <- herb.wint %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_herb, col = as.factor(sex_fix))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +#, col = "#00BA38") +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Female" = "darkorange", "Male" = "darkorchid3", "Range" = "chartreuse3")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "", 
       y = "",
       x = "",
       subtitle = "(a) Winter") +
  ggtitle("Herbaceous") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "top",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


## Spring ----
spr_herb <- herb.spr %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_herb, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "", 
       y = "Selection Strength",
       x = "",
       subtitle = "(b) Spring") +
  ggtitle("") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(#plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "top",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

## Summer ----
sum_herb <- herb.sum %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_herb, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "", 
       y = "",
       x = "",
       subtitle = "(c) Summer") +
  ggtitle("") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(#plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Fall ----
fall_herb <- herb.fall %>%
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
         pred_observed = Herb_beta - herb_no.int_fall) %>%
  # Plot against avail
  ggplot(aes(x = m_SC_herb, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "", 
       y = "",
       x = "Availability",
       subtitle = "(d) Fall") +
  ggtitle("") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Arrange plots
all.herb <- wint_herb/spr_herb/sum_herb/fall_herb
dev.off()
#save output graph
ggsave("herb.png", all.herb, path = plot_dir,
       width = 5, height = 12, unit = "in")


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

# Calculate fit line
shrub_fit <- line(model = Shrub.mod.full, variable = "m_SC_shrub")

# Winter
shrub_no.int_wint <- my_predict(model = Shrub.mod.full, data = shrub.wint, ranef = F, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "m_SC_shrub", 
                                target_intercept = c("is.Winter"))
# Spring
shrub_no.int_spr <- my_predict(model = Shrub.mod.full, data = shrub.spr, ranef = F, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "m_SC_shrub", 
                               target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
shrub_no.int_sum<- my_predict(model = Shrub.mod.full, data = shrub.sum, ranef = F, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "m_SC_shrub", 
                              target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
shrub_no.int_fall <- my_predict(model = Shrub.mod.full, data = shrub.fall, ranef = F, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "m_SC_shrub", 
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
wint_shrub <- shrub.wint %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_shrub, col = as.factor(sex_fix))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
 # geom_point(aes(y = pred_observed), col = "#00BA38") +
  geom_point(aes(y = pred_observed )) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Female" = "darkorange", "Male" = "darkorchid3", "Range" = "chartreuse3")) +
 # scale_color_manual(values = c("Range" = "#00BA38")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "", 
       y = "",
       x = "",
       subtitle = "(a) Winter") +
  ggtitle("Shrub") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "top",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


## Spring ----
spr_shrub <- shrub.spr %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_shrub, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "", 
       y = " ",
       x = "",
       subtitle = "(b) Spring") +
  ggtitle("") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "top",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


## Summer ----
sum_shrub <- shrub.sum %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_shrub, col = as.factor(mig.tend))) +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "", 
       y = " ",
       x = "",
       subtitle = "(c) Summer") +
  ggtitle("") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Fall ----
fall_shurb <- shrub.fall %>%
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
  # Plot against avail
  ggplot(aes(x = m_SC_shrub, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "", 
       y = "",
       x = "Availability",
       subtitle = "(d) Fall") +
  ggtitle("") +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Arrange plots
all.shrub <- wint_shrub/spr_shrub/sum_shrub/fall_shurb
dev.off()
# save output graph
ggsave("shrub_part_resid.png", all.shrub, path = plot_dir,
       width = 5, height = 12, unit = "in")

# ex: Tree model -----
# Separate the seasons
Tree.wint <- sub.dat %>% 
  filter(is.Winter == 1)
Tree.spr <- sub.dat %>% 
  filter(is.Spring == 1)
Tree.sum <- sub.dat %>% 
  filter(is.Summer == 1)
Tree.fall <- sub.dat %>% 
  filter(is.Fall == 1)

# Calculate fit line
Tree_fit <- line(model = Tree.mod.full, variable = "m_SC_tree")

# Winter
Tree_no.int_wint <- my_predict(model = Tree.mod.full, data = Tree.wint, ranef = F, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "m_SC_tree", 
                               target_intercept = c("is.Winter"))
# Spring
Tree_no.int_spr <- my_predict(model = Tree.mod.full, data = Tree.spr, ranef = F, 
                              partial_resid = TRUE, intercept = TRUE, 
                              target_predictor = "m_SC_tree", 
                              target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
Tree_no.int_sum<- my_predict(model = Tree.mod.full, data = Tree.sum, ranef = F, 
                             partial_resid = TRUE, intercept = TRUE, 
                             target_predictor = "m_SC_tree", 
                             target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
Tree_no.int_fall <- my_predict(model = Tree.mod.full, data = Tree.fall, ranef = F, 
                               partial_resid = TRUE, intercept = TRUE, 
                               target_predictor = "m_SC_tree", 
                               target_intercept = c("is.Fall", "I(is.Fall * is.res)"))

# abline
# Winter
wint_line <- Tree_fit %>%
  mutate(mig.tend = ifelse(group == "is.Winter", "Range", is.res)) %>%
  filter(group == "is.Winter") %>% 
  distinct()
# Spring
spr_line <- Tree_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Spring") %>% 
  distinct()
# Summer
sum_line <- Tree_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Summer") %>% 
  distinct()
# Fall
fall_line <- Tree_fit %>%
  mutate(mig.tend = case_when(is.res == 0 ~ "Mover",
                              is.res == 1 ~ "Resident")) %>%
  filter(group == "is.Fall") %>% 
  distinct()


# Plot -----
## Winter ----
wint <- Tree.wint %>%
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
         pred_observed = Tree_beta - Tree_no.int_wint,
         sex_fix = case_when(sex == "M" ~ "Male",
                             sex == "F" ~ "Female",
                             sex == "U" ~ "Female")) %>%
  # Plot against avail
  ggplot(aes(x = m_SC_tree, col = as.factor(sex_fix))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # scale_alpha(range = c(0.002, 0.008)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Female" = "darkorange", "Male" = "darkorchid3", "Range" = "chartreuse3")) +
  #scale_color_manual(values = c("Range" = "#00BA38")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "",# size = "Weighted",
       y = "",
       x = "",
       subtitle = "(a) Winter") +
  ggtitle("Tree") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "top",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))


## Spring ----
spr <- Tree.spr %>%
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
         pred_observed =  Tree_beta - Tree_no.int_spr) %>%
  # Plot against avail
  ggplot(aes(x = m_SC_tree, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "",# size = "Weighted",
       y = "Selection Strength",
       x = "",
       subtitle = "(b) Spring") +
  ggtitle("") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "top",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

## Summer ----
sum <- Tree.sum %>%
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
         pred_observed =  Tree_beta - Tree_no.int_sum) %>%
  # Plot against avail
  ggplot(aes(x = m_SC_tree, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "",# size = "Weighted",
       y = "",
       x = "",
       subtitle = "(c) Summer") +
  ggtitle("") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Fall ----
fall <- Tree.fall %>%
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
         pred_observed = Tree_beta - Tree_no.int_fall) %>%
  # Plot against avail
  ggplot(aes(x = m_SC_tree, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
  # scale_size_continuous(range = c(0.01, 1.2), 
  #                       labels = ~scales::comma(x = -.x),
  #                       name = "Weighted") +
  labs(col = "", fill = "",# size = "Weighted",
       y = "",
       x = "Availability",
       subtitle = "(d) Fall") +
  ggtitle("") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Arrange plots
all.Tree <- wint/ spr/ sum/ fall
dev.off()
# save output graph
ggsave("tree_part_resid.png", all.Tree, path = plot_dir,
       width = 5, height = 12, unit = "in")

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

# Calculate fit line
Asp_sin_fit <- line(model = Asp_sin.mod.full, variable = "m_SC_a.sin")

# Winter
Asp_sin_no.int_wint <- my_predict(model = Asp_sin.mod.full, data = Asp_sin.wint, ranef = F, 
                                  partial_resid = TRUE, intercept = TRUE, 
                                  target_predictor = "m_SC_a.sin", 
                                  target_intercept = c("is.Winter"))
# Spring
Asp_sin_no.int_spr <- my_predict(model = Asp_sin.mod.full, data = Asp_sin.spr, ranef = F, 
                                 partial_resid = TRUE, intercept = TRUE, 
                                 target_predictor = "m_SC_a.sin", 
                                 target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
Asp_sin_no.int_sum<- my_predict(model = Asp_sin.mod.full, data = Asp_sin.sum, ranef = F, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "m_SC_a.sin", 
                                target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
Asp_sin_no.int_fall <- my_predict(model = Asp_sin.mod.full, data = Asp_sin.fall, ranef = F, 
                                  partial_resid = TRUE, intercept = TRUE, 
                                  target_predictor = "m_SC_a.sin", 
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
         pred_observed = Asp_sin_beta - Asp_sin_no.int_wint,
         sex_fix = case_when(sex == "M" ~ "Male",
                             sex == "F" ~ "Female",
                             sex == "U" ~ "Female")) %>%
  # Plot against avail
  ggplot(aes(x = m_SC_a.sin, col = as.factor(sex_fix))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed )) +
  # scale_alpha(range = c(0.002, 0.008)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Female" = "#00BFC4", "Male" = "#C77CFF", "Range" = "#00BA38")) +
  labs(col = "", fill = "",# size = "Weighted",
       y = "",
       x = "",
       subtitle = "(a) Winter") +
  ggtitle("Easting") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "top",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
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
  # Plot against avail
  ggplot(aes(x = scaled_Asp_sin, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  labs(col = "", fill = "",# size = "Weighted",
       y = " ",
       x = "",
       subtitle = "(b) Spring") +
  ggtitle("") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "top",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
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
  # Plot against avail
  ggplot(aes(x = scaled_Asp_sin, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  labs(col = "", fill = "",# size = "Weighted",
       y = "",
       x = "",
       subtitle = "(c) Summer") +
  ggtitle("") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
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
  # Plot against avail
  ggplot(aes(x = scaled_Asp_sin, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  labs(col = "", fill = "",# size = "Weighted",
       y = "",
       x = "Availability",
       subtitle = "(d) Fall") +
  ggtitle("") +
  theme(text = element_text(size = 15))  +
  coord_cartesian(ylim = c(-10, 10),  xlim = c(-2, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        legend.position = "",
        plot.subtitle = element_text(hjust = 1)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Arrange plots
all.Asp_sin <- wint/spr/sum/fall

# save output graph
ggsave("asp_part_resid.png", all.Asp_sin, path = plot_dir,
       width = 5, height = 12, unit = "in")

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

# Calculate fit line
Asp_cos_fit <- line(model = Asp_cos.mod.full, variable = "scaled_Asp_cos")

# Winter
Asp_cos_no.int_wint <- my_predict(model = Asp_cos.mod.full, data = Asp_cos.wint, ranef = TRUE, 
                                  partial_resid = TRUE, intercept = TRUE, 
                                  target_predictor = "scaled_Asp_cos", 
                                  target_intercept = c("is.Winter"))
# Spring
Asp_cos_no.int_spr <- my_predict(model = Asp_cos.mod.full, data = Asp_cos.spr, ranef = TRUE, 
                                 partial_resid = TRUE, intercept = TRUE, 
                                 target_predictor = "scaled_Asp_cos", 
                                 target_intercept = c("is.Spring", "I(is.Spring * is.res)"))
# Summer
Asp_cos_no.int_sum<- my_predict(model = Asp_cos.mod.full, data = Asp_cos.sum, ranef = TRUE, 
                                partial_resid = TRUE, intercept = TRUE, 
                                target_predictor = "scaled_Asp_cos", 
                                target_intercept = c("is.Summer", "I(is.Summer * is.res)"))
# Fall
Asp_cos_no.int_fall <- my_predict(model = Asp_cos.mod.full, data = Asp_cos.fall, ranef = TRUE, 
                                  partial_resid = TRUE, intercept = TRUE, 
                                  target_predictor = "scaled_Asp_cos", 
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
  # Plot against avail
  ggplot(aes(x = scaled_Asp_cos, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed), col = "#00BA38") +
  # scale_alpha(range = c(0.002, 0.008)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y, color = "Range"), data = wint_line) +
  geom_line(aes(x = x, y = upr, color = "Range"), linetype = "dashed", data = wint_line) +
  geom_line(aes(x = x, y = lwr, color = "Range"), linetype = "dashed", data = wint_line) +
  scale_color_manual(values = c("Range" = "#00BA38")) +
  labs(col = "", fill = "", 
       y = "Northness",
       x = "log(Availability)") +
  ggtitle("Winter") +
  coord_cartesian(ylim = c(-7, 7),  xlim = c(-4, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

## Spring ----
spr <- Asp_cos.spr %>%
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
  # Plot against avail
  ggplot(aes(x = scaled_Asp_cos, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = spr_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = spr_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = spr_line) +
  labs(col = "", fill = "", 
       y = "Northness",
       x = "log(Availability)") +
  ggtitle("Spring") +
  coord_cartesian(ylim = c(-7, 7),  xlim = c(-4, 4)) +
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
  # Plot against avail
  ggplot(aes(x = scaled_Asp_cos, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = sum_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = sum_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = sum_line) +
  labs(col = "", fill = "",
       y = "Northness",
       x = "log(Availability)") +
  ggtitle("Summer") +
  coord_cartesian(ylim = c(-7, 7),  xlim = c(-4, 4)) +
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
  # Plot against avail
  ggplot(aes(x = scaled_Asp_cos, col = as.factor(mig.tend))) +
  # geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  # add in predictions
  geom_point(aes(y = pred_observed)) +
  # this needs to be changed to abline
  geom_line(aes(x = x, y = y), data = fall_line) +
  geom_line(aes(x = x, y = upr), linetype = "dashed", data = fall_line) +
  geom_line(aes(x = x, y = lwr), linetype = "dashed", data = fall_line) +
  labs(col = "", fill = "", 
       y = "Northness",
       x = "log(Availability)") +
  ggtitle("Fall") +
  coord_cartesian(ylim = c(-7, 7),  xlim = c(-4, 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = guide_legend(order = 2), 
         col = guide_legend(order = 1))

# Arrange plots
all.Asp_cos <- grid.arrange(wint, spr, sum, fall)

# save output graph
ggsave("Asp_cos_part_resid-seas.png", all.Asp_cos, path = plot_dir,
       width = 6, height = 4, unit = "in")


# Put plots together -----
plot_all <- gridExtra::grid.arrange(all.elev, all.rough, all.herb,
                                    all.shrub, all.Tree,
                                    all.Asp_sin, all.Asp_cos)
dev.off()
# save output graph
ggsave("All_part_resid-seas.png", plot_all, path = plot_dir,
       width = 12, height = 14, unit = "in")

# static ----
plot_static <- gridExtra::grid.arrange(all.elev, all.rough)
dev.off()
# save output graph
ggsave("Static_part_resid.png", plot_static, path = plot_dir,
       width = 12, height = 10, unit = "in")

# Dynamic -----
plot_dy <- gridExtra::grid.arrange(all.herb,
                                   all.shrub, all.Tree)
dev.off()
# save output graph
ggsave("Dynamic_part_resid-seas.png", plot_dy, path = plot_dir,
       width = 12, height = 14, unit = "in")
# Brian code
# Use data from '12_avail-manuscript_plotting.R'

head(elev.wint)

dd <- elev.wint

dd$is.Winter <- 0
dd$scaled_log_Elev2 <- dd$scaled_log_Elev
# dd$scaled_log_Elev <- 0
dd$pred <- predict(Elev.mod.full, newdata = dd)

ggplot(dd, aes(x = scaled_log_Elev2, y = pred)) +
  geom_point() +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-0.04, 0.04))


# # Create fit line for availability ----
# line <- function(model = NULL, variable = NULL){
#   
#   # list seasons
#   all_groups <- c("is.Winter", "is.Spring", "is.Summer", "is.Fall")
#   
#   # variable = variable
#   variable <- variable
#   
#   # est data frame
#   avail_line <-  data.frame()
#   
#   ## Loop over seasons ----
#   for(i in 1:length(all_groups)){
#     # Select single season
#     group <- all_groups[i]
#     
#     # Grab coeff information
#     beta <- summary(model)$coefficients %>%
#       as.data.frame()
#     beta <- beta %>%
#       dplyr::mutate(Fixed_Effects = row.names(beta)) %>%
#       dplyr::relocate(Fixed_Effects, .before = Estimate)
#     row.names(beta) <- NULL
#     
#     # Find intercept
#     int <- beta$Estimate[which(beta$Fixed_Effects %in% group)]
#     # Find slope
#     slope <- beta$Estimate[which(beta$Fixed_Effects %in% variable)]
#     
#     # If Winter, no distinction between movement classification
#     if(group == "is.Winter"){
#       int_adjust = 0
#     } else{
#       # If not Winter, intercept adjustment for movement classification
#       int_adjust <- beta$Estimate[which(beta$Fixed_Effects %in% paste0("I(", group, " * is.res)"))]
#     } # end if else
#     # Temporary df to store results
#     temp <- data.frame(avail = c(-4, 4),
#                        # Season
#                        group = group,
#                        # Movement
#                        is.res = c(0, 1)) %>%
#       expand.grid() %>%
#       distinct() %>%
#       # Calculate intercept/intercept adjustment
#       mutate(intercept = ifelse(is.res == 0, int, int + int_adjust),
#              # slope = slope since PDSI does not directly interact w/ movement
#              slope = slope) %>%
#       # y axis for fit line
#       mutate(y = (slope * avail) + intercept)
#     
#     # Combine df together
#     avail_line <- rbind(avail_line, temp)
#     
#   } # end loop
#   
#   return(avail_line)
# } # end line function


