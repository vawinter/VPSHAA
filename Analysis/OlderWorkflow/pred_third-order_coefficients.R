# Table of betas (mean, CI) from MEM fo appendix S6

rm(list = ls())
gc()

# Libraries
library(dplyr)
library(broom)
library(lubridate)
library(raster)
library(rnaturalearth)
library(sf)
library(knitr)
library(kableExtra)
#library(ragg)
#source("Analysis/OlderWorkflow/11_MEM-full.R")

# Attach sex and migration status variables ----
sex_status <- function(x, sex, status) {
  # Sex
  x$is.Male <- as.numeric(sex == "male")
  # Status
  x$is.res <- as.numeric(status == "res")
  # Return
  return(x)
}
# Line fun for conf int----
line <- function(model = NULL, variable = NULL, xlim = c(0, 1), ci_level = 0.95){
  pred_dat <- expand.grid(x = seq(xlim[1], xlim[2], length.out = 1),
                          group = c("is.Winter", "is.Spring", "is.Summer", "is.Fall"),
                          is.Male = c(0, 1),
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

# Set options
options(scipen = 999)

# Load in data
m_sd <- read.csv("Data/Processed/Mean_sd/20221019_mean-sd_all.csv", header = T)
mods <- readRDS("Data/Outputs/MEM_outputs/20230321_model_outputs.rds")

# Load in data created using same workflow from 01_RSF-analysis-code
pred_dat <- read.csv("Data/Outputs/2021_pred/20221024_2021-10.6.csv", header = T)
pred_dat <- pred_dat[(pred_dat$used_in + pred_dat$used_out) > 70,]

# Scale and center
# Set SND NA values to 0
pred_dat$m_snd[is.na(pred_dat$m_snd)] <- 0

## PDSI ----
# a. find mean
pdsi_m <- m_sd$PDSI[1]
# b. find sd
pdsi_s <- m_sd$PDSI[2]
#c. subtract and divide
pred_dat$scaled_PDSI <- ((pred_dat$m_PDSI - pdsi_m)/ pdsi_s)

## road ----
pred_dat$m_road_no_0 <- pred_dat$m_road
pred_dat$m_road_no_0[pred_dat$m_road == 0] <- min(pred_dat$m_road[pred_dat$m_road > 0])
# a. find mean
road_m <- m_sd$log_Road[1]
# b. find sd
road_s <-m_sd$log_Road[2]
#c. subtract and divide
pred_dat$scaled_log_Road <- ((log(pred_dat$m_road_no_0) - road_m)/ road_s)

## Intercept -----
# need to multiply by SE for this 
pred_dat$Intercept_beta_scale <- 0

## SND ----
pred_dat$m_snd_no_0 <- pred_dat$m_snd
pred_dat$m_snd_no_0[pred_dat$m_snd == 0] <- min(pred_dat$m_snd[pred_dat$m_snd > 0]) 
# a. find mean
SND_m <- m_sd$log_SND[1]
# b. find sd
SND_s <- m_sd$log_SND[2]
#c. subtract and divide
pred_dat$scaled_log_SND <- ((log(pred_dat$m_snd_no_0) - SND_m)/ SND_s)

# Create sex/mig specific data
DAT <- dplyr::bind_rows(
  "fem_res" = pred_dat %>%
    sex_status("female", "res"),
  "fem_mig" = pred_dat %>%
    sex_status("female", "mig"),
  "male_res" = pred_dat %>%
    sex_status("male", "res"),
  "male_mig" = pred_dat %>%
    sex_status("male", "mig"),
  .id = "sex_status")

season <- unique(DAT$month_long)
sex_status <- unique(DAT$sex_status)

# Predict elevation
# Set availability to constant i.e., 0
DAT$m_SC_elev <- 0 

DAT$Elev.mod.prediction.full <- predict(object = mods$Elev.mod.full,
                                        newdata = DAT,
                                        re.form = NA)
# Get CI
mods$Elev.mod.full$m_SC_elev
elev <- line(mods$Elev.mod.full, variable = "m_SC_elev")
elev$Covariate <- "Elevation"


# Print the first few rows of the predictions with confidence intervals
print(head(DAT))

# Predict roughness
# Set availability to constant i.e., 0
DAT$m_SC_rough <- 0 
DAT$Rough.mod.prediction.full <- predict(object = mods$Rough.mod.full,
                                         newdata = DAT,
                                         re.form = NA)
# Get CI
rough <- line(mods$Rough.mod.full, variable = "m_SC_rough")
rough$Covariate <- "Roughness"

# Predict herbaceous
# Set availability to constant i.e., 0
DAT$m_SC_herb <- 0 
DAT$Herb.mod.prediction.full <- predict(object = mods$Herb.mod.full,
                                        newdata = DAT,
                                        re.form = NA)
# Get CI
herb <- line(mods$Herb.mod.full, variable = "m_SC_herb")
herb$Covariate <- "Herbaceous"

# Predict shrub
# Set availability to constant i.e., 0
DAT$m_SC_shrub <- 0 
DAT$Shrub.mod.prediction.full <- predict(object = mods$Shrub.mod.full,
                                         newdata = DAT,
                                         re.form = NA)
# Get CI
shrub <- line(mods$Shrub.mod.full, variable = "m_SC_shrub")
shrub$Covariate <- "Shrub"

# Predict tree
# Set availability to constant i.e., 0
DAT$m_SC_tree <- 0 
DAT$Tree.mod.prediction.full <- predict(object = mods$Tree.mod.full,
                                        newdata = DAT,
                                        re.form = NA)

# Get CI
tree <- line(mods$Tree.mod.full, variable = "m_SC_tree")
tree$Covariate <- "Tree"

# Predict asp_sin
# Set availability to constant i.e., 0
DAT$m_SC_a.sin <- 0 
DAT$Asp_sin.mod.prediction.full <- predict(object = mods$Asp_sin.mod.full,
                                           newdata = DAT,
                                           re.form = NA)

# Get CI
asp_sin <- line(mods$Asp_sin.mod.full, variable = "m_SC_a.sin")
asp_sin$Covariate <- "Aspect (Easting)"

# Predict asp_cos
# Set availability to constant i.e., 0
DAT$m_SC_a.cos <- 0 
DAT$Asp_cos.mod.prediction.full <- predict(object = mods$Asp_cos.mod.full,
                                           newdata = DAT,
                                           re.form = NA)

# Get CI
asp_cos <- line(mods$Asp_cos.mod.full, variable = "m_SC_a.cos")
asp_cos$Covariate <- "Aspect (Northing)"

# Now that we have all betas, we can create our table
betas_ci <- rbind(elev, rough, herb, shrub, tree, asp_sin, asp_cos) %>% 
  mutate(Status = case_when(is.res == 0 ~ "Mover",
                            is.res==1 ~ "Resident"),
         Season = case_when(group == "is.Winter" ~ "Winter",
                            group == "is.Spring" ~ "Spring",
                            group == "is.Summer" ~ "Summer",
                            TRUE  ~ "Fall"),
        Sex = case_when(is.Male == 1 ~ "Male",
                        TRUE ~ "Female")) %>% 
  mutate(y = round(y, digits = 3),
         lwr = round(lwr, digits = 3),
         upr = round(upr, digits = 3)) %>% 
  dplyr::select(-c(x, group, is.Male, is.res)) %>% 
  rename("Beta" = y,
         "Lower CI" = lwr,
         "Upper CI" = upr) %>% 
  filter(!Sex == "Female") %>% 
  relocate(Covariate, Status, Season, .before=Beta) %>% 
  dplyr::select(-Sex) %>% 
  group_by(Season, Status) %>% 
  rename("Movement Status" = Status) %>% 
  filter(Covariate %in% c("Elevation", "Roughness", "Aspect (Easting)", "Aspect (Northing)"))
  
betas_ci$`Movement Status` <- ifelse(betas_ci$Season == "Winter", "Ranging", betas_ci$`Movement Status`)

betas_ci <- betas_ci %>% 
  distinct()  


# # Create the table
# extentpoints <- kable(betas_ci,
#                       booktabs = T,
#                       escape = F,
#                       caption = "",
#                       format = "latex",
#                       caption = "Predicted mean $\beta$ and 95\% confidence intervals (CI) 
#                       from habitat selection analyses completed at third-order 
#                       scales across individuals and years per season for females",
#                       align = c("lrccccccccc")) %>%
#   collapse_rows(columns = c(1,2), valign = "top") %>%
# #  collapse_rows(columns = 3, latex_hline = "major",valign = "middle") %>%
# #  add_header_above(c( "", "", "Second-order" = 3, "Third-order" = 3), line = T)  %>%
#   kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

# Create the table
extentpoints_static <- kable(betas_ci,
                      booktabs = T,
                      escape = F,
                      format = "latex",
                      caption = "Predicted mean $\\beta$ values and 95% confidence intervals (CI) for males using third-order habitat selection analyses for static covariates.",
                      align = c("l", "r", "c", "c", "c", "c", "c", "c", "c", "c", "c")) %>%
  collapse_rows(columns = c(1,2), valign = "top") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)


writeClipboard(extentpoints_static)

# Create the table
extentpoints_dyn <- kable(betas_ci,
                             booktabs = T,
                             escape = F,
                             format = "latex",
                             caption = "Predicted mean $\\beta$ values and 95% confidence intervals (CI) for males using third-order habitat selection analyses for dynamic covariates",
                             align = c("l", "r", "c", "c", "c", "c", "c", "c", "c", "c", "c")) %>%
  collapse_rows(columns = c(1,2), valign = "top") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)


writeClipboard(extentpoints_dyn)

# Combine results into a single data frame
combined_results <- do.call(rbind, lapply(names(results), function(season) {
  data.frame(
    Season = season,
    results[[season]],
    stringsAsFactors = FALSE
  )
}))
