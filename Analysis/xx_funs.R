# Functions and libraries code for Winter et al. XXXX

# "Forecasting Animal Distribution through Individual
#  Habitat Selection: Insights for Population Inference and
#  Transferable Predictions"

# Load packages ----
library(amt)
library(tidyverse)
library(lubridate)
library(amt)
library(DBI)
library(ggplot2)
library(raster)
library(sf)
library(tidyr)
library(MuMIn)
library(lme4)
library(MASS)
library(lmerTest)
library(effects)
library(patchwork)
library(lubridate)
library(gridExtra)

# fitting MEM models
fit_model <- function(response, fixed_effects, weights, data) {
  full_formula <- as.formula(paste(response, "~ 0 +", paste(fixed_effects, collapse = " + "), 
                                   "+ (1 | unit:year) + (1 | ID)"))
  null_formula <- as.formula(paste(response, "~ 1 + (1 | unit:year) + (1 | ID)"))
  
  full_model <- lmer(full_formula, weights = weights, data = data, REML = TRUE)
  null_model <- lmer(null_formula, weights = weights, data = data, REML = TRUE)
  
  rsq_full <- r.squaredGLMM(full_model)
  rsq_null <- r.squaredGLMM(null_model)
  
  result <- list(
    summary_full = summary(full_model),
    summary_null = summary(null_model),
    rsquared_full = rsq_full,
    rsquared_null = rsq_null
  )
  
  return(result)
}

# Function to calculate inverse variance weights
inv_weights <- function(x) {
  temp <- 1 / (x^2)
  return(temp / sum(temp, na.rm = TRUE))
}

# Function to scale and center covariates
scale_and_center <- function(x) {
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  return((x - mean_x) / sd_x)
}

# Set logs
log_transform <- function(x) {
  no_0 <- ifelse(x == 0, min(x[x > 0]), x)
  log_mean <- mean(log(no_0), na.rm = TRUE)
  log_sd <- sd(log(no_0), na.rm = TRUE)
  (log(no_0) - log_mean) / log_sd
}

# Let's write a function that will take our centroid coordinates and return an
# extent that covers 25 sq. km.
# Edit 1/12: 10 x 10 buffer (100 sq. km.)

# x: x-coordinate
# y: y-coordinate
# half_side: number giving half the length of a side of the square of the
# final exent. E.g., to get a 25 sq. km extent, each side of the square is
# 5 km, so half a side is 2.5 km = 2500 m
# 1/12: 10 km, half side is 5 km = 5000 m 
make_extent <- function(x, y, half_side = 50000) {
  xmin <- x - half_side
  xmax <- x + half_side
  ymin <- y - half_side
  ymax <- y + half_side
  # Create 'extent' object 
  ext <- extent(xmin, xmax, ymin, ymax)
  # Return
  return(ext)
}

# Line functoin fro plotting ----
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


# Function to generate partial residuals plot for a specific covariate
generate_partial_residuals_plot <- function(data, model, covar_name, group_var, interaction_var,
                                            y_name, x_name) {
  # Create a filtered dataset based on group_var and interaction_var
  subset_data <- data %>% filter({{group_var}} == 1, {{interaction_var}} == 1)
  
  # Generate partial residuals
  partial_resid <- my_predict(model = model, data = subset_data, ranef = F,
                              partial_resid = TRUE, intercept = TRUE,
                              target_predictor = covariate,
                              target_intercept = c("{{group_var}}", "I({{group_var}} * {{interaction_var}})"))
  
  # Additional plotting code...
  partial_resid_plot <- ggplot(subset_data, aes(x = m_SC_elev, col = as.factor(tendency))) +
    geom_point(aes(y = partial_resid )) +
    geom_line(aes(x = x, y = y), data = partial_resid_line) +
    geom_line(aes(x = x, y = lwr), linetype = "dashed", data = partial_resid_line) +
    geom_line(aes(x = x, y = upr), linetype = "dashed", data = partial_resid_line) +
    scale_color_manual(values = c("Mover" = "deeppink3", "Resident" = "cyan3")) +
    labs(col = "", fill = "",
         y = y_name,
         x = x_name,
         subtitle = paste("(a)", covar_name)) +
    ggtitle("") +
    coord_cartesian(ylim = c(-50, 50),  xlim = c(-2, 2)) +
    theme_classic() +
    theme(text = element_text(size = 15),
          legend.position = "",
          plot.subtitle = element_text(hjust = 1)) +
    guides(size = guide_legend(order = 2), 
           col = guide_legend(order = 1))
  
  return(partial_resid_plot)
}
