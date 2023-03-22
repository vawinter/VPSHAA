# a script that generates plots like the one you generated for your parameter 
# estimates, but for Veronica.
# What I am imagining are 7 panels 
# (one for each of Veronica's 7 habitat variables), 
# each with the value of the coefficient and its 95% CI on the y-axis, 
# and 8 categories on the x-axis: 
#   winter-female ('is.winter')
#   winter-male ('is.winter' + 'I(is.winter * is.male)')
#   spring-mover ('is.spring')
#   spring-resident ('is.spring' + 'I(is.spring * is.res)')
#   summer-mover ('is.summer')
#   summer-resident ('is.summer' + 'I(is.summer * is.res)')
#   fall-mover ('is.fall')
#   fall-resident ('is.fall' + 'I(is.fall * is.res)'). 
# The script with the 7 models and the data are attached

# Libraries
library(tidyverse)
library(MuMIn)
library(lme4)
library(MASS)
library(lmerTest)
library(effects)

# Load in data
#dat <- read.csv("buffer/chapter1/20220719_outputs/20220718_MEM_prep.csv")


# -------------------------------------------------X
# ----------------- Generate Models ----------------
# -------------------------------------------------X
source("Analysis/11_MEM-full.R")

# -------------------------------------------------X
# ---- Coefficients, SE, & Confidence Intervals ---- 
# -------------------------------------------------X
# Function to calculate the estimate and confidence bands of each group, 
# relative to the reference group
# https://www.r-bloggers.com/2019/04/how-do-we-combine-errors-the-linear-case/
# var(AQ + BW) = (A^2 * (σ_Q)^2) + (B^2 * (σ_W)^2) + (2*A*B*σ_QW)
# A = variable A (e.g. refence group ie is_F.deer.adu.win)
# B = variable A (e.g. e.g. another group like is_M.deer.adu.win)
# Q = mean (beta estimate) of variable A 
#     (e.g. beta of reference group ie is_F.deer.adu.win)
# W = mean (beta estimate) of variable B 
#     (e.g. beta of another group like is_M.deer.adu.win)
# σ_Q = variance of variable A
# σ_W = variance of variable B
# σ_QW = covariance of Q and W

# If there is not a global intercept, the estimate is simply the beta coefficient
# and the lower and upper bounds are SE * +/- 1.95
coef_conf <- function(model, ci_level = 0.95){
  library(dplyr)
  
  # Get betas
  b <- fixef(model)
  
  # Get all the indicator names
  indicators <- names(b)[grepl("is", names(b)) & !grepl("PDSI", names(b))]
  
  # Get global intercept index (0 if there is not an intercept)
  global_i <- which(names(b) == "(Intercept)")
  A <- ifelse(length(global_i) > 0, 1, 0)
  Q <- ifelse(A != 0, b[global_i], 0)
  names(Q) <- "(Intercept)"
  
  # alpha for confidence interval
  alpha <- 1 - ci_level
  
  # Quantiles for confidence interval
  q <- c(alpha/2, 1 - alpha/2)
  
  # Critical values
  cv <- qnorm(q)
  
  conf <- do.call(rbind, lapply(indicators, function(ind){
    # Get indicator intercept index
    int_i <- which(ind == names(b))
    B <- 1
    W <- b[int_i]
    
    # Combine both intercept indicies
    i <- c(global_i, int_i)
    
    # Get vcov matrix
    S <- vcov(model)[i, i]
    var_Q <- ifelse(A != 0, S[1, 1], 0)
    var_W <- ifelse(A != 0, S[2, 2], S)
    cov_QW <- ifelse(A != 0, S[upper.tri(S)], 0) # or lower tri, it's the same
    
    # Standard Error (sigma) of Q and W 
    # (var = SE^2, SE = sqrt(var))
    se_Q <- sqrt(var_Q)
    se_W <- sqrt(var_W)
    
    # Calculate var(AQ + BW) 
    var <- (A^2 * var_Q) + (B^2 * var_W) + (2*A*B*cov_QW)
    
    # Calculate SE(AQ + BW)
    se <- ifelse(A != 0, sqrt(var_Q + 0.75^2 * var_W - 2 * 0.75 * cov_QW), se_W)
    
    # Bound of CI
    ci_W <- W + cv * se
    ci_Q <- Q + cv * se_Q
    
    df <- data.frame(estimate = c(Q, W), lwr = c(ci_Q[1], ci_W[1]),
                     upr = c(ci_Q[2], ci_W[2]))
    return(df)
  })) 
  conf <- unique(conf)
  
  # add a column for the parameter names
  conf$parameter <- row.names(conf)
  row.names(conf) <- NULL
  
  # add a column with the categories that correspond to the indicator parameters
  conf$param_cat <- ifelse(conf$parameter == "is.Winter", "Winter-Female",
    ifelse(conf$parameter == "I(is.Winter * is.Male)", "Winter-Male",
        ifelse(conf$parameter == "is.Spring", "Spring-Mover",
            ifelse(conf$parameter == "I(is.Spring * is.res)", "Spring-Resident",
                ifelse(conf$parameter == "is.Summer", "Summer-Mover",
                    ifelse(conf$parameter == "I(is.Summer * is.res)", "Summer-Resident",
                        ifelse(conf$parameter == "is.Fall", "Fall-Mover",
                            ifelse(conf$parameter == "I(is.Fall * is.res)", "Fall-Resident", NA)
                            )))))))
                                       
  # filter out rows that are zero (ie if there was no global intercept)
  conf <- conf[conf$estimate != 0 & conf$lwr != 0 & conf$upr != 0, ]
  
  # Add a column for p-values
  conf$p_value <- summary(model)$coefficients[conf$parameter, "Pr(>|t|)"]
  
  # relocate columns
  conf <- conf[, c("parameter", "param_cat", "estimate", "lwr", "upr", "p_value")]
  
  return(conf)
}

conf_df <- coef_conf(Elev.mod.full) %>%
  # Elevation
  mutate(model = "Elevation") %>%
  relocate(model, .before = parameter) %>%
  # Roughness
  rbind(coef_conf(Rough.mod.full) %>%
          mutate(model = "Roughness") %>%
          relocate(model, .before = parameter)) %>%
  # Herb
  rbind(coef_conf(Herb.mod.full) %>%
          mutate(model = "Herbaceous") %>%
          relocate(model, .before = parameter)) %>%
  # Shrub
  rbind(coef_conf(Shrub.mod.full) %>%
          mutate(model = "Shrub") %>%
          relocate(model, .before = parameter)) %>%
  # Tree
  rbind(coef_conf(Tree.mod.full) %>%
          mutate(model = "Tree") %>%
          relocate(model, .before = parameter)) %>%
  # Sin Aspect
  rbind(coef_conf(Asp_sin.mod.full) %>%
          mutate(model = "Easting") %>%
          relocate(model, .before = parameter)) %>%
  # Cos Aspect
  rbind(coef_conf(Asp_cos.mod.full) %>%
          mutate(model = "Northing") %>%
          relocate(model, .before = parameter))

# -------------------------------------------------X
# ---------------------- Plot ----------------------
# -------------------------------------------------X
# my_pal <- c("#002c4a", "#003b64", "#005696", "#3f6cb1", "#7b8cc4", "#a0a0c8", 
#             "#c8b4c1", "#ebc8bb", "#ffe1ca", "#fff0e5", "#fffaf7")

my_pal <- c("#fffaf7", "#fff0e5", "#ffe1ca", "#ebc8bb", "#c8b4c1", "#a0a0c8", 
            "#7b8cc4", "#3f6cb1", "#005696", "#003b64", "#002c4a")

# conf_df %>%
#   ggplot(aes(y = param_cat, col = p_value)) +
#   geom_linerange(aes(xmin = lwr, xmax = upr)) +
#   geom_point(aes(x = estimate)) +
#   geom_vline(xintercept = 0, col = "grey", linetype = 2) +
#   facet_wrap(~model, nrow = 2, ncol = 4,
#              # labeller = data.frame(c("Elevation", "Roughness", "Herbaceous",
#                           # "Shrub", "Tree", "Eastness", "Northness"))
#              ) +
#   scale_color_gradientn(colors = my_pal,
#                         limits = c(0, 0.05)) +
#   labs(x = "Coefficient Estimate", y = "Category", col = "p-value") 
# ggsave("Figures_and_Results/TWS/model-dist.png", width = 6, height = 5, units = "in")

data_new <- conf_df                              # Replicate data
data_new$group <- factor(conf_df$param_cat,      # Reordering group factor levels
                         levels = c("Fall-Mover", "Summer-Mover", "Spring-Mover", 
                                    "Winter-Female", "Fall-Resident", "Summer-Resident",
                                    "Spring-Resident", "Winter-Male"))
data_new %>%
  ggplot(aes(y = model, col = p_value)) +
  geom_linerange(aes(xmin = lwr, xmax = upr, size = 0.015)) +
  geom_point(aes(x = estimate, size = 0.05)) +
  geom_vline(xintercept = 0, col = "grey5", linetype = 2) +
  facet_wrap(~group, nrow = 2, ncol = 4) +
  scale_color_gradient(low = "lightblue", high = "#00008B",
                        na.value = "grey",
                        limits = c(0, 0.05)) +
  labs(x = "Coefficient Estimate", y = "Habitat Attribute", col = "p-value") +
  guides(size = "none") +
  theme_bw() +
  scale_y_discrete(limits = rev(unique(conf_df$model)),
                   labels = rev(unique(conf_df$model))) +
  theme(text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 11, color = "black", angle = -90),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 12, color = "black")) 

ggsave("Figures_and_Results/TWS/model-dist_v2.png", width = 8, height = 6, units = "in")

