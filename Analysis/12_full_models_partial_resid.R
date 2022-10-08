#-Adapted from Ronan code 05b_full_models_-X
# -----------------------------------------X
# - FULL MODEL FOR ALL RESPONSE VARIABLES -X
# -----------------------------------------X
# --------------- 2022-05-05 --------------X 
# ------------------ RBH ------------------X 
# -----------------------------------------X
###########################################X
#--------- Last edited 05/31/2022----------X
###########################################X

# Model Structure:
#   Response Variable ~ available LFs + available environmental covariates +
#       log_selection LFs + log_selection environmental covariates + 
#       boolean indicator groups + (available LFs):(boolean indicator groups) +
#       (1|animal ID:year)

# Weights: N points (normalized)

# Reference Group: 
#   Female Adult Deer in Winter 

# 2022-05-16
# Changing model structure from 05a
#   - removing interactions (except for roads and fences)
#   - changing random effects to 1|animal_id:hr_year 
#     (may or may not need to add 1|animal_id)
#   - adding log(selection ratios) into models (not interacting)
# Changing partial residuals format
#   - predictions = model formula x data (real or fake)
#   - set beta_i (e.g. avail roads, avail roads interacting with a group, etc) 
#     to 0 (i.e. knocked out)
#   - predictions (that ignore the knocked out effects) = 
#         model formula (with knocked out betas) x data frame (real or fake)
#   - residuals = predictions - observed
#   - partial residuals = predictions (that ignore knocked out effects) - observed

# ------------------------------------X
#  ------------ 1. Set Up ------------
# ------------------------------------X
library(tidyverse)
library(lme4)

# ------------------------------------X
# ------- 5. Partial Residuals ------
# ------------------------------------X
# NOTE!!!! ----
# Geom_smooth will be impacted by the points removed when adjusting for the scale!!

#   - predictions = model formula x data (real or fake)
#   - set beta_i (e.g. avail roads, avail roads interacting with a group, etc) 
#     to 0 (i.e. knocked out)
#   - predictions (that ignore the knocked out effects) = 
#         model formula (with knocked out betas) x data (real or fake)
#   - residuals = observed - predictions
#   - partial residuals = observed - predictions (that ignore knocked out effects)

# partial_resid = F
#   ranef = F: 
#     E[Y] = Betas * X
#   ranef = T: 
#     E[Y] = (Betas * X) + Z

# partial_resid = T
#   ranef = F:
#     E[Y] = Betas (target covariate set to 0) * X
#   ranef = T:
#     E[Y] = (Betas (target covariate set to 0) * X) + Z
#   (if intercept = T, Betas for intercepts are also knocked out)

# mult 1.96 to SE

my_predict <- function(model, data, ranef = TRUE, 
                       partial_resid = TRUE, intercept = FALSE, target = NULL){
  
  # extract Betas and turn into a data frame
  beta <- summary(model)$coefficients %>%
    as.data.frame()
  beta <- beta %>%
    dplyr::mutate(Fixed_Effects = row.names(beta)) %>%
    dplyr::relocate(Fixed_Effects, .before = Estimate)
  row.names(beta) <- NULL
  
  # check if knocking out an effect or not
  if(partial_resid){
    # if(!(target %in% c("road", "fence", "roughness", "elevation", "snd",
    #                    "tree", "shrub", "NDVI"))){
    #   stop("target must be either 'road', 'fence', 'roughness', 'elevation', 
    #   'snd', 'tree', 'shrub', or 'NDVI'")
    # }
    # 
    # get the column indices for the rows in the Betas that have the target name
    target_i <- which(grepl(target, beta$Fixed_Effects))
    
    # check if intercepts should be knocked out as well
    if(intercept){
      # if so, find the row indices for the main intercept and the is_[group]
      # intercepts (not including the interactions with the is_[group])
      int_i <- which(grepl("is", beta$Fixed_Effects) & 
                       !grepl(" * scaled_", beta$Fixed_Effects) |
                       grepl("Intercept", beta$Fixed_Effects))
      beta$Estimate[int_i] <- 0
    }
    
    # set those Betas to 0
    beta$Estimate[target_i] <- 0
  } # end of partial residual check
  
  # pull the terms in the model that are not interactions
  fe_no.int <- beta %>%
    # str_remove(Fixed_Effects, ":")
    dplyr::filter(!grepl(" * ", Fixed_Effects)) %>%
    dplyr::select(Fixed_Effects) %>%
    pull()
  
  # make a data frame for the Xs
  X <- data %>%
    # add an Intercept column to the data (all 1s)
    dplyr::mutate(`(Intercept)` = 1)
  # pull just the columns in the terms in the model (not including interactions)
  X <- X %>% 
    dplyr::select(which(colnames(X) %in% fe_no.int))
  
  # pull the terms in the model that are interactions
  fe_int <- beta %>%
    dplyr::filter(str_detect(Fixed_Effects, " * ")) %>%
    dplyr::select(Fixed_Effects) %>%
    dplyr::pull()
  
  # check if there are any interactions
  if(length(fe_int) > 0){
    # loop through the interaction terms
    for(i in 1:length(fe_int)){
      # pull the separate terms
      terms <- fe_int[i] %>%
        stringr::str_remove("I\\(") %>%
        stringr::str_remove("\\)") %>%
        stringr::str_split(" * ") %>% 
        unlist()
      
      # pull the columns for each term
      x1 <- data[, colnames(data) == terms[1]]
      x2 <- data[, colnames(data) == terms[3]] 
      
      # multiply them together
      x_int <- x1 * x2
      
      # make the interaction a column
      X[, fe_int[i]] <- x_int
    } # end of for loop for interactions
  } # end of check for interactions
  
  # reorder columns to match the order of the betas
  X <- X[, match(beta$Fixed_Effects, colnames(X))]
  
  # convert betas and X into matrices
  beta <- beta$Estimate %>% as.matrix()
  X <- X %>% as.matrix()
  
  # dot multiply matrices together
  betaX <- X %*% beta
  
  # if ranef = TRUE, add random effects
  if(ranef){
    # extract the random effects intercept adjustments
    re <- ranef(model)
    re_names <- names(re)
    
    # set up Betas * X + Z (the loop will add each iteration of Z)
    betaXZ <- betaX
    
    # loop through each random effect (if there are more than 1)
    for(j in 1:length(re_names)){
      # pull the intercept adjustments for this random effect
      re_coef <- re[[j]]
      
      # pull the terms for each intercept
      re_terms <- row.names(re_coef)
      
      # check if there's a : in the term (ie an interaction)
      if(str_detect(re_names[j], ":")){
        # pull the two separate names
        re_names_int <- str_split(re_names[j], ":") %>%
          unlist()
        
        # loop through every term for this random effect
        for(k in 1:length(re_terms)){
          # split the term by the : to get the two separate terms
          terms <- str_split(re_terms[k], ":") %>% 
            unlist()
          
          # make new columns in the coefficients dataframe for the terms
          re_coef[k, re_names_int[1]] <- terms[1]
          re_coef[k, re_names_int[2]] <- terms[2]
        } # end of for loop for this RE's terms
        
        # pull the data that matches the random effects' names
        Z <- data[, colnames(data) %in% re_names_int]
        
        # join the intercept to the Z table based on the random effect terms
        Z <- Z %>%
          left_join(re_coef, by = re_names_int)
        
      } else{ # if it's not an interaction 
        # make a column for the term
        re_coef[, re_names[j]] <- re_terms
        
        # pull the data that matches the random effects' names
        Z <- data %>%
          dplyr::select(which(colnames(data) %in% re_names[j]))
        
        # join the intercept to the Z table based on the random effect term
        Z <- Z %>%
          dplyr::left_join(re_coef, by = re_names[j])
      } # end of ifelse check for interactions
      
      # pull just the intercept column and convert to a matrix
      Z <- Z %>%
        dplyr::select(`(Intercept)`) %>%
        as.matrix()
      
      # add to the Betas * X matrix 
      # (if this is the first (or only) iteration of the loop, betaXZ = betaX)
      betaXZ <- betaXZ + Z
      
    } # end of for loop for each random effect
    
    return(as.numeric(betaXZ)) # a vector
    
  } else{ # if ranef = FALSE
    
    return(as.numeric(betaX)) # a vector
    
  } # end of ifelse check for random effects
} # end of function

# # Ronan examples -----
# ### iii. Roughness ----
# # calculate predication
# dat.preds_rough_full <- my_predict(fit.rough, dat.full, ranef = T, 
#                                   partial_resid = F)
# 
# # calculate predictions with no road effects
# dat.preds_rough_no.road <- my_predict(fit.rough, dat.full, ranef = T, 
#                                      partial_resid = T, intercept = F, 
#                                      target = "road")
# 
# # calculate predications with no road effects and no intercepts
# dat.preds_rough_no.road_no.int <- my_predict(fit.rough, dat.full, ranef = T, 
#                                             partial_resid = T, intercept = T, 
#                                             target = "road")
# 
# head(dat.preds_rough_full)
# head(dat.preds_rough_no.road)
# head(dat.preds_rough_no.road_no.int)
# head(dat.full$avail_sc_roughness - dat.preds_rough_no.road_no.int)
# 
# # Plot
# dat.full %>%
#   mutate(pred = dat.preds_rough_full,
#          resid = avail_sc_roughness - pred,
#          pred_no.road = dat.preds_rough_no.road,
#          part.resid_no.road = avail_sc_roughness - pred_no.road,
#          part.resid_no.road_no.int = dat.preds_rough_no.road_no.int,
#          pred_no.road_no.int = avail_sc_roughness - part.resid_no.road_no.int,
#          is_M = as.factor(is_M),
#          is_prong = as.factor(is_prong),
#          is_sum = as.factor(is_sum)) %>%
#   # View()
#   ggplot(aes(x = avail_sc_road, col = is_prong)) +
#   geom_point(aes(y = pred_no.road_no.int), alpha = 0.25, shape = 1) +
#   geom_hline(yintercept = 0, linetype = 2, col = "grey") +
#   geom_smooth(aes(y = pred_no.road_no.int, fill = is_prong), method = "lm") +
#   facet_grid(is_M ~ is_sum, 
#              labeller = labeller(is_M = sex_labs, is_sum = ssn_labs)) +
#   ylim(c(-2.5, 2.5)) +
#   xlim(c(-2, 3)) +
#   scale_color_hue(labels = spp_labs) +
#   scale_fill_hue(labels = spp_labs) +
#   labs(col = "", fill = "", 
#        y = "log(Selection Ratio) Partial Residuals", 
#        x = "Available Road Density (scaled & centered)",
#        title = "Roughness") +
#   theme_bw()
# ggsave("rough_part_resid_no_road_no_int.png", path = plot_dir, 
#        width = 6, height = 4, unit = "in")
# 
### iv. Elevation ----
# calculate predication
dat.preds_elev_full <- my_predict(fit.elev, dat.full, ranef = T,
                                   partial_resid = F)

# calculate predictions with no road effects
dat.preds_elev_no.road <- my_predict(fit.elev, dat.full, ranef = T,
                                      partial_resid = T, intercept = F,
                                      target = "road")

# calculate predications with no road effects and no intercepts
dat.preds_elev_no.road_no.int <- my_predict(fit.elev, dat.full, ranef = T,
                                             partial_resid = T, intercept = T,
                                             target = "road")

head(dat.preds_elev_full)
head(dat.preds_elev_no.road)
head(dat.preds_elev_no.road_no.int)
head(dat.full$avail_sc_elevation - dat.preds_elev_no.road_no.int)

# Plot
dat.full %>%
  mutate(pred = dat.preds_elev_full,
         resid = avail_sc_elevation - pred,
         pred_no.road = dat.preds_elev_no.road,
         part.resid_no.road = avail_sc_elevation - pred_no.road,
         part.resid_no.road_no.int = dat.preds_elev_no.road_no.int,
         pred_no.road_no.int = avail_sc_elevation - part.resid_no.road_no.int,
         is_M = as.factor(is_M),
         is_prong = as.factor(is_prong),
         is_sum = as.factor(is_sum)) %>%
  # View()
  ggplot(aes(x = avail_sc_road, col = is_prong)) +
  geom_point(aes(y = pred_no.road_no.int), alpha = 0.25, shape = 1) +
  geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  geom_smooth(aes(y = pred_no.road_no.int, fill = is_prong), method = "lm") +
  facet_grid(is_M ~ is_sum,
             labeller = labeller(is_M = sex_labs, is_sum = ssn_labs)) +
  ylim(c(-2, 2)) +
  xlim(c(-2, 3)) +
  scale_color_hue(labels = spp_labs) +
  scale_fill_hue(labels = spp_labs) +
  labs(col = "", fill = "",
       y = "log(Selection Ratio) Partial Residuals",
       x = "Available Road Density (scaled & centered)",
       title = "Elevation") +
  theme_bw()
ggsave("elev_part_resid_no_road_no_int.png", path = plot_dir,
       width = 6, height = 4, unit = "in")

# ### v. Snow Depth ----
# # calculate predication
# dat.preds_snd_full <- my_predict(fit.snd, dat.full, ranef = T, 
#                                    partial_resid = F)
# 
# # calculate predictions with no road effects
# dat.preds_snd_no.road <- my_predict(fit.snd, dat.full, ranef = T, 
#                                       partial_resid = T, intercept = F, 
#                                       target = "road")
# 
# # calculate predications with no road effects and no intercepts
# dat.preds_snd_no.road_no.int <- my_predict(fit.snd, dat.full, ranef = T, 
#                                              partial_resid = T, intercept = T, 
#                                              target = "road")
# 
# head(dat.preds_snd_full)
# head(dat.preds_snd_no.road)
# head(dat.preds_snd_no.road_no.int)
# head(dat.full$avail_sc_snd.med - dat.preds_snd_no.road_no.int)
# 
# # Plot
# dat.full %>%
#   mutate(pred = dat.preds_snd_full,
#          resid = avail_sc_snd.med - pred,
#          pred_no.road = dat.preds_snd_no.road,
#          part.resid_no.road = avail_sc_snd.med - pred_no.road,
#          part.resid_no.road_no.int = dat.preds_snd_no.road_no.int,
#          pred_no.road_no.int = avail_sc_snd.med - part.resid_no.road_no.int,
#          is_M = as.factor(is_M),
#          is_prong = as.factor(is_prong),
#          is_sum = as.factor(is_sum)) %>%
#   # View()
#   ggplot(aes(x = avail_sc_road, col = is_prong)) +
#   geom_point(aes(y = pred_no.road_no.int), alpha = 0.25, shape = 1) +
#   geom_hline(yintercept = 0, linetype = 2, col = "grey") +
#   geom_smooth(aes(y = pred_no.road_no.int, fill = is_prong), method = "lm") +
#   facet_grid(is_M ~ is_sum, 
#              labeller = labeller(is_M = sex_labs, is_sum = ssn_labs)) +
#   ylim(c(-2, 2)) +
#   xlim(c(-2, 3)) +
#   scale_color_hue(labels = spp_labs) +
#   scale_fill_hue(labels = spp_labs) +
#   labs(col = "", fill = "", 
#        y = "log(Selection Ratio) Partial Residuals", 
#        x = "Available Road Density (scaled & centered)",
#        title = "Snow Depth") +
#   theme_bw()
# ggsave("snd_part_resid_no_road_no_int.png", path = plot_dir, 
#        width = 6, height = 4, unit = "in")
# 
# ### vi. Tree Cover ----
# # calculate predication
# dat.preds_tree_full <- my_predict(fit.tree, dat.full, ranef = T, 
#                                    partial_resid = F)
# 
# # calculate predictions with no road effects
# dat.preds_tree_no.road <- my_predict(fit.tree, dat.full, ranef = T, 
#                                       partial_resid = T, intercept = F, 
#                                       target = "road")
# 
# # calculate predications with no road effects and no intercepts
# dat.preds_tree_no.road_no.int <- my_predict(fit.tree, dat.full, ranef = T, 
#                                              partial_resid = T, intercept = T, 
#                                              target = "road")
# 
# head(dat.preds_tree_full)
# head(dat.preds_tree_no.road)
# head(dat.preds_tree_no.road_no.int)
# head(dat.full$avail_sc_cover.tree - dat.preds_tree_no.road_no.int)
# 
# # Plot
# dat.full %>%
#   mutate(pred = dat.preds_tree_full,
#          resid = avail_sc_cover.tree - pred,
#          pred_no.road = dat.preds_tree_no.road,
#          part.resid_no.road = avail_sc_cover.tree - pred_no.road,
#          part.resid_no.road_no.int = dat.preds_tree_no.road_no.int,
#          pred_no.road_no.int = avail_sc_cover.tree - part.resid_no.road_no.int,
#          is_M = as.factor(is_M),
#          is_prong = as.factor(is_prong),
#          is_sum = as.factor(is_sum)) %>%
#   # View()
#   ggplot(aes(x = avail_sc_road, col = is_prong)) +
#   geom_point(aes(y = pred_no.road_no.int), alpha = 0.25, shape = 1) +
#   geom_hline(yintercept = 0, linetype = 2, col = "grey") +
#   geom_smooth(aes(y = pred_no.road_no.int, fill = is_prong), method = "lm") +
#   facet_grid(is_M ~ is_sum, 
#              labeller = labeller(is_M = sex_labs, is_sum = ssn_labs)) +
#   ylim(c(-2.5, 2.5)) +
#   xlim(c(-2, 3)) +
#   scale_color_hue(labels = spp_labs) +
#   scale_fill_hue(labels = spp_labs) +
#   labs(col = "", fill = "", 
#        y = "log(Selection Ratio) Partial Residuals", 
#        x = "Available Road Density (scaled & centered)",
#        title = "Tree Cover") +
#   theme_bw()
# ggsave("tree_part_resid_no_road_no_int.png", path = plot_dir, 
#        width = 6, height = 4, unit = "in")
# 
# # DONE!