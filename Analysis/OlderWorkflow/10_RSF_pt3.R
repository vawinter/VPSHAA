#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##----------- January 2022  -------------X
#########################################X
##------ Last edited: 10/10/2022  -------X
#########################################X
##-------- Last ran: 10/11/2022 ---------X
#########################################X

# Based on code Space Use Ecology: from 06_HSA_pt2.
# I am going to fit a set of simple resource selection
# functions (eHSF) for estimating relative habitat selection from
# Pronghorn GPS data. 

# Goals for this script:
#       1. Fit GLM per indiv (using scaled and centered variables)
#             a. see script (10_RSF-prep_pt2.1.R)
#       2. save betas and st. error outputs for each indiv and covar

#clean up my R environment 
rm(list = ls())
gc()

# Load in packages
library(dplyr)

# Load in data ----
RSF_dat <-  readRDS("Data/Processed/RSF_data/20230812_3rd-order_RSF-prep.rds")

# Set up loop ----
# iterate over unique individual/month/year combinations
months <- unique(RSF_dat$month)
years <- unique(RSF_dat$year)

# DF for output ----
glm_df <- data.frame(ID = NA,
                     # Fill with betas
                     Intercept_beta = NA,
                     Elev_beta = NA,
                     Asp_sin_beta = NA,
                     Asp_cos_beta = NA,
                     Rough_beta = NA,
                     Herb_beta = NA,
                     Shrub_beta = NA,
                     Tree_beta = NA,
                     # Fill with st. errors
                     Intercept_stder = NA,
                     Elev_stder = NA,
                     Asp_sin_stder = NA,
                     Asp_cos_stder = NA,
                     Rough_stder = NA,
                      Herb_stder = NA,
                     Shrub_stder = NA,
                     Tree_stder = NA,
                     # month and year of GLM
                     month = NA,
                     year = NA,
                     # Available points
                     avail_pts = NA)

# forumla

base <- case_ ~ scaled_Elev + scaled_Rough + scaled_Herb + scaled_Shrub + 
  scaled_Tree + scaled_Asp_sin + scaled_Asp_cos

# Loop over
for(y in years){
  
  # print status
  print(paste(y))
  
  # filter by year
  x <- RSF_dat %>% 
    filter(year == y)
  
for(m in months){
  
  # print status
  print(paste(m))
  
# filter by month
  dat <- x %>% 
     filter(month == m)

  indiv <- unique(dat$ID)

  for(i in indiv){
  
   # print status
  print(paste(i))
  
    # Subset individual data
  unq <- dat %>% 
    filter(ID == i)
  
  mod <- glm(data = unq, formula = base, weights = w, family = binomial)
  
  # # save outputs from model
  temp <- data.frame(ID = i,
                     # Betas from GLM
                     Intercept_beta = coef(summary(mod))[, "Estimate"]["(Intercept)"],
                     Elev_beta = coef(summary(mod))[, "Estimate"]["scaled_Elev"],
                     Asp_sin_beta = coef(summary(mod))[, "Estimate"]["scaled_Asp_sin"],
                     Asp_cos_beta = coef(summary(mod))[, "Estimate"]["scaled_Asp_cos"],
                     Rough_beta = coef(summary(mod))[, "Estimate"]["scaled_Rough"],
                     Herb_beta = coef(summary(mod))[, "Estimate"]["scaled_Herb"],
                     Shrub_beta = coef(summary(mod))[, "Estimate"]["scaled_Shrub"],
                     Tree_beta = coef(summary(mod))[, "Estimate"]["scaled_Tree"],
                     # Fill with st. errors
                     Intercept_stder = coef(summary(mod))[, "Std. Error"]["(Intercept)"],
                     Elev_stder = coef(summary(mod))[, "Std. Error"]["scaled_Elev"],
                     Asp_sin_stder = coef(summary(mod))[, "Std. Error"]["scaled_Asp_sin"],
                     Asp_cos_stder = coef(summary(mod))[, "Std. Error"]["scaled_Asp_cos"],
                     Rough_stder = coef(summary(mod))[, "Std. Error"]["scaled_Rough"],
                     Herb_stder = coef(summary(mod))[, "Std. Error"]["scaled_Herb"],
                     Shrub_stder = coef(summary(mod))[, "Std. Error"]["scaled_Shrub"],
                     Tree_stder = coef(summary(mod))[, "Std. Error"]["scaled_Tree"],
                     # month and year of individual GLM
                     month = unique(unq$month),
                     year = unique(unq$year),
                     # find number of available pts
                     avail_pts = sum(unq$case_ == "FALSE"),
                     row.names = NULL)
  
  # Combine together df's
  glm_df <-  rbind(temp, glm_df)
  }
 }
}
# Save output ----
# Move columns
glm_df <- glm_df %>% 
  relocate(month, year, .before = Intercept_beta) 

# remove last row of NA's
# Remove first row of NAs
glm_df <- glm_df[-1236, ]

# Save output
outdir <- "Data/Outputs/RSF_outputs/"
#dir.create(outdir)

saveRDS(glm_df, paste0(outdir, "20230812_eHSF_output.rds"))


# Test
x <- readRDS("../eHSF/Data/Outputs/RSF_outputs/20221018_eHSF_output.rds")

# Group by Covariate and calculate the mean for table for appendix s4
mean_results <- x %>%
  mutate(
    Intercept_lower = Intercept_beta - 1.96 * Intercept_stder / sqrt(n()),
    Intercept_upper = Intercept_beta + 1.96 * Intercept_stder / sqrt(n()),
    Elev_lower = Elev_beta - 1.96 * Elev_stder / sqrt(n()),
    Elev_upper = Elev_beta + 1.96 * Elev_stder / sqrt(n()),
    Rough_lower = Rough_beta - 1.96 * Rough_stder / sqrt(n()),
    Rough_upper = Rough_beta + 1.96 * Rough_stder / sqrt(n()),
    Herb_lower = Herb_beta - 1.96 * Herb_stder / sqrt(n()),
    Herb_upper = Herb_beta + 1.96 * Herb_stder / sqrt(n()),
    Shrub_lower = Shrub_beta - 1.96 * Shrub_stder / sqrt(n()),
    Shrub_upper = Shrub_beta + 1.96 * Shrub_stder / sqrt(n()),
    Tree_lower = Tree_beta - 1.96 * Tree_stder / sqrt(n()),
    Tree_upper = Tree_beta + 1.96 * Tree_stder / sqrt(n()),
    Asp_sin_lower = Asp_sin_beta - 1.96 * Asp_sin_stder / sqrt(n()),
    Asp_sin_upper = Asp_sin_beta + 1.96 * Asp_sin_stder / sqrt(n()),
    Asp_cos_lower = Asp_cos_beta - 1.96 * Asp_cos_stder / sqrt(n()),
    Asp_cos_upper = Asp_cos_beta + 1.96 * Asp_cos_stder / sqrt(n()),
  ) %>% 
  group_by(month) %>%
  summarize(
  '(Intercept)_beta' = round(mean(Intercept_beta), digits = 3),
    Elevation_beta = round(mean(Elev_beta), digits = 3),
    Roughness_beta = round(mean(Rough_beta), digits = 3),
    Herbaceous_beta = round(mean(Herb_beta), digits = 3),
    Shrub_beta = round(mean(Shrub_beta), digits = 3),
    Tree_beta = round(mean(Tree_beta), digits = 3),
  "Aspect (Easting)_beta" = round(mean(Asp_sin_beta), digits = 3),
  "Aspect (Northing)_beta" = round(mean(Asp_cos_beta), digits = 3),
  '(Intercept)_lower' = round(mean(Intercept_lower), digits = 3),
  '(Intercept)_upper' = round(mean(Intercept_upper), digits = 3),
  Elevation_lower = round(mean(Elev_lower), digits = 3),
  Elevation_upper = round(mean(Elev_upper), digits = 3),
  Roughness_lower = round(mean(Rough_lower), digits = 3),
  Roughness_upper = round(mean(Rough_upper), digits = 3),
  Herbaceous_lower = round(mean(Herb_lower), digits = 3),
  Herbaceous_upper = round(mean(Herb_upper), digits = 3),
  Shrub_lower = round(mean(Shrub_lower), digits = 3),
  Shrub_upper = round(mean(Shrub_upper), digits = 3),
  Tree_lower = round(mean(Tree_lower), digits = 3),
  Tree_upper = round(mean(Tree_upper), digits = 3),
  "Aspect (Easting)_lower" = round(mean(Asp_sin_lower), digits = 3),
  "Aspect (Easting)_upper" = round(mean(Asp_sin_upper), digits = 3),
  "Aspect (Northing)_lower" = round(mean(Asp_cos_lower), digits = 3),
  "Aspect (Northing)_upper" = round(mean(Asp_cos_upper), digits = 3)) %>% 
  mutate(Season = case_when(
    month == "02" ~ "Winter",
    month == "04" ~ "Spring",
    month == "07" ~ "Summer",
    month == "11" ~ "Fall",
    TRUE ~ month)) %>% 
  relocate("Season", .before = '(Intercept)_beta') %>% 
  dplyr::select(-month) %>%
  pivot_longer(
    cols = c("(Intercept)_beta", "Elevation_beta", "Roughness_beta", "Herbaceous_beta", 
             "Shrub_beta", "Tree_beta", 
             "Aspect (Easting)_beta", "Aspect (Northing)_beta", 
             "(Intercept)_lower", "(Intercept)_upper", "Elevation_lower", "Elevation_upper", 
             "Roughness_lower", "Roughness_upper", "Herbaceous_lower", "Herbaceous_upper", 
             "Shrub_lower", "Shrub_upper", "Tree_lower", "Tree_upper", 
             "Aspect (Easting)_lower", "Aspect (Easting)_upper", "Aspect (Northing)_lower", "Aspect (Northing)_upper"),
    names_to = c("Covariate", "Beta", ".value"),
    names_pattern = "(.*)(.*)_(.*)"
  ) %>% 
dplyr::select(-Beta) %>% 
  rename(Beta2 = beta,
         LowerCI2 = lower,
         UpperCI2 = upper) %>% 
  as.data.frame()
# DONE! 
mean_results
