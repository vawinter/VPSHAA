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
#       1. Fit GLM per indiv (using scaled and centerd variables)
#             a. see script (10_RSF-prep_pt2.1.R)
#       2. save betas and st. error outputs for each indiv and covar

#clean up my R environment 
rm(list = ls())
gc()

# Load in packages
library(dplyr)
library(data.table)

# Load in data ----
RSF_dat <-  readRDS("Data/Processed/RSF_data/20221011_3rd-order_RSF-prep.rds")

# Set up loop ----
# iterate over unique individual/month/year combinations
months <- unique(RSF_dat$month)
years <- unique(RSF_dat$year)

# DF for output ----
# Update: glm doesn't include snow - 4/1
# Update: glm doesn't include snow or bio now - 4/7
glm_df <- data.frame(ID = NA,
                     # Fill with betas
                     Intercept_beta = NA,
                     Elev_beta = NA,
                    # SND_beta = NA,
                     Asp_sin_beta = NA,
                     Asp_cos_beta = NA,
                     Rough_beta = NA,
                     #RAP_bio_beta = NA,
                     Herb_beta = NA,
                     Shrub_beta = NA,
                     Tree_beta = NA,
                     # Fill with st. errors
                     Intercept_stder = NA,
                     Elev_stder = NA,
                    # SND_stder = NA,
                     Asp_sin_stder = NA,
                     Asp_cos_stder = NA,
                     Rough_stder = NA,
                    # RAP_bio_stder = NA,
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
                  #   SND_beta = coef(summary(mod))[, "Estimate"]["SND"],
                     Asp_sin_beta = coef(summary(mod))[, "Estimate"]["scaled_Asp_sin"],
                     Asp_cos_beta = coef(summary(mod))[, "Estimate"]["scaled_Asp_cos"],
                     Rough_beta = coef(summary(mod))[, "Estimate"]["scaled_Rough"],
                  #   RAP_bio_beta = coef(summary(mod))[, "Estimate"]["RAP_bio"],
                     Herb_beta = coef(summary(mod))[, "Estimate"]["scaled_Herb"],
                     Shrub_beta = coef(summary(mod))[, "Estimate"]["scaled_Shrub"],
                     Tree_beta = coef(summary(mod))[, "Estimate"]["scaled_Tree"],
                     # Fill with st. errors
                     Intercept_stder = coef(summary(mod))[, "Std. Error"]["(Intercept)"],
                     Elev_stder = coef(summary(mod))[, "Std. Error"]["scaled_Elev"],
                  #   SND_stder = coef(summary(mod))[, "Std. Error"]["SND"],
                     Asp_sin_stder = coef(summary(mod))[, "Std. Error"]["scaled_Asp_sin"],
                     Asp_cos_stder = coef(summary(mod))[, "Std. Error"]["scaled_Asp_cos"],
                     Rough_stder = coef(summary(mod))[, "Std. Error"]["scaled_Rough"],
                  #   RAP_bio_stder = coef(summary(mod))[, "Std. Error"]["RAP_bio"],
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

saveRDS(glm_df, paste0(outdir, "20221011_eHSF_output.rds"))


# Test
x <- readRDS("Data/Outputs/RSF_outputs/20221011_eHSF_output.rds")

# DONE!

