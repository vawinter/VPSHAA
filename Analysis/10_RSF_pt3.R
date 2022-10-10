#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##----------- January 2022  -------------X
#########################################X
##------ Last edited: 10/10/2022  -------X
#########################################X
##------- Last ran: May 25, 2022 --------X
#########################################X

# Based on code Space Use Ecology: from 06_HSA_pt2.
# I am going to fit a set of simple resource selection
# functions (RSFs) for estimating relative habitat selection from
# Pronghorn GPS data. 

# Goals for this script:
#       1. Fit GLM per indiv 
#       2. save betas and st. error outputs for each indiv and covar

#clean up my R environment 
rm(list = ls())
gc()

# Load in packages
library(dplyr)
library(rlist)
library(purrr)
library(data.table)

# source function
source("99_funs.R")

# Load in data ----
# directory
#dir <- "../RSF_data/"

dir <- "https://usu.app.box.com/folder/150482148530"

mod_dats <- list.files(dir, full.names = T)[!list.files(dir) %in% c("202102_model_data.rds",
                                                                   "202104_model_data.rds",
                                                                   "202107_model_data.rds",
                                                                   "202111_model_data.rds",
                                                                   "20220620-mod_final_comb-IP.csv",
                                                                   "20220629-mod_final_comb_IP.csv",
                                                                   "final_outputs",
                                                                   "old_data" )]

# mod_dats <-  readRDS("../RSF_data/202011_model_data_updated-ind.rds")
# Set up loop ----
# DF for output
# Update: glm doesn't include snow - 4/1
# Update: glm doesn't include snow or bio now - 4/7
glm_df <- data.frame(ID = NA,
                     # Fill with betas
                     Intercept_beta = NA,
                     Elev_beta = NA,
                     #SND_beta = NA,
                     # Asp_sin_beta = NA,
                     # Asp_cos_beta = NA,
                     # Rough_beta = NA,
                     # #RAP_bio_beta = NA,
                     # Herb_beta = NA,
                     # Shrub_beta = NA,
                     # Tree_beta = NA,
                     # Fill with st. errors
                     Intercept_stder = NA,
                     Elev_stder = NA,
                    #  #SND_stder = NA,
                    #  Asp_sin_stder = NA,
                    #  Asp_cos_stder = NA,
                    #  Rough_stder = NA,
                    # # RAP_bio_stder = NA,
                    #  Herb_stder = NA,
                    #  Shrub_stder = NA,
                    #  Tree_stder = NA,
                     # month and year of GLM
                     month = NA,
                     year = NA,
                     # Available points
                     avail_pts = NA)

# Loop over
for(m in mod_dats){
 
   # The mod_dat file
  dat <- readRDS(m)
  
  # Format 'base' naming for July and other months
  #if(!(dat$month == "07")) {
    # base <- case_ ~ Elevation + SND + Asp_sin + Asp_cos +
    #   Roughness + RAP_bio + RAP_cover + Shrub + Tree

   # } else {
  #
    base <- case_ ~ Elevation# + Roughness + Herb + Shrub + Tree +
     # Asp_sin + Asp_cos 
    
#  }
  
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
                     Elev_beta = coef(summary(mod))[, "Estimate"]["Elevation"],
                   # SND_beta = coef(summary(mod))[, "Estimate"]["SND"],
                   #   Asp_sin_beta = coef(summary(mod))[, "Estimate"]["Asp_sin"],
                   #   Asp_cos_beta = coef(summary(mod))[, "Estimate"]["Asp_cos"],
                   #   Rough_beta = coef(summary(mod))[, "Estimate"]["Roughness"],
                   # #  RAP_bio_beta = coef(summary(mod))[, "Estimate"]["RAP_bio"],
                   #   Herb_beta = coef(summary(mod))[, "Estimate"]["Herb"],
                   #   Shrub_beta = coef(summary(mod))[, "Estimate"]["Shrub"],
                   #   Tree_beta = coef(summary(mod))[, "Estimate"]["Tree"],
                   #   # Fill with st. errors
                     Intercept_stder = coef(summary(mod))[, "Std. Error"]["(Intercept)"],
                     Elev_stder = coef(summary(mod))[, "Std. Error"]["Elevation"],
                   # SND_stder = coef(summary(mod))[, "Std. Error"]["SND"],
                  #    Asp_sin_stder = coef(summary(mod))[, "Std. Error"]["Asp_sin"],
                  #    Asp_cos_stder = coef(summary(mod))[, "Std. Error"]["Asp_cos"],
                  #    Rough_stder = coef(summary(mod))[, "Std. Error"]["Roughness"],
                  # #   RAP_bio_stder = coef(summary(mod))[, "Std. Error"]["RAP_bio"],
                  #    Herb_stder = coef(summary(mod))[, "Std. Error"]["Herb"],
                  #    Shrub_stder = coef(summary(mod))[, "Std. Error"]["Shrub"],
                  #    Tree_stder = coef(summary(mod))[, "Std. Error"]["Tree"],
                  #    # month and year of individual GLM
                     month = unique(unq$month),
                     year = unique(unq$year),
                     # find number of available pts
                     avail_pts = sum(unq$case_ == "FALSE"),
                     row.names = NULL)
  
  # Combine together df's
  glm_df <-  rbind(temp, glm_df)
 }
}

# Save output ----
# Move columns
glm_df <- glm_df %>% 
  relocate(month, year, .before = Intercept_beta)

# write csv
outdir <- "../RSF_data/test_outputs/"
dir.create(outdir)

write.csv(glm_df, paste0(outdir, "20220725_elev_glm.csv"), row.names = FALSE)


# Test
x <- read.csv("../RSF_data/final_outputs/20220407_glm.csv")

# DONE!

