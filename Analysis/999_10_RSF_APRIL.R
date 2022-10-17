#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##----------- January 2022  -------------X
#########################################X

# Based on code Space Use Ecology: from 06_HSA_pt2.
# I am going to fit a set of simple resource selection
# functions (RSFs) for estimating relative habitat selection from
# Pronghorn GPS data. 

# Goals for this script:
#       1. Fit GLM per indiv for April
#           - Apr with snow
#           - Apr w/ out snow
#       2. save betas and st. error outputs for each indiv and covar
#       3. Combine w/ and w/ out snow together in CSV for formatting

#clean up my R environment 
rm(list = ls())
gc()

# Load in packages
library(dplyr)
library(rlist)
library(purrr)

# source functions
source("99_funs.R")

# Load data
dat <-  readRDS("../RSF_data/202004_model_data.rds")

# Turn weights column from chr to numeric bec i suck 
dat$w <- as.numeric(dat$w)

# 2. mark if inidiv has 0's in SND
dat$snow <- ifelse(dat$SND == 0, 1, 0)

# # Filter indiv with SND = 0
apr_ns1 <- dat %>% 
  filter(snow == 1)

# # Filter other indiv with snow
apr1 <- dat %>% 
  filter(snow == 0)

# indiv in both?
common <- intersect(apr1$ID, apr_ns1$ID)
# put into vector
common_id <- as.vector(common)

# # Filter indiv with commonality in both
apr_ns <-apr_ns1 %>% 
  filter(!(ID %in% common_id))

apr <- apr1 %>% 
  filter(!(ID %in% common_id))

# put those indiv in their own df
com <- dat %>% 
  filter(ID %in% common_id)

# Okay (for now)! Ready to run model

## Full run ----
# create data frame to store results

# SNOW
indiv <- unique(apr$ID)
betas <- list()
stder <- list()
avail_pts <- list()

# # Format base formula for model (apr)
base <- case_ ~ Elevation + SND + Asp_sin + Asp_cos +
  Roughness + RAP_bio + RAP_cover + Shrub + Tree

for(i in (indiv)){
  
  # Subset individual data
  unq <- apr %>% 
    filter(ID == i)
  
  mod <- glm(data = unq, formula = base, weights = w, family = binomial)
  
  # # save outputs from model
  betas[[i]] <- coef(summary(mod))[, "Estimate"]
  stder[[i]] <- coef(summary(mod))[, "Std. Error"]
  
  avail_pts[[i]] <-  sum(unq$case_ == "FALSE")
  
}

# put both lists into one
input_list <- list(betas, stder)

# combine lists
combined_output1 <- reduce(input_list, cat_lists)

# add in avail pts
input_list2 <- list(combined_output1, avail_pts)

# combine lists
comb_all <- reduce(input_list2, cat_lists)

# Save ----
write.csv(comb_all, "../RSF_data/apr20_betas_stder_comb.csv", row.names = TRUE)

# NO SNOW -----

# create data frame to store results
indiv2 <- unique(apr_ns$ID)
betas2 <- list()
stder2 <- list()
avail_pts2 <- list()

# # Format base formula for model (apr ns)
no_snow <- case_ ~ Elevation + Asp_sin + Asp_cos +
  Roughness + RAP_bio + RAP_cover + Shrub + Tree

for(i in (indiv2)){
  
  # Subset individual data
  unq <- apr_ns %>% 
    filter(ID == i)
  
  mod <- glm(data = unq, formula = no_snow, weights = w, family = binomial)
  
  # # save outputs from model
  betas2[[i]] <- coef(summary(mod))[, "Estimate"]
  stder2[[i]] <- coef(summary(mod))[, "Std. Error"]
  
  avail_pts2[[i]] <-  sum(unq$case_ == "FALSE")
  
}

# put both lists into one
input_list2 <- list(betas2, stder2)

# combine lists
combined_output2 <- reduce(input_list2, cat_lists)

# add in avail pts
input_list3 <- list(combined_output2, avail_pts2)

# combine lists
comb_all2 <- reduce(input_list3, cat_lists)

# Save
write.csv(comb_all2, "../RSF_data/apr_ns20_betas_stder_comb.csv", row.names = TRUE)

# BOTH
# create data frame to store results
indiv3 <- unique(com$ID)
betas3 <- list()
stder3 <- list()
avail_pts3 <- list()


base <- case_ ~ Elevation + SND + Asp_sin + Asp_cos +
  Roughness + RAP_bio + RAP_cover + Shrub + Tree

for(i in (indiv3)){
  
  # Subset individual data
  unq <- com %>% 
    filter(ID == i)
  
  mod <- glm(data = unq, formula = base, weights = w, family = binomial)
  
  # # save outputs from model
  betas3[[i]] <- coef(summary(mod))[, "Estimate"]
  stder3[[i]] <- coef(summary(mod))[, "Std. Error"]
  
  avail_pts3[[i]] <-  sum(unq$case_ == "FALSE")
  
}

# put both lists into one
input_list4 <- list(betas3, stder3)

# combine lists
combined_output4 <- reduce(input_list4, cat_lists)

# add in avail pts
input_list5 <- list(combined_output4, avail_pts3)

# combine lists
comb_all3 <- reduce(input_list5, cat_lists)

# Save
write.csv(comb_all3, "../RSF_data/apr_com20_betas_stder_comb.csv", row.names = TRUE)


# DONE!
#--------------- END ---------------#
#--------------- END ---------------#
#--------------- END ---------------#
#--------------- END ---------------#
#--------------- END ---------------#

# add columns for now snow april to be even
# combined_output_2$SND_beta <- NA
# combined_output_2$SND_stder <- NA

# OTHER CODE
# # Format base formula for model (no snow apr)
#   base <- case_ ~ Elevation + Asp_sin + Asp_cos + Roughness + RAP_bio +
#     RAP_cover + Shrub + Tree
# 
# # # VW edit from Tal 1/4/22
# if (((sum(dat$SND[!dat$case_] == 0) / sum(!dat$case_)) >= 0.1) &
#     length(unique(dat$SND[!dat$case_])) >= 10){
#   base <- case_ ~ Elevation + SND + Asp_sin + Asp_cos +
#     Roughness + RAP_bio + RAP_cover + Shrub + Tree
# } else {
#   base <- case_ ~ Elevation + Asp_sin + Asp_cos + Roughness + RAP_bio +
#     RAP_cover + Shrub + Tree
# }

# Occured while runing 201911 and 202001:
# Error in if (((sum(dat$SND[!dat$case_] == 0)/sum(!dat$case_)) >= 0.1) &  :
#              missing value where TRUE/FALSE needed
#              
# 1. Load in data
# directory
# dir <- "../RSF_data/"
# 
# # name the months
# months <- "04"
# 
# # define the file path
# file_path <- c(paste0(dir, "2018", months, "_model_data.rds"),
#                paste0(dir, "2019", months, "_model_data.rds"),
#                paste0(dir, "2020", months, "_model_data.rds"))
# 
# 
# # name the files
# names(months) <- months
# names(file_path) <- months
# 
# # Load in using lapply
# dat <- do.call(rbind, lapply(file_path, readRDS))