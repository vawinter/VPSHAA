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
#       1. Get mean availability for each covariate/indiv/season
#       2. Combine w/ 10_rsf_restructuing.R output file for that season/year

#clean up my R environment 
rm(list = ls())
gc()

# Load in packages
library(dplyr)
library(lubridate)

# Load in data
# mod_dats <- readRDS("../RSF_data/202011_model_data.rds")
# Load in data ----
# directory
dir <- "../RSF_data/"

mod_dats <- list.files(dir, full.names = T)[!list.files(dir) %in% c("202102_model_data.rds",
                                                                    "202104_model_data.rds",
                                                                    "202107_model_data.rds",
                                                                    "202111_model_data.rds",
                                                                    "20220629-mod_final_comb_IP.csv",
                                                                    "old_data",
                                                                    "final_outputs",
                                                                    "20220620-mod_final_comb-IP.csv")]



#mod <- readRDS("../RSF_data/202011_model_data_updated-ind.rds")

# Formatting ----
# Create empty data frame to store results
output <- data.frame(ID = NA,
                     # Mean of covariates
                     m_elev = NA,
                     m_snd = NA,
                     m_a.sin = NA,
                     m_a.cos = NA,
                     m_rough = NA,
                     m_bio = NA,
                     m_herb = NA,
                     m_shrub = NA,
                     m_tree = NA,
                     # month and year
                     month = NA,
                     year = NA)


# Loop over
for(m in mod_dats){
  
  # The mod_dat file
  mod <- readRDS(m)
  
  # Loop over 
  # # need to filter by month and year first 
  indiv <- unique(mod$ID)
  
  for(i in 1:length(indiv)){
    # Store in list
    print(paste(i, indiv[i]))
    # Subset individual data
    dat <- mod %>% 
      filter(ID == indiv[i],
             case_ == "FALSE")
    # Get mean avail. for each covariate
    # Store results for the current individual
    res <- data.frame(ID = indiv[i],
                      m_elev = mean(dat$Elevation),
                      m_snd = mean(dat$SND),
                      m_a.sin = mean(dat$Asp_sin),
                      m_a.cos = mean(dat$Asp_cos),
                      m_rough = mean(dat$Roughness),
                      m_bio = mean(dat$RAP_bio),
                      m_herb = mean(dat$Herb),
                      m_shrub = mean(dat$Shrub),
                      m_tree = mean(dat$Tree),
                      # month and year 
                      month = unique(dat$month),
                      year = unique(dat$year))
    # Bind to the general results table
    output <- rbind(output, res)
    
  }
}

# Remove first row of NAs
output <- output[-1, ]
table(output$month)
table(output$year)

# Save as .rds 
#saveRDS(output, "../RSF_data/final_outputs/20220714_m_avail_2021.rds")
#write.csv(output, "../RSF_data/final_outputs/20220714_m_avail_2021.csv", row.names = FALSE)

out_dir <- "../../Data/Chapter1/20220723_ouptuts/"
dir.create(out_dir)
write.csv(output, "../../Data/Chapter1/20220723_ouptuts/20220723_m_avail.csv", row.names = FALSE)

# DONE!