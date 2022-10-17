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

# Load in data
mod <-readRDS("Data/Processed/RSF_data/20221011_3rd-order_RSF-prep.rds")
mod$SND[is.na(mod$SND)] <- 0
mod$SND <- as.numeric(mod$SND)

# Formatting ----
# iterate over unique individual/month/year combinations
months <- unique(mod$month)
years <- unique(mod$year)

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
                     # Mean scaled and centered
                     m_SC_elev = NA,
                   #  m__SC_snd = NA,
                     m_SC_a.sin = NA,
                     m_SC_a.cos = NA,
                     m_SC_rough = NA,
                     m_SC_bio = NA,
                     m_SC_herb = NA,
                     m_SC_shrub = NA,
                     m_SC_tree = NA,
                     # month and year
                     month = NA,
                     year = NA)


# Loop over
# Loop over
for(y in years){
  
  # print status
  print(paste(y))
  
  # filter by year
  x <- mod %>% 
    filter(year == y)
  
  for(m in months){
    
    # print status
    print(paste(m))
    
    # filter by month
    y <- x %>% 
      filter(month == m)
    
    indiv <- unique(y$ID)
    
  for(i in 1:length(indiv)){
    # Store in list
    print(paste(i, indiv[i]))
    # Subset individual data
    dat <- y %>% 
      filter(ID == indiv[i],
             case_ == "FALSE")
    
    # Get mean avail. for each covariate
    # Store results for the current individual
    temp <- data.frame(ID = indiv[i],
                      m_elev = mean(dat$Elevation),
                      m_snd = mean(dat$SND),
                      m_a.sin = mean(dat$Asp_sin),
                      m_a.cos = mean(dat$Asp_cos),
                      m_rough = mean(dat$Roughness),
                      m_bio = mean(dat$RAP_bio),
                      m_herb = mean(dat$Herb),
                      m_shrub = mean(dat$Shrub),
                      m_tree = mean(dat$Tree),
                      # Mean scaled and centered
                      m_SC_elev = mean(dat$scaled_Elev),
                     # m__SC_snd = mean(dat$scaled_SND),
                      m_SC_a.sin = mean(dat$scaled_Asp_sin),
                      m_SC_a.cos = mean(dat$scaled_Asp_cos),
                      m_SC_rough = mean(dat$scaled_Rough),
                      m_SC_bio = mean(dat$scaled_RAP_bio),
                      m_SC_herb = mean(dat$scaled_Herb),
                      m_SC_shrub = mean(dat$scaled_Shrub),
                      m_SC_tree = mean(dat$scaled_Tree),
                      # month and year 
                      month = unique(dat$month),
                      year = unique(dat$year),
                     row.names = NULL)
    
    # Bind to the general results table
    output <- bind_rows(temp, output)
    
     }
  }
}

# Remove first row of NAs
output <- output[-1, ]
table(output$month)
table(output$year)

# Save as .rds 
saveRDS(output, "Data/Outputs/RSF_outputs/20221011_m_avail.rds")

#out_dir <- "../../Data/Chapter1/20220723_ouptuts/"
#dir.create(out_dir)
#write.csv(output, "../../Data/Chapter1/20220723_ouptuts/20220723_m_avail.csv", row.names = FALSE)

# DONE!