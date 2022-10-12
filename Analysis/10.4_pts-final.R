#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- January 2022 ---------------X
#########################################X
#---- Organizing used vs avail pts ------X
##------ Last edited: 04/07/2022  -------X
#########################################X

# Goals for this script:
#   a. Organized used pts with final all seasons csv

# Clean env
rm(list = ls())
gc()

# Load in libraries ----
library(dplyr)# Read in files
library(lubridate)

# # directory
dir <- "../RSF_data/test_outputs/"

# list files
# mod_files <- list.files(dir, full.names = T, pattern = "20220714")
# mod_files <- list.files(dir, full.names = T)

# read in files individually
glm <- readRDS("Data/Outputs/RSF_ouputs/20221011_eHSF_output.rds")
avail <- readRDS("Data/Ouptuts/RSF_outputs/20221011_m_avail.rds")
used <- read.csv("../RSF_data/final_outputs/20220620_used.csv", header = T)
road <- read.csv("../RSF_data/final_outputs/20220620_road-data_comb.csv", header= T)
pdsi <- read.csv("../RSF_data/final_outputs/20220620_drought_comb.csv", header = T)


# Join together avail and used df
x <- used %>% 
  left_join(avail, by = c("ID", "month", "year")) %>% 
  left_join(road, by  = c("ID", "month", "year")) %>% 
  left_join(pdsi, by = c("ID", "month", "year")) %>% 
  arrange(ID)

# Join x with glm and reformat all df together
final <- x %>% 
  left_join(glm, by = c("ID", "month", "year")) %>% 
  # rename(used_in = n_in, #rename col
  #        used_out = n_out) %>% 
  relocate(month, year, avail_pts, .before = m_elev) %>%  # move col locas
  mutate(month_long = case_when(.$month == "2" ~ "February",
                                .$month == "4" ~ "April",
                                .$month == "7" ~ "July",
                                .$month == "11" ~ "November")) %>%
  # Turn ID and year to a factor
  mutate(ID = as.factor(ID)) %>% 
  # mutate(year = as.factor(year)) %>% 
  relocate(month_long, .before = avail_pts) 

# # Save
# write.csv(final, "../RSF_data/final_outputs/20220620_comb.csv", row.names = FALSE)
# 
# # Test read
# y <- read.csv("../RSF_data/final_outputs/20220620_comb.csv", header = T, stringsAsFactors = T)
# head(y)

# New columns for season and year
fin <- final %>% 
  # create columns
  mutate(is.Winter = case_when(final$month == "2" ~ 1, TRUE ~ 0),
         is.Spring = case_when(final$month == "4" ~ 1, TRUE ~ 0),
         is.Summer = case_when(final$month == "7" ~ 1, TRUE ~ 0),
         is.Fall = case_when(final$month == "11" ~ 1, TRUE ~ 0),
         is.2018 = case_when(final$year == "2018"~ 1, TRUE ~ 0),
         is.2019 = case_when(final$year == "2019" ~ 1, TRUE ~ 0),
         is.2020 = case_when(final$year == "2020" ~ 1, TRUE ~ 0),
         is.2021 = case_when(final$year == "2021" ~ 1, TRUE ~ 0)) 

head(fin)

# Save
saveRDS(fin, "Data/Outputs/RSF_ouputs/20221011_files-combined.rds")

# DONE!
