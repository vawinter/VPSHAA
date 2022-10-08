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

# read in csv individually
# Update: glm doesn't include snow - 4/1
# Update: glm doesn't include snow or bio now - 4/7
#glm <- read.csv("../RSF_data/final_outputs/20220620_glm.csv", header = T)
glm <- read.csv("../RSF_data/test_outputs/20220725_elev_glm.csv", header = T)
avail <- read.csv("../../Data/Chapter1/20220723_ouptuts/20220723_m_avail.csv", header = T)
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
saveRDS(fin, "../../Data/Chapter1/20220726_2021_files-combined.rds")

# DONE!

# # Format for MEM ----
# rm(list = ls())
# gc()
# 
# # load in tables from db
# library(dplyr)
# library(lubridate)
# library(DBI)
# 
# ## Load in data ----
# # Test read
# fin <- read.csv("../RSF_data/final_outputs/20220524_4.csv", header = T)
# head(fin)
# 
# #a. Query the db 
# prong <- readRDS("cleaned_data/comb_dat_20220524.rds")
# status <- readRDS("../../Data/mig.tend/20220520_ph_mig_unit_save.rds")
# 
# # Grab Ids
# combo <-  intersect(fin$ID, prong$ID)
# 
# # edit ph table
# ph2 <-  prong  %>% 
#   filter(ID %in% combo) %>% 
#   left_join(status, by = c("ID", "unit", "sex")) %>% 
#   dplyr::select(ID,
#          season,
#          unit,
#          sex,
#          age,
#          age_class,
#          b_year,
#          year,
#          tendency)
# 
# 
# # restructure
# ph <- ph2 %>%
#   mutate(mig_year = as.integer(year)) %>%
#   dplyr::select(-year)
# 
# head(ph)
# 
# 
# # New columns for season and year
# fin2 <- fin %>% 
#   # create columns
#   mutate(is.Fall = case_when(fin$month == "11" ~ 1, TRUE ~ 0),
#          is.Spring = case_when(fin$month == "4" ~ 1, TRUE ~ 0),
#          is.Summer = case_when(fin$month == "7" ~ 1, TRUE ~ 0),
#          is.Winter = case_when(fin$month == "2" ~ 1, TRUE ~ 0),
#          is.2018 = case_when(fin$year == "2018"~ 1, TRUE ~ 0),
#          is.2019 = case_when(fin$year == "2019" ~ 1, TRUE ~ 0),
#          is.2020 = case_when(fin$year == "2020" ~ 1, TRUE ~ 0)) %>% 
#   # Filter ID in both
#   filter(ID %in% combo) 
# 
# # Save
# saveRDS(fin2, "../RSF_data/final_outputs/20220525_final_glm-unit_comb.rds")
# 
# # DONE!