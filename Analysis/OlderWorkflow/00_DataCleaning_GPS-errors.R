#################################################X
# ---- CODE FOR CLEANING GPS TRACKING TABLES ----x
#################################################X
# ------- Functions updated: 05/24/2022 ---------X
#--------------------- VAW ----------------------x
#------------------ 05/24/2022 ------------------X
#################################################X
#------------ Last updated: 10-08-2022 ----------X
#################################################X

# clean up my R environment
rm(list = ls()) 
gc()

# Load Packages and Functions ----
library(amt)
library(dplyr)
library(lubridate)

# install cleaning fun
devtools::install_github("jmsigner/amt", ref = "data-cleaning")

# box_dir <- "C:/Users/veron/Box/Avgar Lab on WILD/"
# # path to cleaning functions (can be found in this box folder)
# func_path <- paste0(box_dir, "Code/Functions/datacleaning_funs_BJS.R")
# 
# # Link functions
# source(func_path)

# Set Up ----
## Set Inputs ----

# EPSG code for WGS84 UTM 12N
utm <- 32612

# alpha
alpha_ <- lubridate::hours(24) %>% # 5/13/22: changed from 10 to 24
  as.numeric() %>%
  lubridate::period()

  # delta
  # 2/9/2022: Tal gave criterion per species, this is PH
  speed <- 50
  time <- lubridate::minutes(60)
  delta <- calculate_sdr(speed, time)

# Zeta, eta, and theta
zeta <- 5
eta <- 5
theta <- lubridate::days(1)

## Load Data ----
prong <- readRDS("Data/Raw/20220519_ph_BigQuery-updated.rds")

# Select desired fields and join
dat <- prong %>%
  # rename ID column
  rename(animal_id = AnimalID) %>%
  # rename x and y also
  rename(x = Longitude,
         y = Latitude) %>%
  # turn dop into number not chr
  mutate(dop = as.numeric(Dop)) %>%
  # filter out 2022 data
 # filter(!(year(DateAndTime) %in% 2022)) %>%
  # Remove 'Dop' col
  select(-Dop) %>% 
  # arrange time for make track
  arrange(DateAndTime)

# Set up loop ----
# Grab IDs
id <- unique(dat$animal_id)
# Get empty df
clean <- data.frame()

for(i in 1:length(id)){
  # print status
  print(paste(i, id[i]))
  
  df <- dat %>% 
  # Filter only the specific individual
    filter(animal_id == id[i]) %>% 
  
  # Turn into a track_xyt
    mk_track(.x = x, .y = y, .t = DateAndTime, 
                     all_cols = TRUE, 
                     crs = 4326) %>% 
    # project to given crs
    transform_coords(crs_to = utm) %>%
    
    ### Remove low quality duplicated ----
    flag_duplicates(gamma = minutes(10)) %>% 
    filter(!duplicate_)  %>% 
   
    ### Remove fast steps ----
     flag_fast_steps(delta = delta)# %>%
    # filter(!fast_step_) %>% 
   
    ### Remove fast roundtrips ----
     flag_roundtrips(delta = delta, epsilon = 10, time_unit = "secs")# %>% 
    # filter(!fast_roundtrip_) %>% 
   
    ## Remove mortality/drop-off clusters ----
    flag_defunct_clusters(zeta = zeta, eta = eta, theta = theta) #%>%
    #filter(!defunct_cluster_)

  # combine together
  clean <- rbind(df, clean)
}

# Checks ----
# Check flags
table(clean$fast_roundtrip_)

# Rename and reorganize
dat_clean <- clean %>%
  # move ID column to front
  relocate(animal_id, .before = x_) %>%
  # move ID column
  relocate(animal_id, .before = x_) %>%
  # rename columns to original nomenclature
  rename(ID = animal_id,
         dt = t_,
         x = x_,
         y = y_) %>% 
  # remove unnecessary col
  select(-defunct_cluster_,
         -duplicate_,
         -fast_step_,
         -fast_roundtrip_)

# Check rows
nrow(dat)
# [1] 2332467
nrow(clean)
# [1] 2331594
nrow(dat_clean)
# [1] 2331594
# How many indivs left?
x <- unique(dat_clean$ID)
id

## Save outputs ----
# Save cleaned data
saveRDS(clean, "Data/Processed/clean_ph_20221010.rds")
# Save combined, filtered df's
saveRDS(dat_clean, "Data/Processed/comb_dat_20221010.rds")

# DONE!

x <- readRDS("Data/Processed/clean_ph_20221010.rds")

