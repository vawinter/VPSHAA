#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Habitat Selection & RSFs  -----X
##---------- January 2022 ---------------X
#########################################X
#---- Organizing used vs avail pts ------X
##------ Last edited: 05/18/2022  -------X
#########################################X

# Goals for this script:
#   a. Combine individual characteristic with glm output data


# Clean env
rm(list = ls())
gc()

# load in tables from db
library(dplyr)
library(lubridate)
library(DBI)
library(tidyr)
library(stringr)

## Load in data ----
# Test read
fin <- readRDS("Data/Outputs/RSF_outputs/20221018_files-combined.rds")
head(fin)

#a. Query the db 
ph_db <- dbConnect(drv = RSQLite::SQLite(), # wherever I saved db
                   "Data/Processed/pronghorn.db")

#b. Pull tables
prong <- dbGetQuery(ph_db, "SELECT * FROM pronghorn;")
status <- dbGetQuery(ph_db, "SELECT * FROM status;") %>% 
  filter(!year == "2021")

# # fix status file to longer
# stat_fix <- status %>% 
#   pivot_longer(!ID, names_to = "mig.season", values_to = "mig_tend") %>% 
#   # seperate into season/year
#   separate(mig.season, c("mig_season", "year")) %>% 
#   mutate(mig_tend = as.factor(mig_tend))

# intersect the IDs to limit to only indiv in fin
combo <-  intersect(fin$ID, prong$ID)

# edit ph table
ph2 <-  prong  %>% 
  # filter IDs
  filter(ID %in% combo) %>% 
  # Join w/ status table
  left_join(status, by = "ID") %>% 
  # Select only needed columns
  dplyr::select(- CollarID)  

# Restructuring ----
# Fill new df
# ph <- ph2
# # Fill blank spaces with NA
# ph[ph == "" | ph == " "] <- "NA" 
# # Check output
# table(ph$tendency == "NA")
# 
# # Filter out unk w/ no assoc. season
# ph2 <-  ph %>% 
#   filter(!(mig_season == "NA")) %>% 
#   filter(!(tendency == "NA"))
# 
# head(ph2)

table(is.na(ph2$mig_tend))
table(ph2$tendency, ph2$mig_season, is.na(ph2$tendency))


# How do I code this for my table?
#         S   W
# mig    175 120
# nonmig 125 122
# unk    124 182

ph2 %>% 
  summarise(unit) %>% 
  group_by(unit) %>% 
  tally()

# # A tibble: 8 x 2
#   unit                 n
# 1 Antelope Island     95
# 2 Cache                4
# 3 Dugway              46
# 4 North Slope         50
# 5 Plateau            303
# 6 Southwest Desert   208
# 7 West Desert        108
# 8 Woodruff           100

ph2 %>% 
  summarise(sex) %>% 
  group_by(sex) %>% 
  tally()

ph2$age_class[is.na(ph2$age_class)] <- "U"
ph2$age_class[ph2$age_class == "NA"] <- "U"

ph2 %>% 
  summarise(age_class) %>% 
  group_by(age_class) %>% 
  tally()

# JOINING ----
# join with fin table
fin$year <- as.character(fin$year)
ph2$mig_tend <- as.character(ph2$tendency)
ph2$ID <- as.factor(ph2$ID)

fin2 <- fin %>% 
  # mutate year column to match
  mutate( # create season column w/ corresponding months
    mig_season = case_when(
      # Winter mig = nov and feb
      # summer mig is apri and july
      month %in% c(4, 7) ~ "S",
      month %in% c(2, 11) ~ "W"
    )) %>% 
  # left join by all col's that coorespond
  # cahnged to merge 6/29
  merge(ph2, by = c("ID", "year", "mig_season")) %>% 
  #merge(stat_fix, by = c("ID", "year", "mig_tend")) %>% 
  # mutate mig status columns
  dplyr::mutate(
         is.mig = case_when(mig_tend == "mig" ~ 1,
                            TRUE ~ 0),
         is.res = case_when(mig_tend == "res" ~ 1,
                            TRUE ~ 0),
         is.unk_mig = case_when(mig_tend == "unk" ~ 1,
                                TRUE ~ 0),
         # add age class col
         is.Adult = case_when(.$age_class == "adult" ~ 1, TRUE ~ 0),
         is.age_unk = case_when(.$age_class == "U" ~ 1, TRUE ~ 0),
         # add sex column
         is.Male = case_when(.$sex == "M" ~ 1, TRUE ~ 0),
         is.Female = case_when(.$sex == "F"~ 1, TRUE ~ 0),
         is.unk = case_when(.$sex == "U"~ 1, TRUE ~ 0),
         # add in units column
         is.AntelopeIsland = case_when(.$unit == "Antelope Island" ~ 1, TRUE ~ 0),
         # is.BookCliffs = case_when(.$unit == "Book Cliffs" ~ 1, TRUE ~ 0),
         #is.Cache = case_when(.$unit == "Cache"~ 1, TRUE ~ 0),
         is.Dugway = case_when(.$unit == "Dugway"~ 1, TRUE ~ 0),
         is.NorthSlope = case_when(.$unit == "North Slope"~ 1, TRUE ~ 0),
         is.Plateau = case_when(.$unit == "Plateau"~ 1, TRUE ~ 0),
         is.SouthwestDesert = case_when(.$unit == "Southwest Desert"~ 1, TRUE ~ 0),
         is.WestDesert = case_when(.$unit == "West Desert"~ 1, TRUE ~ 0),
         is.Woodruff = case_when(.$unit == "Woodruff"~ 1, TRUE ~ 0)
  ) %>%
  # remove unnecessary rows
  dplyr::select(-mig_season) %>% 
  distinct() 

# Check
head(fin2)

# Save
write.csv(fin2, "Data/Outputs/RSF_outputs/20221018-10.5.csv", row.names = FALSE)

