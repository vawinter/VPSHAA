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
fin <- readRDS("../../Data/Chapter1/20220726_test-elev_files-combined.rds")
#readRDS("../RSF_data/final_outputs/20220525_final_glm-unit_comb.rds")
head(fin)

#a. Query the db 
ph_db <- dbConnect(drv = RSQLite::SQLite(), # wherever I saved db
                   "../../Data/PH_data/pronghorn.db")

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
  mutate( # create season column w/ cooresponding months
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
  mutate(is.mig = case_when(mig_tend == "1" ~ 1,
                            TRUE ~ 0),
         is.res = case_when(mig_tend == "0" ~ 1,
                            TRUE ~ 0),
         is.unk_mig = case_when(mig_tend == "0.5" ~ 1,
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
write.csv(fin2, "../../Data/Chapter1/20220726-elev_files-10.5.csv", row.names = FALSE)

# 
# # 3/9/2022 ------
# rm(list = ls())
# 
# # Finding and fixing NA's
# ph_dat <- readRDS("cleaned_data/comb_dat_20220218.rds")
# dat <- read.csv("../RSF_data/final_outputs/20220518_mod_final_comb-unit-sex.csv",
#                 header = T)
# 
# # NAs?
# which(is.na(dat$sex))
# 
# prob <- c("PR17M0003", "PR17M0005", "PR17M0006",
#           "PR17M0007", "PR17M0009", "PR17M0013", "PR17M0015",
#           "PR18F0007", "PR18M0004", "PR19F0014", "PR19F0032",
#           "PR19M0006", "PR20F0064")
# 
# # select prob individuals
# ph <-  ph_dat %>% 
#   dplyr::select(ID,
#          CaptureUnit,
#          Sex) %>% 
#   filter(ID %in% prob) %>% 
#   distinct()
# 
# # filter out NA
# dat2 <- dat %>% 
#   relocate(ID,
#            sex,
#            unit,
#            age, .before = used_out)
# 
# # who is NA
# which(is.na(dat2$unit))
# 
# # fill in sex
# dat2[286:289, 2] <- "M"
# dat2[301:304, 2] <- "M"
# dat2[313:316, 2] <- "M"
# dat2[324:326, 2] <- "M"
# dat2[338:340, 2] <- "M"
# dat2[371:373, 2] <- "M"
# dat2[390:392, 2] <- "M"
# dat2[609:610, 2] <- "F"
# dat2[748, 2] <- "M"
# dat2[872, 2] <- "F"
# dat2[930:934, 2] <- "F"
# dat2[956, 2] <- "M"
# dat2[1161:1163, 2] <- "F"
# 
# # Fill unit
# dat2[286:289, 3] <- "Plateau"
# dat2[301:304, 3] <- "Plateau"
# dat2[313:316, 3] <- "Plateau"
# dat2[324:326, 3] <- "Plateau"
# dat2[338:340, 3] <- "Plateau"
# dat2[371:373, 3] <- "Plateau"
# dat2[390:392, 3] <- "Plateau"
# dat2[609:610, 3] <- "Southwest Desert"
# dat2[748, 3] <- "Southwest Desert"
# dat2[872, 3] <- "Antelope Island"
# dat2[930:934, 3] <- "Antelope Island"
# dat2[956, 3] <- "Antelope Island"
# dat2[1161:1163, 3] <- "North Slope"
# 
# 
# # Check
# check <- dat2 %>% 
#   filter(ID %in% prob) %>% 
#   dplyr::select(ID,
#          sex,
#          unit)
# 
# # fix new sex knowns
# which(dat2$ID == "PR19U0001")
# s <- c("PR19U0001", "PR17U0002",
#        "PR17U0003", "PR18U0002", "PR17U0005", "PR17U0006")
# # fill in sex
# dat2[530:540, 2] <- "F"
# dat2[541:546, 2] <- "F"
# dat2[558:564, 2] <- "M"
# dat2[565:567, 2] <- "M"
# dat2[809:814, 2] <- "F"
# dat2[976:982, 2] <- "F"
# 
# which(dat2$ID == "PR17U0001")
# which(dat2$ID == "PR17U0004")
# which(dat2$ID == "PR17U0007")
# which(dat2$ID == "PR18U0003")
# 
# # fill in age
# dat2[530:540, 4] <- "5.5"
# dat2[541:546, 4] <- "3.5"
# dat2[558:564, 4] <- "4.5"
# dat2[565:567, 4] <- "4.5"
# dat2[809:814, 4] <- "7.5"
# dat2[976:982, 4] <- "5.5"
# dat2[518:529, 4] <- "3.5"
# dat2[547:557, 4] <- "5.5"
# dat2[568:579, 4] <- "4.5"
# dat2[815:817, 4] <- "4.5"
# 
# # reformat other cols based on what we just fixed
# fin <- dat2 %>% 
#   mutate(# fix sex column
#          is.Male = case_when(.$sex == "M" ~ 1, TRUE ~ 0),
#          is.Female = case_when(.$sex == "F"~ 1, TRUE ~ 0),
#          is.unk = case_when(.$sex == "U"~ 1, TRUE ~ 0),
#          # fix units column
#          is.AntelopeIsland = case_when(.$unit == "Antelope Island" ~ 1, TRUE ~ 0),
#          # is.BookCliffs = case_when(.$unit == "Book Cliffs" ~ 1, TRUE ~ 0),
#          is.Woodruff = case_when(.$unit == "Cache" |.$unit == "Woodruff" ~ 1, TRUE ~ 0),
#          is.Dugway = case_when(.$unit == "Dugway"| .$unit == "West Desert"~ 1, TRUE ~ 0),
#          is.NorthSlope = case_when(.$unit == "North Slope"~ 1, TRUE ~ 0),
#          is.Plateau = case_when(.$unit == "Plateau"~ 1, TRUE ~ 0),
#          is.SouthwestDesert = case_when(.$unit == "Southwest Desert"~ 1, TRUE ~ 0),
#          # climactic regions
#          clim.reg = case_when(.$unit == "Antelope Island" ~ "North Central",
#                                    .$unit == "Book Cliffs" ~ "Uinta Basin", 
#                                    .$unit == "Cache" ~ "North Central", 
#                                    .$unit == "Dugway" ~ "Western", 
#                                    .$unit == "North Slope"~ "Northern Mountains", 
#                                    .$unit == "Plateau"~ "South Central", 
#                                    .$unit == "Southwest Desert" ~ "Western", 
#                                    .$unit == "West Desert"~ "Western", 
#                                    .$unit == "Woodruff"~ "Northern Mountains"),
#          # add in clim region bouillons
#          is.NorthCentral = case_when(.$unit == "Antelope Island" ~ 1, TRUE ~ 0),
#          # is.BookCliffs = case_when(.$unit == "Book Cliffs" ~ 1, TRUE ~ 0),
#          is.NorthMnt = case_when(.$unit == "North Slope" |.$unit == "Woodruff" ~ 1, TRUE ~ 0),
#          is.Western = case_when(.$unit == "Southwest Desert"| .$unit == "West Desert"~ 1, TRUE ~ 0),
#          is.SouthCentral = case_when(.$unit == "Plateau"~ 1, TRUE ~ 0),
#                   # mountain ph or desert
#           is.mountain = case_when(.$unit == "Antelope Island" ~ 1,
#                                   # .$unit == "Book Cliffs" ~ 1, 
#                                    .$unit == "Cache" ~ 1, 
#                                    .$unit == "Dugway" ~ 0, 
#                                    .$unit == "North Slope"~ 1, 
#                                    .$unit == "Plateau"~ 0, 
#                                    .$unit == "Southwest Desert"~ 0, 
#                                    .$unit == "West Desert"~ 0, 
#                                    .$unit == "Woodruff"~ 1)
#    ) %>% 
#  select(-is.Cache,
#         -is.WestDesert)
# 
# head(fin)
# 
# # Save ----
# write.csv(fin, "../RSF_data/final_outputs/20220419_mod_final_comb-unit-sex.csv", row.names = FALSE)