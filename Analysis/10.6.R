
# 3/9/2022 ------
rm(list = ls())
gc()
library(dplyr)

# Finding and fixing NA's
dat <- read.csv("../../Data/Chapter1/20220726-elev_files-10.5.csv", header = T)


# JOINING ----
# join with fin table
fin2 <- dat %>% 
  # mutate mig status columns
  mutate(is.mig = case_when(mig_tend == "1" ~ 1,
                            TRUE ~ 0),
         # is.mig = case_when(tendency == "mig" ~ 1,
         #                    tendency == "nonmig" ~ 0,
         #                    tendency == "unk" ~ NA_real_), 
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
         is.Dugway = case_when(.$unit == "Dugway" | .$unit == "West Desert" ~ 1, TRUE ~ 0),
         is.NorthSlope = case_when(.$unit == "North Slope"~ 1, TRUE ~ 0),
         is.Plateau = case_when(.$unit == "Plateau"~ 1, TRUE ~ 0),
         is.SouthwestDesert = case_when(.$unit == "Southwest Desert"~ 1, TRUE ~ 0),
         # is.WestDesert = case_when(.$unit == "West Desert"~ 1, TRUE ~ 0),
         is.Woodruff = case_when(.$unit == "Woodruff"|.$unit == "Cache" ~ 1, TRUE ~ 0)
  ) #%>%
# remove unnecessary rows
dplyr::select(-mig_season)


# reformat other cols based on what we just fixed
fin <- fin2 %>% 
  mutate(# fix sex column
    is.Male = case_when(.$sex == "M" ~ 1, TRUE ~ 0),
    is.Female = case_when(.$sex == "F"~ 1, TRUE ~ 0),
    is.unk = case_when(.$sex == "U"~ 1, TRUE ~ 0),
    # fix units column
    is.AntelopeIsland = case_when(.$unit == "Antelope Island" ~ 1, TRUE ~ 0),
    # is.BookCliffs = case_when(.$unit == "Book Cliffs" ~ 1, TRUE ~ 0),
    is.Woodruff = case_when(.$unit == "Cache" |.$unit == "Woodruff" ~ 1, TRUE ~ 0),
    is.Dugway = case_when(.$unit == "Dugway"| .$unit == "West Desert"~ 1, TRUE ~ 0),
    is.NorthSlope = case_when(.$unit == "North Slope"~ 1, TRUE ~ 0),
    is.Plateau = case_when(.$unit == "Plateau"~ 1, TRUE ~ 0),
    is.SouthwestDesert = case_when(.$unit == "Southwest Desert"~ 1, TRUE ~ 0),
    # climactic regions
    clim.reg = case_when(.$unit == "Antelope Island" ~ "North Central",
                         .$unit == "Book Cliffs" ~ "Uinta Basin", 
                         .$unit == "Cache" ~ "North Central", 
                         .$unit == "Dugway" ~ "Western", 
                         .$unit == "North Slope"~ "Northern Mountains", 
                         .$unit == "Plateau"~ "South Central", 
                         .$unit == "Southwest Desert" ~ "Western", 
                         .$unit == "West Desert"~ "Western", 
                         .$unit == "Woodruff"~ "Northern Mountains"),
    # add in clim region bouillons
    is.NorthCentral = case_when(.$unit == "Antelope Island" ~ 1, TRUE ~ 0),
    # is.BookCliffs = case_when(.$unit == "Book Cliffs" ~ 1, TRUE ~ 0),
    is.NorthMnt = case_when(.$unit == "North Slope" |.$unit == "Woodruff" ~ 1, TRUE ~ 0),
    is.Western = case_when(.$unit == "Southwest Desert"| .$unit == "West Desert"~ 1, TRUE ~ 0),
    is.SouthCentral = case_when(.$unit == "Plateau"~ 1, TRUE ~ 0),
    # mountain ph or desert
    is.mountain = case_when(.$unit == "Antelope Island" ~ 1,
                            .$unit == "Book Cliffs" ~ 1, 
                            .$unit == "Cache" ~ 1, 
                            .$unit == "Dugway" ~ 0, 
                            .$unit == "North Slope"~ 1, 
                            .$unit == "Plateau"~ 0, 
                            .$unit == "Southwest Desert"~ 0, 
                            .$unit == "West Desert"~ 0, 
                            .$unit == "Woodruff"~ 1)
  ) %>% 
  relocate(sex, unit, .before = used_out)

# table(which(is.na(fin$sex)))
# fin[798, 2] <- "M"

# Save ----
write.csv(fin, "../../Data/Chapter1/20220726-elev_files-10.6.csv", row.names = FALSE)