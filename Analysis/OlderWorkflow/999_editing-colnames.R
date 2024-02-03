# renaming columns for easier readability

dat2 <- dat %>% 
  rename(m_SC_a.cos = m__SC_a.cos,
         m_SC_a.sin = m__SC_a.sin,
         m_SC_bio = m__SC_bio,
         m_SC_rough = m__SC_rough,
         m_SC_herb = m__SC_herb,
         m_SC_shrub = m__SC_shrub)

write.csv(dat2, "Data/Outputs/RSF_outputs/20221017_mem-prep-v2.csv", row.names = T)



fin2 <- dat %>% 
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
