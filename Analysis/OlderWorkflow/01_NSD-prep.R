#####################################################X
#---------Code for cleaning pronghorn data ----------X
#-------------------February 2021--------------------X
#--------------------VAW & BJS-----------------------X
#####################################################X

# clean up my R environment
rm(list = ls()) 
gc()

# Load packages ----
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(amt)
library(DBI)

# Update 4/20/2022 
ph_dat <- readRDS("Data/Processed/comb_dat_20220524.rds")

# indiv who I need NSD'd fos
#ns <- c("PR17F0037", "PR17F0036", "PR17M0033", "PR19F0033", "PR20U0003")

# ph <- ph_dat %>% 
#   filter(ID %in% ns)
# 
# z <- unique(ph$ID)

# Drop individuals without enough data
# Who has very few locations? Remove all with < 30 locations
d <- ph_dat %>%
  group_by(ID) %>%
  tally() %>%
  arrange(n) %>% 
  filter(n > 30) 

# # Keep relevant columns and rename
dat <- ph_dat %>%
  # Keep relevant columns
  select(ID,
         y,
         x ,
         dt) %>%
  # Format timestamp
  mutate(dt = ymd_hms(dt)) %>%
  # Drop problem individuals
  filter(ID %in% d$ID) %>%
  # Get rid of exact duplicates
  distinct(ID, x, y, dt) %>%
  # Now pick the second when there are two fixes at same time
  mutate(row = 1:nrow(.)) %>%
  group_by(ID, dt) %>%
  filter(row == last(row, order_by = row))
 
# # # Convert to UTM
# dat_sf <- st_as_sf(dat,
#                    coords = c("x", "y"),
#                    crs = 4326) %>%
#   st_transform(32612)
# 
# # Convert from sf back to ordinary data.frame
# dat <- dat_sf %>%
#   mutate(x = st_coordinates(geometry)[, 1],
#          y = st_coordinates(geometry)[, 2]) %>%
#   st_drop_geometry()

# Find the location closest to noon
dat$noon <- dat$dt
hour(dat$noon) <- 12
dat$diff_noon <- difftime(dat$dt, dat$noon, units = "hours")
dat$date <- as.Date(dat$dt)

# Now group by ID and date and keep the one closest to noon
# If there's a tie (e.g., 11am vs 1pm), keep earlier
dat2 <- dat %>% 
  group_by(ID, date) %>% 
  filter(dt == first(dt, order_by = abs(diff_noon))) %>% 
  ungroup() %>% 
  select(ID, dt, x, y, diff_noon)

# # Check an example
# View(dat2[130, ]) # diff from noon is 6 hours
# # On this date, this individual 
# dat %>% 
#   filter(ID == "PR17F0018",
#          month(time) == 12,
#          day(time) == 19,
#          year(time) == 2017) %>% 
#   View()

# Looks good. Points at 11am almost every day for all individuals

# ... format data as steps with amt ----
nst <- dat2 %>% 
  nest(data = -ID) %>% 
  mutate(steps = lapply(data, function(x) {
    x %>% 
      # Convert data to a 'track_xyt' object
      make_track(x, y, dt, diff_noon = diff_noon,  crs = 32612) %>% 
      # Sort by time
      arrange(t_) %>% 
      # Convert from points to steps
      steps() %>% 
      # Convert dt_ from 'difftime' to 'numeric' (in hours)
      mutate(dt_ = as.numeric(dt_, units = "hours")) %>% 
      # Keep only steps that are exactly 24 h
     filter(dt_ == 24)
  })) 

# When you're done with the steps, unnest
unnst <- nst %>% 
  select(-data) %>% 
  unnest(cols = steps)

# Sample size for each individual
unnst %>% 
  group_by(ID) %>% 
  tally() %>% 
  View()

# ... now convert steps back to points ----
nst_pts <- unnst %>% 
  select(ID, x1_, x2_, y1_, y2_, t1_, t2_) %>% 
  # Nest step 1 and step 2
  nest(loc1 = c(x1_, y1_, t1_),
       loc2 = c(x2_, y2_, t2_)) %>% 
  # Rename to just x, y, t
  mutate(
    loc1 = lapply(loc1, function(x) {
      names(x) <- c("x", "y", "t")
      return(x)
    }),
    loc2 = lapply(loc2, function(x) {
      names(x) <- c("x", "y", "t")
      return(x)
    })
  )

# Unnest start points
st_pts <- nst_pts %>% 
  select(-loc2) %>% 
  unnest(cols = loc1)

# Unnest end points
end_pts <- nst_pts %>% 
  select(-loc1) %>% 
  unnest(cols = loc2)

# Combine and remove duplicates
pts <- bind_rows(st_pts, end_pts) %>% 
  distinct() %>% 
  arrange(ID, t)

# Save ----
# Save steps
#saveRDS(unnst, "Data/PH_data/processed_data/20220420_pronghorn_daily_cleaned_steps.rds")
# Save points
saveRDS(pts, "Data/Processed/20220526_ph_cleaned_points.rds")

#### Pre-prep for HMM ------
ph_dat <- readRDS("Data/Processed/20220526_ph_cleaned_points.rds")
class(ph_dat)
str(ph_dat)
summary(ph_dat)

# Rename and reorder columns
ph <- ph_dat %>% 
  select(ID,
         x,
         y,
         dt = t)
class(ph)

ph %>% 
  summarize(n_distinct(ID))

# change to data frame from tbl_df
dat <- as.data.frame(ph)
class(dat)
str(dat)

# combine with status information
# ph_2 <- ph %>%
#   left_join(status, by = "ID")

str(dat)

saveRDS(ph_2, "Data/Processed/20220420_pronghorn_daily_pts_LL.rds")
# Convert to UTM
dat_sf <- st_as_sf(dat,
                   coords = c("x", "y"),
                   crs = 4326) %>%
  st_transform(32612)

# Convert from sf back to ordinary data.frame
dat <- dat_sf %>%
  mutate(x = st_coordinates(geometry)[, 1],
         y = st_coordinates(geometry)[, 2]) %>%
  st_drop_geometry()

# looks good!
# Save ---
saveRDS(dat, "Data/Processed/20220526_ph_daily_points.rds")
#write.csv(dat, "pronghorn_points_pre-prep2.csv")

# OLD CODE ----
# #1. Query the db ----
# ph_db <- dbConnect(drv = RSQLite::SQLite(), # wherever I saved db
#         "/Users/veron/Documents/Projects/Data/PH_data/pronghorn.db")
# 
# # pull tables
# prong <- dbGetQuery(ph_db, "SELECT * FROM pronghorn;")
# track <- dbGetQuery(ph_db, "SELECT * FROM tracking;")
# status <- dbGetQuery(ph_db, "SELECT * FROM status;")
# # Select desired fields and join
# 
# ph <- prong %>% 
#   as_tibble() %>% 
#   left_join(track, by = "ID") %>% 
#   left_join(status, by = "ID") %>%
#   filter(!tendency == "unk")


