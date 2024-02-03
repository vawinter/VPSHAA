# clean env
rm(list = ls())
gc()

# libraries
library(dplyr)
library(amt)
library(ggplot2)
library(lubridate)

# Import data to the db ---
dat <- readRDS("Data/Processed/comb_dat_20220524.rds")
# grab ids
id <- unique(dat$ID)


# See what fix rates are for all indivs
# make empty list
rate <- list()
# 2. Loop
for(k in 1:length(id)){
  
  # print status
  print(paste(k))
  
  # Subset individual data
  d <- filter(dat, ID == id[k])
  
  # Turn into a track_xyt
  rate[[k]] <- mk_track(d, x, y, dt, crs = 32612,
                        all_cols = T) %>% 
    # sample fix rate per individual
    summarize_sampling_rate(time_unit = "hour") 
  
  
}

# grab names 
names(rate) <- id
# combine files
to_fix <- do.call(rbind, lapply(rate, data.frame))
to_fix$ID <- row.names(to_fix)

# plot
plot(to_fix$q1)

# Fix fixrates across the dataset
id <- unique(dat$ID)
# make empty list
newdat <- list()
# 2. Loop
for(i in 1:length(id)){
  
  # print status
  print(paste(i))
  
  # Subset individual data
  d <- filter(dat, ID == id[i])
  
  # Turn into a track_xyt
  newdat <- mk_track(d, x, y, dt, crs = 32612, all_cols = T) %>% 
    # sample fix rate per individual
    track_resample() %>% 
    steps_by_burst()
  
}

# grab names 
names(newdat) <- id
# combine files
fixed <- do.call(rbind, lapply(newdat, data.frame))
fixed$ID <- row.names(fixed)
# remove period after ID #
new_ID <- gsub("\\..*", "", fixed$ID)

# reformat
checked <- fixed %>%
  # replcae IDs with new col
  mutate(ID = new_ID) 

head(checked)

# ... now convert steps back to points ----
nst_pts <- checked %>% 
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
  arrange(ID, t) %>% 
  rename(dt = t) %>% 
  # join to full data
  left_join(dat, by = c("ID", "dt", "x", "y")) 

# check
# See what fix rates are for all indivs
# make empty list
rate_ck <- list()
id <- unique(pts$ID)

# 2. Loop
for(k in 1:length(id)){
  
  # print status
  print(paste(k))
  
  # Subset individual data
  d <- filter(pts, ID == id[k])
  
  # Turn into a track_xyt
  rate_ck[[k]] <- mk_track(d, x, y, dt, crs = 32612,
                        all_cols = T) %>% 
    # sample fix rate per individual
    summarize_sampling_rate(time_unit = "hour") 
  
  
}

# grab names 
names(rate_ck) <- id
# combine files
to_ck <- do.call(rbind, lapply(rate_ck, data.frame))
to_ck$ID <- row.names(to_ck)

plot(to_ck$q1)

#

# save file
saveRDS(pts, "Data/Processed/20220813_cleaned-data.rds")
