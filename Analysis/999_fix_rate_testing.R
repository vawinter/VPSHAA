# June 20th, 2022
# getting clean data file of 2 hr fixes in all seasons/years

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
mem <- readRDS("../RSF_data/old_data/final_outputs/20220526_MEM_prep_v2.rds")
nov <- readRDS("cleaned_data/20220531_ph_nov2020.rds")

# filter indivs w/ >1000 used pts from MEM file
used <- mem[(mem$used_in + mem$used_out) > 1000,]
# grab ids
id <- unique(used$ID)

# Novemeber 2020 ----
# filter those from the cleaned data file
filt <- dat %>% 
  filter(ID %in% id,
         # month & year of problem points
         month(dt) %in% 11,
         year(dt) %in% 2020) 

# filter those from the cleaned data file
filt_dat <-  dat %>% 
  anti_join(filt)

# combine cleaned files together
cleaned_filt <- rbind(filt_dat, nov)
# check
# grab ids
id <- unique(used$ID)

# November 2020 ----
# filter those from the cleaned data file
filt <- cleaned_filt %>% 
  filter(ID %in% id,
         # month & year of problem points
         month(dt) %in% 11,
         year(dt) %in% 2020) 


# save file
saveRDS(cleaned_filt, "cleaned_data/20220620_cleaned-data.rds")

# DONE!

# May 31, 2022 -----
# Trying to understand why indivs have >1000 used pts in my RSF

# clean env
rm(list = ls())
gc()

# libraries
library(dplyr)
library(amt)
library(ggplot2)
library(lubridate)

# Import data to the db ---
dat <- readRDS("cleaned_data/comb_dat_20220524.rds")
mem <- readRDS("../RSF_data/old_data/final_outputs/20220526_MEM_prep_v2.rds")

# filter indivs w/ >1000 used pts from MEM file
used <- mem[(mem$used_in + mem$used_out) > 1000,]
# grab ids
id <- unique(used$ID)

# November 2020 ----
# filter those from the cleaned data file
filt <- dat %>% 
  filter(ID %in% id,
         # month & year of problem points
         month(dt) %in% 11,
         year(dt) %in% 2020) 


# make empty list
rate <- list()
id <- unique(filt$ID)
# 2. Loop
for(k in 1:length(id)){
  
  # print status
  print(paste(k))
  
  # Subset individual data
  d <- filter(filt, ID == id[k])
  
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

# reformat
check <- to_fix %>% 
  relocate(ID, .before = min)

# save this file
#write.csv(check, "../../Data/PH_data/checking_fixes/20220531_checked_nov2020.csv", row.names = FALSE)

# Track resample fun amt:

# BJS notes: The function doesn't resample anything. Given a desired fix rate and some tolerance around that, 
# it will group your existing data into "bursts" by creating a new column called burst_. 
# You can then pass the track to steps_by_burst() to create regular steps within a burst but 
# not between bursts.

# make empty list
newdat <- list()

# 2. Loop
for(i in 1:length(id)){
  
  # print status
  print(paste(i))
  
  # Subset individual data
  d <- filter(filt, ID == id[i])
  
  # Turn into a track_xyt
  newdat[[i]] <- mk_track(d, x, y, dt, crs = 32612) %>% 
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
  left_join(filt, by = c("ID", "dt", "x", "y")) 

# Save ---
saveRDS(pts, "cleaned_data/20220531_ph_nov2020.rds")

# remove those individuals from cleaned MEM file
mem <- readRDS("../RSF_data/final_outputs/20220526_MEM_prep_v2.rds")
# filter indivs w/ >1000 used pts from MEM file
new_mem <- mem[(mem$used_in + mem$used_out) < 1000,]

saveRDS(new_mem, "../RSF_data/final_outputs/20220531_MEM_prep.rds")

# DONE!!

# October 2020 ----
# check same individuals a month prior for comparison
# filter those from the cleaned data file
comp <- dat %>% 
  filter(ID %in% id,
         # month & year of problem points
         month(dt) %in% 10,
         year(dt) %in% 2020) 

# make empty list
comp_rate <- list()

# 2. Loop
for(k in 1:length(id)){
  
  # print status
  print(paste(k))
  
  # Subset individual data
  d <- filter(comp, ID == id[k])
  
  # Turn into a track_xyt
  comp_rate[[k]] <- mk_track(d, x, y, dt, crs = 32612,
                        all_cols = T) %>% 
    # sample fix rate per individual
    summarize_sampling_rate(time_unit = "hour") 
  
  
}

# grab names 
names(comp_rate) <- id
# combine files
to_comp <- do.call(rbind, lapply(comp_rate, data.frame))
to_comp$ID <- row.names(to_comp)

# reformat
coop_check <- to_comp %>% 
  relocate(ID, .before = min)

# save this file
write.csv(comp_check, "../../Data/PH_data/checking_fixes/20220531_checked_oct2020.csv", row.names = FALSE)

# Create plots of tracks for both Oct and Nov. 
plots <- list()

# for loop to create plots per indiv
for(i in 1:length(id)){
  # Store in list
  print(paste(i, id[i]))
  # store in list 
  plots[[i]] <- filt %>%
    filter(ID == id[i]) %>%
    # This pipes the subsetted data.frame to ggplot
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    theme_minimal()+
    ggtitle(id[i])
  # start export as pdf
  # pdf(paste(dir, "plot_2021_", i, ".pdf", sep = "")) 
  print(plots[[i]]) 
  #dev.off() # finish export
}

# get 2 hr fixes for these individuals ----




# DONE!