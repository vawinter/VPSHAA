# Step 1, combine 3rd- & 2nd-order maps
# Scale 3rd order habitat selection so that each 10-km pixel sums to 1
#   [i.e., Pr(3rd-order use | 2nd-order use)]
# Scale 2nd order habitat selection so that entire raster sums to 1
#   [i.e., Pr(2nd-order use)]
# Combine by multiplying
#   [i.e., Pr(3rd-order use | 2nd-order use) * Pr(2nd-order use)
#                                                         = Pr(3rd-order use)]
#
# Step 2, subset to pronghorn GPS locations
# Keep just those 10-km pixels with at least 1 pronghorn location
#
# Step 3, bin 30-m cells
#   Sort by combined eHSF, break into ~ 30 bins w/ equal area
#     (= equal # of raster cells)
#   Count GPS locations (30 m)/individual (10 km) in each bin
#   Caculate average eHSF in each bin
#
# Step 4, calculate Spearman's R


# Last edited 9/12/2023

rm(list = ls())
gc()

# Load packages ----
library(lubridate)
library(terra)
library(dplyr)
library(sf)
#library(ggpubr)

# Step 1: Load data ----
# Pronghorn GPS data
ph_dat <- readRDS("20230914_ph-status_2021.rds")


# Combinations of maps ----
combs <- expand.grid(sex = c("f", "m"),
                     stat = c("res", "mig"),
                     seas = c("winter", "spring", "summer", "fall"),
                     stringsAsFactors = FALSE)

# Get rid of winter migrants
combs <- combs[which(combs$stat != "mig" | combs$seas!= "winter"), ]

# add in numeric column for filtering
combs <- combs %>%
  mutate(s.num = case_when(seas == "winter" ~ 02,
                          seas == "spring" ~ 04,
                          seas == "summer" ~ 07,
                          seas == "fall" ~ 11),
         sex_filt = case_when(sex == "f" ~ "F",
                              sex == "m" ~ "M"))

# Create storage df's
spear.corr.df <- data.frame(season = NA,
                            status = NA,
                            sex = NA,
                            mean = NA,
                            SD = NA,
                            n = NA)
comb.spear.corr.df <- data.frame(season = NA,
                                 status = NA,
                                 sex = NA,
                                 rank = NA)

# Cluster
raster::beginCluster(15)
# Make map ----
for(c in 3:nrow(combs)){ # STARTING AT 3 BECAUSE WINTER

  # Print status
  cat("\nCombination", c, "of", nrow(combs), "\n")

# Step 1: Get season, sex, status data organized ----
  seas <- combs$seas[c]
  sex <- combs$sex[c]
  sex_filt <- combs$sex_filt[c]
  stat <- combs$stat[c]
  s.num <- combs$s.num[c]

# D3 data frame (Normalized data frame from 04_NormalizedMapping.R)
d3 <- readRDS(paste0("out/NULLBoyce_d3-",seas,"-", sex,"-", stat,".rds"))
# 3rd-order raster
r3 <- rast(paste0("out/NULL/",seas, "_", sex,"_",stat,"_3rd_norm.tif")) # use non-normalized maps
# 2nd-order raster
r2 <- rast(paste0("../Winter_etal_map/out/2nd-order_map/", seas, "_2nd-order_map.tif"))
# Combined order
comb <- rast(paste0("out/NULL/",seas, "_", sex,"_",stat,"_combined.tif"))

# Step 2 -- Pronghorn GPS ----
# Organize GPS data
ph <- ph_dat %>%
  # grab desired columns
  dplyr::select(ID,
                x,
                y,
                dt,
                tendency,
                sex) %>%
  # format the other columns
  mutate(season = month(dt),
         x = as.numeric(x),
         y = as.numeric(y),
         # create numeric IDs
         num_id = as.numeric(factor(ID))) %>%
  # filter out prediction year
  filter(year(dt) %in% "2021",
         # filter season
         season %in% s.num,
         # sex
        sex == sex_filt,
         # migratory status
          #tendency == stat
  ) %>%
  # Now, grab data that I want
  dplyr::select(num_id, x, y) %>%
  # set as data frame
  as.data.frame()

# Attach the 2nd-order cell numbers
ph$cell_2nd <- cellFromXY(r2, ph[, c("x", "y")])

# Attach the 3rd-order cell numbers
ph$cell_3rd <- cellFromXY(r3, ph[, c("x", "y")])

# 2nd-order cells to keep
keepers <- sort(unique(ph$cell_2nd))

# Subset the data.frame
sub <- d3 %>%
  filter(cell_2nd %in% keepers) %>%
  mutate(cell_3rd = factor(cell_3rd))

# How many GPS points in each 3rd-order cell?
gps <- ph %>%
  mutate(cell_3rd = factor(cell_3rd, levels = levels(sub$cell_3rd))) %>%
  group_by(cell_3rd, .drop = FALSE) %>%
  summarize(n_gps = n())

# How many individuals in each 2nd-order cell?
ind <- ph %>%
  group_by(cell_2nd) %>%
  summarize(n_ind = n_distinct(num_id),
            # how many gps point?
            n_gps = n()) %>%
  # remove cell w. less than 30 locs
  filter(n_gps > 30) %>%
  # remove new col
  dplyr::select(-n_gps)

# redefine 2nd-order cells to keep based on cells w. greater than 30 locs
keepers <- sort(unique(ind$cell_2nd))

# Step 3 -- Bin ----
# Number of bins
nbins <- 5

# Step 4 -- Correlation ----
# ... just third order ----
second <- unique(ind$cell_2nd)
spear.corr <- vector()
# spear.corr.df <- data.frame()
# loop
for(s in 1:length(second)){
  sub2 <- sub %>%
    filter(cell_2nd == second[s])
  # figure out how many third order in second?
  # define here
  num <- sub2 %>%
    group_by(cell_2nd) %>%
    summarize(n = n())

  # Times to repeat
  times <- floor(nrow(sub2)/nbins)
  # Group labels
  groups <- rep(1:nbins, each = times)
  # How many are missing?
  (missing <- nrow(sub2) - length(groups))
  # Pad those cells with NA (they will get left out)
  g <- c(groups, rep(NA, missing))

  # Create dataset
  dat2 <- sub2 %>%
    # Join number of individuals
    left_join(ind, by = "cell_2nd") %>%
    # Join number of GPS points
    left_join(gps, by = "cell_3rd") %>%
    # Calculate gps/ind
    mutate(gps_per_ind = n_gps/n_ind) %>%
    # # group by second order cells
    # Arrange
    arrange(desc(pr3_2), by_group = TRUE) %>%
    # Assign sorted data to groups
    mutate(group = g) %>%
    group_by(group) %>%
    # Get rid of NAs for group
    filter(!is.na(group),
           !is.na(log.eHSF)) %>%
    # Summarize by group
    summarize(mean_eHSF = mean(exp(pr3_2), na.rm = TRUE),
              mean_gps_per_ind = mean(gps_per_ind, na.rm = TRUE),
              second = mean(exp(pr2), na.rm = TRUE))

  # Spearman's R
  # store here
  spear.corr[s] <- cor(dat2$mean_eHSF, dat2$mean_gps_per_ind, method = "spearman")
 # spear.corr.df <- rbind(dat2, spear.corr.df)
}# end loop

# remove NAs
spear.corr <- na.omit(spear.corr)
len <- length(spear.corr)
m <- mean(spear.corr)
SD <- sd(spear.corr)

# Store in temporary df
temp1 <- data.frame(season = seas,
                    status = stat,
                    sex = sex,
                    mean = m,
                    SD = SD,
                    n = len,
                    row.names = NULL)

# Done w. third order, now combined....

# ... combined ----
nbins <- 5 * length(keepers) # for proportional scaling
# Times to repeat
times <- floor(nrow(sub)/nbins)

# Group labels
groups <- rep(1:nbins, each = times)

# How many are missing?
(missing <- nrow(sub) - length(groups))

# Pad those cells with NA (they will get left out)
g <- c(groups, rep(NA, missing))

# Check
identical(length(g), nrow(sub))

# Create dataset
dat <- sub %>%
  # Join number of individuals
  left_join(ind, by = "cell_2nd") %>%
  # Join number of GPS points
  left_join(gps, by = "cell_3rd") %>%
  # mutate(gps_2nd = sum(n_gps)) %>% ## return here 9/22
  # Calculate gps/ind
  mutate(gps_per_ind = n_gps/n_ind) %>%
  # Arrange
  arrange(desc(log.eHSF)) %>%
  # Assign sorted data to groups
  mutate(group = g) %>%
  group_by(group) %>%
  # Get rid of NAs for group
  filter(!is.na(group),
         !is.na(log.eHSF)) %>%
  # Summarize by group
  summarize(mean_eHSF = mean(exp(log.eHSF), na.rm = TRUE),
            mean_gps_per_ind = mean(gps_per_ind, na.rm = TRUE),
            n = n())


# Spearman's R
dat <- na.omit(dat)
R <- cor(dat$mean_eHSF, dat$mean_gps_per_ind, method = "spearman")

# load into temporary df
temp2 <- data.frame(rank = R,
                     season = seas,
                     status = stat,
                       sex = sex,
                    row.names = NULL)

# bind df's together
spear.corr.df <-  rbind(temp1, spear.corr.df)
comb.spear.corr.df <- rbind(temp2, comb.spear.corr.df)

# end
}

endCluster()

# garbage clean-up
gc()

#spear.corr.df <- spear.corr.df[-3,]
# combine together
val <- spear.corr.df %>%
  left_join(comb.spear.corr.df, by = c("sex", "status", "season")) %>%
  relocate(c("sex", "status", "season", "rank"), .before = mean) %>%
  relocate("n", .after = SD) %>%
  arrange(season = desc(season),
          status,
          sex)
val <- na.omit(val)# remove last row of NAs


# Save
write.csv(val, "../../PubFigs/nullValidation_table3.csv", row.names = F)
# DONE!

# # Another way to get that:
# m <- lm(rank(gps_per_ind) ~ rank(log.eHSF), data = dat)
# # Correlation coefficient
# sqrt(summary(m)$r.squared) * sign(coef(m)[[2]])
#
# # Plot
# c <- ggplot(dat, aes(x = rank(log.eHSF), y = rank(log_gps_per_ind))) +
#   geom_line() +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   ggtitle("Combind order") +
#   xlab("eHSF Rank") +
#   ylab("GPS/individual Rank") +
#   theme_bw()
#
#
# # Plot
# r <- ggplot(dat2, aes(x = rank(log.eHSF), y = rank(log_gps_per_ind))) +
#   geom_line() +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   ggtitle("2nd order") +
#   xlab("eHSF Rank") +
#   ylab("GPS/individual Rank") +
#   theme_bw()
#
# # Plot together
# plot_grid(r, t, c)

