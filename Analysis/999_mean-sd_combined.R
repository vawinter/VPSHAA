# Format means and sd files
library(dplyr)

# file pre mem
x <- read.csv("Data/Processed/Mean_sd/20221018_mean-sd-preRSF.csv", header = T)

# file pre rsf
y <- read.csv("Data/Processed/Mean_sd/20220718_mean-sd_snow-road.csv", header = T)

# join together
all <- merge(x, y)

# save 
write.csv(all, "Data/Processed/Mean_sd/20221019_mean-sd_all.csv", row.names = F)
write.csv(all, "../Winter_etal_map/20221019_mean-sd_all.csv", row.names = F)
