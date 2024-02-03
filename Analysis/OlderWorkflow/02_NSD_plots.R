#####################################################X
#---------Pronghorn MS.c.chapter 1 analysis ---------X
#----------------NSD plots using gps data------------X
#--------------------March 2021----------------------X
#-----------------------VAW--------------------------X
#####################################################X
#########---- Last Edited : 10/08/2022 -----#########X
#####################################################X

# clean up my R environment
rm(list = ls()) 
gc()

# Load in packages ----
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(plyr)

# Source function
source("../../Analysis/scripts/Pronghorn/prep_fun.R")

#Set options----
options(stringsAsFactors = FALSE)
Sys.setenv(TZ = "UTC")
options(scipen = 0)

# 1. Load in data ----
ph <- readRDS("Data/Processed/20220526_ph_daily_points.rds")
class(ph)
str(ph)
#check <- readRDS("../../Data/to_check.rds")
# 2. filter out year ---
# want range to be between 02/01/year - 02/01/ next year
dat <- ph %>%
 # filter(ID %in% check) %>% 
  filter(dt >= "2018-01-01" & dt <= "2018-12-31") %>% 
  mutate(ddply(data, .(ID), summarise, days = length(dt)))

total <- ddply(dat, .(ID), summarise, days = length(dt)) %>% 
  filter(days >= 90)

data <- dat %>% 
  filter(ID %in% total$ID)

test <- ddply(data, .(ID), summarise, days = length(dt))

str(data)

# 3. Calculate NSD  ----
# create columns
data$Xdist <- NA
data$Ydist <- NA
data$NSD <- NA

# this needs to be point on January 1st
# need this for 1st row for each indiv - here is creating for first indiv in data
data$Xdist[1] <- data$x[2] - data$x[1]
data$Ydist[1] <- data$y[2] - data$y[1]
data$NSD[1]   <- as.integer(sqrt(data$Xdist[1]^2+data$Ydist[1]^2))

# # want this grouped by indiv
# individuals <- data.frame(ID = rep(unique(data$ID), 2),
#                           mig_season = rep(c("S", "W")),
#                           tendency = NA,
#                           year = rep("2020"))

individuals <- unique(data$ID)

# empty data frame for storage
data2 <- data.frame()
dir <- ("DataVis/nice_plots")
dir.create(dir)

# nested for loop to loop NSD calculation over entire data set
for(i in 1:length(individuals)){
  # print status
  print(paste(i, individuals[i]))
  # temporary file where I can filter based on indiv to get indiv calculations
  temp <- data %>% 
    filter(ID == individuals[i])
  # for loop to loop calculations over temp data frame
  if(nrow(temp) > 2){
    for(j in 2:(nrow(temp) -1)){
      # first calculations for each indiv 
      temp$Xdist[1] <- temp$x[2] - temp$x[1]
      temp$Ydist[1] <- temp$y[2] - temp$y[1]
      temp$NSD[1]   <- as.integer(temp$Xdist[1]^2+temp$Ydist[1]^2)
      # subsequent calculations for rest of the data per indiv
    temp$Xdist[j] <- temp$x[j+1] - temp$x[j] + temp$Xdist[j-1]
    temp$Ydist[j] <- temp$y[j+1] - temp$y[j] + temp$Ydist[j-1]
    temp$NSD[j] <- as.numeric(temp$Xdist[j]^2 + temp$Ydist[j]^2)
    }
  }else{
    next
  }
  # combine the two data frames
    data2 <- rbind(data2, temp)
}

# 4. plot distance v date for every indiv----

#  store the plots as elements of a list to be able
plots <- list()
# log y
data2$log_NSD <- log(data2$NSD)
# get range
max(table(data2$log_NSD))

#range <- as.POSIXct(c("2018-01-01", "2018-12-31"))

# for loop to create plots per indiv
for(i in 1:length(individuals)){
  # Store in list
  print(paste(i, individuals[i]))
  # store in list based on indivs w/ data frame created in last loop
  plots[[i]] <- data2 %>%
    filter(ID == individuals[i]) %>%
    # This pipes the subsetted data.frame to ggplot
     ggplot(aes(x = dt, y = NSD)) +
           geom_point() +
         labs(y = "NSD",
         x = "",
         title = "") +
    scale_y_log10(limits = c(1,3e8)) +
     #ylim(c(0, 25)) +
    expand_limits(x = as.POSIXct(c("2021-01-01", "2021-12-31"))) +
        #ggtitle(individuals[i]) +
    theme_light() 
  # start export as pdf
  tiff(paste(dir, individuals[i], ".tiff", sep = "")) 
  print(plots[[i]]) 
  dev.off() # finish export
}

# # plots whatever plot is specified
# plot(plots[[3]])
# 
# # PLotting for thesis ----
  #indiv <- c("PR17M0003", "PR17M0011", "PR18M0006")
  indiv2 <- c("PR17M0017", "PR17M0026",  "PR17M0014")
#"PR17F0017",
# 
plot1 <- data2 %>%
  filter(ID == indiv2[2]) %>%
  # This pipes the subsetted data.frame to ggplot
  ggplot(aes(x = dt, y = NSD)) +
  geom_point() +
  geom_line() +
  labs(y = "NSD",
       x = "Date",
       title = "D") +
  scale_y_log10(limits = c(1,3e8)) +
  expand_limits(x = as.POSIXct(c("2018-01-01", "2018-12-31"))) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot2 <- data2 %>%
  filter(ID == indiv2[3]) %>%
  # This pipes the subsetted data.frame to ggplot
  ggplot(aes(x = dt, y = NSD)) +
  geom_point() +
  geom_line() +
  labs(y = "NSD",
       x = "Date",
       title = "E") +
  scale_y_log10(limits = c(1,3e8)) +
  expand_limits(x = as.POSIXct(c("2018-01-01", "2018-12-31"))) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

  plot3 <- data2 %>%
    filter(ID == indiv2[1]) %>%
    # This pipes the subsetted data.frame to ggplot
    ggplot(aes(x = dt, y = NSD)) +
    geom_point() +
    geom_line() +
    labs(y = "NSD",
         x = "Date",
         title = "F") +
    scale_y_log10(limits = c(1,4e8)) +
    expand_limits(x = as.POSIXct(c("2018-01-01", "2018-12-31"))) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # start export as pdf
  #tiff(paste(dir, "plot_ph_", i, ".tiff", sep = ""))

plot_all <- gridExtra::grid.arrange(plot1, plot2, plot3,widths = c(8,8,8),
                                    nrow = 1)

# plots whatever plot is specified
plot(plots[[3]])

outdir <- "DataVis"
plots <- paste0(outdir, "Chapter1/nice_plots")
if(!dir.exists(plots)){dir.create(plots)}

ggsave(plot = plot_all,
       filename = "NSD_plots_0706-2.tiff",
       path = plots,
       width = 9, height = 7,
       # you can change to png or tif or whatever you need
       device = "tiff")
# # Done!
# # Done!