#####################################################X
#---------Database creation and management ----------X
#-------- Script for est and linking sql db ---------X
#-------------------February 2021--------------------X
#--------------------  VAW   ------------------------X
#####################################################X 
#-------------- Updated: 10/08/2022 -----------------X
#####################################################X

#clean your R environment up
rm(list = ls())
gc()

# Load packages ---
library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)

# Source function
source("/Users/veron/Documents/Projects/Analysis/scripts/Pronghorn/prep_fun.R")

# Import data to the db ---
dat <- readRDS("Data/Processed/comb_dat_20220524.rds")

# Pronghorn table prep ----
# Filter desired fields for ph table
ph <- dat %>%
  dplyr::select(ID,
         CollarID,
         mortality = Mortality,
         sex = Sex,
         age_class = CurrentCohort,
         age = CurrentAge,
         b_year = BirthYear,
         unit = CaptureUnit) 

# Check and remove duplicates
unique(ph$ID)
ph2 <- ph[!duplicated(ph$ID),]

anyDuplicated(ph2$ID)

# Save output ----
write.csv(ph, "Data/PH_data/db_data/20220524_ph_table.csv", row.names = FALSE)

## Tracking table prep ----
# Format tracking info 
track <- dat %>% 
  dplyr::select(ID,
         x,
         y,
         dop,
         obj = OBJECTID,
         NumSats,
         dim = Dimension,
         dt) %>% 
  # Change dt column to character
  mutate(dt = as.character(dt))

# Check data
str(track)
t2 <- sort(dat$dt)
tail(t2)
head(t2)

# Save ouptut
write.csv(track, "Data/PH_data/db_data/20220525_tracking.csv", row.names = FALSE)

# Status --
dir <- "Data/mig.tend/"
dm <- list.files(dir, pattern = ".csv", full.names = T) 
status <- do.call(rbind, lapply(dm, read.csv))

# save
write.csv(status, "Data/PH_data/db_data/20220526_status.csv", row.names = FALSE)
# Capture ---
capture <- dat %>%
  # select animal info data
  select(ID,
         capture_unit = CaptureUnit,
         capture_subunit = CaptureSubunit,
         cap_long = CaptureLongitude,
         cap_lat = CaptureLatitude,
         cap_date = BeginDate)

# Save output
write.csv(capture, "Data/PH_data/db_data/20220525_capture.csv", row.names = FALSE)

#### Database structuring/restructuring ----
#clean your R environment up
rm(list = ls())
gc()

# Load packages ---
library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)

# read in cleaned csv files
ph <- readRDS("Data/PH_data/db_data/20220524_ph_table.rds") 
track <- readRDS("Data/PH_data/db_data/20220525_tracking.rds")
status <- read.csv("Data/PH_data/db_data/20220526_status.csv", header = T)
capture <- read.csv("Data/PH_data/db_data/20220525_capture.csv", header = T)

str(ph)
str(track)
class(ph)
ph <- data.frame(ph)
track <- data.frame(track)
capture <- data.frame(capture)

# Est. database connection ---
ph_db <- dbConnect(drv = RSQLite::SQLite(), # wherever I saved db
                   "/Users/veron/Box/Projects/Data/PH_data/pronghorn.db")

class(ph_db)

# Table 1: Pronghorn ----

# 1. Drop the current table
dbExecute(ph_db, "DROP table pronghorn;")

# 2. Recreate the table
dbExecute(ph_db, "CREATE TABLE pronghorn (
             ID varchar(10),
             CollarID varchar(10),
             mortality char(6),
             sex char(1),
             age_class varchar(8),
             age varchar(5),
             b_year varchar(5),
             unit varchar(50),
             PRIMARY KEY (ID)
);")

# 3. Input data to table
dbWriteTable(ph_db, "pronghorn", ph, append = TRUE)

# 4. Check that it worked: query the db  
dbGetQuery(conn = ph_db, statement = "SELECT* FROM pronghorn LIMIT 10;")

# Table 2: Tracking ----

# 1. Drop the table
dbExecute(ph_db, "DROP table tracking;")

# 2. Recreate the table
dbExecute(ph_db, "CREATE TABLE tracking (
  ID varchar(10),
          x double,
          y double,
          dop char(3),
          obj varchar(8),
          NumSats char(2),
          dim char(1),
          dt text,
          FOREIGN KEY(ID) REFERENCES pronghorn(ID)
);")


# 3. Input data to table
dbWriteTable(ph_db, "tracking", track, append = TRUE)

# 4. Query the db 
dbGetQuery(conn = ph_db, statement = "SELECT* FROM tracking LIMIT 10;")


# Table 3: Status ----  

# 1.Drop the table
#Drop table
dbExecute(ph_db, "DROP table status;")

# 2.Recreate the table
dbExecute(ph_db, "CREATE TABLE status (
ID varchar(10),
mig_season varcahr (2),
tendency varchar(8),
year text,
FOREIGN KEY(ID) REFERENCES pronghorn(ID)
);")

# 3. Input data to table 
dbWriteTable(ph_db, "status", status, append = TRUE)

# 4. Query the db
dbGetQuery(conn = ph_db, statement = "SELECT* FROM status LIMIT 10;")

# Table 4: Capture ----  
#Drop table
dbExecute(ph_db, "DROP table capture;")

# Create the table
dbExecute(ph_db, "CREATE TABLE capture (
          cap_event INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
          ID varchar(10),
          capture_unit varchar(10),
          capture_subunit varchar (10),
          cap_long double,
          cap_lat double,
          cap_date text,
          FOREIGN KEY(ID) REFERENCES pronghorn(ID)
     );")

# Fill in data          
dbWriteTable(ph_db, "capture", capture, append = TRUE)

# 4. Query the db
dbGetQuery(conn = ph_db, statement = "SELECT* FROM capture LIMIT 10;")


# DONE!!


# Trigger functions ----

# dbExecute(ph_db, 
#           "ALTER TABLE Individuals ADD COLUMN update_timestampe varchar;")
# 
# dbExecute(ph_db, "CREATE TABLE add_update_timestamp
#           AFTER INSERT
#           ON Individuals
#           FOR EACH ROW
#           BEGIN
#           UPDATE Individuals
#           SET update_timestamp = datetime('now')
#           WHERE ph_id = new.ph_ID;
#           END;")



