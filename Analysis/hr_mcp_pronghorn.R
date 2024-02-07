

# loop to create 95% MCP for individuals in each season/year
library(dplyr)
library(amt)
library(lubridate)
library(DBI)
library(knitr)
library(kableExtra)

#### Appendix B table: Average pronghorn home range per season/year/status
# pronghorn data
p <- readRDS("Data/Processed/20220813_cleaned-data.rds")

prong <- p %>%                     
  filter(month(dt) %in% c(02, 04, 07, 11)) %>% 
  filter(!year(dt) %in% 2017)


# 2. Prep data with 'amt' ----
ph_track <- mk_track(tbl = prong, .x = x, .y = y, .t = dt, ID = ID, #uid=uid is where you can rename for ex. id
                        crs = 32612) #code for UTM zone 12 WGS84

# Estimate 95% isopleth
ph_ID <- unique(ph_track$ID)
months <- c(02, 04, 07, 11)  # Assuming these are numeric months
years <- 2018:2021
# Create lists to store results
ph_sub <- list()
mcp <- list()

# Create an empty data frame to store results
results_df <- NULL

# Loop over pronghorn IDs
for (i in 1:length(ph_ID)) {
  # Print status
  print(paste("Processing pronghorn ID:", ph_ID[i]))
  
  # Subset to individual
  ph_sub[[i]] <- ph_track %>% 
    filter(ID == ph_ID[i])
  
  for (y in 1:length(years)) {
    # Subset data for the specific year
    ph_sub_y <- ph_sub[[i]] %>%
      filter(year(t_) == years[y])
    
    # Check if subset is not empty
    if (nrow(ph_sub_y) > 0) {
      for (m in 1:length(months)) {
        # Subset data for the specific month
        ph_sub_month <- ph_sub_y %>%
          filter(month(t_) %in% months[m])
        
        # Check if subset is not empty
        if (nrow(ph_sub_month) > 0) {
          # Fit MCP for the specific month
          mcp_result <- hr_mcp(x = ph_sub_month, 
                               levels = c(0.95)) 
                               #keep.data = TRUE)
          
          ## create single individual i, in month m, in year y
          tmp.df <- data.frame(
            month = months[m],
            year = years[y],
            ID = ph_ID[i],
            mcp_95_meters = as.vector(mcp_result$mcp$area),
            mcp_95_km = (as.vector(mcp_result$mcp$area)/1000)
          )
          
          ## save output
          results_df <- rbind(
            results_df,
            tmp.df
          )
          
       } else {
          # Print a message if the subset is empty
          print(paste("Skipping Month", months[m], "for ID:", ph_ID[i], "and Year:", years[y]))
        }
      }
    } else {
      # Print a message if the subset is empty
      print(paste("Skipping Year", years[y], "for ID:", ph_ID[i]))
    }
  }
}


#a. Query the db 
ph_db <- dbConnect(drv = RSQLite::SQLite(), # wherever I saved db
                   "Data/Processed/pronghorn.db")

#b. Pull tables
prong <- dbGetQuery(ph_db, "SELECT * FROM pronghorn;")
status <- dbGetQuery(ph_db, "SELECT * FROM status;") 

# intersect the IDs to limit to only indiv in fin
combo <-  intersect(results_df$ID, prong$ID)


# edit ph table
ph <-  prong  %>% 
  # filter IDs
  filter(ID %in% combo) %>% 
  # Join w/ status table
  left_join(status, by = "ID") %>% 
  mutate(year = as.integer(year))

# # Create a 'summer' and 'winter' movement class
# fin <- results_df %>%
#   mutate(
#     mig_season = case_when(
#       month %in% c(04, 07) ~ "S",
#       month %in% c(02, 11) ~ "W",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   # join data frame with behvaiors
#   left_join(ph, by = c("ID", "year", "mig_season")) %>% 
#   mutate(movement = case_when(
#     tendency == "mig" ~ 'Range-shifter',
#     tendency == "res" ~ 'Resident',
#     tendency == 'unk' ~ 'Nomad',
#     TRUE ~ 'Range-shifter'
#      )
#     )%>% 
#   group_by(month, year, movement) %>%
#   reframe(median_95_km = round(median(mcp_95_km[mcp_95_km > 0]), digits = 2),
#             range_95_km = range(mcp_95_km[mcp_95_km > 0])) %>%
#   filter(!year == 2021,
#          !is.na(median_95_km)) %>% 
#   rename('Year' = year, 'Season' = month, 'Movement Status' = movement, 
#          'Median 95% MCP' = median_95_km) %>% 
#   relocate('Year', .before = 'Season') 


fin <- results_df %>%
  mutate(
    mig_season = case_when(
      month %in% c(04, 07) ~ "S",
      month %in% c(02, 11) ~ "W",
      TRUE ~ NA_character_
    )
  ) %>%
  # join data frame with behaviors
  left_join(ph, by = c("ID", "year", "mig_season")) %>% 
  mutate(movement = case_when(
    tendency == "mig" ~ 'Range-shifter',
    tendency == "res" ~ 'Resident',
    tendency == 'unk' ~ 'Nomad',
    TRUE ~ 'Range-shifter'
  ))%>% 
  group_by(month, year, movement) %>%
  reframe(
    median_95_km = round(ifelse(length(mcp_95_km[mcp_95_km > 0]) > 0, median(mcp_95_km[mcp_95_km > 0]), NA), digits = 2),
    range_95_km = round(ifelse(length(mcp_95_km[mcp_95_km > 0]) > 0, range(mcp_95_km[mcp_95_km > 0]), NA), digits = 2),
    count_n = n(),
  ) %>%
  filter(
    year != 2021,
    !is.na(median_95_km)
  ) %>% 
  rename(
    'Year' = year,
    'Season' = month,
    'Movement Status' = movement,
    'Median 95% MCP' = median_95_km,
    'Minimum 95% MCP' = range_95_km,
    'n' = count_n
  ) %>% 
  relocate('Year', .before = 'Season') %>% 
  relocate('n', .before = 'Median 95% MCP') %>% 
  relocate('Minimum 95% MCP', .before = 'Median 95% MCP')


# order year in ascending order
final_table <- fin[order(fin$Year), ]

# Create the table
hr <- kable(final_table, 
                      booktabs = T,
                      escape = F,
                      caption = "Median and minimum 95% MCP for each season and year per movement class.",
                      format = "latex",
                      align = c("lrccccccccc")) %>% 
  collapse_rows(columns = c(1,2), latex_hline = "major",valign = "middle") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(hr)

#### Appendix B table: GPS locations outside availability domain
# Load in data
dat <- read.csv( "Data/Outputs/RSF_outputs/20221018-10.6.csv",
                 header = T, na.strings = c("", "N/A", "NA"))

# Summarize the data for each threshold
summary_20 <- dat %>%
  filter((used_in + used_out) < 70) %>%
  group_by(month, year, is.mig, is.res, is.unk_mig) %>%
  summarize(count_20 = n(), .groups = "drop")

summary_10 <- dat %>%
  filter((used_in + used_out) < 35) %>%
  group_by(month, year, is.mig, is.res, is.unk_mig) %>%
  summarize(count_10 = n(), .groups = "drop")

summary_5 <- dat %>%
  filter((used_in + used_out) < 18) %>%
  group_by(month, year, is.mig, is.res, is.unk_mig) %>%
  summarize(count_5 = n(), .groups = "drop")

# Perform a full join to ensure all month/year combinations are present
combined_summary <- full_join(summary_20, summary_10, 
                              by = c("month", "year", "is.mig", "is.res", "is.unk_mig"))
combined_summary <- full_join(combined_summary, summary_5, 
                              by = c("month", "year", "is.mig", "is.res", "is.unk_mig"))

# Replace NA with 0 for counts
combined_summary[is.na(combined_summary)] <- 0

# Create a Season/Year column and a Migratory Status column
combined_all <- combined_summary %>%
  mutate(Migratory_Status = case_when(is.mig == 1 ~ "Range-shifter", 
                                      is.res == 1 ~"Resident",
                                      is.unk_mig == 1 ~ "Nomad",
                                      TRUE ~ "Nomad"),
         Season = case_when(month == '2' ~ "Winter",
                            month == '4' ~ "Spring",
                            month == '7' ~ "Summer",
                            month == '11' ~ "Fall")) %>% 
  dplyr::select(-c(is.mig, is.res, is.unk_mig)) %>% 
  distinct()

# Select and rename the columns for the final table
final_table <- combined_all %>%
  dplyr::select(Season, year, Migratory_Status, count_20, count_10, count_5) %>%
  rename('Year' = year, 'Movement Status' = Migratory_Status, '20%' = count_20, '10%' = count_10, '5%' = count_5) %>% 
  relocate('Year', .before = 'Season')

# order year in ascending order
final_table <- final_table[order(final_table$Year), ]

# Create the table
extentpoints <- kable(final_table, 
                      booktabs = T,
                      escape = F,
                      caption = "Number of individuals in each season and year with 20%, 10%, and 5% GPS location 
      outside the 10x10km availability domain in each season.",
                      format = "latex",
                      align = c("lrccccccccc")) %>% 
  collapse_rows(columns = 1, latex_hline = "major",valign = "middle") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(extentpoints)
