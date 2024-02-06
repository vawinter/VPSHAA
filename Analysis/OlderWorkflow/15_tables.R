
# Formatting tables for overleaf

# Libraries
library(dplyr)
library(lubridate)
library(DBI)
library(knitr)
library(kableExtra)

# Table 1: references for covaraites ----
table1 <- data.frame("Covariates" = c("Elevation", "Roughness","Aspect (Easting)", "Aspect (Northing)",
                                      "Herbaceous cover", "Shrub cover", "Tree cover"),
                     "Temporally Static/Dynamic" = c("static", "static","static","static", "dynamic",
                                                     "dynamic", "dynamic"),
                     "Expected effect" = c("avoidance","avoidance","avoidance", "selection",
                                           "selection","selection", "avoidance"),
                     "References" = c("(cite{beale_forage_1970, ogara_pronghorn_2004, zeller_forecasting_2021})",
                                      "(cite{beale_forage_1970, ogara_pronghorn_2004})",
                                      "(cite{guisan_glm_1999, maggini_stratified_2002, hirzel_habitat_2008})", 
                                      "(cite{guisan_glm_1999, maggini_stratified_2002, hirzel_habitat_2008})",
                                      "(cite{aikens_greenscape_2017, aikens_drought_2020})",
                                      "(cite{beale_forage_1970, ogara_pronghorn_2004, berger_last_2004})",
                                      "(cite{ogara_pronghorn_2004, larsen_does_2011})"),
                     check.names = FALSE) %>% 
  knitr::kable(booktabs = T,
               escape = F,
               caption = "Fine-scale habitat covaraites thought to be important 
               to pronghorn ecology and hence used as eHSF predictors (steps 4).
               All Covariates were measured at 30m resolution.",
               format = "html",
               align = c("lrccccccccc")) %>% 
  column_spec(2,bold=T,latex_column_spec = ">{\\\\color{black}}c") %>% 
  collapse_rows(columns = 2, latex_hline = "major",valign = "middle") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(table1)


# Table 2: references for other supporting variables ----
table2 <- data.frame("Variables" = c("Mean Elevation", "Mean Roughness","Mean Easting", "Mean Northing",
                                     "Mean Road Density",
                                      "Mean Herbaceous Cover", "Mean Shrub Cover", "Mean Tree Cover",
                                      "Mean Snow Depth", "Mean Herbaceous Biomass",
                                      "Mean Palmer Drought Severity Index"),
                     "Temporally Static/Dynamic" = c(rep("static", 5), rep("dynamic", 6)),
                   #  "References" = NA,
                     
                     # c("(Beale and Smith 1970, O’Gara et al. 2004, Zeller et al. 2021)",
                     #                "(Beale and Smith 1970, O’Gara et al. 2004)",
                     #                rep("(Guisan et al. 1999, Maggini et al. 2002, Hirzel and Le Lay 2008)",2),
                     #                "(Aikens et al. 2017, 2020b)",
                     #                "(Beale and Smith 1970a, O’Gara et al. 2004, Berger 2004)",
                     #                "(O’Gara et al. 2004, Larsen et al. 2011)"),
                     check.names = FALSE) %>% 
  knitr::kable(booktabs = T,
               escape = F,
               caption = "Coarse-scale habitat covariates expected to influence 
               the selection for the seven fine-scale covariates listed in 'Environmental
               Covaraites'",
               format = "html",
               align = c("lrccccccccc")) %>% 
  column_spec(2,bold=T,latex_column_spec = ">{\\\\color{black}}c") %>% 
  collapse_rows(columns = 2, latex_hline = "major",valign = "middle") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(table2)

# Table 3: Migration summaries ----
## Load in data 

mig <- read.csv("Data/Processed/mig-comb_20220628.csv", header = T)

table(mig$year, mig$tendency, mig$mig_season)

mig_sum <- mig %>% 
  filter(mig_season == "S",
         year)

mig_tab <- data.frame("Year" = c("2018", "2018", "2019", "2019", "2020","2020",
                                 "2021", "2021"),
                      "Migration Period" = c("Spring", "Fall", "Spring", "Fall",
                                             "Spring", "Fall", "Spring", "Fall"),
                      "Range-shifter" = c("35", "29", "25", "52", 
                                          "43", "28", "75", "80"),
                      "Resident" = c("20", "36", "23", "19", "18", "19", "8", "6"),
                      "Nomadic" = c("45", "35", "52", "29", "39", "53", "17", "14"),
                      check.names = FALSE) %>% 
  knitr::kable(booktabs = T,
               escape = F,
               caption = "Observed parentage of bi-annual pronghorn movement 
               in Utah between 2018-2021, grouped by migratory status 
               (range-shifting, resident, and nomadic). Percentages were 
               calculated based out of 100 on total collared population for 
               that season/year.",
               format = "latex",
               align = c("lrccccccccc")) %>% 
 # column_spec(2,bold=T,latex_column_spec = ">{\\\\color{black}}c") %>% 
  collapse_rows(columns = 1, latex_hline = "major",valign = "middle") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(mig_tab)

# table 4 goodness of fit ----
table4 <- data.frame("Covariates" = c("Elevation", "Roughness", "Aspect (Easting)", "Aspect (Northing)",
                                      "Herbaceous cover", "Shrub cover", 
                                      "Tree cover"),
                     # Full mod
                     "R² (marginal)" = c("57", "43", "12", "33", "21", "50", "23"),
                     "R² (conditional)" = c("99", "99", "99", "99", "99", "99", "99"),
                     # Seas mod
                     "R² (marginal)" = c("02", "20", "02", "20", "01", "12", "06"),
                     "R² (conditional)" = c("99", "99", "99", "99", "99", "99", "99"),
                     # Null mod
                     "R² (marginal)" = c("0", "0", "0", "0", "0", "0", "0"),
                     "R² (conditional)" = c("99", "99", "99", "99", "99", "99", "99"),
                     check.names = FALSE) %>% 
  knitr::kable(booktabs = T,
               escape = F,
               caption = "Goodness of fit: Marginal and conditional R² 
               percentage values for each Full, Null, and Season covariate model output.",
               format = "html",
               align = c("lrccccccccc")) %>% 
  #column_spec(2,bold=T,latex_column_spec = ">{\\\\color{black}}c") %>% 
 # collapse_rows(columns = 2, latex_hline = "major",valign = "middle") %>%
 # add_header_above(c(" ", "marginal" = 1, "conditional" = 3), align = "r") %>% 
  add_header_above(c(" ", "Full model" = 2, "Season Model" = 2, "Null Model" = 2)) %>% 
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(table4)


# table 5 -----
table5 <- data.frame("Selection Coefficients" = 
                       c("Elevation", "Roughness","Aspect (Easting)", "Aspect (Northing)",
                                         "Herbaceous cover", "Shrub cover", "Tree cover"),
                     # Full
                     "Full model" = c("0.088", "0.254", "-0.0365", "0.336","0.040", "0.156","0.223"),
                     # Season
                     "Season model" = c("-0.094", "-0.015","0.056", "0.360", "-0.001", "0.353","0.0276"),
                     # Null
                     "Null model" = c("-8.20E-17", "1.16E-16","-2.96E-16", "3.99E-16", "-9.02E-17", "-4.40E-16","-1.03E-16"),
                     # Full and season
                     "(Full vs Season)" = c("0.091", "0.137", "-0.046", "-0.022", "0.30", "-0.10", "-0.030"),
                     "(Full vs Null)" = c("0.045", "0.130", "-0.018", "0.158", "0.030", "0.077", "0.108"),
                     check.names = FALSE) %>% 
  knitr::kable(booktabs = T,
               escape = F,
               # caption = "Out of sample validation: Pearson's weighted coorelation estimations
               # applied to out-of-sample data for estimating predictive capacity for
               # full vs season and full vs null models, and the goodness-of-prediction metric.
               # Positive values indicate the predictive performance
               # of the full covariate model was better than the null covariate
               # model. Negative values indicate that the null covariate model
               # had better predictive performance than the full covariate model.",
               format = "html",
               align = c("lrccccccccc")) %>% 
 # kable_styling(font_size = 30, html_font = "Arial Nova") %>% 
  add_header_above(c("Obsereved vs Predicted" = 1, " ", " "," ", "Goodness-of-prediction" = 1, "Goodness-of-prediction" = 1), 
                   line = F) %>% 
  #column_spec(2,bold=T,latex_column_spec = ">{\\\\color{black}}c") %>% 
  # collapse_rows(columns = 2, latex_hline = "major",valign = "middle") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(table5)


# table 6 -----
table6 <- data.frame("Season" = c("Winter", "Winter", "Spring", "Spring", "Spring", 
                                  "Spring", "Summer", "Summer", "Summer", "Summer",
                                  "Fall",   "Fall",   "Fall",   "Fall"),
                     # status
                     "Status" = c("Resident", "Resident", "Resident", "Resident", "Mover",    
                                  "Mover",  "Resident", "Resident", "Mover","Mover",    
                                  "Resident", "Resident", "Mover", "Mover"),
                     # mean
                     "Sex" = 
                       c("F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M"),
                     "Mean" = c("0.17",
                                "-0.01",
                                "0.56",
                                "-0.48",
                                "0.38",
                                "0.46",
                                "0.15",
                                "-0.33",
                                "0.22",
                                "0.15",
                                "0.56",
                                "0.84",
                                "0.27",
                                "0.14"),
                     # sd
                     "SD" = c("0.73",
                              "0.69",
                              "0.55",
                              "0.73",
                              "0.67",
                              "0.60",
                              "0.60",
                              "0.65",
                              "0.73",
                              "0.85",
                              "0.38",
                              "0.19",
                              "0.73",
                              "0.72"),
                     "n" = c("77",
                             "49",
                             "11",
                             "7",
                             "92",
                             "31",
                             "6",
                             "7",
                             "91",
                             "27",
                             "7",
                             "2",
                             "79",
                             "37"),
                     # combined
                     "Rank value" = c("0.40",
                                      "0.22",
                                      "0.70",
                                      "-0.6",
                                      "0.82",
                                      "0.89",
                                      "0.2",
                                      "-0.61",
                                      "0.58",
                                      "0.62",
                                      "0.89",
                                      "0.67",
                                      "0.70",
                                      "0.66"),
                     # null
                     "Rank value" = c("0.35",
                                      "0.63",
                                      "0.54",
                                      "0.80",
                                      "0.85",
                                      "0.76",
                                      "0.61",
                                      "-0.13",
                                      "0.95",
                                      "0.61",
                                      "0.86",
                                      "0.87",
                                      "0.62",
                                      "0.83"),
                         check.names = FALSE) %>% 
  knitr::kable(booktabs = T,
               escape = F,
               caption = "Out of sample validation: Spearman’s rank correlation estimations applied to out-of-sample
data for estimating predictive capacity of third- and combined-order mapping for each
season-status-sex combination. Values closer to one demonstrate higher association of prediction,
and a negative value demonstrates a negative association of prediction.",
               format = "latex",
               align = c("lrccccccccc")) %>% 
 # kable_styling(font_size = 30, html_font = "Arial Nova") %>% 
  add_header_above(c( "", "", "", "Third-order" = 3, "Combined-order" = 1, "Null" = 1), 
                    line = F) %>% 
  collapse_rows(columns = 1, valign = "top") %>%
 # row_spec(c(3, 5, 6, 9, 10, 11, 13, 14), bold = T, underline = F) %>%
  row_spec(c(2, 6, 10), extra_css = "border-bottom: 1px solid;", bold = F) %>% 
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE) 

writeClipboard(table6)


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
