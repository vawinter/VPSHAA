
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
                     "Temporally Static/Dynamic" = c(rep("static", 4), rep("dynamic", 3)),
                     "Expected effect" = c(rep("avoidance", 3), rep("selection", 3), rep("avoidance", 1)),
                     "References" = NA,
                       
                       c("(Beale and Smith 1970, O’Gara et al. 2004, Zeller et al. 2021)",
                                      "(Beale and Smith 1970, O’Gara et al. 2004)",
                                      rep("(Guisan et al. 1999, Maggini et al. 2002, Hirzel and Le Lay 2008)",2),
                                      "(Aikens et al. 2017, 2020b)",
                                      "(Beale and Smith 1970a, O’Gara et al. 2004, Berger 2004)",
                                      "(O’Gara et al. 2004, Larsen et al. 2011)"),
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
                     "References" = NA,
                     
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
 # collapse_rows(columns = 2, latex_hline = "major",valign = "middle") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(mig_tab)

# table 4 goodness of fit ----
table4 <- data.frame("Covariates" = c("Elevation", "Roughness","Aspect (Easting)", "Aspect (Northing)",
                                      "Herbaceous cover", "Shrub cover", "Tree cover"),
                     "R² (marginal)" = c("57", "44", "21", "53", "23", "58", "30"),
                     "R² (conditional)" = c("99", "99", "99", "99", "99", "99", "99"),
                     check.names = FALSE) %>% 
  knitr::kable(booktabs = T,
               escape = F,
               caption = "Goodness of fit: Marginal and conditional R² 
               percentage values for each covariate model output.",
               format = "latex",
               align = c("lrccccccccc")) %>% 
  #column_spec(2,bold=T,latex_column_spec = ">{\\\\color{black}}c") %>% 
 # collapse_rows(columns = 2, latex_hline = "major",valign = "middle") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(table4)


# table 5

table5 <- data.frame("Obsereved vs Predicted 
                     Selection Coefficients" = 
                       c("Elevation", "Roughness","Aspect (Easting)", "Aspect (Northing)",
                                      "Herbaceous cover", "Shrub cover", "Tree cover"),
                     "MAE Full" = c("3.61", "0.57", "0.89", "0.98","0.86", "0.48", "1.70"),
                     "MAE Null" = c("3.08", "0.43", "0.30", "0.28","0.67", "0.46", "1.15"),
                     "Pseudo R²" = c("-0.17", "-0.31", "-1.97", "-2.50","-0.28", "-0.41", "-0.48"),
                     check.names = FALSE) %>% 
  knitr::kable(booktabs = T,
               escape = F,
               caption = "Out of sample validation: Mean Absolute Error 
               (MAE) when applied to out-of-sample data for predictions of 
               full vs null model, and the difference between the two. 
               Positive pseudo-R² values indicate the predictive performance 
               of the full covariate model was better than the null covariate 
               model. Negative values indicate that the null covariate model 
               had better predictive performance than the full covariate model.",
             #  format = "latex",
             format = "html",
               align = c("lrccccccccc")) %>% 
  #column_spec(2,bold=T,latex_column_spec = ">{\\\\color{black}}c") %>% 
  # collapse_rows(columns = 2, latex_hline = "major",valign = "middle") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(table5)
