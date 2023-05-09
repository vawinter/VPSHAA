
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
               format = "latex",
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
               format = "latex",
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
table4 <- data.frame("Covariates" = c("Elevation", "Roughness", "Aspect (Easting)", "Aspect (Northing)",
                                      "Herbaceous cover", "Shrub cover", 
                                      "Tree cover"),
                     "R² (marginal)" = c("57", "43", "12", "33", "21", "50", "23"),
                     "R² (conditional)" = c("99", "99", "99", "99", "99", "99", "99"),
                     "R² (marginal)" = c("02", "20", "02", "20", "01", "12", "06"),
                     "R² (conditional)" = c("99", "99", "99", "99", "99", "99", "99"),
                     check.names = FALSE) %>% 
  knitr::kable(booktabs = T,
               escape = F,
               caption = "Goodness of fit: Marginal and conditional R² 
               percentage values for each Full and Null covariate model output.",
               format = "latex",
               align = c("lrccccccccc")) %>% 
  #column_spec(2,bold=T,latex_column_spec = ">{\\\\color{black}}c") %>% 
 # collapse_rows(columns = 2, latex_hline = "major",valign = "middle") %>%
 # add_header_above(c(" ", "marginal" = 1, "conditional" = 3), align = "r") %>% 
  add_header_above(c(" ", "Full model" = 2, "Null Model" = 2)) %>% 
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(table4)


# table 5

table5 <- data.frame("Selection Coefficients" = 
                       c("Elevation", "Roughness","Aspect (Easting)", "Aspect (Northing)",
                                         "Herbaceous cover", "Shrub cover", "Tree cover"),
                     "Full model" = c("0.088", "0.254", "-0.0365", "0.336","0.040", "0.156","0.223"),
                     "Null model" = c("-0.103", "0.003","0.043", "0.360", "-0.05", "0.332","0.0293"),
                     "Goodness-of-fit" = c("0.095", "0.113", "-0.404", "-0.012", "0.043", "-0.088", "-0.035"),
                     check.names = FALSE) %>% 
  knitr::kable(booktabs = T,
               escape = F,
               caption = "Out of sample validation: Pearson's weighted coorelation estimations
               applied to out-of-sample data for estimating predictive capacity for
               full vs null models, and the goodness-of-fit metric.
               Positive values indicate the predictive performance
               of the full covariate model was better than the null covariate
               model. Negative values indicate that the null covariate model
               had better predictive performance than the full covariate model.",
               format = "latex",
               align = c("lrccccccccc")) %>% 
  add_header_above(c("Obsereved vs Predicted" = 1, " ", " "," "), line = F) %>% 
  #column_spec(2,bold=T,latex_column_spec = ">{\\\\color{black}}c") %>% 
  # collapse_rows(columns = 2, latex_hline = "major",valign = "middle") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(table5)
