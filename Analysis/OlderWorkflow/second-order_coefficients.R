# Clean env
rm(list = ls())
gc()

# libraries
library(dplyr)
library(broom)
library(lubridate)
library(raster)
library(rnaturalearth)
library(sf)
library(knitr)
library(kableExtra)
#library(ragg)

# Function to calculate harmonic mean
harm <- function(x, na.rm = FALSE) {
  return(1/mean(1/x, na.rm = na.rm))
}

# Data load in ----
## Load in ph data
ph_dat <- readRDS("comb_dat_20220524.rds")

# Directories ----
# Get extent for cell of interest
cell = readRDS("out/cells_good.rds")

# ## Load in shape of utah
# Utah
gsl <- st_read("geo/salt_lake.shp") %>%
  st_transform(32612)
ut <- st_read("geo/utah.shp") %>%
  st_transform(32612)

## List covariate files ----
dir2 <- "2021_stacks/"
landscapes <- list.files(dir2, full.names = T)

# for proj
temp <- raster(landscapes[[1]])

# separate per season
winter <- stack(landscapes[1])
names(winter) <- c("elevation", "snd", "asp_sin", "asp_cos", "roughness", "bio",
                   "herb", "shrub", "tree")


spring <- stack(landscapes[2])
names(spring) <- c("elevation", "snd", "asp_sin", "asp_cos", "roughness", "bio",
                   "herb", "shrub", "tree")

summer <- stack(landscapes[3])
names(summer) <- c("elevation", "asp_sin", "asp_cos", "roughness", "bio",
                   "herb", "shrub", "tree")

fall <- stack(landscapes[4])
names(fall) <- c("elevation", "snd", "asp_sin", "asp_cos", "roughness", "bio",
                 "herb", "shrub", "tree")

# Set up loop ----
## Make a list of seasons
season <- c("Winter", "Spring", "Summer", "Fall")

## Make list of rasters
seas_rast <- c(winter, spring, summer, fall)
names(seas_rast) <- season

# Season centroids ----
## Step 1: ----
# Centroid for each individual
cents <- ph_dat %>%
  mutate(season = case_when(
    month(dt) == 2 ~ "Winter",
    month(dt) == 4 ~ "Spring",
    month(dt) == 7 ~ "Summer",
    month(dt) == 11 ~ "Fall",
    TRUE ~ NA_character_
  ),
  year = year(dt)) %>%
  # Drop data not assigned a season
  filter(!is.na(season)) %>%
  group_by(ID, year, season) %>%
  summarize(n = n(),
            mean_x = mean(x),
            mean_y = mean(y),
            med_x = median(x),
            med_y = median(y),
            hm_x = harm(x),
            hm_y = harm(y))


# Initialize an empty list to store results
results <- list()
conf_intervals <- list()

# Loop through each season and fit a model
for(s in season){
  # Filter out appropriate season
  cents_split <- cents[which(cents$season == s), ]

  rsf_model <- case ~ elevation  + roughness + herb + shrub + tree + asp_sin +
    asp_cos


  ## Step 2: ----
  # get available data
  avail <- as.data.frame(seas_rast[[s]], xy = TRUE) %>%
    mutate(case = 0,
           cell = 1:nrow(.)) %>%
    filter(!if_any(everything(), is.na))

  ## Step 3: ----
  # Get used data
  used_cells <- cellFromXY(seas_rast[[s]], cbind(cents_split$hm_x, cents_split$hm_y))
  used <- avail %>%
    filter(cell %in% used_cells) %>%
    mutate(case = 1) %>%
    distinct()

  ## Step 4: ----
  # Combine into one df
  rsf_dat <- rbind(used, avail) %>%
    # give weights to available points
    mutate(weight = case_when(
      case == 0 ~ 1e5,
      case == 1 ~ 1
    ))

  # Fit the regression model (modify the formula as needed)
  rsf <- glm(data = rsf_dat, formula = rsf_model, weights = weight, family = binomial)
  # Get the summary of the model
  summary_model <- summary(rsf)

  # Extract coefficients
  coefficients <- summary_model$coefficients[, 1:2]  # Extract only coefficients and standard errors

  # Calculate mean, lower CI, and upper CI
  mean_values <- coefficients[, 1]
  lower_ci <- coefficients[, 1] - 1.96 * coefficients[, 2]  # Assuming a 95% CI
  upper_ci <- coefficients[, 1] + 1.96 * coefficients[, 2]

  # Create a data frame with results for the current season
  results_df <- data.frame(
    Season = s,
    Covaraite = rownames(coefficients),
    Beta = round(mean_values, digits = 3),
    LowerCI = round(lower_ci, digits = 3),
    UpperCI = round(upper_ci, digits = 3)
  )

  # Add the results to the list with the season name as the key
  results[[s]] <- results_df
}

# Combine results for all seasons into a single data frame
all_results <- do.call(rbind, results)

row.names(all_results) <- NULL

all_results <- all_results %>%
  mutate(Covariate = case_when(
    Covaraite == "elevation" ~ "Elevation",
    Covaraite == "roughness" ~ "Roughness",
    Covaraite == "herb" ~ "Herbaceous",
    Covaraite == "shrub" ~ "Shrub",
    Covaraite == "tree" ~ "Tree",
    Covaraite == "asp_sin" ~ "Aspect (Easting)",
    Covaraite == "asp_cos" ~ "Aspect (Northing)",
    TRUE ~ Covaraite  # If none of the cases match, keep the original value
  )) %>%
  dplyr::select(-Covaraite) %>%
  relocate(Covariate, .before = "Beta")

# # Calculate summary statistics across all seasons
# summary_across_seasons <- aggregate(cbind(Mean_beta, LowerCI, UpperCI) ~ Covaraite, data = all_results, FUN = function(x) c(Mean = mean(x), LowerCI = min(x), UpperCI = max(x)))
#
# # Print the summary across all seasons
# print(summary_across_seasons)
all_results

x2 <- all_results %>%
  left_join(mean_results) %>% # mean results from 10_RSF_pt3.R
  filter(!Covariate == '(Intercept)')


# Create the table
extentpoints <- kable(x2,
                      booktabs = T,
                      escape = F,
                      caption = "",
                      format = "latex",
                      align = c("lrccccccccc")) %>%
  collapse_rows(columns = 1, latex_hline = "major",valign = "middle") %>%
  add_header_above(c( "", "", "Second-order" = 3, "Third-order" = 3), line = T)  %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "striped"), full_width = FALSE)

writeClipboard(extentpoints)

# Combine results into a single data frame
combined_results <- do.call(rbind, lapply(names(results), function(season) {
  data.frame(
    Season = season,
    results[[season]],
    stringsAsFactors = FALSE
  )
}))

