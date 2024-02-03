# Cross coorelation plots
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpmisc)
library(wCorr)

# Read in data
dat <- read.csv("Data/Outputs/RSF_outputs/20221018_mem-prep.csv", header = T)
head(dat)

# Step 1: multiply each sel coeff by its weights
# Aim: Looking to see if betas are correlated
weightedCorr(dat$Elev_beta,
             dat$Herb_beta,
             method = "Pearson",
             weights = dat$weight_Elev)

weightedCorr(dat$Herb_beta,
             dat$Shrub_beta,
             method = "Pearson",
             weights = dat$weight_Shrub)


# Other code ----
# Step 1: multiply each sel coeff by its weights
elevation <- dat$Elev_beta * dat$weight_Elev
roughness <- dat$Rough_beta * dat$weight_Rough
herb <- dat$Herb_beta * dat$weight_Herb
shrub <- dat$Shrub_beta * dat$weight_Shrub
tree <- dat$Tree_beta * dat$weight_Tree
sin <- dat$Asp_sin_beta * dat$weight_Asp_sin
cos <- dat$Asp_cos_beta * dat$weight_Asp_cos

# Not sure if I am structuring this right....
weightedCorr(elevation,
             roughness,
             method = "Pearson")

# Step 2: Cross coorelation plots
ccf(elevation, roughness)
print(ccf(elevation, roughness))

ccf(elevation, herb)
print(ccf(elevation, herb))

ccf(elevation, shrub)
print(ccf(elevation, herb))

ccf(elevation, tree)
print(ccf(elevation, tree))

ccf(herb, shrub)
print(ccf(herb, shrub))

ccf(herb, tree)
print(ccf(herb, tree))


# Step 3: Regular plots
plot(elevation, roughness)
abline(lm(roughness ~ elevation)) # y ~ x

plot(elevation, herb)
abline(lm(herb ~ elevation)) # y ~ x

plot(elevation, roughness)
abline(lm(roughness ~ elevation)) # y ~ x

plot(elevation, herb)
abline(lm(herb ~ elevation)) # y ~ x

plot(herb, shrub)
abline(lm(shrub ~ herb)) # y ~ x

plot(herb, tree)
abline(lm(tree ~ herb)) # y ~ x

plot(shrub, tree)
abline(lm(tree ~ shrub)) # y ~ x


# Step 4: Nicer graphs
# Format df w. betas*weight and SEs
adj_betas <- data.frame(elevation, roughness, herb, shrub, tree, sin, cos)

ggplot(adj_betas, aes(x=elevation, y=roughness)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  labs(x = "Elevation", y = "Roughness") +
  theme_classic()

ggplot(adj_betas, aes(x=herb, y=shrub)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  labs(x = "Herbaceous", y = "Shrub") +
  theme_classic()

ggplot(adj_betas, aes(x=herb, y=tree)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  labs(x = "Herbaceous", y = "Tree") +
  theme_classic()

# More playing around ----
# Format df w. betas*weight and SEs
# adj_betas2 <- data.frame(elevation, roughness, herb, shrub, tree, sin, cos) %>% 
#  mutate(ID = row_number()) %>% 
#  pivot_longer(!ID, names_to = "Covariate", values_to = "Beta")
# 
# se <- data.frame(elevation = dat$Elev_stder, roughness = dat$Rough_stder, 
#                  herb = dat$Herb_stder, shrub = dat$Shrub_stder, 
#                  tree = dat$Tree_stder, sin = dat$Asp_sin_stder, 
#                  cos = dat$Asp_cos_stder) %>%
#   # Create unique identifyer row
#  mutate(ID = row_number()) %>% 
#  pivot_longer(!ID, names_to = "Covariate", values_to = "SE")
# 
# # Join df together
# adj_df <- adj_betas %>%
#   left_join(se, by = c("ID", "Covariate"))

