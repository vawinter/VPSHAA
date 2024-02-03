# Compliling boxplots

# Libraries
library(tidyverse)
library(cowplot)
library(lme4)
library(MASS)
library(lmerTest)
library(effects)

# Load in data
#dat <- read.csv("buffer/chapter1/20220719_outputs/20220718_MEM_prep.csv")


# -------------------------------------------------X
# ----------------- Generate Models ----------------
# -------------------------------------------------X
source("Analysis/11_MEM-full.R")

# boxplots
g <- dat %>%
  mutate(season = case_when(month == "2" ~ "Winter",
                            month == "4" ~ "Spring",
                            month == "7" ~ "Summer",
                            month == "11" ~ "Fall")) %>%
  ggplot(aes(x = season, y = Asp_sin_beta)) +
  geom_boxplot(fill = "gray") +
  labs(x = "Season", y = "",
       title = "Easting",
       subtitle = "(f)") +
  theme_bw() +
  theme(text = element_text(size = 15))  +
  # ylim(-1, 1) +
  theme(plot.title = element_text(hjust = 0.5))

# boxplots
h <- dat %>%
  mutate(season = case_when(month == "2" ~ "Winter",
                            month == "4" ~ "Spring",
                            month == "7" ~ "Summer",
                            month == "11" ~ "Fall")) %>%
  ggplot(aes(x = season, y = Asp_cos_beta)) +
  geom_boxplot(fill = "gray") +
  labs(x = "Season", y = "Selection Coefficient",
       title = "Northing",
       subtitle = "(f)") +
  theme_bw() +
  theme(text = element_text(size = 15))  +
  # ylim(-1, 1) +
  theme(plot.title = element_text(hjust = 0.5))

# boxplots
a <- dat %>%
  mutate(season = case_when(month == "2" ~ "Winter",
                            month == "4" ~ "Spring",
                            month == "7" ~ "Summer",
                            month == "11" ~ "Fall")) %>%
  ggplot(aes(x = season, y = Elev_beta)) +
  geom_boxplot(fill = "gray") +
  labs(x = "Season", y = "Selection Coefficient",
       title = "Elevation",
       subtitle = "(a)") +
  theme_bw() +
  theme(text = element_text(size = 15))  +
  # ylim(-1, 1) +
  theme(plot.title = element_text(hjust = 0.5))

# boxplots
b <- dat %>%
  mutate(season = case_when(month == "2" ~ "Winter",
                            month == "4" ~ "Spring",
                            month == "7" ~ "Summer",
                            month == "11" ~ "Fall")) %>%
  ggplot(aes(x = season, y = Rough_beta)) +
  geom_boxplot(fill = "gray") +
  labs(x = "Season", y = "",
       title = "Roughness",
       subtitle = "(b)") +
  theme_bw() +
  theme(text = element_text(size = 15))  +
  # ylim(-1, 1) +
  theme(plot.title = element_text(hjust = 0.5))

# boxplots
c <- dat %>%
  mutate(season = case_when(month == "2" ~ "Winter",
                            month == "4" ~ "Spring",
                            month == "7" ~ "Summer",
                            month == "11" ~ "Fall")) %>%
  ggplot(aes(x = season, y = Herb_beta)) +
  geom_boxplot(fill = "gray") +
  labs(x = "Season", y = "Selection Coefficient",
       title = "Herbaceous",
       subtitle = "(c)") +
  theme_bw() +
  theme(text = element_text(size = 15))  +
  # ylim(-1, 1) +
  theme(plot.title = element_text(hjust = 0.5))
# boxplots
d <- dat %>%
  mutate(season = case_when(month == "2" ~ "Winter",
                            month == "4" ~ "Spring",
                            month == "7" ~ "Summer",
                            month == "11" ~ "Fall")) %>%
  ggplot(aes(x = season, y = Shrub_beta)) +
  geom_boxplot(fill = "gray") +
  labs(x = "Season", y = "",
       title = "Shrub",
       subtitle = "(d)") +
  theme_bw() +
  theme(text = element_text(size = 15))  +
  # ylim(-1, 1) +
  theme(plot.title = element_text(hjust = 0.5))

# boxplots
e <- dat %>%
  mutate(season = case_when(month == "2" ~ "Winter",
                            month == "4" ~ "Spring",
                            month == "7" ~ "Summer",
                            month == "11" ~ "Fall")) %>%
  ggplot(aes(x = season, y = Tree_beta)) +
  geom_boxplot(fill = "gray") +
  labs(x = "Season", y = "Selection Coefficient",
       title = "Tree",
       subtitle = "(e)") +
  theme_bw() +
  theme(text = element_text(size = 15))  +
  # ylim(-1, 1) +
  theme(plot.title = element_text(hjust = 0.5))


x <- ggdraw() +
  draw_plot(e, x = 0, y = .5, height = .5, width = .5) +
  draw_plot(g, x = .5, y = .5, height = .5, width = .5) +
  draw_plot(h, x = .25, y = 0, height = .5, width = .5)

y <- gridExtra::grid.arrange(a, b, c, d)

bt <- gridExtra::grid.arrange(y, x)

dev.off()


ggsave("Figures_and_Results/TWS/boxplots.png", bt,  width = 12, height = 14, units = "in")
