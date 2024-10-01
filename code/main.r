
# Mariners 2025 Analytics Intern Problem Set
# Author: Austin Craig
# Description: Main File. Execute project files in correct sequence.

# Import libraries
renv::restore()
library(here)
library(tidyverse)
library(tidymodels)
library(viridis)

# Import project R files
source(here("code", "build.r"))

# Set seed for reproducibility
set.seed(116)

# build + clean data
train = as_tibble(read.csv(here("data", "raw", "data-train.csv")))
test = as_tibble(read.csv(here("data", "raw", "data-test.csv")))
train_cl = clean_and_save(train, "train.RData")    # see "build.r" for this function
test_cl = clean_and_save(test, "test.RData")

# Histogram of hit balls by horizontal exit angle, grouped by if caught
ggplot(train_cl, aes(x = horz_exit_angle)) +
    geom_histogram(data = subset(train_cl, is_airout == 0), alpha = 0.5, aes(fill = factor(is_airout))) +
    geom_histogram(data = subset(train_cl, is_airout == 1), alpha = 0.5, aes(fill = factor(is_airout))) +
    scale_color_viridis(discrete = TRUE) +
    theme_linedraw() +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    labs(x = "Horizontal Exit Angle (degrees)", y = "Count of Hit Balls", fill = "Airout")
ggsave(here("output", "figs", "horz_exit_angle_hist.png"))

# Plot exit speed x vert exit angle, colored by barreled status
ggplot(train_cl, aes(x = vert_exit_angle, y = exit_speed, color = as.factor(barrelled))) +
    geom_point() +
    theme_linedraw() +
    scale_color_viridis(discrete = TRUE) +
    labs(
        x = "Vertical Exit Angle (degrees)",
        y = "Exit Speed (mph)",
        color = "Barreled Classification"
    )
ggsave(here("output", "figs", "barrelled.png"))
