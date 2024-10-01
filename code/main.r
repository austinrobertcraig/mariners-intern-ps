
# Mariners 2025 Analytics Intern Problem Set
# Author: Austin Craig
# Description: Main File. Execute project files in correct sequence.

# Import libraries
renv::restore()
library(here)
library(ggplot2)
library(dplyr)
library(viridis)

# Import project R files
source(here("code", "build.r"))

# Set seed for reproducability
set.seed(116)

# build + clean data
train = read.csv(here("data", "raw", "data-train.csv"))
test = read.csv(here("data", "raw", "data-test.csv"))
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

ggplot(train_cl, aes(x = exit_speed, y = is_airout)) +
    geom_point()
