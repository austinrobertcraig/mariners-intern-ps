
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
source(here("code", "models.r"))
source(here("code", "figs.r"))

# Set seed for reproducibility
set.seed(116)

# Load data --------------------------------------

train = as_tibble(read.csv(here("data", "raw", "data-train.csv")))
test = as_tibble(read.csv(here("data", "raw", "data-test.csv")))
train_cl = clean_and_save(train, "train.RData")    # see "build.r" for this function
test_cl = clean_and_save(test, "test.RData")

# prepare folds for cross-validation
folds = vfold_cv(train_cl, v = 10)

# Plots ------------------------------------------
# See "figs.r" for these functions
print("Generating figures...")

# Histogram of hit balls by horizontal exit angle, grouped by if is air out
horz_exit_angle_hist(train_cl)

# Plot exit speed x vert exit angle, colored by barreled status
barrelled(train_cl)
print("Done!")

# Formula Strings for Models ---------------------------

# list of venue indicator variables
venues = train %>%
    distinct(venue_id) %>%
    pull(venue_id)
venues_str = paste("venue_id", venues, sep = "_", collapse = " + ")

# list of barrelled indicator variables
barrelled_str = paste("barrelled", 0:6, sep = "_", collapse = " + ")

# Model 1: Naive Logistic Regression -------------------

# build formula
xvars = paste("temperature", "inning", "top", "pre_balls",
    "pre_strikes", "exit_speed", "hit_spin_rate", "vert_exit_angle",
    "horz_exit_angle", "days_since_open", "level_A", "right_bat",
    "right_pitch", "same_handed", venues_str, sep = " + ")
formula1 = as.formula(paste("is_airout ~ ", xvars))

# train and evaluate model
model1 = logit_model(formula1, train_cl)
collect_metrics(model1)
# accuracy: 0.646
# log loss: 0.616

# Model 2: Better Logistic Regression -------------------

# build formula
xvars = paste("temperature", "inning", "top", "pre_balls",
    "pre_strikes", "exit_speed", "hit_spin_rate", "vert_exit_angle",
    "horz_exit_angle", "horz_exit_angle2", "days_since_open", "level_A", "right_bat",
    "right_pitch", "same_handed", venues_str, barrelled_str,
    sep = " + ")
formula2 = as.formula(paste("is_airout ~ ", xvars))

# train and evaluate model
model2 = logit_model(formula2, train_cl)
collect_metrics(model2)
# accuracy: 0.866
# log loss: 0.313
# just adding the squared term on horizontal exit angle reduces log loss from 0.545 to 0.313

