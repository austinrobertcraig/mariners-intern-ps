
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
train_cl = clean_and_save(train, "train.RData")    # see "build.r"
test_cl = clean_and_save(test, "test.RData")

# format outcome as factor
train_cl = train_cl %>% mutate(is_airout = as.factor(is_airout))

# prepare folds for cross-validation
folds = vfold_cv(train_cl, v = 10)

# Plots ------------------------------------------
# See "figs.r" for these functions
print("Generating figures...")

# Histogram of hit balls by horizontal exit angle, grouped by if is air out
horz_exit_angle_hist(train_cl)

# Plot exit speed x vert exit angle, colored by barreled status
barreled(train_cl)
print("Done!")

# Formula Strings for Models ---------------------------

# list of venue indicator variables
venues = train %>%
    distinct(venue_id) %>%
    pull(venue_id)
venues_str = paste("venue_id", venues, sep = "_", collapse = " + ")

# list of barreled indicator variables
barreled_str = paste("barreled", 0:6, sep = "_", collapse = " + ")

# string of predictors for formulas
xvars_naive = paste("temperature", "inning", "top", "pre_balls",
    "pre_strikes", "exit_speed", "hit_spin_rate", "vert_exit_angle",
    "horz_exit_angle", "days_since_open", "level_A", "right_bat",
    "right_pitch", "same_handed", venues_str, sep = " + ")

xvars_full = paste("temperature", "inning", "top", "pre_balls",
    "pre_strikes", "exit_speed", "exit_speed2", "hit_spin_rate", 
    "vert_exit_angle", "horz_exit_angle", "horz_exit_angle2", 
    "extreme_horz_angle", "days_since_open", "level_A",
    "right_bat", "right_pitch", "same_handed", venues_str, barreled_str,
    sep = " + ")

xvars = paste("temperature", "exit_speed", "exit_speed2", "hit_spin_rate",
    "vert_exit_angle", "horz_exit_angle", "horz_exit_angle2", venues_str,
    barreled_str, sep = " + ")

# create formulas
formula_naive = as.formula(paste("is_airout ~ ", xvars_naive))
formula_full = as.formula(paste("is_airout ~ ", xvars_full))
formula_standard = as.formula(paste("is_airout ~ ", xvars))

# Model 1: Naive Logistic Regression -------------------

model1 = logit_model(formula_naive, train_cl, folds)
collect_metrics(model1)
# accuracy: 0.646
# log loss: 0.616

# Model 2: Better Logistic Regression -------------------

model2 = logit_model(formula_standard, train_cl, folds)
collect_metrics(model2)
# accuracy: 0.868
# log loss: 0.308
# just adding the squared term on horizontal exit angle reduces log loss from 0.545 to 0.313

# Model 3: Logistic + PCA -------------------

# create df to track log loss by number of components
components = as_tibble(
    data.frame(n = seq(1, 40, 5), log_loss = 1)
    )

# test for optimal number of components
for (i in 1:nrow(components)) {
    cat("Step", i)
    model = logit_pca(train_cl, components$n[i], folds)
    metrics = collect_metrics(model)
    # record log-loss
    components$log_loss[i] = metrics$mean[2]
}

# draw scatterplot
optimal_components(components)
# performs worse across the board than regular logit

# Model 4: Random Forest ------------------------

model4 = rf_model(formula_standard, train_cl, folds)
collect_metrics(model4)
# accuracy: 0.887
# log loss: 0.258

# Model 5: SVC ----------------------------------

model5 = svc_model(formula_standard, train_cl, folds, cv = TRUE)
collect_metrics(model5)
# accuracy: 
# log loss: 

# Train the best model and predict p_airout ------------------------

final_model = rf_model(formula_standard, train_cl, folds, cv = FALSE)
preds = predict(final_model, test_cl)
test = test %>%
    add_column(preds) %>%
    mutate(p_airout = .pred_class) %>%
    select(-.pred_class)

write_csv(test, here("output", "data-test-final.csv"))

# Question 2: Player analysis ------------------------

