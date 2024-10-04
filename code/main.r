
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

# Train the best model and predict p_airout ------------------------

final_model = rf_model(formula_standard, train_cl, folds, cv = FALSE)
preds = predict(final_model, test_cl)
test = test %>%
    add_column(preds) %>%
    mutate(p_airout = .pred_class) %>%
    select(-.pred_class)

write_csv(test, here("output", "data-test-final.csv"))

# Question 2: Player analysis ------------------------

# predict which fielder "should" catch the ball

# Step 1: build + train a model on airouts only to predict which fielder will catch it
player_analysis_df = train_cl %>%
    filter(is_airout == 1) %>%
    mutate(
        first_fielder_pos = case_when(
            first_fielder == lf_id ~ "lf",
            first_fielder == cf_id ~ "cf",
            first_fielder == rf_id ~ "rf"
        )
    ) %>%
    mutate(first_fielder_pos = as.factor(first_fielder_pos))

folds_pos = vfold_cv(player_analysis_df, v = 10)
xvars_pos = paste("exit_speed", "hit_spin_rate", "vert_exit_angle", "horz_exit_angle", "right_bat", "right_pitch", venues_str, barreled_str, sep = " + ")
formula_pos = as.formula(paste("first_fielder_pos ~ ", xvars_pos))

model_pos_cv = rf_pos_pred(formula_pos, player_analysis_df, folds_pos)
collect_metrics(model_pos)
# accuracy: 0.934
# log-loss: 0.243

# Step 2: after training the model on airouts, apply it to the whole data set to "fill in" the predicted fielder on plays which were not airouts
model_pos = rf_pos_pred(formula_pos, player_analysis_df, folds_pos, cv = FALSE)
preds_pos = predict(model_pos, train_cl)
train_cl = train_cl %>%
    add_column(preds_pos) %>%
    mutate(.pred_class = as.character(.pred_class)) %>%
    mutate(
        responsible_fielder = case_when(
            # if airout, assign first_fielder
            is_airout == 1 &
            first_fielder == lf_id ~ "lf",
            is_airout == 1 &
            first_fielder == cf_id ~ "cf",
            is_airout == 1 &
            first_fielder == rf_id ~ "rf",
            # if not airout, use predicted fielder
            .default = .pred_class
        )
    ) %>%
    select(-.pred_class) %>%
    # identify the responsible player
    mutate(
        responsible_fielder_id = case_when(
            responsible_fielder == "lf" ~ lf_id,
            responsible_fielder == "cf" ~ cf_id,
            responsible_fielder == "rf" ~ rf_id
        )
    )
# 938 observed differences between predicted and actual fielders

# get list of distinct players involved in an airout
players = train_cl %>%
    distinct(first_fielder) %>%
    filter(!is.na(first_fielder)) %>%
    pull(first_fielder)

# calculate each player's Air Outs Above Expected (AOAE):
aoae = as_tibble(
    data.frame(player_id = players, expected_ao = 0, actual_ao = 0, aoae = 0)
    )

for (i in 1:nrow(aoae)) { #nrow(aoae)
    # train model on all other players, then predict this player's AOAE
    cat("Player", i)
    temp_train = train_cl %>%
        filter(responsible_fielder_id != aoae$player_id[i])
    temp_test = train_cl %>%
        filter(responsible_fielder_id == aoae$player_id[i])

    model = rf_model(formula_standard, temp_train, folds, cv = FALSE)
    preds = predict(model, temp_test)
    temp_test = temp_test %>%
        add_column(preds) %>%
        mutate(
            is_airout = as.numeric(is_airout) - 1,
            .pred_class = as.numeric(.pred_class) - 1
        )

    expected_air_outs = sum(temp_test$.pred_class)
    actual_air_outs = sum(temp_test$is_airout)

    aoae$expected_ao[i] = expected_air_outs
    aoae$actual_ao[i] = actual_air_outs
    aoae$aoae[i] = actual_air_outs - expected_air_outs
}

# Plot results
aoae_scatter(aoae)
aoae %>% filter(player_id == 15411)