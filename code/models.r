
# Mariners 2025 Analytics Intern Problem Set
# Author: Austin Craig
# Description: Implements several models for predicting the air out probability for batted balls.
# All functions return a fitted model

library(ranger)
library(LiblineaR)

# Logit regression
logit_model <- function(formula, df, folds) {
    # build recipe
    logit_rec =
        recipe(formula, data = df) %>%
        # interaction term for hit speed x exit angle
        step_interact(terms = hit_speed:vert_exit_angle) %>%
        # remove predictors which have the same value for all obs
        step_zv(all_predictors())

    # build model
    logit_model =
        logistic_reg() %>%
        set_engine("glm")

    # build workflow
    logit_wkflow =
        workflow() %>%
        add_model(logit_model) %>%
        add_recipe(logit_rec)

    # fit model
    logit_fit =
        logit_wkflow %>%
        fit_resamples(
            folds,
            metrics = metric_set(accuracy, mn_log_loss))

    return(logit_fit)
}

# Logit + PCA
logit_pca <- function(df, n_components, folds) {
    # build recipe
    logit_rec =
        recipe(is_airout ~ ., data = df) %>%
        # remove non-informative variables
        step_select(-ends_with("_id")) %>%
        step_select(-"first_fielder") %>%
        # remove predictors which have the same value for all obs
        step_zv(all_predictors()) %>%
        # PCA can't handle missing values, so remove variables with missings
        # in our case, this is just spin_rate
        step_filter_missing(all_numeric(), threshold = 0) %>%
        # apply PCA
        step_center(all_numeric()) %>%
        step_normalize(all_numeric()) %>%
        step_pca(all_numeric(), num_comp = n_components)

    # build model
    logit_model =
        logistic_reg() %>%
        set_engine("glm")

    # build workflow
    logit_wkflow =
        workflow() %>%
        add_model(logit_model) %>%
        add_recipe(logit_rec)

    # fit model
    logit_fit =
        logit_wkflow %>%
        fit_resamples(
            folds,
            metrics = metric_set(accuracy, mn_log_loss))

    return(logit_fit)
}

# Random Forest Model
rf_model <- function(formula, df, folds, cv = TRUE) {

    cores = parallel::detectCores()

    # build recipe
    rf_rec =
        recipe(formula, data = df) %>%
        # interaction term for hit speed x exit angle
        step_interact(terms = hit_speed:vert_exit_angle) %>%
        # remove features with missing values
        step_filter_missing(all_numeric(), threshold = 0) %>%
        # remove predictors which have the same value for all obs
        step_zv(all_predictors())

    # build model
    rf_model =
        rand_forest(trees = 128) %>%
        set_engine("ranger", num.threads = cores) %>%
        set_mode("classification")

    # build workflow
    rf_wkflow =
        workflow() %>%
        add_model(rf_model) %>%
        add_recipe(rf_rec)

    # fit model
    if (cv) {
        rf_fit =
            rf_wkflow %>%
            fit_resamples(
                folds,
                metrics = metric_set(accuracy, mn_log_loss))
    } else {
        rf_fit =
            rf_wkflow %>%
            fit(data = df)
    }

    return(rf_fit)
}

# Random Forest Model to predict fielder
rf_pos_pred <- function(formula, df, folds, cv = TRUE) {

    cores = parallel::detectCores()

    # build recipe
    rf_rec =
        recipe(formula, data = df) %>%
        # remove features with missing values
        step_filter_missing(all_numeric(), threshold = 0) %>%
        # remove predictors which have the same value for all obs
        step_zv(all_predictors())

    # build model
    rf_model =
        rand_forest(trees = 128) %>%
        set_engine("ranger", num.threads = cores) %>%
        set_mode("classification")

    # build workflow
    rf_wkflow =
        workflow() %>%
        add_model(rf_model) %>%
        add_recipe(rf_rec)

    # fit model
    if (cv) {
        rf_fit =
            rf_wkflow %>%
            fit_resamples(
                folds,
                metrics = metric_set(accuracy, mn_log_loss))
    } else {
        rf_fit =
            rf_wkflow %>%
            fit(data = df)
    }

    return(rf_fit)
}