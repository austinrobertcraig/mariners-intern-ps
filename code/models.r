
# Mariners 2025 Analytics Intern Problem Set
# Author: Austin Craig
# Description: Implements several models for predicting the air out probability for batted balls.
# All functions return a fitted model

library(ranger)

# Logit regression
logit_model <- function(formula, df) {
    # build recipe
    logit_rec =
        recipe(formula, data = df) %>%
        # format outcome as factor
        step_mutate(is_airout = as.factor(is_airout)) %>%
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
logit_pca <- function(df, components) {
    # build recipe
    logit_rec =
        recipe(is_airout ~ ., data = df) %>%
        # format outcome as factor
        step_mutate(is_airout = as.factor(is_airout)) %>%
        # remove non-informative variables
        step_select(-ends_with("_id")) %>%
        step_select(-"first_fielder") %>%
        # remove predictors which have the same value for all obs
        step_zv(all_predictors()) %>%
        # apply PCA
        step_normalize(all_numeric()) %>%
        step_center(all_numeric())

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
rf_model <- function(formula, df) {
    # build recipe
    rf_rec =
        recipe(formula, data = df) %>%
        # format outcome as factor
        step_mutate(is_airout = as.factor(is_airout)) %>%
        # remove predictors which have the same value for all obs
        step_zv(all_predictors())

    # build model
    rf_model =
        rand_forest(trees = 1000) %>%
        set_engine("ranger") %>%
        set_mode("classification")

    # build workflow
    rf_wkflow =
        workflow() %>%
        add_model(rf_model) %>%
        add_recipe(rf_rec)

    # fit model
    rf_fit =
        rf_wkflow %>%
        fit_resamples(
            folds,
            metrics = metric_set(accuracy, mn_log_loss))

    return(rf_fit)
}