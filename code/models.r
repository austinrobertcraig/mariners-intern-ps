
# Mariners 2025 Analytics Intern Problem Set
# Author: Austin Craig
# Description: Implements several models for predicting the air out probability for batted balls.
# All functions return a fitted model

library(ranger)
library(kernlab)

# Logit regression
logit_model <- function(formula, df, folds) {
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
logit_pca <- function(df, n_components, folds) {
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
        # PCA can't handle missing values, so remove variables with missings
        # in our case, this is just spin_rate
        step_filter_missing(all_numeric(), threshold = 0) %>%
        # apply PCA
        step_normalize(all_numeric()) %>%
        step_center(all_numeric()) %>%
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
rf_model <- function(formula, df, folds) {
    # build recipe
    rf_rec =
        recipe(formula, data = df) %>%
        # format outcome as factor
        step_mutate(is_airout = as.factor(is_airout)) %>%
        # remove predictors which have the same value for all obs
        step_zv(all_predictors())

    # build model
    rf_model =
        rand_forest(trees = 100) %>%
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

# Linear SVC
svc_model <- function(formula, df, folds) {
    # build recipe
    svc_rec =
        recipe(formula, data = df) %>%
        # format outcome as factor
        step_mutate(is_airout = as.factor(is_airout)) %>%
        # remove predictors which have the same value for all obs
        step_zv(all_predictors()) %>%
        # normalize and center
        step_center(all_numeric()) %>%
        step_normalize(all_numeric())

    # build model
    svc_model =
        svm_linear(cost = 1) %>%
        set_engine("kernlab") %>%
        set_mode("classification")

    # build workflow
    svc_wkflow =
        workflow() %>%
        add_model(svc_model) %>%
        add_recipe(svc_rec)

    # fit model
    svc_fit =
        svc_wkflow %>%
        fit_resamples(
            folds,
            metrics = metric_set(accuracy, mn_log_loss))

    return(svc_fit)
}