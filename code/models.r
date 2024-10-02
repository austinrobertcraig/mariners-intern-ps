
# Mariners 2025 Analytics Intern Problem Set
# Author: Austin Craig
# Description: Implements several models for predicting the air out probability for batted balls.
# All functions return a fitted model

# Logit regression
logit_model <- function(formula, df) {
    # build recipe
    logit_rec =
        recipe(formula, data = df) %>%
        # format outcome as factor
        step_mutate(is_airout = as.factor(is_airout))
        # remove predictors which have the same value for all obs
        #step_zv(all_predictors())

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

