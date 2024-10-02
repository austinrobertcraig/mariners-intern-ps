
# Mariners 2025 Analytics Intern Problem Set
# Author: Austin Craig
# Description: Contains functions which create the plots included in write-up.

# Histgram: Count of Hit Balls by Exit Angle, grouped by air out
horz_exit_angle_hist <- function(df) {
    ggplot(df, aes(x = horz_exit_angle)) +
        geom_histogram(data = subset(df, is_airout == 0), alpha = 0.5, aes(fill = factor(is_airout))) +
        geom_histogram(data = subset(df, is_airout == 1), alpha = 0.5, aes(fill = factor(is_airout))) +
        scale_fill_viridis(
            discrete = TRUE,
            option = "cividis") +
        theme_linedraw(base_size = 14) +
        scale_y_continuous(expand = expansion(mult = c(0, .1))) +
        labs(
            x = "Horizontal Exit Angle (degrees)",
            y = "Count of Hit Balls",
            fill = "Airout")
    ggsave(here("output", "figs", "horz_exit_angle_hist.png"))

    return()
}

# Hit Type by Exit Speed and Vertical Exit Angle
barrelled <- function(df) {
    ggplot(df, aes(x = vert_exit_angle, y = exit_speed, color = as.factor(barrelled))) +
        geom_point() +
        theme_linedraw(base_size = 14) +
        scale_color_viridis(
            discrete = TRUE,
            labels = c("Poorly (Topped)", "Poorly (Under)", "Flare or Burner", "Solid Contact", "Barrelled"),
            option = "cividis") +
        labs(
            x = "Vertical Exit Angle (degrees)",
            y = "Exit Speed (mph)",
            color = "Hit Type"
        )
    ggsave(here("output", "figs", "barrelled.png"))

    return()
}

# Scatter plot of optimal number of components for logit
optimal_components <- function(components) {
    ggplot(components, aes(x = n, y = log_loss)) +
        geom_point() +
        theme_linedraw() +
        labs(
            x = "Number of Components",
            y = "Log-Loss (10-fold CV)"
        )
    ggsave(here("output", "figs", "optimal_components.png"))

    return()
}