
# Mariners 2025 Analytics Intern Problem Set
# Author: Austin Craig
# Description: Contains functions which create the plots included in write-up.

library(ggrepel)

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
barreled <- function(df) {
    ggplot(df, aes(x = vert_exit_angle, y = exit_speed, color = as.factor(barreled))) +
        geom_point() +
        theme_linedraw(base_size = 14) +
        scale_color_viridis(
            discrete = TRUE,
            labels = c("Poorly (Topped)", "Poorly (Under)", "Flare or Burner", "Solid Contact", "Barreled"),
            option = "cividis") +
        labs(
            x = "Vertical Exit Angle (degrees)",
            y = "Exit Speed (mph)",
            color = "Hit Type"
        )
    ggsave(here("output", "figs", "barreled.png"))

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

# Scatter plot of AOAE, highlighting player 15411
aoae_scatter <- function(df) {

    p_15411 = df %>% filter(player_id == 15411)

    ggplot(df, aes(y = actual_ao, x = expected_ao)) +
        geom_point() +
        theme_linedraw() +
        labs(
            y = "Air Outs",
            x = "Expected Air Outs"
        ) +
        geom_point(
            data = p_15411,
            color = "red", size = 3, shape = "circle open"
        ) +
        geom_text_repel(
            data = p_15411,
            aes(label = player_id)
        ) +
        geom_abline(
            intercept = 0,
            slope = 1,
            color = "grey"
        )
    ggsave(here("output", "figs", "aoae_scatter.png"))
}

# Box plot of normalized mAOAE, highlighting player 15411
aoae_box <- function(df) {
    p_15411 = df %>%
        filter(player_id == 15411) %>%
        mutate(yvar = 0)

    ggplot(df, aes(x = maoae_normalized)) +
        geom_boxplot(alpha = 0.05) +
        labs(
            x = "Normalized mAOAE"
        ) +
        geom_point(
            data = p_15411,
            color = "red",
            aes(x = maoae_normalized, y = yvar)
        )
    ggsave(here("output", "figs", "aoae_box.png"))
}