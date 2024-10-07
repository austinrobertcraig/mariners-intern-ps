
# Mariners 2025 Analytics Intern Problem Set
# Author: Austin Craig
# Description: Contains functions which create the plots included in write-up.

library(viridis)
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
            labels = c("Poorly (Topped)", "Poorly (Under)", "Flare or Burner", "Solid Contact", "Barrel"),
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

# Density plot of mAOAE, highlighting player 15411
aoae_density <- function(df) {
    p_15411 = df %>%
        filter(player_id == 15411)

    ggplot(df, aes(x = maoae)) +
        geom_density(alpha = 0.05, fill = "blue") +
        labs(
            x = "mAOAE",
            y = "Density"
        ) +
        geom_vline(xintercept = p_15411$maoae) +
        theme_linedraw(base_size = 16)
    ggsave(here("output", "figs", "aoae_density.png"))
}

# Empirical CDF plot of mAOAE, highlighting player 15411
aoae_ecdf <- function(df) {
    p_15411 = df %>%
        filter(player_id == 15411)

    ggplot(df, aes(x = maoae)) +
        stat_ecdf(geom = "step") +
        labs(
            x = "mAOAE",
            y = "ECDF"
        ) +
        geom_vline(xintercept = p_15411$maoae) +
        theme_linedraw(base_size = 16)
    ggsave(here("output", "figs", "aoae_ecdf.png"))
}

# Bar Chart of Player 15411's performance vs hit type
hit_type_15411 <- function(df) {
    barreled_scores = df %>%
        group_by(barreled) %>%
        summarize(
            avg_score = mean(score)
        ) %>%
        mutate(barreled_desc = case_when(
            barreled == 3 ~ "Poorly Hit (Under)",
            barreled == 4 ~ "Flare or Burner",
            barreled == 5 ~ "Solid Contact",
            barreled == 6 ~ "Barrel"
        )) %>%
        mutate(barreled = as.numeric(barreled))

    ggplot(barreled_scores) +
        geom_bar(
            aes(x = reorder(barreled_desc, barreled), y = avg_score, fill = avg_score),
            stat = "identity") +
        theme_classic(base_size = 16) +
        theme(legend.position = "none") +
        labs(
            x = "Hit Type",
            y = "mAOAE"
        ) +
        scale_fill_viridis(option = "cividis") +
        geom_text(
            aes(x = reorder(barreled_desc, barreled), y = avg_score, label = round(avg_score, 3),
            vjust = ifelse(avg_score < 0, 1.5, -0.5)
            )
        ) +
        geom_hline(yintercept = 0, size = 1)

    ggsave(here("output", "figs", "hit_type_15411.png"))
}

# plot trend line of player 15411's score over time
score_plot_15411 <- function(df) {
    ggplot(df, aes(x = days_since_open, y = score)) +
        geom_smooth() +
        geom_vline(xintercept = 29) +
        geom_hline(yintercept = 0) +
        theme_linedraw(base_size = 16) +
        labs(
            x = "Day of Season",
            y = "mAOAE"
        )

    ggsave(here("output", "figs", "score_plot_15411.png"))
}