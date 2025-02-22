
# Mariners 2025 Analytics Intern Problem Set
# Author: Austin Craig
# Description: Cleans raw data. Saves data to ~data/derived

library(fastDummies)

# Function to build and clean data
# Should be called twice, once on train set and once on test set
# Parameters:
#   df: raw data to be cleaned
#   filename: string; used to save .RData file.
clean_and_save <- function(df, filename) {

    # reformat date as days since opening day
    df = df %>%
        mutate(gamedate = as.Date(gamedate, format = "%Y-%m-%d")) %>%
        mutate(days_since_open = gamedate - min(gamedate)) %>%
        select(!gamedate)

    # format level as indicator
    df = df %>%
        mutate(level_A = as.numeric(level == "A")) %>%
        select(!level)

    # new vars for bat and pitch handedness
    df = df %>% mutate(
        right_bat = as.numeric(bat_side == "R"), 
        right_pitch = as.numeric(pitch_side == "R"), 
        same_handed = as.numeric(bat_side == pitch_side)
        ) %>%
        select(!c(bat_side, pitch_side))

    # gen squared horz_exit_angle and exit speed
    df = df %>% mutate(
        horz_exit_angle2 = horz_exit_angle^2,
        exit_speed2 = exit_speed^2)

    # gen indicator for extreme horizontal exit angle
    threshold = 75  #degrees away from dead center considered an "extreme" angle
    df = df %>% mutate(extreme_horz_angle = as.numeric(abs(horz_exit_angle) > threshold))

    # hit type classification
    # see https://fantasy.fangraphs.com/blasts-a-subset-of-barrels/
    df = df %>% mutate(
        barreled = case_when(
            # barreled - 6
            (exit_speed * 1.5 - vert_exit_angle) >= 117 &
            (exit_speed + vert_exit_angle) >= 124 &
            exit_speed >= 98 &
            vert_exit_angle >= 4 &
            vert_exit_angle <= 50 ~ 6,
            # solid contact - 5
            (exit_speed * 1.5 - vert_exit_angle) >= 111 &
            (exit_speed + vert_exit_angle) >= 119 &
            exit_speed >= 95 &
            vert_exit_angle >= 0 &
            vert_exit_angle <= 52 ~ 5,
            # flare or burner - 4
            (exit_speed * 2 - vert_exit_angle) >= 87 &
            vert_exit_angle <= 41 &
            (exit_speed * 2 + vert_exit_angle) <= 175 &
            (exit_speed + vert_exit_angle * 1.3) >= 89 &
            exit_speed >= 59 &
            exit_speed <= 72 ~ 4,
            # flare or burner - 4
            (exit_speed + vert_exit_angle * 1.3) <= 112 &
            (exit_speed + vert_exit_angle * 1.55) >= 92 &
            exit_speed >= 72 &
            exit_speed <= 86 ~ 4,
            # flare or burner - 4
            vert_exit_angle <= 20 &
            (exit_speed + vert_exit_angle * 2.4) >= 98 &
            exit_speed >= 86 &
            exit_speed <= 95 ~ 4,
            # flare or burner - 4
            (exit_speed - vert_exit_angle) >= 76 &
            (exit_speed + vert_exit_angle * 2.4) >= 98 &
            exit_speed >= 95 &
            vert_exit_angle <= 30 ~ 4,
            # poorly_under - 3
            (exit_speed + vert_exit_angle * 2) >= 116 ~ 3,
            # poorly_topped - 2
            (exit_speed + vert_exit_angle * 2) < 116 ~ 2,
            # poorly_weak - 1
            exit_speed <= 59 ~ 1,
            # unclassified - 0
            .default = 0
            )
        )

    # set of indicators for barreled
    # init set of columns as zeros in case all are missing
    df = df %>%
        add_column(
            barreled_0 = 0,
            barreled_1 = 0,
            barreled_2 = 0,
            barreled_3 = 0,
            barreled_4 = 0,
            barreled_5 = 0,
            barreled_6 = 0,
            )
    df = dummy_cols(df, select_columns = "barreled")

    # set of indicators for venue_id
    df = dummy_cols(df, select_columns = "venue_id")
    #df = select(df, !venue_id)

    # impute missing hit_spin_rate at the mean
    impute_val = mean(df$hit_spin_rate, na.rm = TRUE)
    df = df %>% mutate(
        hit_spin_rate = case_when(
            is.na(hit_spin_rate) ~ impute_val,
            .default = hit_spin_rate
            )
        )


    save(df, file = here("data", "derived", filename))
    cat("Successfully saved", filename)
    return(df)
}