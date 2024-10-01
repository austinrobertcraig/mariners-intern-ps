
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
        mutate(level_A = (level == "A")) %>%
        select(!level)

    # new vars for bat and pitch handedness
    df = df %>% mutate(
        right_bat = (bat_side == "R"), 
        right_pitch = (pitch_side == "R"), 
        same_handed = (bat_side == pitch_side)
        ) %>%
        select(!c(bat_side, pitch_side))

    # hit type classification
    # see https://fantasy.fangraphs.com/blasts-a-subset-of-barrels/
    df = df %>% mutate(
        barrelled = case_when(
            # barrelled - 6
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

    # set of indicators for barrelled
    # init set of columns as zeros in case all are missing
    df = df %>%
        add_column(
            barrelled_0 = 0,
            barrelled_1 = 0,
            barrelled_2 = 0,
            barrelled_3 = 0,
            barrelled_4 = 0,
            barrelled_5 = 0,
            barrelled_6 = 0,
            )
    df = dummy_cols(df, select_columns = "barrelled")

    # set of indicators for venue_id
    df = dummy_cols(df, select_columns = "venue_id")
    df = select(df, !venue_id)


    save(df, file = here("data", "derived", filename))
    cat("Successfully saved", filename)
    return(df)
}