
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

    # barreled
    # 0 = unclassified
    # 1 = poorly_weak
    # 2 = poorly_topped
    # 3 = poorly_under
    # 4 = flare_or_burner
    # 5 = solid_contact
    # 6 = barrelled
    # see https://fantasy.fangraphs.com/blasts-a-subset-of-barrels/
    df = df %>% mutate(
        barrelled = case_when(
            # barrelled - 6
            (exit_speed * 1.5 - vert_exit_angle) >= 117 &
            (exit_speed + vert_exit_angle) >= 124 &
            exit_speed >= 98 &
            vert_exit_angle >= 4 &
            vert_exit_angle <= 50 ~ "barrelled",
            # solid contact - 5
            (exit_speed * 1.5 - vert_exit_angle) >= 111 &
            (exit_speed + vert_exit_angle) >= 119 &
            exit_speed >= 95 &
            vert_exit_angle >= 0 &
            vert_exit_angle <= 52 ~ "solid_contact",
            # flare or burner - 4
            (exit_speed * 2 - vert_exit_angle) >= 87 &
            vert_exit_angle <= 41 &
            (exit_speed * 2 + vert_exit_angle) <= 175 &
            (exit_speed + vert_exit_angle * 1.3) >= 89 &
            exit_speed >= 59 &
            exit_speed <= 72 ~ "flare_or_burner",
            # flare or burner - 4
            (exit_speed + vert_exit_angle * 1.3) <= 112 &
            (exit_speed + vert_exit_angle * 1.55) >= 92 &
            exit_speed >= 72 &
            exit_speed <= 86 ~ "flare_or_burner",
            # flare or burner - 4
            vert_exit_angle <= 20 &
            (exit_speed + vert_exit_angle * 2.4) >= 98 &
            exit_speed >= 86 &
            exit_speed <= 95 ~ "flare_or_burner",
            # flare or burner - 4
            (exit_speed - vert_exit_angle) >= 76 &
            (exit_speed + vert_exit_angle * 2.4) >= 98 &
            exit_speed >= 95 &
            vert_exit_angle <= 30 ~ "flare_or_burner",
            # poorly_under - 3
            (exit_speed + vert_exit_angle * 2) >= 116 ~ "poorly_under",
            # poorly_topped - 2
            (exit_speed + vert_exit_angle * 2) < 116 ~ "poorly_topped",
            # poorly_weak - 1
            exit_speed <= 59 ~ "poorly_weak",
            # unclassified - 0
            .default = "unclassified"
            )
        )

    # set of indicators for barrelled
    df = dummy_cols(df, select_columns = "barrelled")

    # set of indicators for venue_id
    df = dummy_cols(df, select_columns = "venue_id")
    df = select(df, !venue_id)


    save(df, file = here("data", "derived", filename))
    cat("Successfully saved", filename)
    return(df)
}