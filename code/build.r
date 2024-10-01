
# Mariners 2025 Analytics Intern Problem Set
# Author: Austin Craig
# Description: Cleans raw data. Saves data to ~data/derived

# Function to build and clean data
# Should be called twice, once on train set and once on test set
# Parameters:
#   df: raw data to be cleaned
#   filename: string; used to save .RData file.
clean_and_save <- function(df, filename) {

    # reformat date as days since opening day
    df$gamedate2 = as.Date(df$gamedate, format = "%Y-%m-%d")
    df$days_since_open = df$gamedate2 - min(df$gamedate2)

    # format level as int
    df$level = as.factor(df$level)

    # new vars for bat and pitch handedness
    df$right_bat = (df$bat_side == "R")
    df$right_pitch = (df$pitch_side == "R")
    df$same_handed = (df$bat_side == df$pitch_side)

    save(df, file = here("data", "derived", filename))
    cat("Successfully saved", filename)
    return(df)
}