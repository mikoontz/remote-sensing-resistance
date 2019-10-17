# Purpose: get list of Earth Engine asset IDs from full FRAP perimeter dataset
# in order to feed them to python Earth Engine script for exporting

library(tidyverse)
library(lubridate)

fire_meta <- read.csv("data/data_output/ee_fire-samples/fires-strat-samples_metadata_2018_48-day-window_L4578_none-interp_all.csv") %>% dplyr::arrange(alarm_date)

epoch <- ymd_hm("1970-01-01 00:00", tz = "zulu")
fire_meta$print_alarm_date <- fire_meta$alarm_date / 1000 + epoch
fire_meta$alarm_year <- year(fire_meta$print_alarm_date)
fire_meta <- filter(fire_meta, alarm_year > 1983)

# Adding leading 0's to month and day: https://stackoverflow.com/questions/5812493/how-to-add-leading-zeros

fire_meta$print_alarm_date = paste0(year(fire_meta$print_alarm_date), 
                                    str_pad(string = month(fire_meta$print_alarm_date), width = 2, pad = "0"), 
                                    str_pad(string = day(fire_meta$print_alarm_date), width = 2, pad = "0"))

# Concatenate these strings so they can be pasted into the Python code.
# Pretty kludge-y. Maybe writing to a .csv and importing that .csv as a 
# python dataframe makes more sense.
# Shout out to Erica Rettig for this code (as an answer to an unrelated
# Davis R Users Group question) and for linking to this SO question/answer:
# https://stackoverflow.com/questions/23585306/insert-comma-between-characters-with-r
cat(paste0('"', paste0(fire_meta$system.index, collapse="\", \""), '"'))
cat(paste0('"', paste0(fire_meta$print_alarm_date, collapse="\", \""), '"'))

# Paste the resulting strings, put it in between square brackets, and call it a
# python list!