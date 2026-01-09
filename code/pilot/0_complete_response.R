source(here::here('code', 'setup.R'))

pilot_start <- ymd_hms('2025-10-14 21:00:00')
pilot_end <- ymd_hms('2025-10-21 16:00:00')

# Connect to database
#### surveydown::sd_db_config()
db <- sd_db_connect()

# original db
data_raw <- sd_get_data(db)

# removing testing entries
data_raw <- data_raw %>%
  filter(!is.na(psid), nchar(psid) >= 10) %>%

  # Drop people who got screened out
  filter(!is.na(current_page), current_page == "end") %>%
  select(-current_page) %>%

  # Drop those who completed before the adjustments
  filter(time_start >= pilot_start) %>%
  filter(time_start <= pilot_end)

# Some special variables:
# session_id = a unique ID for the Run - should be the same across all surveys
# time_start = time stamp when survey was started
# time_end   = time stamp when survey ended
# time_p_*** = Time page *** was reached
# time_q_*** = Time question *** was last answered

# Compute time values for each page
data <- data_raw %>%
  mutate(
    # Compute time through whole survey
    time_start = ymd_hms(time_start, tz = "UTC"),
    time_end = ymd_hms(time_end, tz = "UTC"),
    time_total = as.numeric(time_end - time_start, units = "secs"),
    # Compute time through just the cbc questions
    ## Vehicle
    time_p_vehicle_pageQ1_button = ymd_hms(
      time_p_vehicle_pageQ1_button,
      tz = "UTC"
    ),
    time_p_vehicle_pageQ6_button = ymd_hms(
      time_p_vehicle_pageQ6_button,
      tz = "UTC"
    ),
    time_vehicle_cbc_total = as.numeric(
      time_p_vehicle_pageQ6_button - time_p_vehicle_pageQ1_button,
      units = "secs"
    ),
    ## Battery
    time_p_battery_pageQ1_button = ymd_hms(
      time_p_battery_pageQ1_button,
      tz = "UTC"
    ),
    time_p_battery_pageQ6_button = ymd_hms(
      time_p_battery_pageQ6_button,
      tz = "UTC"
    ),
    time_battery_cbc_total = as.numeric(
      time_p_battery_pageQ6_button - time_p_battery_pageQ1_button,
      units = "secs"
    ),
    time_min_total = time_total / 60,
    time_min_vehicle_cbc = time_vehicle_cbc_total / 60,
    time_min_battery_cbc = time_battery_cbc_total / 60
  )


data <- data %>%
  # Bot
  filter(is.na(attention_check_toyota)) %>%

  # Survey question removed
  select(
    !c(
      attention_check_toyota,
      attitudes_1_a,
      attitudes_1_b,
      attitudes_2_a,
      attitudes_2_b,
      battery_attribute
    )
  )
# Drop respondents that had a missing budget (somehow)
filter(!is.na(next_veh_budget))

#n_distinct(data$session_id)  # 98

write_csv(
  data,
  here(
    "data",
    "pilot",
    "survey_data.csv"
  )
)
