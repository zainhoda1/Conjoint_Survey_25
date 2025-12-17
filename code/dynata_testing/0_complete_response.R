source(here::here('code', 'setup.R'))

pilot_start <- ymd_hms('2025-12-17 14:27:00')   #2025-12-17 14:26:24 UTC
#pilot_end <- ymd_hms('2025-10-21 16:00:00')

# Connect to database
#### surveydown::sd_db_config()
#db <- sd_db_connect()

# original db
#data_raw <- sd_get_data(db)
# write_csv(data_raw, here('data', 'dynata_testing', 'data_raw.csv'))

data_raw <- read_csv(here('data', 'dynata_testing', 'data_raw.csv'))

# removing testing entries
data_raw <- data_raw %>%
  filter(!is.na(psid), nchar(psid) >= 10) %>% 
  filter()

nrow(data_raw)

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
  mutate(
    budget = case_when(
      next_veh_budget %in% c("5000", "10000", "15000", "20000") ~ 'low',
      TRUE ~ 'high'
    )
  )

nrow(data)

# Drop those who missed attention checks
data <- data %>%
  # Bot
  filter(is.na(attention_check_toyota))

nrow(data)

# Drop people who got screened out

data <- data %>%
  filter(!is.na(current_page), current_page == "end") %>%
  
  # Drop those who completed before the adjustments
  filter(time_start >= pilot_start) %>% 
  #filter(time_start <= pilot_end) %>% 
  
  select(-current_page)

nrow(data)

# Approval drops ----

# These are being dropped to not approve them only
# Further data cleaning happens later

# Drop anyone who didn't complete all choice questions
data_approval <- data %>%
  filter(!is.na(vehicle_cbc_q1_button)) %>%
  filter(!is.na(vehicle_cbc_q2_button)) %>%
  filter(!is.na(vehicle_cbc_q3_button)) %>%
  filter(!is.na(vehicle_cbc_q4_button)) %>%
  filter(!is.na(vehicle_cbc_q5_button)) %>%
  filter(!is.na(vehicle_cbc_q6_button)) %>%
  filter(!is.na(battery_cbc_q1_button)) %>%
  filter(!is.na(battery_cbc_q2_button)) %>%
  filter(!is.na(battery_cbc_q3_button)) %>%
  filter(!is.na(battery_cbc_q4_button)) %>%
  filter(!is.na(battery_cbc_q5_button)) %>%
  filter(!is.na(battery_cbc_q6_button))

nrow(data_approval)

# Drop anyone who answered the same question for all vehicle questions
data_approval <- data_approval %>%
  mutate(
    cbc_all_same = (vehicle_cbc_q1_button == vehicle_cbc_q2_button) &
      (vehicle_cbc_q2_button == vehicle_cbc_q3_button) &
      (vehicle_cbc_q3_button == vehicle_cbc_q4_button) &
      (vehicle_cbc_q4_button == vehicle_cbc_q5_button) &
      (vehicle_cbc_q5_button == vehicle_cbc_q6_button)
  ) %>%
  filter(!cbc_all_same) %>%
  select(-cbc_all_same)

nrow(data_approval)

# Drop anyone who answered the same question for all battery questions

data_approval <- data_approval %>%
  mutate(
    cbc_all_same = (battery_cbc_q1_button == battery_cbc_q2_button) &
      (battery_cbc_q2_button == battery_cbc_q3_button) &
      (battery_cbc_q3_button == battery_cbc_q4_button) &
      (battery_cbc_q4_button == battery_cbc_q5_button) &
      (battery_cbc_q5_button == battery_cbc_q6_button)
  ) %>%
  filter(!cbc_all_same) %>%
  select(-cbc_all_same)

nrow(data_approval)

# Join demographics and check for approvals

# demos <- read_csv(here(
#   'data',
#   'dynata_testing',
#   'prolific_demographics.csv'
# )) %>%
#   clean_names() %>%
#   filter(status != 'SCREENED OUT')
# 
# nrow(demos)

# data_approval %>%
#   mutate(completion_code = as.character(completion_code)) %>%
#   select(participant_id = prolific_pid, completion_code) %>%
#   left_join(
#     demos,
#     by = c('participant_id', 'completion_code')
#   ) %>%
#   select(participant_id) %>%
#   write_csv(
#     here(
#       "data",
#       "dynata_testing",
#       "approve.csv"
#     )
#   )

# Save

write_csv(
  data,
  here(
    "data",
    "dynata_testing",
    "data.csv"
  )
)
