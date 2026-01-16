source(here::here('code', 'setup.R'))
source(here::here('code', 'prolific_testing', 'approval_functions.R'))
# --------------------------------------------------------------------------
# Load the data set:

# data_vehicle_prolific <- read_parquet(here(
#   "data",
#   "prolific_testing",
#   "choice_data_vehicle.parquet"
# ))
# 
# data_battery_prolific <- read_parquet(here(
#   "data",
#   "prolific_testing",
#   "choice_data_battery.parquet"
# ))
# 
# 
# data_vehicle_dynata <- read_parquet(here(
#   "data",
#   "dynata_testing",
#   "choice_data_vehicle.parquet"
# ))
# 
# data_battery_dynata <- read_parquet(here(
#   "data",
#   "prolific_testing",
#   "choice_data_battery.parquet"
# ))

data_raw <- read_csv(here('data', 'prolific_testing', 'data_raw.csv'))


# data_raw <- read_csv(here(
#   "data",
#   "prolific_testing",
#   "preview_data.csv"
# ))

# removing testing entries
data_raw <- data_raw %>%
  filter(!is.na(prolific_pid), nchar(prolific_pid) >= 10)

nrow(data_raw)

# Keep first occurrence
data_raw <- data_raw %>% distinct(prolific_pid, .keep_all = TRUE)

nrow(data_raw)

# Check for approvals (need to fix this code)

data_approval <- check_all_approvals(data_raw)

data_approval %>%
  count(status, reason)

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
  #filter(time_start >= pilot_start) %>%
  #filter(time_start <= pilot_end) %>%

  select(-current_page)

nrow(data)

# Drop people whose next vehicle is NUll

data <- data %>%
  filter(!is.na(next_veh_style))
# Save

write_parquet(
  data,
  here(
    "data",
    "prolific_testing",
    "data.parquet"
  )
)


# Join demographics and check for approvals

demos1 <- read_csv(here(
  'data',
  'prolific_testing',
  'prolific_demographics1.csv'
)) %>%
  clean_names() %>%
  filter(status != 'SCREENED OUT') %>%
  mutate(source = 'demos1')
demos2 <- read_csv(here(
  'data',
  'prolific_testing',
  'prolific_demographics2.csv'
)) %>%
  clean_names() %>%
  filter(status != 'SCREENED OUT') %>%
  mutate(source = 'demos2')
demos <- rbind(demos1, demos2) %>%
  rename(prolific_pid = participant_id)

nrow(demos)

data_approval %>%
  filter(status == 'good') %>%
  left_join(
    data %>%
      select(prolific_pid, completion_code) %>%
      distinct(),
    by = 'prolific_pid'
  ) %>%
  mutate(completion_code = as.character(completion_code)) %>%
  select(prolific_pid, completion_code) %>%
  left_join(
    demos,
    by = c('prolific_pid', 'completion_code')
  ) %>%
  select(prolific_pid, source) %>%
  arrange(source) %>%
  # filter(source == 'demos2') %>%
  write_parquet(
    here(
      "data",
      "prolific_testing",
      "approve.parquet"
    )
  )

data_approval %>%
  filter(status == 'bad') %>%
  write_parquet(
    here(
      "data",
      "prolific_testing",
      "reject.parquet"
    )
  )

