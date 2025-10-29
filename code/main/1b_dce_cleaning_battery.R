source(here::here('code', 'setup.R'))

# Import raw data

data_raw <- read_csv(here(
  "data",
  "main",
  "survey_data.csv"
))

# Read in choice questions and join it to the choice_data

survey <- read_parquet(here(
  "data",
  "doe",
  "design-10-14-25",
  'design_battery.parquet'
))


# Format and join the three surveys -------

# Compute time values for each page
data <- data_raw %>%
  # Select important columns
  select(
    session_id,
    time_start,
    time_min_total,
    time_min_battery_cbc,
    battery_respID,
    next_veh_budget,
    next_veh_style,
    starts_with("battery_cbc_q"),
    -battery_cbc_q0_button
  ) %>% 
  rename(respID = battery_respID)  #changed battery_respID to just respID for ease of use

glimpse(data)

# Drop respondents who went too fast
# Look at summary of completion times
summary(data$time_min_total)
summary(data$time_min_battery_cbc)
data <- data %>%
  # Drop anyone who finished the choice question section in under 30 second
  filter(time_min_battery_cbc >= 0.5) %>%

  # dropping non-unique respID (keeping first one)
  distinct(respID, .keep_all = TRUE)

# Drop anyone who didn't complete all choice questions
data <- data %>%
  filter(!is.na(battery_cbc_q1_button)) %>%
  filter(!is.na(battery_cbc_q2_button)) %>%
  filter(!is.na(battery_cbc_q3_button)) %>%
  filter(!is.na(battery_cbc_q4_button)) %>%
  filter(!is.na(battery_cbc_q5_button)) %>%
  filter(!is.na(battery_cbc_q6_button))
nrow(data)

# Drop anyone who answered the same question for all choice questions
data <- data %>%
  mutate(
    cbc_all_same = (battery_cbc_q1_button == battery_cbc_q2_button) &
      (battery_cbc_q2_button == battery_cbc_q3_button) &
      (battery_cbc_q3_button == battery_cbc_q4_button) &
      (battery_cbc_q4_button == battery_cbc_q5_button) &
      (battery_cbc_q5_button == battery_cbc_q6_button)
  ) %>%
  filter(!cbc_all_same) %>%
  select(-cbc_all_same)
nrow(data)

# Create choice data ---------

# First convert the data to long format
choice_data <- data %>%
  pivot_longer(
    cols = battery_cbc_q1_button:battery_cbc_q6_button,
    names_to = "qID",
    values_to = "choice"
  ) %>%
  # Convert the qID variable and choice column to a number
  mutate(
    qID = parse_number(qID),
    choice = parse_number(choice),
    vehicle_type = case_when(
      next_veh_style == 'Car / sedan / hatchback' ~ 'car',
      next_veh_style == 'SUV / crossover' ~ 'suv'
    )
  ) %>%
  select(-next_veh_style)

head(choice_data)

glimpse(choice_data)
glimpse(survey)


choice_data <- choice_data %>%
  left_join(survey, by = c("respID", "qID"))


# Convert choice column to 1 or 0 based on if the alternative was chosen
choice_data <- choice_data %>%
  mutate(
    choice = ifelse(choice == altID, 1, 0),
    veh_price = veh_price * next_veh_budget
  )

glimpse(choice_data)

nrow(data)

# Create new values for respID & obsID
nRespondents <- nrow(data)
nAlts <- max(survey$altID)
nQuestions <- max(survey$qID)
choice_data$respID <- rep(seq(nRespondents), each = nAlts * nQuestions)
choice_data$obsID <- rep(seq(nRespondents * nQuestions), each = nAlts)

# Reorder columns - it's nice to have the "ID" variables first
choice_data <- choice_data %>%
  select(ends_with("ID"), "choice", everything())

head(choice_data)

# Save cleaned data for modeling
write_csv(
  choice_data,
  here(
    "data",
    "main",
    "battery_choice_data.csv"
  )
)
