source(here::here('code', 'setup.R'))

# Import raw data

data_raw <- read_csv(here(
  "data",
  "prolific_testing",
  "data.csv"
))

## Checking input data:

data_raw %>%
  group_by(next_veh_style, budget) %>%
  count()

# Select important columns
data <- data_raw %>%
  select(
    prolific_pid,
    time_min_total,
    time_min_vehicle_cbc,
    respID,
    next_veh_budget,
    vehicle_type = next_veh_style,
    budget,
    starts_with("vehicle_cbc_q")
  )

nrow(data)

# Summary of reasons to drop respondents

data_approval <- check_all_approvals(data_raw)
data_approval %>%
  count(status, reason)

# Drop bad respondents

data_vehicle <- data %>%
  left_join(
    data_approval %>%
      select(prolific_pid, status),
    by = "prolific_pid"
  ) %>%
  filter(status == "good")

nrow(data_vehicle)

# Drop anyone who got the demo question wrong:

data_vehicle <- data_vehicle %>%
  filter(vehicle_cbc_q0_button %in% c('option_1', 'option_4')) %>%
  select(-vehicle_cbc_q0_button)

nrow(data_vehicle)

# Drop respondents who went too fast
# Look at summary of completion times
summary(data_vehicle$time_min_total)
summary(data_vehicle$time_min_vehicle_cbc)

# Drop anyone who finished the choice question section in under 1 minute
data_vehicle <- data_vehicle %>%
  filter(time_min_vehicle_cbc >= 0.5) %>%
  # dropping non-unique respID (keeping first one)
  distinct(respID, .keep_all = TRUE)

nrow(data_vehicle)

# Create vehicle choice data ---------

# Read in choice questions and join it to the choice_data

survey_vehicle <- read_parquet(here(
  "data",
  "doe",
  "12-10-25",
  'design_vehicle.parquet'
))

# First convert the data to long format
choice_data_vehicle <- data_vehicle %>%
  pivot_longer(
    cols = vehicle_cbc_q1_button:vehicle_cbc_q6_button,
    names_to = "qID",
    values_to = "choice"
  ) %>%
  # Convert the qID variable and choice column to a number
  mutate(
    qID = parse_number(qID),
    choice = parse_number(choice)
  ) %>%
  left_join(
    survey_vehicle,
    by = c("vehicle_type", "budget", "respID", "qID")
  ) %>%
  # Convert choice column to 1 or 0 based on if the alternative was chosen
  mutate(
    choice = ifelse(choice == altID, 1, 0)
  )

head(choice_data_vehicle)

# Remove bad respID

# Create new values for respID & obsID
nRespondents <- nrow(data_vehicle)
nAlts <- max(survey_vehicle$altID)
nQuestions <- max(survey_vehicle$qID)
choice_data_vehicle$respID <- rep(seq(nRespondents), each = nAlts * nQuestions)
choice_data_vehicle$obsID <- rep(seq(nRespondents * nQuestions), each = nAlts)

# Reorder columns - it's nice to have the "ID" variables first
choice_data_vehicle <- choice_data_vehicle %>%
  select(ends_with("ID"), "choice", everything()) %>%
  select(-operating_cost_text, -range, -prolific_pid)

head(choice_data_vehicle)

# Save cleaned data for modeling
write_csv(
  choice_data_vehicle,
  here(
    "data",
    "prolific_testing",
    "choice_data_vehicle.csv"
  )
)
