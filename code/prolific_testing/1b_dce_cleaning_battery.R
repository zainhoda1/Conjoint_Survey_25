source(here::here('code', 'setup.R'))
source(here::here('code', 'prolific_testing', 'approval_functions.R'))
# Import raw data

data_raw <- read_parquet(here(
  "data",
  "prolific_testing",
  "data_round_2+3+4_may_26.parquet"
))

data_approval <- read_parquet(here(
  "data",
  "prolific_testing",
  "approve_round2+3+4.parquet"
))

# Read in choice questions and join it to the choice_data

survey_battery <- read_parquet(here(
  "data",
  "doe",
  "02-06-26",
  'design_battery.parquet'
))

 nrow(data_raw)

## Checking input data:

data_raw %>%
  group_by(next_veh_style, budget) %>%
  count()

data_raw |>
  group_by(collection_round) |> 
  summarise(counts= n()) |> 
  pivot_wider(names_from= 'collection_round', values_from = 'counts')

# Select important columns
data <- data_raw %>%
  # Select important columns
  select(
    prolific_pid,
    time_start,
    time_min_total,
    time_min_battery_cbc,
    battery_respID,
    next_veh_budget,
    vehicle_type = next_veh_style,
    budget,
    starts_with("battery_cbc_q"),
    collection_round
  ) %>%
  select(-next_veh_budget) %>%
  rename(respID = battery_respID)

nrow(data)

# Drop bad respondents


data_battery <- data %>%
  inner_join(
    data_approval %>%
      select(prolific_pid),
    by = "prolific_pid"
  )

# b<-data_battery |> 
#  filter(battery_cbc_q1_button=="option_4" & 
#   battery_cbc_q2_button=="option_4" &
#    battery_cbc_q3_button=="option_4" & 
#   battery_cbc_q4_button=="option_4" &
#    battery_cbc_q5_button=="option_4" & 
#   battery_cbc_q6_button=="option_4"
#   )

print('Drop bad respondents')
nrow(data_battery)

data_battery |>
  group_by(collection_round) |> 
  summarise(counts= n()) |> 
  pivot_wider(names_from= 'collection_round', values_from = 'counts')

# Battery filtering ----

# Drop anyone who didn't complete all choice questions
data_battery <- data_battery %>%
  filter(!is.na(battery_cbc_q1_button)) %>%
  filter(!is.na(battery_cbc_q2_button)) %>%
  filter(!is.na(battery_cbc_q3_button)) %>%
  filter(!is.na(battery_cbc_q4_button)) %>%
  filter(!is.na(battery_cbc_q5_button)) %>%
  filter(!is.na(battery_cbc_q6_button))

print("Drop anyone who didn't complete all choice questions")
nrow(data_battery)

data_battery |>
  group_by(collection_round) |> 
  summarise(counts= n()) |> 
  pivot_wider(names_from= 'collection_round', values_from = 'counts')

# Drop anyone who answered the same question for all choice questions except no-choice
data_battery <- data_battery %>%
  mutate(
    cbc_all_same = (battery_cbc_q1_button == battery_cbc_q2_button) &
      (battery_cbc_q2_button == battery_cbc_q3_button) &
      (battery_cbc_q3_button == battery_cbc_q4_button) &
      (battery_cbc_q4_button == battery_cbc_q5_button) &
      (battery_cbc_q5_button == battery_cbc_q6_button) &
      (battery_cbc_q2_button != 'option_4')
  ) %>%
  filter(!cbc_all_same) %>%
  select(-cbc_all_same)

print('Drop anyone who answered the same question for all choice questions except no-choice')
nrow(data_battery)

data_battery |>
  group_by(collection_round) |> 
  summarise(counts= n()) |> 
  pivot_wider(names_from= 'collection_round', values_from = 'counts')


# Drop respondents who went too fast
# Look at summary of completion times
summary(data_battery$time_min_total)
summary(data_battery$time_min_battery_cbc)

# Drop anyone who finished the choice question section in under 30 seconds
data_battery <- data_battery %>%
  filter(time_min_battery_cbc >= 0.5) %>%
  # dropping non-unique respID (keeping first one)
  distinct(respID,prolific_pid, .keep_all = TRUE)

print('Drop respondents who went too fast < 30 sec')
nrow(data_battery)

data_battery |>
  group_by(collection_round) |> 
  summarise(counts= n()) |> 
  pivot_wider(names_from= 'collection_round', values_from = 'counts')

# Create battery choice data ---------

# First convert the data to long format
choice_data_battery <- data_battery %>%
  pivot_longer(
    cols = battery_cbc_q1_button:battery_cbc_q6_button,
    names_to = "qID",
    values_to = "choice"
  ) %>%
  # Convert the qID variable and choice column to a number
  mutate(
    qID = parse_number(qID),
    choice = parse_number(choice)
  ) %>%
  left_join(
    survey_battery,
    by = c("vehicle_type", "budget", "respID", "qID")
  ) %>%
  # Convert choice column to 1 or 0 based on if the alternative was chosen
  mutate(
    choice = ifelse(choice == altID, 1, 0)
  )

head(choice_data_battery)

# Remove bad respID

# Create new values for respID & obsID
nRespondents <- nrow(data_battery)
nAlts <- max(survey_battery$altID)
nQuestions <- max(survey_battery$qID)
choice_data_battery$respID <- rep(seq(nRespondents), each = nAlts * nQuestions)
choice_data_battery$obsID <- rep(seq(nRespondents * nQuestions), each = nAlts)

# Reorder columns - it's nice to have the "ID" variables first
choice_data_battery <- choice_data_battery %>%
  select(ends_with("ID"), "choice", everything())

# Fix no choice coding for battery health attributes
choice_data_battery <- choice_data_battery %>%
  mutate(
    battery_health_year0 = ifelse(no_choice, NA, battery_health_year0),
    battery_health_year3 = ifelse(no_choice, NA, battery_health_year3),
    battery_health_year8 = ifelse(no_choice, NA, battery_health_year8)
  ) %>%
  mutate(psid = prolific_pid) %>%
  select(-prolific_pid, -battery_condition)

head(choice_data_battery)

# Save cleaned data for modeling
write_parquet(
  choice_data_battery,
  here(
    "data",
    "prolific_testing",
    "choice_data_battery_round2+3+4_may_26.parquet"
  )
)

