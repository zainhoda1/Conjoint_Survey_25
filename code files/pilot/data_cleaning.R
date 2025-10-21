# Load libraries
library(fastDummies)
library(here)
library(lubridate)
library(tidyverse)
library(arrow)

# Change dplyr settings so I can view all columns
options(dplyr.widtkh = Inf)


# Import raw data

data_raw <- read_csv(here(
  "code files",
  "pilot",
  "data",
  "survey_data.csv"
))

# Read in choice questions and join it to the choice_data

survey <- read_parquet(here(
  "code files",
  "Design folders",
  "Design-10_14_25",
  'design_vehicle.parquet'
))


# removing testing entries
data_raw <- data_raw %>%
  filter(!is.na(psid), nchar(psid) >= 10)


# Format and join the three surveys -------

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
    time_p_vehicle_pageQ1_button = ymd_hms(
      time_p_vehicle_pageQ1_button,
      tz = "UTC"
    ),
    time_p_vehicle_pageQ6_button = ymd_hms(
      time_p_vehicle_pageQ6_button,
      tz = "UTC"
    ),
    time_cbc_total = as.numeric(
      time_p_vehicle_pageQ6_button - time_p_vehicle_pageQ1_button,
      units = "secs"
    )
  ) %>%
  # Select important columns
  select(
    session_id,
    time_start,
    time_total,
    time_cbc_total,
    respID,
    next_veh_budget,
    next_veh_style,
    current_page,
    starts_with("vehicle_cbc_q")
  )

head(data)


# Filter out bad responses ---------

nrow(data)

# Drop people who got screened out
data <- data %>%
  filter(!is.na(current_page), current_page == "end") # 2025-08-07 18:38:21

nrow(data)

# Drop those who completed before the adjustments
data <- data %>%
  filter(time_start > '2025-10-14 00:00:00') #2025-08-14 14:08:00 # 2025-08-06 18:38:21

nrow(data)

# Drop respondents that had a missing budget (somehow)
data <- data %>%
  filter(!is.na(next_veh_budget))

nrow(data)

# Drop anyone who didn't complete all choice questions
data <- data %>%
  filter(!is.na(vehicle_cbc_q1_button)) %>%
  filter(!is.na(vehicle_cbc_q2_button)) %>%
  filter(!is.na(vehicle_cbc_q3_button)) %>%
  filter(!is.na(vehicle_cbc_q4_button)) %>%
  filter(!is.na(vehicle_cbc_q5_button)) %>%
  filter(!is.na(vehicle_cbc_q6_button))
nrow(data)

# Drop anyone who got the demo question wrong:

data <- data %>%
  filter(vehicle_cbc_q0_button %in% c('option_1', 'option_4')) %>%
  select(-vehicle_cbc_q0_button)
nrow(data)


# Drop anyone who answered the same question for all choice questions
data <- data %>%
  mutate(
    cbc_all_same = (vehicle_cbc_q1_button == vehicle_cbc_q2_button) &
      (vehicle_cbc_q2_button == vehicle_cbc_q3_button) &
      (vehicle_cbc_q3_button == vehicle_cbc_q4_button) &
      (vehicle_cbc_q4_button == vehicle_cbc_q5_button) &
      (vehicle_cbc_q5_button == vehicle_cbc_q6_button)
  ) %>%
  filter(!cbc_all_same) %>%
  select(-cbc_all_same)
nrow(data)

# Drop respondents who went too fast
data <- data %>%
  mutate(
    # Convert time to minutes
    time_min_total = time_total / 60,
    time_min_cbc = time_cbc_total / 60
  )

# Look at summary of completion times
summary(data$time_min_total)
summary(data$time_min_cbc)

# Drop anyone who finished the choice question section in under 1 minute
# data <- data %>%
#   filter(time_min_cbc >= 1)
nrow(data)


# Create choice data ---------

# First convert the data to long format
choice_data <- data %>%
  pivot_longer(
    cols = vehicle_cbc_q1_button:vehicle_cbc_q6_button,
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

# temp_survey <- survey %>%
#   filter(respID == 2859,
#          vehicle_type == 'car')
#
# temp_data <- data_raw %>%
#   filter(respID == 2859 ,
#     next_veh_style == 'Car / sedan / hatchback')
#
# temp_choice <- choice_data %>%
#   filter(respID == 2859,
#          vehicle_type == 'car')
#
# c1 <- choice_data

choice_data <- choice_data %>%
  left_join(survey, by = c("vehicle_type", "respID", "qID"))

# temp_choice1 <- choice_data %>%
#   filter(respID == 2859,
#          vehicle_type == 'car')

# Convert choice column to 1 or 0 based on if the alternative was chosen
choice_data <- choice_data %>%
  mutate(
    choice = ifelse(choice == altID, 1, 0),
    price = price * next_veh_budget
  )

head(choice_data)

# Remove bad respID
choice_data <- choice_data %>%
  filter(respID != 3587)
data <- data %>%
  filter(respID != 3587)

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
    "code files",
    "pilot",
    "data",
    "vehicle_choice_data.csv"
  )
)
