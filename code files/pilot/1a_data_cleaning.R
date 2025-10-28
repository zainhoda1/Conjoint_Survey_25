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



# Compute time values for each page
data <- data_raw %>%
  # Select important columns
  select(
    session_id,
    time_start,
    time_total,
    time_min_vehicle_cbc,
    respID,
    next_veh_budget,
    next_veh_style,
    starts_with("vehicle_cbc_q")
  )

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
# Look at summary of completion times
summary(data$time_min_total)
summary(data$time_min_cbc)

# Drop anyone who finished the choice question section in under 1 minute
data <- data %>%
  filter(time_min_vehicle_cbc >= 1) %>% 
  # dropping non-unique respID (keeping first one)
  distinct(respID, .keep_all = TRUE) 
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
