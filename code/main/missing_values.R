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
  'design_vehicle.parquet'
))


# Compute time values for each page
data <- data_raw %>%
  # Select important columns
  select(
    -starts_with("time")
  )



# Drop anyone who didn't complete all choice questions
data <- data %>%
  filter(!is.na(vehicle_cbc_q1_button)) %>%
  filter(!is.na(vehicle_cbc_q2_button)) %>%
  filter(!is.na(vehicle_cbc_q3_button)) %>%
  filter(!is.na(vehicle_cbc_q4_button)) %>%
  filter(!is.na(vehicle_cbc_q5_button)) %>%
  filter(!is.na(vehicle_cbc_q6_button))
# nrow(data)

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
# nrow(data)



na_counts <- data %>% 
  summarize_all(~sum(is.na(.))) %>% 
  pivot_longer(!session_id, names_to = 'columns', values_to = 'count_NA')


missing_household_veh_fuel<- data %>% 
  filter(is.na(household_veh_fuel) ,
         household_veh_count != 0
         ) %>% 
  select(psid, respID, household_veh_fuel, next_veh_budget, household_veh_count, primary_veh_cost, primary_veh_refuel,
         primary_veh_new_used, primary_veh_obtain_how, primary_veh_payment, primary_veh_range)

missing_primary_veh_payment <- data %>% 
  filter(is.na(primary_veh_payment) ,
         household_veh_count != 0) %>% 
   group_by(primary_veh_obtain_how) %>% 
   count()


missing_primary_veh_payment <- data %>% 
  filter(is.na(primary_veh_payment) ,
         household_veh_count != 0) %>% 
  select(psid, respID, household_veh_fuel, next_veh_budget, household_veh_count, primary_veh_cost, primary_veh_refuel,
         primary_veh_new_used, primary_veh_obtain_how, primary_veh_payment, primary_veh_range)

