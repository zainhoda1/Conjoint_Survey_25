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
data <- data_raw 
  # Select important columns
  # select(
  #   session_id,
  #   time_start,

  #   time_min_total,
  #   time_min_vehicle_cbc,
  #   respID,
  #   next_veh_budget,
  #   next_veh_style,
  #   starts_with("vehicle_cbc_q")
  # )

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
summary(data$time_min_vehicle_cbc)

# Drop anyone who finished the choice question section in under 1 minute
data <- data %>%
  filter(time_min_vehicle_cbc >= 0.5) %>%
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


choice_data <- choice_data %>%
  left_join(survey, by = c("vehicle_type", "respID", "qID"))


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

choice_data<- choice_data %>% 
  select(-starts_with('time'))

# # Save cleaned data for modeling
# write_csv(
#   choice_data,
#   here(
#     "data",
#     "main",
#     "vehicle_choice_data.csv"
#   )
# )


choice_data %>% 
  group_by( primary_veh_fuel) %>% 
  count()

choice_data %>% 
  group_by( charger_access) %>% 
  count()

choice_data %>% 
  group_by( neighbor_ev_info) %>% 
  count()

choice_data %>% 
  group_by( max_subsidy) %>% 
  count()

choice_data %>% 
  group_by( next_veh_fuel_new_bev) %>% 
  count()

choice_data %>% 
  group_by( next_veh_fuel_used_bev) %>% 
  count()


########  selecting those who chose BEV at least once:
temp1 <- choice_data %>% 
  select(qID, choice, respID, psid, session_id, altID, vehicle_type) 

temp2 <- survey %>% 
  select (profileID, respID, qID, powertrain, vehicle_type, altID) 

temp3 <- temp1 %>%
  left_join(temp2, by = c("vehicle_type", "respID", "qID", 'altID')) 

temp4 <- temp3 %>%
  group_by(respID) %>%
  summarize(count = sum(choice == 1 & powertrain == "bev")) %>% 
  filter(count>0) 


selected_bev_list <- temp4$respID


##################

grouping <- 'next_vehicle_bev'

positive_group <-  choice_data %>% 
  #filter(max_subsidy == 7500) %>% 
  #filter(respID %in%  selected_bev_list) %>% 
  filter((next_veh_fuel_new_bev %in% c('very_likely','somewhat_likely' )) | (next_veh_fuel_new_bev %in% c('very_likely','somewhat_likely')) ) 




  
  data <- positive_group %>%
    mutate(
      price = price / 10000, # 0.5-6
      range_bev = range_bev / 100, # 0.5 - 2.5
      range_phev = range_phev / 10, # 1 - 4
      mileage = mileage * 10, # 2 - 6
      age = age * 10, # 2 - 8
      operating_cost = operating_cost # 0.3 - 2.5,
    ) 
  
  # Dummy encode
  data <- cbc_encode(
    data,
    coding = 'dummy',
    ref_levels = list(powertrain = 'gas')
  ) %>%
    as.data.frame()
  
  # glimpse(data)
  
  # Estimate MNL model
  
  # First create some dummy coded variables for categorical variables
  #data <- dummy_cols(data, c('powertrain'))
  
  # data %>%
  #   count(choice, qID)
  
  # data %>%
  #   count(no_choice, choice)
  
  # ---- Estimate MNL model (range:yr0 + degradation) ----
  ## --- Preference Space ----
  mnl_pref <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "price",
      "mileage",
      "age",
      "operating_cost",
      "range_bev",
      "range_phev",
      "powertrainbev",
      "powertrainphev",
      "powertrainhev",
      "no_choice"
    )
  )
  
  
  # View summary of results
  summary(mnl_pref)
  
  # Check the 1st order condition: Is the gradient at the solution zero?
  mnl_pref$gradient
  
  # 2nd order condition: Is the hessian negative definite?
  # (If all the eigenvalues are negative, the hessian is negative definite)
  eigen(mnl_pref$hessian)$values
  
  ## --- WTP Space ----
  
  mnl_wtp_pos <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "mileage",
      "age",
      "operating_cost",
      "range_bev",
      "range_phev",
      "powertrainbev",
      "powertrainphev",
      "powertrainhev",
      "no_choice"
    ),
    scalePar = "price",
    numMultiStarts = 20,
    numCores = 1
  )
  
  # View summary of results
  summary(mnl_wtp_pos)
  
  
  #########################################
  
  
  negative_group <-  choice_data %>% 
    #filter(max_subsidy == 7500) %>% 
    #filter(!respID %in%  selected_bev_list)
  filter((!next_veh_fuel_new_bev %in% c('very_likely','somewhat_likely' )) & (!next_veh_fuel_new_bev %in% c('very_likely','somewhat_likely' )) )
  
  
  data <- negative_group %>%
    mutate(
      price = price / 10000, # 0.5-6
      range_bev = range_bev / 100, # 0.5 - 2.5
      range_phev = range_phev / 10, # 1 - 4
      mileage = mileage * 10, # 2 - 6
      age = age * 10, # 2 - 8
      operating_cost = operating_cost # 0.3 - 2.5,
    ) 
  
  # Dummy encode
  data <- cbc_encode(
    data,
    coding = 'dummy',
    ref_levels = list(powertrain = 'gas')
  ) %>%
    as.data.frame()
  
  # glimpse(data)
  
  # Estimate MNL model
  
  # First create some dummy coded variables for categorical variables
  #data <- dummy_cols(data, c('powertrain'))
  
  # data %>%
  #   count(choice, qID)
  
  # data %>%
  #   count(no_choice, choice)
  
  # ---- Estimate MNL model (range:yr0 + degradation) ----
  ## --- Preference Space ----
  mnl_pref <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "price",
      "mileage",
      "age",
      "operating_cost",
      "range_bev",
      "range_phev",
      "powertrainbev",
      "powertrainphev",
      "powertrainhev",
      "no_choice"
    )
  )
  
  
  # View summary of results
  summary(mnl_pref)
  
  # Check the 1st order condition: Is the gradient at the solution zero?
  mnl_pref$gradient
  
  # 2nd order condition: Is the hessian negative definite?
  # (If all the eigenvalues are negative, the hessian is negative definite)
  eigen(mnl_pref$hessian)$values
  
  ## --- WTP Space ----
  
  mnl_wtp_neg <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "mileage",
      "age",
      "operating_cost",
      "range_bev",
      "range_phev",
      "powertrainbev",
      "powertrainphev",
      "powertrainhev",
      "no_choice"
    ),
    scalePar = "price",
    numMultiStarts = 20,
    numCores = 1
  )
  
  # View summary of results
  summary(mnl_wtp_neg)
  
  ###########################################
  
  summary(mnl_wtp_pos)
  summary(mnl_wtp_neg)
  
  wtp_pos_group <- round(10000 * (as.numeric(mnl_wtp_pos$coefficients[7]) + 2 * as.numeric(mnl_wtp_pos$coefficients[5])), 2)
  wtp_neg_group <- round(10000 * (as.numeric(mnl_wtp_neg$coefficients[7]) + 2 * as.numeric(mnl_wtp_neg$coefficients[5])), 2)
  
    
  print(paste0("Willingness to pay for models based on ", grouping ))
  print(paste0("positive group : ",wtp_pos_group))
  print(paste0("negative group : ",wtp_neg_group))
``  
  
        
        
  