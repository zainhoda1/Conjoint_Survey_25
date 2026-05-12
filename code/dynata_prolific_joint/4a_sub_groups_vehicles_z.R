source(here::here('code', 'setup.R'))

#######
# Load the Dataset:

data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_vehicle.parquet"
))


data_raw_dynata <- read_parquet(here(
  "data",
  "dynata_testing",
  "data.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'), -respID) %>%
  mutate(data_source = 'dynata')

data_raw_prolific <- read_parquet(here(
  "data",
  "prolific_testing",
  "data.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'), -respID) %>%
  mutate(psid = prolific_pid, data_source = 'prolific') %>%
  select(-study_id, -prolific_session_id, -prolific_pid)


data_raw_prolific_round2 <- read_parquet(here(
  "data",
  "prolific_testing",
  "data_round2_feb26.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'), -respID) %>%
  mutate(psid = prolific_pid, data_source = 'prolific') %>%
  select(-study_id, -prolific_session_id, -prolific_pid)


data_joint %>% 
  group_by( data_source) %>% 
  count()

#########

data <- data_joint %>%
  mutate(
    price = price / 10000, # 0.5-6
    range_bev = range_bev / 100, # 0.5 - 2.5
    mileage = mileage / 10000, # 2 - 6
    age = age, # 2 - 8
    operating_cost = operating_cost / 10 # 0.3 - 2.5,
  ) 

data_raw_joined <- rbind(
  rbind(data_raw_dynata, data_raw_prolific),
  data_raw_prolific_round2
)


data_raw_joined <- data_raw_joined |> 
  filter(psid %in% unique(data_joint$psid) )



#########

run_model <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "powertrainbev",
      "powertrainhev",
      "range_bev",
      "mileage",
      "age",
      "operating_cost",
      "price",
      "no_choice"
    )
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}

#WTP Model:

run_model_wtp <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "no_choice",
      "powertrainbev",
      "powertrainhev",
      "range_bev",
      "mileage",
      "age",
      "operating_cost"
    ),
    scalePar = 'price',
    numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}

encoding <- function(data) {
  data <- cbc_encode(
    data,
    coding = 'dummy',
    ref_levels = list(powertrain = 'gas', vehicle_type = 'car', budget = 'low')
  )
  return(data)
}

#########

#data_raw_joined <- left_join(data_joint, data_raw_joined, by = c('psid', 'data_source' , 'budget', 'next_veh_budget'))

data_raw_joined %>%
  group_by(primary_veh_fuel) %>%
  count()

data_raw_joined %>%
  group_by(charger_access) %>%
  count()

data_raw_joined %>%
  group_by(neighbor_ev_info) %>%
  count()

data_raw_joined %>%
  group_by(max_subsidy) %>%
  count()

data_raw_joined %>%
  group_by(next_veh_fuel_new_bev) %>%
  count()

data_raw_joined %>%
  group_by(next_veh_fuel_used_bev) %>%
  count()


data_raw_joined %>%
  group_by(next_veh_fuel_new_bev) %>%
  count()


positive_group <- data_raw_joined %>%
  filter(
    (next_veh_fuel_new_bev %in%
      c('very_likely', 'somewhat_likely', 'neutral')) |
      (next_veh_fuel_used_bev %in%
        c('very_likely', 'somewhat_likely', 'neutral'))
  ) %>%
  select(psid)

negative_group <- data_raw_joined %>%
  filter(
    (!next_veh_fuel_new_bev %in%
      c('very_likely', 'somewhat_likely', 'neutral')) |
      (!next_veh_fuel_used_bev %in%
        c('very_likely', 'somewhat_likely', 'neutral'))
  ) %>%
  select(psid)


positive_group_encoded <- encoding(
  inner_join(data, positive_group, by = 'psid') %>% select(-psid)
)

negative_group_encoded <- encoding(
  inner_join(data, negative_group, by = 'psid') %>% select(-psid)
)

model_positive_vehicle <- run_model(positive_group_encoded)

model_negative_vehicle <- run_model(negative_group_encoded )

model_positive_group_car <- run_model(
  positive_group_encoded %>% filter(vehicle_typesuv == 0)
)

model_negative_group_car <- run_model(
  negative_group_encoded %>% filter(vehicle_typesuv == 0)
)

model_positive_group_suv <- run_model(
  positive_group_encoded %>% filter(vehicle_typesuv == 1)
)

model_negative_group_suv <- run_model(
  negative_group_encoded %>% filter(vehicle_typesuv == 1)
)

wtp_model_positive_group_car <- run_model_wtp(
  positive_group_encoded %>% filter(vehicle_typesuv == 0)
)

wtp_model_negative_group_car <- run_model_wtp(
  negative_group_encoded %>% filter(vehicle_typesuv == 0)
)

wtp_model_positive_group_suv <- run_model_wtp(
  positive_group_encoded %>% filter(vehicle_typesuv == 1)
)

wtp_model_negative_group_suv <- run_model_wtp(
  negative_group_encoded %>% filter(vehicle_typesuv == 1)
)

#####################################################################

wtp_model_positive_group_vehicle_low <- run_model_wtp(
  positive_group_encoded %>% filter(budgethigh == 0 )
)

wtp_model_negative_group_vehicle_low <- run_model_wtp(
  negative_group_encoded %>% filter( budgethigh == 0 )
)

wtp_model_positive_group_vehicle_high <- run_model_wtp(
  positive_group_encoded %>% filter(budgethigh == 1 )
)

wtp_model_negative_group_vehicle_high <- run_model_wtp(
  negative_group_encoded %>% filter( budgethigh == 1 )
)


wtp_model_positive_group_car_low <- run_model_wtp(
  positive_group_encoded %>% filter(vehicle_typesuv == 0, budgethigh == 0 )
)

wtp_model_negative_group_car_low <- run_model_wtp(
  negative_group_encoded %>% filter(vehicle_typesuv == 0, budgethigh == 0 )
)

wtp_model_positive_group_car_high <- run_model_wtp(
  positive_group_encoded %>% filter(vehicle_typesuv == 0, budgethigh == 1 )
)

wtp_model_negative_group_car_high <- run_model_wtp(
  negative_group_encoded %>% filter(vehicle_typesuv == 0,  budgethigh == 1 )
)

wtp_model_positive_group_suv_low <- run_model_wtp(
  positive_group_encoded %>% filter(vehicle_typesuv == 1, budgethigh == 0 )
)

wtp_model_negative_group_suv_low <- run_model_wtp(
  negative_group_encoded %>% filter(vehicle_typesuv == 1, budgethigh == 0 )
)

wtp_model_positive_group_suv_high <- run_model_wtp(
  positive_group_encoded %>% filter(vehicle_typesuv == 1, budgethigh == 1 )
)

wtp_model_negative_group_suv_high <- run_model_wtp(
  negative_group_encoded %>% filter(vehicle_typesuv == 1, budgethigh == 1 )
)

#####################

summary(model_positive_group_car)
summary(model_negative_group_car)
summary(model_positive_group_suv)
summary(model_negative_group_suv)


summary(wtp_model_positive_group_car)
summary(wtp_model_negative_group_car)
summary(wtp_model_positive_group_suv)
summary(wtp_model_negative_group_suv)

# summary(wtp_model_positive_group_car_low)
# summary(wtp_model_negative_group_car_low)
# summary(wtp_model_positive_group_car_high)
# summary(wtp_model_negative_group_car_high)

# summary(wtp_model_positive_group_suv_low)
# summary(wtp_model_negative_group_suv_low)
# summary(wtp_model_positive_group_suv_high)
# summary(wtp_model_negative_group_suv_high)

#####################



#Generate confidence intervals:

conf_model_positive_vehicle <- create_confidence_intervals(model_positive_vehicle)
conf_model_negative_vehicle <- create_confidence_intervals(model_negative_vehicle)

conf_model_positive_group_car <- create_confidence_intervals(model_positive_group_car)
conf_model_negative_group_car <- create_confidence_intervals(model_negative_group_car)
conf_model_positive_group_suv <- create_confidence_intervals(model_positive_group_suv)
conf_model_negative_group_suv <- create_confidence_intervals(model_negative_group_suv)


conf_model_positive_vehicle

conf_model_negative_vehicle

conf_model_positive_group_car

conf_model_negative_group_car

conf_model_positive_group_suv

conf_model_negative_group_suv

#######################



######################################

# Save model object

save(
  model_positive_group_car,
  file = here("models", "model_positive_group_car.RData")
)

save(
  model_negative_group_car,
  file = here("models", "model_negative_group_car.RData")
)

save(
  model_positive_group_suv,
  file = here("models", "model_positive_group_suv.RData")
)

save(
  model_negative_group_suv,
  file = here("models", "model_negative_group_suv.RData")
)

