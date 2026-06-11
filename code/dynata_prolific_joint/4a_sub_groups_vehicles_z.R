source(here::here('code', 'setup.R'))

#######
# Load the Dataset:

data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_vehicle.parquet"
 )) 
# |> 
#   filter(collection_round != 'round_4')


data_raw_dynata <- read_parquet(here(
  "data",
  "dynata_testing",
  "data.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'),
 -respID) %>%
  mutate(data_source = 'dynata')

data_raw_prolific <- read_parquet(here(
  "data",
  "prolific_testing",
  "data.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'), -respID) %>%
  mutate(psid = prolific_pid, data_source = 'prolific') %>%
  select(-study_id, -prolific_session_id, -prolific_pid, -current_page)


data_raw_prolific_round2 <- read_parquet(here(
  "data",
  "prolific_testing",
  "data_round_2+3+4_may_26.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'), -respID) %>%
  mutate(psid = prolific_pid, data_source = 'prolific') %>%
  select(-study_id, -prolific_session_id, -prolific_pid, -current_page
  )


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


likely_bev_adopter <- data_raw_joined %>%
  filter(
    (next_veh_fuel_new_bev %in%
      c('very_likely', 'somewhat_likely')) |   #, 'neutral'
      (next_veh_fuel_used_bev %in%
        c('very_likely', 'somewhat_likely'))  #, 'neutral'
  ) %>%
  select(psid)

nrow(likely_bev_adopter)

unlikely_bev_adopter <- data_raw_joined %>%
  filter(
    (next_veh_fuel_new_bev %in%
      c('very_unlikely', 'somewhat_unlikely'))  &
      (next_veh_fuel_used_bev %in%
        c('very_unlikely', 'somewhat_unlikely'))
  ) %>%
  select(psid, next_veh_fuel_new_bev, next_veh_fuel_used_bev)

nrow(unlikely_bev_adopter)



charger_access_yes_group <- data_raw_joined %>%
  filter(
    (charger_access  == 'yes')
  ) %>%
  select(psid)

charger_access_no_group <- data_raw_joined %>%
  filter(
    (!charger_access  == 'yes')
  ) %>%
  select(psid)

neighbor_ev_yes <- data_raw_joined %>%
  filter(
    (!charger_access  == 'yes')
  ) %>%
  select(psid)

likely_bev_adopter_encoded <- encoding(
  inner_join(data, likely_bev_adopter, by = 'psid') %>% select(-psid)
)

unlikely_bev_adopter_encoded <- encoding(
  inner_join(data, unlikely_bev_adopter, by = 'psid') %>% select(-psid)
)

charger_access_yes_encoded  <-  encoding(
  inner_join(data, charger_access_yes_group, by = 'psid') %>% select(-psid)
)

charger_access_no_encoded  <-  encoding(
  inner_join(data, charger_access_no_group, by = 'psid') %>% select(-psid)
)

neighbor_ev_yes_encodeing <- encoding(
  inner_join(data, neighbor_ev_yes, by = 'psid') %>% select(-psid)
)

model_positive_vehicle <- run_model(likely_bev_adopter_encoded)

model_negative_vehicle <- run_model(unlikely_bev_adopter_encoded )

model_likely_bev_adopter_car <- run_model(
  likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 0)
)

model_unlikely_bev_adopter_car <- run_model(
  unlikely_bev_adopter_encoded %>% filter(vehicle_typesuv == 0)
)

model_likely_bev_adopter_suv <- run_model(
  likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 1)
)

model_unlikely_bev_adopter_suv <- run_model(
  unlikely_bev_adopter_encoded %>% filter(vehicle_typesuv == 1)
)

model_charger_access_yes <- run_model(charger_access_yes_encoded)

model_charger_access_no <- run_model(charger_access_no_encoded)

model_neighbor_ev_yes_encodeing <- run_model(neighbor_ev_yes_encodeing)


wtp_model_likely_bev_adopter_car <- run_model_wtp(
  likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 0)
)

wtp_model_unlikely_bev_adopter_car <- run_model_wtp(
  unlikely_bev_adopter_encoded %>% filter(vehicle_typesuv == 0)
)

wtp_model_likely_bev_adopter_suv <- run_model_wtp(
  likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 1)
)

wtp_model_unlikely_bev_adopter_suv <- run_model_wtp(
  unlikely_bev_adopter_encoded %>% filter(vehicle_typesuv == 1)
)

#####################################################################

wtp_model_likely_bev_adopter_vehicle_low <- run_model_wtp(
  likely_bev_adopter_encoded %>% filter(budgethigh == 0 )
)

wtp_model_unlikely_bev_adopter_vehicle_low <- run_model_wtp(
  unlikely_bev_adopter_encoded %>% filter( budgethigh == 0 )
)

wtp_model_likely_bev_adopter_vehicle_high <- run_model_wtp(
  likely_bev_adopter_encoded %>% filter(budgethigh == 1 )
)

wtp_model_unlikely_bev_adopter_vehicle_high <- run_model_wtp(
  unlikely_bev_adopter_encoded %>% filter( budgethigh == 1 )
)


wtp_model_likely_bev_adopter_car_low <- run_model_wtp(
  likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 0, budgethigh == 0 )
)

wtp_model_unlikely_bev_adopter_car_low <- run_model_wtp(
  unlikely_bev_adopter_encoded %>% filter(vehicle_typesuv == 0, budgethigh == 0 )
)

wtp_model_likely_bev_adopter_car_high <- run_model_wtp(
  likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 0, budgethigh == 1 )
)

wtp_model_unlikely_bev_adopter_car_high <- run_model_wtp(
  unlikely_bev_adopter_encoded %>% filter(vehicle_typesuv == 0,  budgethigh == 1 )
)

wtp_model_likely_bev_adopter_suv_low <- run_model_wtp(
  likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 1, budgethigh == 0 )
)

wtp_model_unlikely_bev_adopter_suv_low <- run_model_wtp(
  unlikely_bev_adopter_encoded %>% filter(vehicle_typesuv == 1, budgethigh == 0 )
)

wtp_model_likely_bev_adopter_suv_high <- run_model_wtp(
  likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 1, budgethigh == 1 )
)

wtp_model_unlikely_bev_adopter_suv_high <- run_model_wtp(
  unlikely_bev_adopter_encoded %>% filter(vehicle_typesuv == 1, budgethigh == 1 )
)

#####################

summary(model_likely_bev_adopter_car)
summary(model_unlikely_bev_adopter_car)
summary(model_likely_bev_adopter_suv)
summary(model_unlikely_bev_adopter_suv)
summary(model_charger_access_yes)
summary(model_charger_access_no)
summary(model_neighbor_ev_yes_encodeing)


summary(wtp_model_likely_bev_adopter_car)
summary(wtp_model_unlikely_bev_adopter_car)
summary(wtp_model_likely_bev_adopter_suv)
summary(wtp_model_unlikely_bev_adopter_suv)

# summary(wtp_model_likely_bev_adopter_car_low)
# summary(wtp_model_unlikely_bev_adopter_car_low)
# summary(wtp_model_likely_bev_adopter_car_high)
# summary(wtp_model_unlikely_bev_adopter_car_high)

# summary(wtp_model_likely_bev_adopter_suv_low)
# summary(wtp_model_unlikely_bev_adopter_suv_low)
# summary(wtp_model_likely_bev_adopter_suv_high)
# summary(wtp_model_unlikely_bev_adopter_suv_high)

#####################



#Generate confidence intervals:

conf_model_positive_vehicle <- create_confidence_intervals(model_positive_vehicle)
conf_model_negative_vehicle <- create_confidence_intervals(model_negative_vehicle)

conf_model_likely_bev_adopter_car <- create_confidence_intervals(model_likely_bev_adopter_car)
conf_model_unlikely_bev_adopter_car <- create_confidence_intervals(model_unlikely_bev_adopter_car)
conf_model_likely_bev_adopter_suv <- create_confidence_intervals(model_likely_bev_adopter_suv)
conf_model_unlikely_bev_adopter_suv <- create_confidence_intervals(model_unlikely_bev_adopter_suv)

conf_model_charger_access_yes <- create_confidence_intervals(model_charger_access_yes)
conf_model_charger_access_no <- create_confidence_intervals(model_charger_access_no)

conf_model_neighbor_ev_yes_encodeing <- create_confidence_intervals(model_neighbor_ev_yes_encodeing)



conf_model_positive_vehicle

conf_model_negative_vehicle

conf_model_likely_bev_adopter_car

conf_model_unlikely_bev_adopter_car

conf_model_likely_bev_adopter_suv

conf_model_unlikely_bev_adopter_suv

conf_model_charger_access_yes

conf_model_charger_access_no

conf_model_neighbor_ev_yes_encodeing

#######################



######################################

# Save model object

save(
  model_likely_bev_adopter_car,
  file = here("models", "model_likely_bev_adopter_car.RData")
)

save(
  model_unlikely_bev_adopter_car,
  file = here("models", "model_unlikely_bev_adopter_car.RData")
)

save(
  model_likely_bev_adopter_suv,
  file = here("models", "model_likely_bev_adopter_suv.RData")
)

save(
  model_unlikely_bev_adopter_suv,
  file = here("models", "model_unlikely_bev_adopter_suv.RData")
)

save(
  model_charger_access_yes,
  file = here("models", "model_charger_access_yes.RData")
)

save(
  model_charger_access_no,
  file = here("models", "model_charger_access_no.RData")
)



############################################################


 data_raw_joined %>%
  filter(
    (next_veh_fuel_new_bev %in%
      c('very_likely', 'somewhat_likely', 'neutral')) |
      (next_veh_fuel_used_bev %in%
        c('very_likely', 'somewhat_likely', 'neutral'))
  )  |> 
   group_by(education) |> summarise(n = n())

