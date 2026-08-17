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

run_mixed_model_1 <- function(data) {
  
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    panelID = "respID",
    pars = c(
      "powertrainbev",
      "powertrainhev",
      "range_bev",
      "mileage",
      "age",
      "operating_cost",
      "no_choice"
    ),
    randPars = c(powertrainbev = 'n',
                 powertrainhev = 'n',
                 range_bev = 'n',
                 mileage = 'n',
                 age = 'n',
                 operating_cost = 'n',
                 no_choice = 'n'
    ),
    scalePar = 'price',
    drawType = 'sobol',
    numDraws = 5000,
    numMultiStarts = 10
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
      (next_veh_fuel_used_bev %in%
        c('very_likely', 'somewhat_likely'))  #, 'neutral'
  ) %>%
  select(psid)

nrow(likely_bev_adopter)


likely_bev_adopter_encoded <- encoding(
  inner_join(data, likely_bev_adopter, by = 'psid') |>
    select(-psid, -collection_round, -data_source))
  
data_car <- encoding (data |> 
  select (-psid, -collection_round, -data_source) ) |> 
  filter(vehicle_typesuv == 0)

data_suv <- encoding (data |> 
  select (-psid, -collection_round, -data_source) ) |> 
  filter(vehicle_typesuv == 1)

data_car_low <- data_car |> 
  filter(budgethigh == 0)

data_car_high <- data_car |> 
  filter(budgethigh == 1)

data_suv_low <- data_suv |> 
  filter(budgethigh == 0)

data_suv_high <- data_suv |> 
  filter(budgethigh == 1)



mixed_model_1_car <- run_mixed_model_1(data_car)
mixed_model_1_suv <- run_mixed_model_1(data_suv)
mixed_model_1_car_low <- run_mixed_model_1(data_car_low)
mixed_model_1_car_high <- run_mixed_model_1(data_car_high)
mixed_model_1_suv_low <- run_mixed_model_1(data_suv_low)
mixed_model_1_suv_high <- run_mixed_model_1(data_suv_high)

mixed_model_1_likely_bev_adopter <- run_mixed_model_1(likely_bev_adopter_encoded)

mixed_model_1_likely_bev_adopter_car <- run_mixed_model_1(
  likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 0))

mixed_model_1_likely_bev_adopter_suv <- run_mixed_model_1(
  likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 1))


######################################

# Save model object

save(
  mixed_model_1_car,
  file = here("models", "mixed_model_1_car.RData"))

save(
  mixed_model_1_suv,
  file = here("models", "mixed_model_1_suv.RData"))

save(
  mixed_model_1_car_low,
  file = here("models", "mixed_model_1_car_low.RData"))

save(
  mixed_model_1_car_high,
  file = here("models", "mixed_model_1_car_high.RData"))

save(
  mixed_model_1_suv_low,
  file = here("models", "mixed_model_1_suv_low.RData"))

save(
  mixed_model_1_suv_high,
  file = here("models", "mixed_model_1_suv_high.RData"))

save(
  mixed_model_1_likely_bev_adopter,
  file = here("models", "mixed_model_1_likely_bev_adopter.RData"))

save(
  mixed_model_1_likely_bev_adopter_car,
  file = here("models", "mixed_model_1_likely_bev_adopter_car.RData"))

save(
  mixed_model_1_likely_bev_adopter_suv,
  file = here("models", "mixed_model_1_likely_bev_adopter_suv.RData"))



############################################################

