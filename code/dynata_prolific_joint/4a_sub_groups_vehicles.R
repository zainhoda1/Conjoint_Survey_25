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
  select(-starts_with('time') , -ends_with('button'), -respID) %>% 
  mutate(data_source = 'dynata')

data_raw_prolific <- read_parquet(here(
  "data",
  "prolific_testing",
  "data.parquet"
)) %>% 
  select(-starts_with('time'), -ends_with('button'), -respID) %>% 
  mutate(psid = prolific_pid,
         data_source = 'prolific') %>%
  select(-study_id, -prolific_session_id, -prolific_pid)


data_raw_prolific_round2 <-  read_parquet(here(
  "data",
  "prolific_testing",
  "data_round2_feb26.parquet"
))%>% 
  select(-starts_with('time'), -ends_with('button'), -respID) %>% 
  mutate(psid = prolific_pid,
         data_source = 'prolific') %>%
  select(-study_id, -prolific_session_id, -prolific_pid)


#########


data <- data_joint %>%
  mutate(
    price = price / 10000, # 0.5-6
    range_bev = range_bev / 100, # 0.5 - 2.5
    mileage = mileage / 10000, # 2 - 6
    age = age, # 2 - 8
    operating_cost = operating_cost / 10 # 0.3 - 2.5,
  )

data_raw_joined <- rbind(rbind(data_raw_dynata, data_raw_prolific), data_raw_prolific_round2)

######### 
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
  return (data)
}

#########






#data_raw_joined <- left_join(data_joint, data_raw_joined, by = c('psid', 'data_source' , 'budget', 'next_veh_budget'))

data_raw_joined %>% 
  group_by( primary_veh_fuel) %>% 
  count()

data_raw_joined %>% 
  group_by( charger_access) %>% 
  count()

data_raw_joined %>% 
  group_by( neighbor_ev_info) %>% 
  count()

data_raw_joined %>% 
  group_by( max_subsidy) %>% 
  count()

data_raw_joined %>% 
  group_by( next_veh_fuel_new_bev) %>% 
  count()

data_raw_joined %>% 
  group_by( next_veh_fuel_used_bev) %>% 
  count()


data_raw_joined %>% 
  group_by(next_veh_fuel_new_bev) %>% 
  count()


positive_group <- data_raw_joined %>% 
  filter((next_veh_fuel_new_bev %in% c('very_likely','somewhat_likely', 'neutral' )) | 
           (next_veh_fuel_new_bev %in% c('very_likely','somewhat_likely',  'neutral')) ) %>% 
  select (psid)

negative_group <- data_raw_joined %>% 
  filter((!next_veh_fuel_new_bev %in% c('very_likely','somewhat_likely', 'neutral' )) | 
           (!next_veh_fuel_new_bev %in% c('very_likely','somewhat_likely',  'neutral')) ) %>% 
  select (psid)


positive_group_encoded <- encoding(inner_join(data, positive_group, by = 'psid')
                                   %>% select (-psid))

negative_group_encoded <- encoding(inner_join(data, negative_group, by = 'psid')
                                   %>% select (-psid))


wtp_model_positive_group_car <- run_model_wtp(
  positive_group_encoded %>% filter(vehicle_typesuv == 0 )
)

wtp_model_negative_group_car <- run_model_wtp(
  negative_group_encoded %>% filter(vehicle_typesuv == 0 )
)

wtp_model_positive_group_suv <- run_model_wtp(
  positive_group_encoded %>% filter(vehicle_typesuv == 1 )
)

wtp_model_negative_group_suv <- run_model_wtp(
  negative_group_encoded %>% filter(vehicle_typesuv == 1 )
)




summary(wtp_model_positive_group_car)
summary(wtp_model_negative_group_car)
summary(wtp_model_positive_group_suv)
summary(wtp_model_negative_group_suv)



