library(tidyverse)
library(cbcTools)
library(logitr)
library(here)
library(data.table)


# ---- Car Survey ----
## ----profile----
### ----car----
#### ----budget <= 20k ----

profiles_car_low <- cbc_profiles(
  powertrain = c('gas', 'bev', 'hev'),
  price = seq(1.0, 2.0, 0.2), # unit: 10000
  range_bev = c(0, seq(0.5, 1.5, 0.5)), # unit: 100
  mileage = seq(2, 6, 0.5), # unit: 10000
  age = seq(0.2, 1.0, 0.2), # make_year changed to age / unit: 10
  operating_cost = seq(0.3, 1.8, 0.3) # unit: 10
)

#### ----budget > 20k ----

profiles_car_high <- cbc_profiles(
  powertrain = c('gas', 'bev', 'hev'),
  price = seq(2.0, 5.0, 0.5), # unit: 10000
  range_bev = c(0, seq(1.0, 2.5, 0.5)), # unit: 100
  mileage = seq(2, 6, 0.5), # unit: 10000
  age = seq(0.2, 1.0, 0.2), # make_year changed to age  / unit: 10
  operating_cost = seq(0.3, 1.8, 0.3) # unit: 10
)


#### ----restrictions ----

profiles_restricted_car <- cbc_restrict(
  profiles_car_low, # un-comment low or high based on profile
  #profiles_car_high,

  # BEV range restrictions
  (powertrain == "gas") & (range_bev != 0),
  (powertrain == "hev") & (range_bev != 0),
  (powertrain == "bev") & (range_bev == 0),
  # Gas efficiency restrictions
  (powertrain == "gas") & (operating_cost < 0.8),
  (powertrain == "bev") & (operating_cost > 1.2),
  (powertrain == "hev") & (operating_cost < 0.6),
  (powertrain == "hev") & (operating_cost > 1.2)
)


### ---- SUV----
#### ----budget <= 20k----
profiles_battery_car_low <- cbc_profiles(
  veh_mileage = seq(1.5, 5, 0.5), # unit: 10000
  veh_price = c(1.5, 2, 2.5, 3), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = c(0.5, 1, 1.5, 2, 2.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)

profiles_battery_car_20K_above <- cbc_profiles(
  veh_mileage = seq(1.5, 5, 0.5), # unit: 10000
  veh_price = c(1.5, 2, 2.5, 3), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = c(0.5, 1, 1.5, 2, 2.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)


profiles_battery_suv_20K_above <- cbc_profiles(
  veh_mileage = seq(1.5, 5, 0.5), # unit: 10000
  veh_price = c(2, 2.5, 3, 3.5), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = c(1.5, 2, 2.5, 3, 3.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)

profiles_battery_suv_20K_below <- cbc_profiles(
  veh_mileage = seq(1.5, 5, 0.5), # unit: 10000
  veh_price = c(2, 2.5, 3, 3.5), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = c(1.5, 2, 2.5, 3, 3.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)

profiles_suv_low <- cbc_profiles(
  powertrain = c('gas', 'bev', 'hev'),
  price = seq(1.5, 2.0, 0.1), # unit: 10000
  range_bev = seq(1.5, 2.5, 0.5), # unit: 100
  mileage = seq(2, 6, 0.5), # unit: 10000
  age = seq(0.2, 1.0, 0.2), # make_year changed to age  / unit: 10
  operating_cost = seq(0.3, 1.8, 0.3) # unit: 10
)

#### ---- budget > 20k----

profiles_suv_high <- cbc_profiles(
  powertrain = c('gas', 'bev', 'hev'),
  price = seq(2.0, 5.0, 0.5), # unit: 10000
  range_bev = seq(2.0, 3.5, 0.5), # unit: 100
  mileage = seq(2, 6, 0.5), # unit: 10000
  age = seq(0.2, 1.0, 0.2), # make_year changed to age /  unit: 10
  operating_cost = seq(0.3, 1.8, 0.3) # unit: 10
)


#### ----restrictions----

profiles_restricted_suv <- cbc_restrict(
  profiles_suv_low, # un-comment low or high based on profile
  #profiles_suv_high,

  # BEV range restrictions
  (powertrain == "gas") & (range_bev != 0),
  (powertrain == "hev") & (range_bev != 0),
  (powertrain == "bev") & (range_bev == 0),

  # Gas efficiency restrictions
  (powertrain == "gas") & (operating_cost < 0.8),
  (powertrain == "bev") & (operating_cost > 1.2),
  (powertrain == "hev") & (operating_cost < 0.8)
)


# ---- Battery Survey----
## ---- Profile ----
### ----car----
#### ---- budget <= 20k----

profiles_battery_car_low <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(1.0, 2.0, 0.2), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = c(0.5, 1.0, 1.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)


#### ----budget > 20k----

profiles_battery_car_high <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(2.0, 4.0, 0.5), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = c(1.0, 1.5, 2.0, 2.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)


###----SUV----
####----budget <= 20k----
profiles_battery_suv_low <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(1.5, 2.5, 0.2), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = seq(1.5, 2.5, 0.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)

####----budget > 20k----
profiles_battery_suv_high <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(2.5, 4.5, 0.5), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = seq(2.0, 3.5, 0.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)


## ---- Set up priors----

priors_fixed_car_low <- cbc_priors(
  profiles = profiles_car_low,
  veh_mileage = -0.1,
  veh_price = -1.2,
  battery_refurbish = c(-0.3, -0.1),
  battery_range_year0 = 0.4,
  battery_degradation = -0.3,
  no_choice = -1.0
)

priors_fixed_car_high <- cbc_priors(
  profiles = profiles_car_high,
  veh_mileage = -0.2, # Each 10000 mile increase reduces utility by 0.5
  veh_price = -1.0, # Each $10000 increase reduces utility by 1
  battery_refurbish = c(-0.4, -0.2), # Cell refurbishment least preferred
  battery_range_year0 = 0.3, # Each 100 mile of range adds utility by 0.3
  battery_degradation = -0.2, # Each 1% of degradation increases subtracts utility by 0.2
  no_choice = -2.0 # There is a strong positive preference for EV, so positive for "no_choice"
)


priors_fixed_suv_low <- cbc_priors(
  profiles = profiles_suv_low,
  veh_mileage = -0.15,
  veh_price = -1.1,
  battery_refurbish = c(-0.35, -0.15),
  battery_range_year0 = 0.45,
  battery_degradation = -0.35,
  no_choice = -0.5
)

priors_fixed_suv_high <- cbc_priors(
  profiles = profiles_suv_high,
  veh_mileage = -0.25,
  veh_price = -0.9,
  battery_refurbish = c(-0.45, -0.25),
  battery_range_year0 = 0.35,
  battery_degradation = -0.25,
  no_choice = -1.5
)

##---- Generate Designs----
n_respondents <- 1000

design_car_low_minoverlap <- cbc_design(
  profiles = profiles_car_low,
  method = 'minoverlap',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_car_low,
  remove_dominant = TRUE
)

design_car_high_minoverlap <- cbc_design(
  profiles = profiles_car_high,
  method = 'minoverlap',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_car_high,
  remove_dominant = TRUE
)

design_suv_low_minoverlap <- cbc_design(
  profiles = profiles_suv_low,
  method = 'minoverlap',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_suv_low,
  remove_dominant = TRUE
)

design_suv_high_minoverlap <- cbc_design(
  profiles = profiles_suv_high,
  method = 'minoverlap',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_suv_high,
  remove_dominant = TRUE
)

saveRDS(
  design_car_low_minoverlap,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_car_low_minoverlap.Rds'
  )
)

saveRDS(
  design_car_high_minoverlap,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_car_high_minoverlap.Rds'
  )
)

saveRDS(
  design_suv_low_minoverlap,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_suv_low_minoverlap.Rds'
  )
)

saveRDS(
  design_suv_low_minoverlap,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_suv_low_minoverlap.Rds'
  )
)

## ---- Output----
design_battery <- rbind(
  design_car_low_minoverlap %>% mutate(vehicle_type = "car", budget = "low"),
  design_car_high_minoverlap %>%
    mutate(vehicle_type = "car", budget = "high"),
  design_suv_low_minoverlap %>% mutate(vehicle_type = "suv", budget = "low"),
  design_suv_high_minoverlap %>% mutate(vehicle_type = "suv", budget = "high")
)

design_rand_output_battery <- design_battery %>%
  mutate(
    veh_mileage = veh_mileage * 10000,
    battery_degradation = battery_degradation / 100,
    battery_range_year0 = battery_range_year0 * 100
  ) %>%
  mutate(
    battery_health_year0 = paste0(round(1 * 100, 0), "%"),
    battery_health_year3 = (1 - battery_degradation)^3,
    battery_health_year8 = (1 - battery_degradation)^8, # round to the closest 5
    battery_range_year3 = round(
      battery_range_year0 * battery_health_year3 / 5
    ) *
      5,
    battery_range_year8 = round(
      battery_range_year0 * battery_health_year8 / 5
    ) *
      5,
    battery_health_year3 = paste0(
      round((1 - battery_degradation)^3 * 100, 0),
      "%"
    ),
    battery_health_year8 = paste0(
      round((1 - battery_degradation)^8 * 100, 0),
      "%"
    )
  ) %>%
  mutate(
    battery_condition = case_when(
      no_choice == 1 ~ NA,
      battery_refurbishcellreplace == 1 ~ 'Refurbished Cell-Replaced',
      battery_refurbishpackreplace == 1 ~ 'Refurbished Pack-Replaced',
      TRUE ~ 'Original Battery'
    )
  ) %>%
  select(-battery_refurbishcellreplace, -battery_refurbishpackreplace)


#write.csv(design_rand_output_battery, here('survey', 'data', 'battery_choice_questions.csv'))

arrow::write_parquet(
  design_rand_output_battery,
  here('survey', 'data', 'design_battery.parquet')
)

saveRDS(
  design_rand_output_battery,
  here('data', 'design_battery.Rds')
)
