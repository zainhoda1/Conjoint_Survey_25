library(tidyverse)
library(cbcTools)
library(logitr)
library(here)
library(data.table)


## Car
# Car with budget <= 20k

profiles_car_low <- cbc_profiles(
  powertrain = c('gas', 'bev','hev'),
  price = seq(1.0, 2.0, 0.2), # unit: 10000
  range_bev = c(0, seq(0.5, 1.5, 0.5)), # unit: 100
  mileage = seq(2, 6, 0.5), # unit: 10000
  age = seq(0.2, 1.0, 0.2), # make_year changed to age / unit: 10
  operating_cost = seq(0.3, 1.8, 0.3) # unit: 10
)

# Car with budget > 20k

profiles_car_high <- cbc_profiles(
  powertrain = c('gas','bev','hev'),
  price = seq(2.0, 5.0, 0.5), # unit: 10000
  range_bev = c(0, seq(1.0, 2.5, 0.5)), # unit: 100
  mileage = seq(2, 6, 0.5), # unit: 10000
  age = seq(0.2, 1.0, 0.2), # make_year changed to age  / unit: 10
  operating_cost = seq(0.3, 1.8, 0.3) # unit: 10
)

######

# Restrictions 

profiles_restricted_car <- cbc_restrict(
  profiles_car_low,  # un-comment low or high based on profile
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


####################


## SUV
# SUV with budget <= 20k

profiles_suv_low <- cbc_profiles(
  powertrain = c('gas', 'bev','hev'),
  price = seq(1.5, 2.0, 0.1), # unit: 10000
  range_bev = seq(1.5, 2.5, 0.5), # unit: 100
  mileage = seq(2, 6, 0.5), # unit: 10000
  age = seq(0.2, 1.0, 0.2), # make_year changed to age  / unit: 10
  operating_cost = seq(0.3, 1.8, 0.3) # unit: 10
)

# SUV with budget > 20k

profiles_suv_high <- cbc_profiles(
  powertrain = c('gas', 'bev','hev'),
  price = seq(2.0, 5.0, 0.5), # unit: 10000
  range_bev = seq(2.0, 3.5, 0.5), # unit: 100
  mileage = seq(2, 6, 0.5), # unit: 10000
  age = seq(0.2, 1.0, 0.2), # make_year changed to age /  unit: 10
  operating_cost = seq(0.3, 1.8, 0.3) # unit: 10
)

############
# Restrictions 

profiles_restricted_suv <- cbc_restrict(
  profiles_suv_low,  # un-comment low or high based on profile
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


##################

### Battery Section:

#Car
# budget <= 20k

profiles_battery_car_low <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(1.0 ,2.0 , 0.2), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = ? , # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)


# budget > 20k

profiles_battery_car_high <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(2.0, 5.0, 0.5), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = ? , # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)

########

#SUV
# budget <= 20k

profiles_battery_suv_low <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(1.5, 2.0, 0.1), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = ? , # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)

# budget > 20k

profiles_battery_suv_high <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(2.0, 5.0, 0.5), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = ? , # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)