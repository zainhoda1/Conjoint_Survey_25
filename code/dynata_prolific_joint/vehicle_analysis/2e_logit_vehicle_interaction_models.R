# After running both Dynata and Prolific
source(here::here('code', 'setup.R'))

# --------------------------------------------------------------------------
# Load the data set:
data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_vehicle.parquet"
))


create_confidence_intervals_interactions <- function(model) {
  # Description:
  # This function takes logit model and returns confidence interval.
  
  coefs <- coef(model)
  # Get the model coefficients and covariance matrix
  covariance <- vcov(model)
  
  # Take 10,000 draws of the coefficients
  coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
  
  # Compute WTP for each coefficient draw
  wtp_draws = -1 * (coef_draws[,] / coef_draws[, 'price'])
  
  # Adding dollar values
  wtp_draws <- wtp_draws %>%
    mutate(across(where(is.numeric), ~ .x * 10^4)) 
    # mutate(
    #   BEV100 = (powertrainbev + range_bev ) ,
    #   BEV200 = (powertrainbev + range_bev * 2) ,
    #   BEV300 = (powertrainbev + range_bev * 3) 
    # )
  
  # For each coefficient, get the mean and 95% confidence interval of WTP
  wtp_ci <- ci(wtp_draws, level = 0.95)%>%
    mutate(across(everything(), ~ round(.x, 2)))
  
  return(wtp_ci)
}

data_joint %>%
  group_by(data_source) %>%
  count()


data <- data_joint %>%
  mutate(
    price = price / 10000, # 0.5-6
    range_bev = range_bev / 100, # 0.5 - 2.5
    mileage = mileage / 10000, # 2 - 6
    age = age, # 2 - 8
    operating_cost = operating_cost / 10,  # 0.3 - 2.5
    log_range_bev = case_when(
      powertrain == 'bev' ~ log(range_bev),
      .default = range_bev
    )
  ) %>%
  select(-psid)

# glimpse(data)

# Dummy encode
data <- cbc_encode(
  data,
  coding = 'dummy',
  ref_levels = list(
    powertrain = 'gas',
    vehicle_type = 'car',
    budget = 'low',
    data_source = 'prolific'
  )
)


# Estimate models

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
run_model_interaction_price_range <- function(data) {
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
      "no_choice",
      "price*range_bev"
    )
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}
run_model_interaction_price_mileage <- function(data) {
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
      "no_choice",
      "price*mileage"
    )
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}
run_model_interaction_price_age <- function(data) {
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
      "no_choice",
      "price*age"
    )
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}

run_model_log_range <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "powertrainbev",
      "powertrainhev",
      "log_range_bev",
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


# Estimate the model

# Base Models:
model_car <- run_model(
  data %>% filter(vehicle_typesuv == 0))

model_suv <- run_model(
  data %>% filter(vehicle_typesuv == 1))

# Interaction Models:
##############
model_car_int_p_a <- run_model_interaction_price_age(
  data %>% filter(vehicle_typesuv == 0))

model_suv_int_p_a <- run_model_interaction_price_age(
  data %>% filter(vehicle_typesuv == 1))

model_car_int_p_r <- run_model_interaction_price_range(
  data %>% filter(vehicle_typesuv == 0))

model_suv_int_p_r <- run_model_interaction_price_range(
  data %>% filter(vehicle_typesuv == 1))

model_car_int_p_m <- run_model_interaction_price_mileage(
  data %>% filter(vehicle_typesuv == 0))

model_suv_int_p_m <- run_model_interaction_price_mileage(
  data %>% filter(vehicle_typesuv == 1))

##########

# Log Range Models
###############
model_car_log_r <- run_model_log_range(
  data %>% filter(vehicle_typesuv == 0))

model_suv_log_r <- run_model_log_range(
  data %>% filter(vehicle_typesuv == 1))

model_car_low_log_r <- run_model_log_range(
  data %>% filter(vehicle_typesuv == 0 & budgethigh == 0)
)
model_car_high_log_r <- run_model_log_range(
  data %>% filter(vehicle_typesuv == 0 & budgethigh == 1)
)
model_suv_low_log_r <- run_model_log_range(
  data %>% filter(vehicle_typesuv == 1 & budgethigh == 0)
)
model_suv_high_log_r <- run_model_log_range(
  data %>% filter(vehicle_typesuv == 1 & budgethigh == 1)
)


### View summary of results
##########


summary(model_car)
summary(model_suv)

summary(model_car_int_p_a)
summary(model_suv_int_p_a)

summary(model_car_int_p_r)
summary(model_suv_int_p_r)

summary(model_car_int_p_m)
summary(model_suv_int_p_m)

summary(model_car_log_r)
summary(model_suv_log_r)

summary(model_car_low_log_r)
summary(model_suv_low_log_r)

summary(model_car_high_log_r)
summary(model_suv_high_log_r)


### Confidence Intervals
################

# Confidence Intervals:

conf_model_car_int_p_a <- create_confidence_intervals_interactions(model_car_int_p_a)
conf_model_suv_int_p_a <- create_confidence_intervals_interactions(model_suv_int_p_a)

conf_model_car_int_p_r <- create_confidence_intervals_interactions(model_car_int_p_r)
conf_model_suv_int_p_r <- create_confidence_intervals_interactions(model_suv_int_p_r)

conf_model_car_int_p_m <- create_confidence_intervals_interactions(model_car_int_p_m)
conf_model_suv_int_p_m <- create_confidence_intervals_interactions(model_suv_int_p_m)


conf_model_car_int_p_a
conf_model_suv_int_p_a

conf_model_car_int_p_r
conf_model_suv_int_p_r

conf_model_car_int_p_m
conf_model_suv_int_p_m
