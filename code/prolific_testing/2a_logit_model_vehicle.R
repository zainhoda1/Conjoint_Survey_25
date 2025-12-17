source(here::here('code', 'setup.R'))

# --------------------------------------------------------------------------
# Load the data set:

data <- read_csv(here(
  "data",
  "prolific_testing",
  "choice_data_vehicle.csv"
)) %>%
  select(-next_veh_budget)

head(data)

glimpse(data)

data <- data %>%
  mutate(
    price = price / 10000, # 0.5-6
    range_bev = range_bev / 100, # 0.5 - 2.5
    mileage = mileage / 10000, # 2 - 6
    age = age, # 2 - 8
    operating_cost = operating_cost / 10 # 0.3 - 2.5,
  )

# Dummy encode
data <- cbc_encode(
  data,
  coding = 'dummy',
  ref_levels = list(powertrain = 'gas')
)

glimpse(data)

data_car_low <- data %>%
  filter(vehicle_typecar == 1, budgetlow == 1)
data_car_high <- data %>%
  filter(vehicle_typecar == 1, budgetlow == 0)
data_suv_low <- data %>%
  filter(vehicle_typecar == 0, budgetlow == 1)
data_suv_high <- data %>%
  filter(vehicle_typecar == 0, budgetlow == 0)


# Estimate models

run_model <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "price",
      "mileage",
      "age",
      "range_bev",
      "operating_cost",
      "powertrainbev",
      "powertrainhev",
      "no_choice"
    )
  )
  cat('N =', length(unique(data$respID)))
  return(model)
}

# Estimate the model
model <- run_model(data_car_low)
model <- run_model(data_car_high)
model <- run_model(data_suv_low)
model <- run_model(data_suv_high)

# View summary of results
summary(model)

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values
