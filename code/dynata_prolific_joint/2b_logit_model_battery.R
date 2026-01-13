# After running both Dynata and Prolific
source(here::here('code', 'setup.R'))

# --------------------------------------------------------------------------
# Load the data set:
data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_battery.parquet"
))


data <- data_joint %>%
  mutate(
    mileage = mileage / 10000, #3 - 6
    price = price / 10000, # 0.5 - 6
    battery_range_year0 = battery_range_year0 / 100, # 1-3
    battery_range_year3 = battery_range_year3 / 100, # 1-3
    battery_range_year8 = battery_range_year8 / 100, # 0.5 - 2
    battery_degradation = (battery_degradation * 10)
  ) %>%
  select(
    -starts_with("battery_health"),
    -starts_with("time")
  )

# glimpse(data)

# Dummy encode
data <- cbc_encode(
  data,
  coding = 'dummy',
  ref_levels = list(
    battery_refurbish = 'original',
    vehicle_type = 'car',
    budget = 'low'
  )
)


# Estimate MNL model

# First create some dummy coded variables for categorical variables
#data <- dummy_cols(data, c('battery_refurbish', 'degradation_high'))

# Clean up names of created variables

glimpse(data)

# Estimate models

run_model1 <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "price",
      "mileage",
      "battery_range_year3",
      "battery_range_year8",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",
      "no_choice"
    )
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}

run_model2 <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "price",
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",
      "no_choice"
    )
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}

data <- data %>%
  mutate(
    price_dynata = price * data_sourcedynata
  )
run_model2_datasource <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "price",
      "price_dynata",
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",
      "no_choice"
    )
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}

run_model_wtp <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "no_choice",
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace"
    ),
    scalePar = 'price',
    numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}

# Estimate the model 1

# Estimate the model
# model_car_low <- run_model1(data_car_low)
# model_car_high <- run_model1(data_car_high)
# model_suv_low <- run_model1(data_suv_low)
# model_suv_high <- run_model1(data_suv_high)
# model_car <- run_model1(data %>% filter(vehicle_typesuv == 0))
# model_suv <- run_model1(data %>% filter(vehicle_typesuv == 1))
# model_all <- run_model1(data)
model_dynata <- run_model2(data %>% filter(data_sourcedynata == 1))
model_prolific <- run_model2(data %>% filter(data_sourcedynata == 0))

# View summary of results
# summary(model_car_low)
# summary(model_car_high)
# summary(model_suv_low)
# summary(model_suv_high)
# summary(model_car)
# summary(model_suv)
# summary(model_all)
summary(model_dynata)
summary(model_prolific)


# Estimate the model 2

# Estimate the model
model_car <- run_model2(data %>% filter(vehicle_typesuv == 0))
model_suv <- run_model2(data %>% filter(vehicle_typesuv == 1))
# model_car_low <- run_model2(data_car_low)
# model_car_high <- run_model2(data_car_high)
# model_suv_low <- run_model2(data_suv_low)
# model_suv_high <- run_model2(data_suv_high)
model_all <- run_model2(data)

# View summary of results
# summary(model_car_low)
# summary(model_car_high)
# summary(model_suv_low)
# summary(model_suv_high)
summary(model_car)
summary(model_suv)
summary(model_all)
