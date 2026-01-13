# After running both Dynata and Prolific
source(here::here('code', 'setup.R'))

# --------------------------------------------------------------------------
# Load the data set:
data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_vehicle.parquet"
))


data <- data_joint %>%
  mutate(
    price = price / 10000, # 0.5-6
    range_bev = range_bev / 100, # 0.5 - 2.5
    mileage = mileage / 10000, # 2 - 6
    age = age, # 2 - 8
    operating_cost = operating_cost / 10 # 0.3 - 2.5,
  )

# glimpse(data)

# Dummy encode
data <- cbc_encode(
  data,
  coding = 'dummy',
  ref_levels = list(powertrain = 'gas', vehicle_type = 'car', budget = 'low')
)

## grouping
# data %>%
#   distinct(respID, vehicle_typesuv, budgethigh) %>%
#   group_by(vehicle_typesuv, budgethigh) %>%
#   summarise(n = n())

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

data <- data %>%
  mutate(
    price_dynata = price * data_sourcedynata,
    bev_dynata = powertrainbev * data_sourcedynata
  )

run_model_datasource <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "powertrainbev",
      "powertrainhev",
      "range_bev",
      "bev_dynata",
      "mileage",
      "age",
      "operating_cost",
      "price",
      "price_dynata",
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

# Estimate the model
# model_car <- run_model(data %>% filter(vehicle_typesuv == 0))
# model_suv <- run_model(data %>% filter(vehicle_typesuv == 1))
# model_car_low <- run_model(data_car_low)
# model_car_high <- run_model(data_car_high)
# model_suv_low <- run_model(data_suv_low)
# model_suv_high <- run_model(data_suv_high)
# model_all <- run_model(data)

model_datasource <- run_model_datasource(data)

wtp_model_dynata <- run_model_wtp(data %>% filter(data_sourcedynata == 1))
wtp_model_prolific <- run_model_wtp(data %>% filter(data_sourcedynata == 0))
wtp_model_car <- run_model_wtp(data %>% filter(vehicle_typesuv == 0))
wtp_model_suv <- run_model_wtp(data %>% filter(vehicle_typesuv == 1))
wtp_model_car_low <- run_model_wtp(
  data %>% filter(vehicle_typesuv == 0 & budgethigh == 0)
)
wtp_model_car_high <- run_model_wtp(
  data %>% filter(vehicle_typesuv == 0 & budgethigh == 1)
)
wtp_model_suv_low <- run_model_wtp(
  data %>% filter(vehicle_typesuv == 1 & budgethigh == 0)
)
wtp_model_suv_high <- run_model_wtp(
  data %>% filter(vehicle_typesuv == 1 & budgethigh == 1)
)
wtp_model_all <- run_model_wtp(data)

### View summary of results
# summary(model_car)
# summary(model_car_low)
# summary(model_car_high)
# summary(model_suv)
# summary(model_suv_low)
# summary(model_suv_high)
# summary(model_all)
# summary(model_datasource)

summary(wtp_model_dynata)
summary(wtp_model_prolific)
summary(wtp_model_car)
summary(wtp_model_suv)
summary(wtp_model_car_low)
summary(wtp_model_car_high)
summary(wtp_model_suv_low)
summary(wtp_model_suv_high)
summary(wtp_model_all)
