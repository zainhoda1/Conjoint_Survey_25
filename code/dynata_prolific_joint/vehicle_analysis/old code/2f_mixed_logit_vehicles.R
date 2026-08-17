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


# run mixed logit model

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




data <- data %>%
  mutate(
    price_dynata = price * data_sourcedynata,
    bev_dynata = powertrainbev * data_sourcedynata
  )


# Estimate the model


mixed_model_1_car_panel <- run_mixed_model_1(
  data %>% filter(vehicle_typesuv == 0)
)

mixed_model_1_suv_panel <- run_mixed_model_1(
  data %>% filter(vehicle_typesuv == 1)
)

mixed_model_1_car_low_panel <- run_mixed_model_1(
  data %>% filter(vehicle_typesuv == 0 & budgethigh == 0)
)

mixed_model_1_car_high_panel <- run_mixed_model_1(
  data %>% filter(vehicle_typesuv == 0 & budgethigh == 1)
)

mixed_model_1_suv_low_panel <- run_mixed_model_1(
  data %>% filter(vehicle_typesuv == 1 & budgethigh == 0)
)

mixed_model_1_suv_high_panel <- run_mixed_model_1(
  data %>% filter(vehicle_typesuv == 1 & budgethigh == 1)
)

################################################################


# Save model object

save(
  mixed_model_1_car_panel,
  file = here("models", "mixed_model_1_car_panel.RData")
)

save(
  mixed_model_1_suv_panel,
  file = here("models", "mixed_model_1_suv_panel.RData")
)

save(
  mixed_model_1_car_low_panel,
  file = here("models", "mixed_model_1_car_low_panel.RData")
)

save(
  mixed_model_1_car_high_panel,
  file = here("models", "mixed_model_1_car_high_panel.RData")
)

save(
  mixed_model_1_suv_low_panel,
  file = here("models", "mixed_model_1_suv_low_panel.RData")
)

save(
  mixed_model_1_suv_high_panel,
  file = here("models", "mixed_model_1_suv_high_panel.RData")
)







