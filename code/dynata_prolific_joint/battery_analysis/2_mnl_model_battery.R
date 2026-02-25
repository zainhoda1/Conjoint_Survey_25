source(here::here('code', 'setup.R'))

# Data Upload----

data_dce <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_logitr_dce_only_battery.parquet"
))

# Estimate MNL model----
## model function ----
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

## model estimation ----
# model_car <- run_model1(data_dce_dummy %>% filter(vehicle_typesuv == 0))
# model_suv <- run_model1(data_dce_dummy %>% filter(vehicle_typesuv == 1))
model1_all <- run_model1(data_dce_dummy)
# model_dynata <- run_model2(data %>% filter(data_sourcedynata == 1))
# model_prolific <- run_model2(data %>% filter(data_sourcedynata == 0))

# View summary of results
# summary(model_car_low)
# summary(model_car_high)
# summary(model_suv_low)
# summary(model_suv_high)
# summary(model_car)
# summary(model_suv)
# summary(model_all)
# summary(model_dynata)
# summary(model_prolific)

# Estimate the model 2
model2_car <- run_model2(data_dce_dummy %>% filter(vehicle_typesuv == 0))
model2_suv <- run_model2(data_dce_dummy %>% filter(vehicle_typesuv == 1))
model2_all <- run_model2(data_dce_dummy)

# Estimate the wtp model
model3_car <- run_model_wtp(data_dce_dummy %>% filter(vehicle_typesuv == 0))
model3_suv <- run_model_wtp(data_dce_dummy %>% filter(vehicle_typesuv == 1))
model3_all <- run_model_wtp(data_dce_dummy)

# View summary of results
# summary(model_car_low)
# summary(model_car_high)
# summary(model_suv_low)
# summary(model_suv_high)
summary(model2_car)
summary(model2_suv)
summary(model3_car)
summary(model3_suv)
summary(model1_all)
summary(model2_all)
summary(model3_all)

# First order condition - Is the gradient close to zero?
model2_all$gradient
#  Second order condition - Is the hessian negative definite?
eigen(model2_all$hessian)$values

save(
  model2_car,
  file = here(
    "code",
    "output",
    "model_output",
    "logitr",
    "battery",
    "mnl_car_pref_model.RData"
  )
)

## model save ----
save(
  model2_suv,
  file = here(
    "code",
    "output",
    "model_output",
    "logitr",
    "battery",
    "mnl_suv_pref_model.RData"
  )
)

save(
  model3_car,
  file = here(
    "code",
    "output",
    "model_output",
    "logitr",
    "battery",
    "mnl_car_wtp_model.RData"
  )
)

save(
  model3_suv,
  file = here(
    "code",
    "output",
    "model_output",
    "logitr",
    "battery",
    "mnl_suv_wtp_model.RData"
  )
)

# result output----
