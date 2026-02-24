source(here::here('code', 'setup.R'))

# Data Upload----

data_dce <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_battery.parquet"
)) %>%
  select(
    -starts_with("battery_health"),
    -starts_with("time")
  ) %>%
  mutate(
    mileage = mileage / 10000, #3 - 6
    price = price / 10000, # 0.5 - 6
    battery_range_year0 = battery_range_year0 / 100, # 1-3
    battery_range_year3 = battery_range_year3 / 100, # 1-3
    battery_range_year8 = battery_range_year8 / 100, # 0.5 - 2
    battery_degradation = (battery_degradation * 10)
  ) %>%
  mutate(
    respID_qID = paste0(respID, "_", qID),
    price = case_when(is.na(price) ~ 0, T ~ price)
  )


data_variable <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_clean_variables.parquet"
))

data_variable <- data_variable %>%
  select(
    psid,
    next_veh_budget,
    ends_with("_num"),
    ends_with("_cate"),
    starts_with("ATT_"),
    starts_with("FA_"),
    starts_with("knowledge_"),
    Veh_hh_fuel,
    starts_with("EV_"),
    starts_with("next_veh_fuel_")
  ) %>%
  mutate(
    hhincome_num_10k = hhincome_num / 10000,
    next_veh_budget_k = next_veh_budget / 1000
  )

# variable correlation
# cor(
#   data_variable$next_veh_budget,
#   data_variable$hhincome_num,
#   use = "complete.obs"
# )

# Data Processing----
data_dce_dummy <- cbc_encode(
  data_dce %>%
    select(!c(psid, respID_qID)),
  coding = 'dummy',
  ref_levels = list(
    battery_refurbish = 'original',
    vehicle_type = 'car',
    budget = 'low'
  )
) %>%
  as.data.frame()


data_dce_dummy <- cbind(
  data_dce_dummy,
  data_dce %>%
    select(psid, respID_qID)
)

summary(data_dce_dummy)
# glimpse(data)

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
