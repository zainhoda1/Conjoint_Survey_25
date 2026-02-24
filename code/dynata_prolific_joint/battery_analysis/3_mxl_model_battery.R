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
    battery_degradation = (battery_degradation * 100)
  ) %>%
  mutate(
    respID_qID = paste0(respID, "_", qID),
    price = case_when(is.na(price) ~ 0, T ~ price)
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

# Estimate MXL model----
## Define random parameters for MXL model
randPars <- c(
  battery_range_year0 = "n",
  battery_degradation = "n",
  battery_refurbishpackreplace = "n",
  battery_refurbishcellreplace = "n"
)
numDraws <- 500 # increase for publication

mxl_model_pref <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    panelID = "respID",
    pars = c(
      "price",
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",
      "no_choice"
    ),
    randPars = randPars,
    numMultiStarts = 10,
    drawType = "sobol",
    numDraws = numDraws,
    numCores = 5,
    set.seed(123)
  )
  # cat('n =', length(unique(data$respID)))
  return(model)
}

mxl_model_wtp <- function(data, wtp_pref_model) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    panelID = "respID",
    pars = c(
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",
      "no_choice"
    ),
    scalePar = 'price',
    randPars = randPars,
    numMultiStarts = 10,
    drawType = "sobol",
    numDraws = numDraws,
    numCores = 5,
    startVals = wtp_pref_model$Estimate,
    set.seed(6789)
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}

# mxl_model_wtp_cor <- function(data, wtp_pref_model) {
#   model <- logitr(
#     data = data,
#     outcome = "choice",
#     obsID = "obsID",
#     panelID = "respID",
#     pars = c(
#       "mileage",
#       "battery_range_year0",
#       "battery_degradation",
#       "battery_refurbishpackreplace",
#       "battery_refurbishcellreplace",
#       "no_choice"
#     ),
#     scalePar = 'price',
#     randPars = randPars,
#     numMultiStarts = 10,
#     drawType = "sobol",
#     numDraws = numDraws,
#     correlation = TRUE,
#     numCores = 5,
#     startVals = wtp_pref_model$Estimate,
#     set.seed(6789)
#   )
#   cat('n =', length(unique(data$respID)))
#   return(model)
# }

## model estimation ----

# Estimate the pref model
pref_model_car <- mxl_model_pref(
  data_dce_dummy %>% filter(vehicle_typesuv == 0)
)
pref_model_suv <- mxl_model_pref(
  data_dce_dummy %>% filter(vehicle_typesuv == 1)
)
# pref_model_all <- mxl_model_pref(data_dce_dummy)
summary(pref_model_car)
summary(pref_model_suv)
# summary(pref_model_all)

wtp_pref_model_car <- wtp(pref_model_car, scalePar = "price")
wtp_pref_model_suv <- wtp(pref_model_suv, scalePar = "price")

# Estimate the wtp model
wtp_model_car <- mxl_model_wtp(
  data_dce_dummy %>% filter(vehicle_typesuv == 0),
  wtp_pref_model_car
)
wtp_model_suv <- mxl_model_wtp(
  data_dce_dummy %>% filter(vehicle_typesuv == 1),
  wtp_pref_model_suv
)
# wtp_model_all <- mxl_model_wtp(data_dce_dummy)
summary(wtp_model_car)
summary(wtp_model_suv)
# summary(wtp_model_all)

wtpCompare(pref_model_car, wtp_model_car, scalePar = 'price')
wtpCompare(pref_model_suv, wtp_model_suv, scalePar = 'price')

### Estimate the correkated wtp model
# wtp_model_car_cor <- mxl_model_wtp_cor(
#   data_dce_dummy %>% filter(vehicle_typesuv == 0),
#   wtp_pref_model_car
# )
# wtp_model_suv_cor <- mxl_model_wtp_cor(
#   data_dce_dummy %>% filter(vehicle_typesuv == 1),
#   wtp_pref_model_suv
# )
# # wtp_model_all <- mxl_model_wtp(data_dce_dummy)
# summary(wtp_model_car_cor)
# summary(wtp_model_suv_cor)

# # First order condition - Is the gradient close to zero?
# model2_all$gradient
# #  Second order condition - Is the hessian negative definite?
# eigen(model2_all$hessian)$values

## model save ----
save(
  pref_model_car,
  file = here(
    "code",
    "output",
    "model_output",
    "logitr",
    "battery",
    "mxl_pref_model_car.RData"
  )
)

save(
  pref_model_suv,
  file = here(
    "code",
    "output",
    "model_output",
    "logitr",
    "battery",
    "mxl_pref_model_suv.RData"
  )
)

save(
  wtp_model_car,
  file = here(
    "code",
    "output",
    "model_output",
    "logitr",
    "battery",
    "mxl_wtp_model_car.RData"
  )
)

save(
  wtp_model_suv,
  file = here(
    "code",
    "output",
    "model_output",
    "logitr",
    "battery",
    "mxl_wtp_model_suv.RData"
  )
)
