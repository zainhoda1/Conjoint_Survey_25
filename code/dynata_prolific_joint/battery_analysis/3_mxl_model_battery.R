source(here::here('code', 'setup.R'))

# Data Upload----

data_dce <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_logitr_dce_only_battery.parquet"
))

# Estimate MXL model----
## Define random parameters for MXL model
randPars <- c(
  mileage = "n",
  battery_range_year0 = "n",
  battery_degradation = "n",
  battery_refurbishpackreplace = "n",
  battery_refurbishcellreplace = "n"
)
numDraws <- 300 # increase for publication

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
  data_dce %>% filter(vehicle_typesuv == 0)
)
pref_model_suv <- mxl_model_pref(
  data_dce %>% filter(vehicle_typesuv == 1)
)
# pref_model_all <- mxl_model_pref(data_dce)
summary(pref_model_car)
summary(pref_model_suv)
# summary(pref_model_all)

wtp_pref_model_car <- wtp(pref_model_car, scalePar = "price")
wtp_pref_model_suv <- wtp(pref_model_suv, scalePar = "price")

# Estimate the wtp model
wtp_model_car <- mxl_model_wtp(
  data_dce %>% filter(vehicle_typesuv == 0),
  wtp_pref_model_car
)
wtp_model_suv <- mxl_model_wtp(
  data_dce %>% filter(vehicle_typesuv == 1),
  wtp_pref_model_suv
)
# wtp_model_all <- mxl_model_wtp(data_dce)
summary(wtp_model_car)
summary(wtp_model_suv)
# summary(wtp_model_all)

wtpCompare(pref_model_car, wtp_model_car, scalePar = 'price')
wtpCompare(pref_model_suv, wtp_model_suv, scalePar = 'price')

### Estimate the correkated wtp model
# wtp_model_car_cor <- mxl_model_wtp_cor(
#   data_dce %>% filter(vehicle_typesuv == 0),
#   wtp_pref_model_car
# )
# wtp_model_suv_cor <- mxl_model_wtp_cor(
#   data_dce %>% filter(vehicle_typesuv == 1),
#   wtp_pref_model_suv
# )
# # wtp_model_all <- mxl_model_wtp(data_dce)
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
