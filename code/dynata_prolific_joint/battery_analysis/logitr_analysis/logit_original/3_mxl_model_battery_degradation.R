source(here::here('code', 'setup.R'))

# Data Upload----

data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery.parquet"
))

data_model <- data_model %>%
  filter(
    !is.na(ATT_range_anxiety) &
      !is.na(ATT_risktaker) &
      !is.na(next_veh_budget_k) &
      !is.na(EV_charger) &
      !is.na(Veh_hh_fuel) &
      !is.na(Veh_primary_range) &
      !is.na(ATT_EVB_environment) &
      !is.na(ATT_EVB_function) &
      !is.na(vehicle_typesuv)
  )

data_dce <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_logitr_dce_only_battery.parquet"
))

data_dce <- data_dce %>%
  filter(psid %in% data_model$psid)

data_dce %>%
  group_by(vehicle_typesuv) %>%
  distinct(psid) %>%
  nrow()

# Estimate MXL model----
## Define random parameters for MXL model
randPars <- c(
  mileage = "n",
  battery_range_year0 = "n",
  battery_degradation = "n",
  battery_refurbishpackreplace = "n",
  battery_refurbishcellreplace = "n"
)
numDraws <- 500 # increase for publication
# mclapply uses fork()-based parallelism, which is incompatible with RStudio on
# macOS: forked workers inherit RStudio's GUI state and crash immediately,
# returning NULLs that break logitr's internal multistart summary.
numCores <- 1

mxl_model_pref <- function(data) {
  set.seed(123)
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
    numCores = numCores
  )
  # cat('n =', length(unique(data$respID)))
  return(model)
}

mxl_model_wtp <- function(data, wtp_pref_model) {
  set.seed(6789)
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
    numCores = numCores,
    startVals = wtp_pref_model_all$Estimate
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

### Estimate the pref model
# pref_model_car <- mxl_model_pref(
#   data_dce %>%
#     filter(vehicle_typesuv == 0) %>%
#     group_by(obsID) %>%
#     filter(n() > 1, sum(choice) == 1) %>%
#     ungroup()
# )
# pref_model_suv <- mxl_model_pref(
#   data_dce %>% filter(vehicle_typesuv == 1)
# )
pref_model_all <- mxl_model_pref(data_dce)
# summary(pref_model_car)
# summary(pref_model_suv)
summary(pref_model_all)

# wtp_pref_model_car <- wtp(pref_model_car, scalePar = "price")
# wtp_pref_model_suv <- wtp(pref_model_suv, scalePar = "price")
wtp_pref_model_all <- wtp(pref_model_all, scalePar = "price")

### Estimate the wtp model
# wtp_model_car <- mxl_model_wtp(
#   data_dce %>% filter(vehicle_typesuv == 0),
#   wtp_pref_model_car
# )
# wtp_model_suv <- mxl_model_wtp(
#   data_dce %>% filter(vehicle_typesuv == 1),
#   wtp_pref_model_suv
# )
wtp_model_all <- mxl_model_wtp(data_dce)
# summary(wtp_model_car)
# summary(wtp_model_suv)
summary(wtp_model_all)

# wtpCompare(pref_model_car, wtp_model_car, scalePar = 'price')
# wtpCompare(pref_model_suv, wtp_model_suv, scalePar = 'price')
wtpCompare(pref_model_all, wtp_model_all, scalePar = 'price')

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
# save(
#   pref_model_car,
#   file = here(
#     "code",
#     "output",
#     "model_output",
#     "battery_analysis",
#     "logitr",
#     "mxl_pref_model_car.RData"
#   )
# )

# save(
#   pref_model_suv,
#   file = here(
#     "code",
#     "output",
#     "model_output",
#     "battery_analysis",
#     "logitr",
#     "mxl_pref_model_suv.RData"
#   )
# )

# save(
#   wtp_model_car,
#   file = here(
#     "code",
#     "output",
#     "model_output",
#     "battery_analysis",
#     "logitr",
#     "mxl_wtp_model_car.RData"
#   )
# )

# save(
#   wtp_model_suv,
#   file = here(
#     "code",
#     "output",
#     "model_output",
#     "battery_analysis",
#     "logitr",
#     "mxl_wtp_model_suv.RData"
#   )
# )

save(
  wtp_model_all,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "mxl_wtp_model_all.RData"
  )
)
