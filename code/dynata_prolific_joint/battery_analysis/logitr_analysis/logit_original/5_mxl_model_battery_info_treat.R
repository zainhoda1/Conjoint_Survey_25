source(here::here('code', 'setup.R'))

# Data Upload----

data_dce <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_logitr_dce_covariate_battery.parquet"
))

table(data_dce$vehicle_typesuv) / 24
table(data_dce$battery_info_treat) / 24
table(data_dce$vehicle_typesuv, data_dce$battery_info_treat) / 24


data_dce_infotreat <- data_dce %>%
  filter(battery_info_treat == 1)

data_dce_noinfotreat <- data_dce %>%
  filter(battery_info_treat == 0)

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


## model estimation ----

# # Estimate the pref model
pref_model_car_treat <- mxl_model_pref(
  data_dce_infotreat %>% filter(vehicle_typesuv == 0)
)
pref_model_suv_treat <- mxl_model_pref(
  data_dce_infotreat %>% filter(vehicle_typesuv == 1)
)
pref_model_car_notreat <- mxl_model_pref(
  data_dce_noinfotreat %>% filter(vehicle_typesuv == 0)
)
pref_model_suv_notreat <- mxl_model_pref(
  data_dce_noinfotreat %>% filter(vehicle_typesuv == 1)
)
# # pref_model_all <- mxl_model_pref(data_dce)
# summary(pref_model_car)
# summary(pref_model_suv)
# # summary(pref_model_all)

wtp_pref_model_car_treat <- wtp(pref_model_car_treat, scalePar = "price")
wtp_pref_model_suv_treat <- wtp(pref_model_suv_treat, scalePar = "price")
wtp_pref_model_car_notreat <- wtp(pref_model_car_notreat, scalePar = "price")
wtp_pref_model_suv_notreat <- wtp(pref_model_suv_notreat, scalePar = "price")

# Estimate the wtp model
wtp_model_car_treat <- mxl_model_wtp(
  data_dce_infotreat %>% filter(vehicle_typesuv == 0),
  wtp_pref_model_car_treat
)
wtp_model_suv_treat <- mxl_model_wtp(
  data_dce_infotreat %>% filter(vehicle_typesuv == 1),
  wtp_pref_model_suv_treat
)

wtp_model_car_notreat <- mxl_model_wtp(
  data_dce_noinfotreat %>% filter(vehicle_typesuv == 0),
  wtp_pref_model_car_notreat
)
wtp_model_suv_notreat <- mxl_model_wtp(
  data_dce_noinfotreat %>% filter(vehicle_typesuv == 1),
  wtp_pref_model_suv_notreat
)
# wtp_model_all <- mxl_model_wtp(data_dce)
summary(wtp_model_car_treat)
summary(wtp_model_suv_treat)
summary(wtp_model_car_notreat)
summary(wtp_model_suv_notreat)
# wtpCompare(pref_model_car, wtp_model_car, scalePar = 'price')
# wtpCompare(pref_model_suv, wtp_model_suv, scalePar = 'price')

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
  wtp_model_car_treat,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "mxl_wtp_model_car_treat.RData"
  )
)

save(
  wtp_model_suv_treat,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "mxl_wtp_model_suv_treat.RData"
  )
)

save(
  wtp_model_car_notreat,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "mxl_wtp_model_car_notreat.RData"
  )
)

save(
  wtp_model_suv_notreat,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "mxl_wtp_model_suv_notreat.RData"
  )
)
