source(here::here('code', 'setup.R'))

# Data Upload----

data_dce_covariate <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_logitr_dce_covariate_battery.parquet"
))

data_dce_covariate <- data_dce_covariate %>%
  mutate(
    hhincome_num_10k = hhincome_num / 10000,
    hhtenure_num = case_when(
      hhtenure_cate == "own" ~ 1,
      hhtenure_cate == "rent" ~ 0,
      TRUE ~ 2
    ),
    EV_charger_num = case_when(
      EV_charger == "yes" ~ 1,
      EV_charger == "no" ~ 0,
      TRUE ~ 2
    ),
    ATT_risktaker_agree = case_when(
      ATT_risktaker %in% c("somewhat_agree", "strongly_agree") ~ 1,
      TRUE ~ 0
    ),
    ATT_risktaker_disagree = case_when(
      ATT_risktaker %in% c("somewhat_disagree", "strongly_disagree") ~ 1,
      TRUE ~ 0
    )
  )

# Estimate MXL model----
## Define interaction terms----
data_interact <- data_dce_covariate %>%
  filter(!is.na(hhincome_num_10k) & !is.na(EV_charger_num)) %>%
  mutate(
    interact_range_income = battery_range_year0 * hhincome_num_10k,
    interact_range_charger = battery_range_year0 * EV_charger_num,
    interact_degradation_income = battery_degradation * hhincome_num_10k,
    interact_refurbishpack_risk = battery_refurbishpackreplace *
      ATT_risktaker_agree,
    interact_refurbishcell_risk = battery_refurbishcellreplace *
      ATT_risktaker_agree
  )
## Define parameters----
pars_pref = c(
  "price",
  "mileage",
  "battery_range_year0",
  "battery_degradation",
  "battery_refurbishpackreplace",
  "battery_refurbishcellreplace",
  "no_choice",
  "interact_range_income",
  "interact_range_charger",
  "interact_degradation_income",
  "interact_refurbishpack_risk",
  "interact_refurbishcell_risk"
)
pars_wtp = c(
  "mileage",
  "battery_range_year0",
  "battery_degradation",
  "battery_refurbishpackreplace",
  "battery_refurbishcellreplace",
  "no_choice",
  "interact_range_income",
  "interact_range_charger",
  "interact_degradation_income",
  "interact_refurbishpack_risk",
  "interact_refurbishcell_risk"
)
randPars <- c(
  mileage = "n",
  battery_range_year0 = "n",
  battery_degradation = "n",
  battery_refurbishpackreplace = "n",
  battery_refurbishcellreplace = "n"
)


## Model functions----
numDraws <- 200 # increase for publication

mxl_model_pref <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    panelID = "respID",
    pars = pars_pref,
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
    pars = pars_wtp,
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
  data_interact %>% filter(vehicle_typesuv == 0)
)
pref_model_suv <- mxl_model_pref(
  data_interact %>% filter(vehicle_typesuv == 1)
)
# pref_model_all <- mxl_model_pref(data_dce_dummy)
summary(pref_model_car)
summary(pref_model_suv)
# summary(pref_model_all)

wtp_pref_model_car <- wtp(pref_model_car, scalePar = "price")
wtp_pref_model_suv <- wtp(pref_model_suv, scalePar = "price")

# Estimate the wtp model
wtp_model_car <- mxl_model_wtp(
  data_interact %>% filter(vehicle_typesuv == 0),
  wtp_pref_model_car
)
wtp_model_suv <- mxl_model_wtp(
  data_interact %>% filter(vehicle_typesuv == 1),
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
    "mxl_pref_interact_model_car.RData"
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
    "mxl_pref_interact_model_suv.RData"
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
    "mxl_wtp_interact_model_car.RData"
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
    "mxl_wtp_interact_model_suv.RData"
  )
)
