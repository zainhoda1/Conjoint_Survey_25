source(here::here('code', 'setup.R'))

# Data Upload----

data_dce <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_logitr_dce_covariate_battery.parquet"
))

# table(data_dce$vehicle_typesuv) / 24
# table(data_dce$battery_info_treat) / 24
# table(data_dce$vehicle_typesuv, data_dce$battery_info_treat) / 24

# Piecewise variables----
# Range year 3: segments 40-130 mi | 130-200 mi | 200+ mi (in 100-mile units)
# Range loss: segments 5-12% | 12-24% | 24%+
# NaN arises for the no_choice alternative where battery_range_year3 = 0,
# making range_loss = 0/0. Replace with 0 before piecewise calculations.
data_dce <- data_dce %>%
  mutate(
    battery_range_loss = ifelse(
      is.nan(battery_range_loss),
      0,
      battery_range_loss
    ),
    battery_range_year3_pw1 = pmin(battery_range_year3, 1.30),
    battery_range_year3_pw2 = pmax(0, pmin(battery_range_year3, 2.00) - 1.30),
    battery_range_year3_pw3 = pmax(0, battery_range_year3 - 2.00),
    battery_range_loss_pw1 = pmin(battery_range_loss, 12),
    battery_range_loss_pw2 = pmax(0, pmin(battery_range_loss, 24) - 12),
    battery_range_loss_pw3 = pmax(0, battery_range_loss - 24),
    # Treatment interactions for range and range loss
    battery_range_year3_pw1_treat = battery_info_treat *
      battery_range_year3_pw1,
    battery_range_year3_pw2_treat = battery_info_treat *
      battery_range_year3_pw2,
    battery_range_year3_pw3_treat = battery_info_treat *
      battery_range_year3_pw3,
    battery_range_loss_pw1_treat = battery_info_treat * battery_range_loss_pw1,
    battery_range_loss_pw2_treat = battery_info_treat * battery_range_loss_pw2,
    battery_range_loss_pw3_treat = battery_info_treat * battery_range_loss_pw3
  )

# Estimate MXL model----
## Define random parameters for MXL model
## Piecewise slopes are fixed: segments are parts of the same utility curve,
## making them all random risks non-monotonic individual utility functions and
## near-unidentifiable covariance structure.
## Mileage and refurbishment remain random to capture individual heterogeneity.
randPars <- c(
  mileage = "n",
  battery_refurbishpackreplace = "n",
  battery_refurbishcellreplace = "n"
)
numDraws <- 300 # increase for publication

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
      # "battery_range_year3_pw1",
      # "battery_range_year3_pw2",
      # "battery_range_year3_pw3",
      # "battery_range_year3_pw1_treat",
      # "battery_range_year3_pw2_treat",
      # "battery_range_year3_pw3_treat",
      "battery_range_loss_pw1",
      "battery_range_loss_pw2",
      "battery_range_loss_pw3",
      "battery_range_loss_pw1_treat",
      "battery_range_loss_pw2_treat",
      "battery_range_loss_pw3_treat",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",
      "no_choice"
    ),
    randPars = randPars,
    numMultiStarts = 10,
    drawType = "sobol",
    numDraws = numDraws,
    numCores = 1
  )
  # cat('n =', length(unique(data$respID)))
  return(model)
}

mxl_model_wtp <- function(data) {
  set.seed(6789)
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    panelID = "respID",
    pars = c(
      "mileage",
      "battery_range_year3_pw1",
      "battery_range_year3_pw2",
      "battery_range_year3_pw3",
      # "battery_range_year3_pw1_treat",
      # "battery_range_year3_pw2_treat",
      # "battery_range_year3_pw3_treat",
      "battery_range_loss_pw1",
      "battery_range_loss_pw2",
      "battery_range_loss_pw3",
      # "battery_range_loss_pw1_treat",
      # "battery_range_loss_pw2_treat",
      # "battery_range_loss_pw3_treat",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",
      "no_choice"
    ),
    scalePar = 'price',
    randPars = randPars,
    numMultiStarts = 10,
    drawType = "sobol",
    numDraws = numDraws,
    numCores = 1
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}


## model estimation ----

# Estimate the pref model
pref_model_car <- mxl_model_pref(
  data_dce %>% filter(vehicle_typesuv == 0)
)
pref_model_suv <- mxl_model_pref(
  data_dce %>% filter(vehicle_typesuv == 1)
)

# Estimate the wtp model
wtp_model_car <- mxl_model_wtp(
  data_dce %>% filter(vehicle_typesuv == 0)
)
wtp_model_suv <- mxl_model_wtp(
  data_dce %>% filter(vehicle_typesuv == 1)
)
summary(pref_model_car)
summary(wtp_model_car)
summary(wtp_model_suv)

## model save ----
save(
  pref_model_car,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "mxl_pref_model_car_piecewise_treat.RData"
  )
)

save(
  pref_model_suv,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "mxl_pref_model_suv_piecewise_treat.RData"
  )
)

save(
  wtp_model_car,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "mxl_wtp_model_car_piecewise_treat.RData"
  )
)

save(
  wtp_model_suv,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "mxl_wtp_model_suv_piecewise_treat.RData"
  )
)
