# rm(list = ls())

source(here::here('code', 'setup.R'))

data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery.parquet"
))

data_model <- data_model %>%
  filter(
    !is.na(ATT_range_anxiety) &
      !is.na(ATT_risktaker) &
      !is.na(hhincome_num_10k) &
      !is.na(EV_charger) &
      !is.na(Veh_hh_fuel) &
      !is.na(Veh_primary_range) &
      !is.na(ATT_EVB_environment) &
      !is.na(ATT_EVB_function) &
      !is.na(vehicle_typesuv)
  )

### ---- MNL (1-class) ----
apollo_initialise()

apollo_control = list(
  modelName       = "piecewise_rangeloss_car_suv_lc_1c_1",
  modelDescr      = "MNL model, piecewise linear range and range loss",
  indivID         = "respID",
  nCores          = 2,
  panelData       = TRUE,
  outputDirectory = paste0(here(), "/code/output/model_output/battery_analysis/apollo")
)
database <- data_model

apollo_beta = c(
  b_no_choice = 0,
  b_mileage   = 0,
  # Range (year 3) piecewise slopes: 40-130 mi | 130-200 mi | 200+ mi
  b_range_pw1 = 0,
  b_range_pw2 = 0,
  b_range_pw3 = 0,
  # Range loss piecewise slopes: 5-12% | 12-24% | 24%+
  b_loss_pw1  = 0,
  b_loss_pw2  = 0,
  b_loss_pw3  = 0,
  b_packreplace = 0,
  b_cellreplace = 0,
  b_price     = 0
)

apollo_fixed = c()

#### GROUP AND VALIDATE INPUTS
apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION
apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P = list()

  V = list()
  V[["alt1"]] <-
    b_mileage * mileage_1 +
    b_range_pw1 * pmin(battery_range_year3_1, 1.30) +
    b_range_pw2 * pmax(0, pmin(battery_range_year3_1, 2.00) - 1.30) +
    b_range_pw3 * pmax(0, battery_range_year3_1 - 2.00) +
    b_loss_pw1 * pmin(battery_range_loss_1, 12) +
    b_loss_pw2 * pmax(0, pmin(battery_range_loss_1, 24) - 12) +
    b_loss_pw3 * pmax(0, battery_range_loss_1 - 24) +
    b_packreplace * battery_refurbishpackreplace_1 +
    b_cellreplace * battery_refurbishcellreplace_1 +
    b_price * price_1

  V[["alt2"]] <-
    b_mileage * mileage_2 +
    b_range_pw1 * pmin(battery_range_year3_2, 1.30) +
    b_range_pw2 * pmax(0, pmin(battery_range_year3_2, 2.00) - 1.30) +
    b_range_pw3 * pmax(0, battery_range_year3_2 - 2.00) +
    b_loss_pw1 * pmin(battery_range_loss_2, 12) +
    b_loss_pw2 * pmax(0, pmin(battery_range_loss_2, 24) - 12) +
    b_loss_pw3 * pmax(0, battery_range_loss_2 - 24) +
    b_packreplace * battery_refurbishpackreplace_2 +
    b_cellreplace * battery_refurbishcellreplace_2 +
    b_price * price_2

  V[["alt3"]] <-
    b_mileage * mileage_3 +
    b_range_pw1 * pmin(battery_range_year3_3, 1.30) +
    b_range_pw2 * pmax(0, pmin(battery_range_year3_3, 2.00) - 1.30) +
    b_range_pw3 * pmax(0, battery_range_year3_3 - 2.00) +
    b_loss_pw1 * pmin(battery_range_loss_3, 12) +
    b_loss_pw2 * pmax(0, pmin(battery_range_loss_3, 24) - 12) +
    b_loss_pw3 * pmax(0, battery_range_loss_3 - 24) +
    b_packreplace * battery_refurbishpackreplace_3 +
    b_cellreplace * battery_refurbishcellreplace_3 +
    b_price * price_3

  V[["no_choice"]] <- b_no_choice

  mnl_settings = list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3, no_choice = 4),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1, no_choice = 1),
    choiceVar    = choice,
    utilities    = V
  )

  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  P[["model"]] = apollo_panelProd(P[["model"]], apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#### MODEL ESTIMATION
model <- apollo_estimate(
  apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
  estimate_settings = list(maxIterations = 5000)
)

apollo_modelOutput(model, modelOutput_settings = list(printPVal = TRUE))
apollo_saveOutput(model, saveOutput_settings = list(printPVal = 2))
