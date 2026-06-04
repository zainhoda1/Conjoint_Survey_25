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

### ----Latent class (c=2)----
model_results <- list()
i = 1
apollo_initialise()

apollo_control = list(
  modelName       = paste0("piecewise_rangeloss_car_suv_lc_2c_", i),
  modelDescr      = "LC model with 2 classes, piecewise linear range and range loss",
  indivID         = "respID",
  nCores          = 2,
  panelData       = TRUE,
  outputDirectory = paste0(here(), "/code/output/model_output/battery_analysis/apollo")
)
database <- data_model

apollo_beta = c(
  # ---- Class A ----
  b_no_choice_a = 0,
  b_mileage_a   = 0,
  b_range_pw1_a = 0,
  b_range_pw2_a = 0,
  b_range_pw3_a = 0,
  b_loss_pw1_a  = 0,
  b_loss_pw2_a  = 0,
  b_loss_pw3_a  = 0,
  b_packreplace_a = 0,
  b_cellreplace_a = 0,
  b_price_a     = 0,

  # ---- Class B ----
  b_no_choice_b = 0,
  b_mileage_b   = 0,
  b_range_pw1_b = 0,
  b_range_pw2_b = 0,
  b_range_pw3_b = 0,
  b_loss_pw1_b  = 0,
  b_loss_pw2_b  = 0,
  b_loss_pw3_b  = 0,
  b_packreplace_b = 0,
  b_cellreplace_b = 0,
  b_price_b     = 0,

  # ---- Class allocation model ----
  # Class B is the reference (fixed to 0)
  delta_a                    = 0,
  gamma_EV_rangeanxiety_a    = 0,
  gamma_risktaker_a          = 0,
  gamma_hhincome_a           = 0,
  gamma_ev_charge_a          = 0,
  gamma_knowledge_ev_a       = 0,
  gamma_evb_environment_agree_a = 0,
  gamma_evb_function_disagree_a = 0,
  gamma_hhveh_fuel_a         = 0,
  gamma_veh_range_a          = 0,
  gamma_suv_a                = 0,

  delta_b                    = 0,
  gamma_EV_rangeanxiety_b    = 0,
  gamma_risktaker_b          = 0,
  gamma_hhincome_b           = 0,
  gamma_ev_charge_b          = 0,
  gamma_knowledge_ev_b       = 0,
  gamma_evb_environment_agree_b = 0,
  gamma_evb_function_disagree_b = 0,
  gamma_hhveh_fuel_b         = 0,
  gamma_veh_range_b          = 0,
  gamma_suv_b                = 0
)

### Class B is the reference class: fix all its allocation parameters to 0
apollo_fixed = c(
  "delta_b",
  "gamma_EV_rangeanxiety_b",
  "gamma_risktaker_b",
  "gamma_hhincome_b",
  "gamma_ev_charge_b",
  "gamma_knowledge_ev_b",
  "gamma_evb_environment_agree_b",
  "gamma_evb_function_disagree_b",
  "gamma_hhveh_fuel_b",
  "gamma_veh_range_b",
  "gamma_suv_b"
)

set.seed(42)
apollo_beta[!names(apollo_beta) %in% apollo_fixed] <-
  runif(sum(!names(apollo_beta) %in% apollo_fixed), -0.1, 0.1)

# Warm-start from converged run (uncomment after first run):
# apollo_beta = apollo_readBeta(
#   apollo_beta, apollo_fixed,
#   "piecewise_rangeloss_car_suv_lc_2c_1", overwriteFixed = FALSE
# )

#### DEFINE LATENT CLASS COMPONENTS
apollo_lcPars = function(apollo_beta, apollo_inputs) {
  lcpars = list()
  lcpars[["b_no_choice"]]   = list(b_no_choice_a,   b_no_choice_b)
  lcpars[["b_mileage"]]     = list(b_mileage_a,     b_mileage_b)
  lcpars[["b_range_pw1"]]   = list(b_range_pw1_a,   b_range_pw1_b)
  lcpars[["b_range_pw2"]]   = list(b_range_pw2_a,   b_range_pw2_b)
  lcpars[["b_range_pw3"]]   = list(b_range_pw3_a,   b_range_pw3_b)
  lcpars[["b_loss_pw1"]]    = list(b_loss_pw1_a,    b_loss_pw1_b)
  lcpars[["b_loss_pw2"]]    = list(b_loss_pw2_a,    b_loss_pw2_b)
  lcpars[["b_loss_pw3"]]    = list(b_loss_pw3_a,    b_loss_pw3_b)
  lcpars[["b_packreplace"]] = list(b_packreplace_a, b_packreplace_b)
  lcpars[["b_cellreplace"]] = list(b_cellreplace_a, b_cellreplace_b)
  lcpars[["b_price"]]       = list(b_price_a,       b_price_b)

  V = list()
  V[["class_a"]] = delta_a +
    gamma_EV_rangeanxiety_a    * (ATT_range_anxiety > 3) +
    gamma_risktaker_a          * (ATT_risktaker > 3) +
    gamma_hhincome_a           * hhincome_num_10k +
    gamma_ev_charge_a          * (EV_charger == "yes") +
    gamma_knowledge_ev_a       * (knowledge_ev == 1) +
    gamma_evb_environment_agree_a * (ATT_EVB_environment > 3) +
    gamma_evb_function_disagree_a * (ATT_EVB_function < 3) +
    gamma_hhveh_fuel_a         * (Veh_hh_fuel == "icev_only") +
    gamma_veh_range_a          * (Veh_primary_range / 100) +
    gamma_suv_a                * (vehicle_typesuv == 1)

  V[["class_b"]] = delta_b +
    gamma_EV_rangeanxiety_b    * (ATT_range_anxiety > 3) +
    gamma_risktaker_b          * (ATT_risktaker > 3) +
    gamma_hhincome_b           * hhincome_num_10k +
    gamma_ev_charge_b          * (EV_charger == "yes") +
    gamma_knowledge_ev_b       * (knowledge_ev == 1) +
    gamma_evb_environment_agree_b * (ATT_EVB_environment > 3) +
    gamma_evb_function_disagree_b * (ATT_EVB_function < 3) +
    gamma_hhveh_fuel_b         * (Veh_hh_fuel == "icev_only") +
    gamma_veh_range_b          * (Veh_primary_range / 100) +
    gamma_suv_b                * (vehicle_typesuv == 1)

  classAlloc_settings = list(
    classes   = c(class_a = 1, class_b = 2),
    utilities = V
  )
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  mnl_settings = list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3, no_choice = 4),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1, no_choice = 1),
    choiceVar    = choice
  )

  for (s in 1:2) {
    V = list()
    V[["alt1"]] <-
      b_mileage[[s]] * mileage_1 +
      b_range_pw1[[s]] * pmin(battery_range_year3_1, 1.30) +
      b_range_pw2[[s]] * pmax(0, pmin(battery_range_year3_1, 2.00) - 1.30) +
      b_range_pw3[[s]] * pmax(0, battery_range_year3_1 - 2.00) +
      b_loss_pw1[[s]] * pmin(battery_range_loss_1, 12) +
      b_loss_pw2[[s]] * pmax(0, pmin(battery_range_loss_1, 24) - 12) +
      b_loss_pw3[[s]] * pmax(0, battery_range_loss_1 - 24) +
      b_packreplace[[s]] * battery_refurbishpackreplace_1 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_1 +
      b_price[[s]] * price_1
    V[["alt2"]] <-
      b_mileage[[s]] * mileage_2 +
      b_range_pw1[[s]] * pmin(battery_range_year3_2, 1.30) +
      b_range_pw2[[s]] * pmax(0, pmin(battery_range_year3_2, 2.00) - 1.30) +
      b_range_pw3[[s]] * pmax(0, battery_range_year3_2 - 2.00) +
      b_loss_pw1[[s]] * pmin(battery_range_loss_2, 12) +
      b_loss_pw2[[s]] * pmax(0, pmin(battery_range_loss_2, 24) - 12) +
      b_loss_pw3[[s]] * pmax(0, battery_range_loss_2 - 24) +
      b_packreplace[[s]] * battery_refurbishpackreplace_2 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_2 +
      b_price[[s]] * price_2
    V[["alt3"]] <-
      b_mileage[[s]] * mileage_3 +
      b_range_pw1[[s]] * pmin(battery_range_year3_3, 1.30) +
      b_range_pw2[[s]] * pmax(0, pmin(battery_range_year3_3, 2.00) - 1.30) +
      b_range_pw3[[s]] * pmax(0, battery_range_year3_3 - 2.00) +
      b_loss_pw1[[s]] * pmin(battery_range_loss_3, 12) +
      b_loss_pw2[[s]] * pmax(0, pmin(battery_range_loss_3, 24) - 12) +
      b_loss_pw3[[s]] * pmax(0, battery_range_loss_3 - 24) +
      b_packreplace[[s]] * battery_refurbishpackreplace_3 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_3 +
      b_price[[s]] * price_3
    V[["no_choice"]] <- b_no_choice[[s]]
    mnl_settings$utilities = V
    P[[paste0("Class_", s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_", s)]] = apollo_panelProd(P[[paste0("Class_", s)]], apollo_inputs, functionality)
  }

  lc_settings = list(inClassProb = P, classProb = pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model_results[[paste0("model_", i)]] = apollo_estimate(
  apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
  estimate_settings = list(maxIterations = 5000)
)

valid_models <- model_results[sapply(model_results, function(m) m$successfulEstimation == TRUE)]

model_summary <- data.frame(
  model_name = names(valid_models),
  AIC = sapply(valid_models, function(x) unname(x$AIC)),
  BIC = sapply(valid_models, function(x) unname(x$BIC))
) %>% arrange(BIC)

model_final <- model_results[[model_summary$model_name[1]]]

apollo_modelOutput(model_final, modelOutput_settings = list(printPVal = TRUE))
apollo_saveOutput(model_final, saveOutput_settings = list(printPVal = 2))

saveRDS(apollo_inputs,
  file = here("code","output","model_output","battery_analysis","apollo",
              "car_suv_lc_2c_apollo_inputs_piecewise_rangeloss.rds"))
saveRDS(apollo_probabilities,
  file = here("code","output","model_output","battery_analysis","apollo",
              "car_suv_lc_2c_apollo_probabilities_piecewise_rangeloss.rds"))
