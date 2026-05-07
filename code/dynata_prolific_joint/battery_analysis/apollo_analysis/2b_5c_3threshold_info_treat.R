# rm(list = ls())

source(here::here('code', 'setup.R'))

data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery.parquet"
))

data_model <- data_model %>%
  filter(
    # !is.na(FA_EV_benefit) &
    # !is.na(FA_EV_anxiety) &
    !is.na(ATT_range_anxiety) &
      !is.na(ATT_risktaker) &
      !is.na(hhincome_num_10k) &
      !is.na(EV_charger) &
      # !is.na(EV_neighbor) &
      # !is.na(knowledge_ev) &
      # !is.na(knowledge_subsidy) &
      # !is.na(Veh_hh_count) &
      !is.na(Veh_hh_fuel) &
      # !is.na(Veh_primary_refuel_monthly) &
      !is.na(Veh_primary_range) &
      !is.na(ATT_EVB_environment) &
      !is.na(ATT_EVB_function) &
      !is.na(vehicle_typesuv)
  )

### ----Latent class (c=5)----
# Create an empty list to store the results of each model run
model_results <- list()
i = 1
# for (i in c(1:10)) {
# Initialize
apollo_initialise()

### Car+SUV
apollo_control = list(
  modelName = paste0("piecewise_rangeloss_info_treat_car_suv_lc_5c_", i),
  modelDescr = "LC model with 5 classes, piecewise linear range and range loss",
  indivID = "respID",
  nCores = 2,
  panelData = TRUE,
  outputDirectory = paste0(
    here(),
    "/code/output/model_output/battery_analysis/apollo"
  )
)
database <- data_model

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  # ---- Class A ----
  b_no_choice_a = 0,
  b_mileage_a = 0,
  # Range (year 3) piecewise slopes: 40-130 mi | 130-200 mi | 200+ mi
  b_range_pw1_a = 0,
  b_range_pw2_a = 0,
  b_range_pw3_a = 0,
  # Range loss piecewise slopes: 5-12% | 12-24% | 24%+
  b_loss_pw1_a = 0,
  b_loss_pw2_a = 0,
  b_loss_pw3_a = 0,
  b_packreplace_a = 0,
  b_cellreplace_a = 0,
  b_price_a = 0,

  # ---- Class B ----
  b_no_choice_b = 0,
  b_mileage_b = 0,
  b_range_pw1_b = 0,
  b_range_pw2_b = 0,
  b_range_pw3_b = 0,
  b_loss_pw1_b = 0,
  b_loss_pw2_b = 0,
  b_loss_pw3_b = 0,
  b_packreplace_b = 0,
  b_cellreplace_b = 0,
  b_price_b = 0,

  # ---- Class C ----
  b_no_choice_c = 0,
  b_mileage_c = 0,
  b_range_pw1_c = 0,
  b_range_pw2_c = 0,
  b_range_pw3_c = 0,
  b_loss_pw1_c = 0,
  b_loss_pw2_c = 0,
  b_loss_pw3_c = 0,
  b_packreplace_c = 0,
  b_cellreplace_c = 0,
  b_price_c = 0,

  # ---- Class D ----
  b_no_choice_d = 0,
  b_mileage_d = 0,
  b_range_pw1_d = 0,
  b_range_pw2_d = 0,
  b_range_pw3_d = 0,
  b_loss_pw1_d = 0,
  b_loss_pw2_d = 0,
  b_loss_pw3_d = 0,
  b_packreplace_d = 0,
  b_cellreplace_d = 0,
  b_price_d = 0,

  # ---- Class E ----
  b_no_choice_e = 0,
  b_mileage_e = 0,
  b_range_pw1_e = 0,
  b_range_pw2_e = 0,
  b_range_pw3_e = 0,
  b_loss_pw1_e = 0,
  b_loss_pw2_e = 0,
  b_loss_pw3_e = 0,
  b_packreplace_e = 0,
  b_cellreplace_e = 0,
  b_price_e = 0,

  # ---- Class allocation model ----
  # Class E is the reference (fixed to 0)
  delta_a = 0,
  gamma_EV_rangeanxiety_a = 0,
  gamma_risktaker_a = 0,
  gamma_hhincome_a = 0,
  gamma_ev_charge_a = 0,
  # gamma_ev_neighbor_a = 0,
  gamma_knowledge_ev_a = 0,
  # gamma_knowledge_subsidy_a = 0,
  gamma_evb_environment_agree_a = 0,
  gamma_evb_function_disagree_a = 0,
  gamma_hhveh_fuel_a = 0,
  # gamma_veh_refuel_a = 0,
  gamma_veh_range_a = 0,
  gamma_suv_a = 0,
  gamma_info_treat_a = 0,

  delta_b = 0,
  gamma_EV_rangeanxiety_b = 0,
  gamma_risktaker_b = 0,
  gamma_hhincome_b = 0,
  gamma_ev_charge_b = 0,
  # gamma_ev_neighbor_b = 0,
  gamma_knowledge_ev_b = 0,
  # gamma_knowledge_subsidy_b = 0,
  gamma_evb_environment_agree_b = 0,
  gamma_evb_function_disagree_b = 0,
  gamma_hhveh_fuel_b = 0,
  # gamma_veh_refuel_b = 0,
  gamma_veh_range_b = 0,
  gamma_suv_b = 0,
  gamma_info_treat_b = 0,

  delta_c = 0,
  gamma_EV_rangeanxiety_c = 0,
  gamma_risktaker_c = 0,
  gamma_hhincome_c = 0,
  gamma_ev_charge_c = 0,
  # gamma_ev_neighbor_c = 0,
  gamma_knowledge_ev_c = 0,
  # gamma_knowledge_subsidy_c = 0,
  gamma_evb_environment_agree_c = 0,
  gamma_evb_function_disagree_c = 0,
  gamma_hhveh_fuel_c = 0,
  # gamma_veh_refuel_c = 0,
  gamma_veh_range_c = 0,
  gamma_suv_c = 0,
  gamma_info_treat_c = 0,

  delta_d = 0,
  gamma_EV_rangeanxiety_d = 0,
  gamma_risktaker_d = 0,
  gamma_hhincome_d = 0,
  gamma_ev_charge_d = 0,
  # gamma_ev_neighbor_d = 0,
  gamma_knowledge_ev_d = 0,
  # gamma_knowledge_subsidy_d = 0,
  gamma_evb_environment_agree_d = 0,
  gamma_evb_function_disagree_d = 0,
  gamma_hhveh_fuel_d = 0,
  # gamma_veh_refuel_d = 0,
  gamma_veh_range_d = 0,
  gamma_suv_d = 0,
  gamma_info_treat_d = 0,

  delta_e = 0,
  gamma_EV_rangeanxiety_e = 0,
  gamma_risktaker_e = 0,
  gamma_hhincome_e = 0,
  gamma_ev_charge_e = 0,
  # gamma_ev_neighbor_e = 0,
  gamma_knowledge_ev_e = 0,
  # gamma_knowledge_subsidy_e = 0,
  gamma_evb_environment_agree_e = 0,
  gamma_evb_function_disagree_e = 0,
  gamma_hhveh_fuel_e = 0,
  # gamma_veh_refuel_e = 0,
  gamma_veh_range_e = 0,
  gamma_suv_e = 0,
  gamma_info_treat_e = 0
)

### Class E is the reference class: fix all its allocation parameters to 0
apollo_fixed = c(
  "delta_e",
  "gamma_EV_rangeanxiety_e",
  "gamma_risktaker_e",
  "gamma_hhincome_e",
  "gamma_ev_charge_e",
  # "gamma_ev_neighbor_e",
  "gamma_knowledge_ev_e",
  "gamma_evb_environment_agree_e",
  "gamma_evb_function_disagree_e",
  "gamma_hhveh_fuel_e",
  # "gamma_veh_refuel_e",
  "gamma_veh_range_e",
  "gamma_suv_e",
  "gamma_info_treat_e"
)

# Randomise free parameters so classes start from different points.
# LC models with identical starting values across classes produce identical
# class probabilities and fail apollo's validation check.
set.seed(42)
apollo_beta[!names(apollo_beta) %in% apollo_fixed] <-
  runif(sum(!names(apollo_beta) %in% apollo_fixed), -0.1, 0.1)

# Once a first run has converged, warm-start subsequent runs from those estimates:
apollo_beta = apollo_readBeta(
  apollo_beta,
  apollo_fixed,
  "piecewise_rangeloss_car_suv_lc_4c_1",
  overwriteFixed = FALSE
)

#### DEFINE LATENT CLASS COMPONENTS
apollo_lcPars = function(apollo_beta, apollo_inputs) {
  lcpars = list()
  lcpars[["b_no_choice"]] = list(
    b_no_choice_a,
    b_no_choice_b,
    b_no_choice_c,
    b_no_choice_d,
    b_no_choice_e
  )
  lcpars[["b_mileage"]] = list(
    b_mileage_a,
    b_mileage_b,
    b_mileage_c,
    b_mileage_d,
    b_mileage_e
  )
  lcpars[["b_range_pw1"]] = list(
    b_range_pw1_a,
    b_range_pw1_b,
    b_range_pw1_c,
    b_range_pw1_d,
    b_range_pw1_e
  )
  lcpars[["b_range_pw2"]] = list(
    b_range_pw2_a,
    b_range_pw2_b,
    b_range_pw2_c,
    b_range_pw2_d,
    b_range_pw2_e
  )
  lcpars[["b_range_pw3"]] = list(
    b_range_pw3_a,
    b_range_pw3_b,
    b_range_pw3_c,
    b_range_pw3_d,
    b_range_pw3_e
  )
  lcpars[["b_loss_pw1"]] = list(
    b_loss_pw1_a,
    b_loss_pw1_b,
    b_loss_pw1_c,
    b_loss_pw1_d,
    b_loss_pw1_e
  )
  lcpars[["b_loss_pw2"]] = list(
    b_loss_pw2_a,
    b_loss_pw2_b,
    b_loss_pw2_c,
    b_loss_pw2_d,
    b_loss_pw2_e
  )
  lcpars[["b_loss_pw3"]] = list(
    b_loss_pw3_a,
    b_loss_pw3_b,
    b_loss_pw3_c,
    b_loss_pw3_d,
    b_loss_pw3_e
  )
  lcpars[["b_packreplace"]] = list(
    b_packreplace_a,
    b_packreplace_b,
    b_packreplace_c,
    b_packreplace_d,
    b_packreplace_e
  )
  lcpars[["b_cellreplace"]] = list(
    b_cellreplace_a,
    b_cellreplace_b,
    b_cellreplace_c,
    b_cellreplace_d,
    b_cellreplace_e
  )
  lcpars[["b_price"]] = list(
    b_price_a,
    b_price_b,
    b_price_c,
    b_price_d,
    b_price_e
  )

  ### Utilities of class allocation model
  V = list()
  V[["class_a"]] = delta_a +
    gamma_EV_rangeanxiety_a * (ATT_range_anxiety > 3) +
    gamma_risktaker_a * (ATT_risktaker > 3) +
    gamma_hhincome_a * hhincome_num_10k +
    gamma_ev_charge_a * (EV_charger == "yes") +
    # gamma_ev_neighbor_a * (EV_neighbor == "yes") +
    gamma_knowledge_ev_a * (knowledge_ev == 1) +
    # gamma_knowledge_subsidy_a * (knowledge_subsidy == 1) +
    gamma_evb_environment_agree_a * (ATT_EVB_environment > 3) +
    gamma_evb_function_disagree_a * (ATT_EVB_function < 3) +
    gamma_hhveh_fuel_a * (Veh_hh_fuel == "icev_only") +
    # gamma_veh_refuel_a * Veh_primary_refuel_monthly +
    gamma_veh_range_a * (Veh_primary_range / 100) +
    gamma_suv_a * (vehicle_typesuv == 1) +
    gamma_info_treat_a * battery_info_treat

  V[["class_b"]] = delta_b +
    gamma_EV_rangeanxiety_b * (ATT_range_anxiety > 3) +
    gamma_risktaker_b * (ATT_risktaker > 3) +
    gamma_hhincome_b * hhincome_num_10k +
    gamma_ev_charge_b * (EV_charger == "yes") +
    # gamma_ev_neighbor_b * (EV_neighbor == "yes") +
    gamma_knowledge_ev_b * (knowledge_ev == 1) +
    # gamma_knowledge_subsidy_b * (knowledge_subsidy == 1) +
    gamma_evb_environment_agree_b * (ATT_EVB_environment > 3) +
    gamma_evb_function_disagree_b * (ATT_EVB_function < 3) +
    gamma_hhveh_fuel_b * (Veh_hh_fuel == "icev_only") +
    # gamma_veh_refuel_b * Veh_primary_refuel_monthly +
    gamma_veh_range_b * (Veh_primary_range / 100) +
    gamma_suv_b * (vehicle_typesuv == 1) +
    gamma_info_treat_b * battery_info_treat

  V[["class_c"]] = delta_c +
    gamma_EV_rangeanxiety_c * (ATT_range_anxiety > 3) +
    gamma_risktaker_c * (ATT_risktaker > 3) +
    gamma_hhincome_c * hhincome_num_10k +
    gamma_ev_charge_c * (EV_charger == "yes") +
    # gamma_ev_neighbor_c * (EV_neighbor == "yes") +
    gamma_knowledge_ev_c * (knowledge_ev == 1) +
    # gamma_knowledge_subsidy_c * (knowledge_subsidy == 1) +
    gamma_evb_environment_agree_c * (ATT_EVB_environment > 3) +
    gamma_evb_function_disagree_c * (ATT_EVB_function < 3) +
    gamma_hhveh_fuel_c * (Veh_hh_fuel == "icev_only") +
    # gamma_veh_refuel_c * Veh_primary_refuel_monthly +
    gamma_veh_range_c * (Veh_primary_range / 100) +
    gamma_suv_c * (vehicle_typesuv == 1) +
    gamma_info_treat_c * battery_info_treat

  V[["class_d"]] = delta_d +
    gamma_EV_rangeanxiety_d * (ATT_range_anxiety > 3) +
    gamma_risktaker_d * (ATT_risktaker > 3) +
    gamma_hhincome_d * hhincome_num_10k +
    gamma_ev_charge_d * (EV_charger == "yes") +
    # gamma_ev_neighbor_d * (EV_neighbor == "yes") +
    gamma_knowledge_ev_d * (knowledge_ev == 1) +
    # gamma_knowledge_subsidy_d * (knowledge_subsidy == 1) +
    gamma_evb_environment_agree_d * (ATT_EVB_environment > 3) +
    gamma_evb_function_disagree_d * (ATT_EVB_function < 3) +
    gamma_hhveh_fuel_d * (Veh_hh_fuel == "icev_only") +
    # gamma_veh_refuel_d * Veh_primary_refuel_monthly +
    gamma_veh_range_d * (Veh_primary_range / 100) +
    gamma_suv_d * (vehicle_typesuv == 1) +
    gamma_info_treat_d * battery_info_treat

  V[["class_e"]] = delta_e +
    gamma_EV_rangeanxiety_e * (ATT_range_anxiety > 3) +
    gamma_risktaker_e * (ATT_risktaker > 3) +
    gamma_hhincome_e * hhincome_num_10k +
    gamma_ev_charge_e * (EV_charger == "yes") +
    # gamma_ev_neighbor_e * (EV_neighbor == "yes") +
    gamma_knowledge_ev_e * (knowledge_ev == 1) +
    # gamma_knowledge_subsidy_e * (knowledge_subsidy == 1) +
    gamma_evb_environment_agree_e * (ATT_EVB_environment > 3) +
    gamma_evb_function_disagree_e * (ATT_EVB_function < 3) +
    gamma_hhveh_fuel_e * (Veh_hh_fuel == "icev_only") +
    # gamma_veh_refuel_e * Veh_primary_refuel_monthly +
    gamma_veh_range_e * (Veh_primary_range / 100) +
    gamma_suv_e * (vehicle_typesuv == 1) +
    gamma_info_treat_e * battery_info_treat

  ### Settings for class allocation model
  classAlloc_settings = list(
    classes = c(
      class_a = 1,
      class_b = 2,
      class_c = 3,
      class_d = 4,
      class_e = 5
    ),
    utilities = V
  )
  # computes the class membership probabilities (pi) for each person
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)

  return(lcpars)
}

#### GROUP AND VALIDATE INPUTS

apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION
apollo_probabilities = function(
  apollo_beta,
  apollo_inputs,
  functionality = "estimate"
) {
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()

  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3, no_choice = 4),
    avail = list(alt1 = 1, alt2 = 1, alt3 = 1, no_choice = 1),
    choiceVar = choice
  )

  ### Loop over classes
  for (s in 1:5) {
    ### Compute class-specific utilities
    V = list()
    V[["alt1"]] <-
      b_mileage[[s]] *
      mileage_1 +
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
      b_mileage[[s]] *
      mileage_2 +
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
      b_mileage[[s]] *
      mileage_3 +
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

    # Model settings
    mnl_settings$utilities = V

    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_", s)]] = apollo_mnl(mnl_settings, functionality)

    ### Take product across observations for same individual
    P[[paste0("Class_", s)]] = apollo_panelProd(
      P[[paste0("Class_", s)]],
      apollo_inputs,
      functionality
    )
  }

  ### Compute latent class model probabilities
  lc_settings = list(inClassProb = P, classProb = pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#### MODEL ESTIMATION

model_results[[paste0("model_", i)]] = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(maxIterations = 5000)
)
# }

# Filter to only include successfully estimated models
valid_models <- model_results[
  sapply(model_results, function(m) m$successfulEstimation == TRUE)
]

# Create a summary table for all runs
model_summary <- data.frame(
  model_name = names(valid_models),
  AIC = sapply(valid_models, function(x) unname(x$AIC)),
  BIC = sapply(valid_models, function(x) unname(x$BIC))
) %>%
  arrange((BIC))

# Identify best-fitting model
best_model_name <- model_summary$model_name[1]
model_final <- model_results[[best_model_name]]

### Show output in screen
apollo_modelOutput(
  model_final,
  modelOutput_settings = list(printPVal = TRUE)
)

### Save output to file(s)
apollo_saveOutput(
  model_final,
  saveOutput_settings = list(printPVal = 2)
)

### CAR+SUV
saveRDS(
  apollo_inputs,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "car_suv_lc_5c_apollo_inputs_piecewise_rangeloss_info_treat.rds"
  )
)

saveRDS(
  apollo_probabilities,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "car_suv_lc_5c_apollo_probabilities_piecewise_rangelos_info_treat.rds"
  )
)
