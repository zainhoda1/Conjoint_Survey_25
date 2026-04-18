rm(list = ls())

source(here::here('code', 'setup.R'))

data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_vehicle.parquet"
)) %>%
  filter(
    # !is.na(hhincome_num) &
    !is.na(FA_EV_benefit) &
      !is.na(FA_EV_anxiety) &
      !is.na(hhincome_num_10k) &
      # !is.na(EV_charger) &
      !is.na(EV_neighbor) &
      !is.na(knowledge_ev) &
      # !is.na(knowledge_subsidy) &
      # !is.na(Veh_hh_count) &
      # !is.na(Veh_hh_fuel) &
      # !is.na(Veh_primary_refuel_monthly) &
      # !is.na(Veh_primary_range)
      !is.na(ATT_risktaker) &
      # !is.na(ATT_price_sensitive) &
      # !is.na(ATT_climate) &

      !is.na(ATT_EVB_environment) &
      !is.na(ATT_EVB_function)
  )

### ----Latent class (c=3)----
# Create an empty list to store the results of each model run
model_results <- list()
i = 1
# for (i in c(1:5)) {
# Initialize
apollo_initialise()

# Define core controls
## CAR
# apollo_control = list(
#   modelName = paste0("car_lc_3c_", i),
#   modelDescr = "LC model with 3 classes with indicator",
#   indivID = "respID",
#   nCores = 2,
#   panelData = TRUE,
#   outputDirectory = paste0(
#     here(),
#     "/code/output/model_output/vehicle_analysis/apollo"
#   )
# )
# database <- data_model %>%
#   filter(vehicle_typesuv == 0)

### SUV
# apollo_control = list(
#   modelName = paste0("suv_lc_3c_", i),
#   modelDescr = "LC model with 3 classes with indicator",
#   indivID = "respID",
#   nCores = 2,
#   panelData = TRUE,
#   outputDirectory = paste0(
#     here(),
#     "/code/output/model_output/vehicle_analysis/apollo"
#   )
# )
# database <- data_model %>%
#   filter(vehicle_typesuv == 1)

### Car+SUV
apollo_control = list(
  modelName = paste0("car_suv_lc_3c_", i),
  modelDescr = "LC model with 3 classes with indicator",
  indivID = "respID",
  nCores = 2,
  panelData = TRUE,
  outputDirectory = paste0(
    here(),
    "/code/output/model_output/vehicle_analysis/apollo"
  )
)
database <- data_model

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  # attributes for each class
  b_no_choice_a = 0,
  b_powertrainbev_a = 0,
  b_powertrainhev_a = 0,
  b_range_bev_a = 0,
  b_mileage_a = 0,
  b_age_a = 0,
  b_operating_cost_a = 0,
  b_price_a = 0,
  b_no_choice_b = 0,
  b_powertrainbev_b = 0,
  b_powertrainhev_b = 0,
  b_range_bev_b = 0,
  b_mileage_b = 0,
  b_age_b = 0,
  b_operating_cost_b = 0,
  b_price_b = 0,
  b_no_choice_c = 0,
  b_powertrainbev_c = 0,
  b_powertrainhev_c = 0,
  b_range_bev_c = 0,
  b_mileage_c = 0,
  b_age_c = 0,
  b_operating_cost_c = 0,
  b_price_c = 0,

  # base intercepts for class allocation model
  # coefficients for covariates in class‐allocation
  delta_a = 0,
  gamma_EV_benefit_a = 0,
  gamma_EV_anxiety_a = 0,
  gamma_hhincome_a = 0,
  # gamma_ev_charge_a = 0,
  gamma_ev_neighbor_a = 0,
  gamma_knowledge_ev_a = 0,
  # gamma_knowledge_subsidy_a = 0,
  # gamma_veh_count_a = 0,
  # gamma_veh_has_bev_a = 0,
  # gamma_veh_refuel_a = 0,
  # gamma_veh_range_a = 0,
  gamma_risk_taking_a = 0,
  # gamma_price_sensitivity_a = 0,
  # gamma_climate_concern_a = 0,
  gamma_evb_environment_positive_a = 0,
  gamma_evb_function_negative_a = 0,

  delta_b = 0.1,
  gamma_EV_benefit_b = 0.1,
  gamma_EV_anxiety_b = 0.1,
  gamma_hhincome_b = 0.1,
  # gamma_ev_charge_b = 0.1,
  gamma_ev_neighbor_b = 0.1,
  gamma_knowledge_ev_b = 0.1,
  # gamma_knowledge_subsidy_b = 0.1,
  # gamma_veh_count_b = 0,
  # gamma_veh_has_bev_b = 0,
  # gamma_veh_refuel_b = 0,
  # gamma_veh_range_b = 0,
  gamma_risk_taking_b = 0,
  # gamma_price_sensitivity_b = 0,
  # gamma_climate_concern_b = 0,
  gamma_evb_environment_positive_b = 0,
  gamma_evb_function_negative_b = 0,
  delta_c = 0.2,
  gamma_EV_benefit_c = 0.2,
  gamma_EV_anxiety_c = 0.2,
  # gamma_ev_charge_c = 0.2,
  gamma_hhincome_c = 0.2,
  gamma_ev_neighbor_c = 0.2,
  gamma_knowledge_ev_c = 0.2,
  # gamma_knowledge_subsidy_c = 0.2,
  # gamma_veh_count_c = 0,
  # gamma_veh_has_bev_c = 0,
  # gamma_veh_refuel_c = 0,
  # gamma_veh_range_c = 0,
  gamma_risk_taking_c = 0,
  # gamma_price_sensitivity_c = 0,
  # gamma_climate_concern_c = 0,
  gamma_evb_environment_positive_c = 0,
  gamma_evb_function_negative_c = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c(
  "delta_a",
  "gamma_EV_benefit_a",
  "gamma_EV_anxiety_a",
  "gamma_hhincome_a",
  # "gamma_ev_charge_a",
  "gamma_ev_neighbor_a",
  "gamma_knowledge_ev_a",
  # "gamma_knowledge_subsidy_a",
  # "gamma_veh_count_a",
  # "gamma_veh_has_bev_a",
  # "gamma_veh_refuel_a",
  # "gamma_veh_range_a",
  "gamma_risk_taking_a",
  # "gamma_price_sensitivity_a",
  # "gamma_climate_concern_a",
  "gamma_evb_environment_positive_a",
  "gamma_evb_function_negative_a"
  # "gamma_ev_charge_b",
  # "gamma_knowledge_subsidy_b",
  # "gamma_veh_count_b",
  # "gamma_veh_has_bev_b",
  # "gamma_veh_refuel_b",
  # "gamma_veh_range_b",

  # "gamma_ev_charge_c",
  # "gamma_knowledge_subsidy_c",
  # "gamma_veh_count_c",
  # "gamma_veh_has_bev_c",
  # "gamma_veh_refuel_c",
  # "gamma_veh_range_c"

  # "gamma_EV_benefit_b",
  # "gamma_EV_anxiety_b",
  # "gamma_hhincome_b",
  # "gamma_ev_charge_b",
  # "gamma_ev_neighbor_b",
  # "gamma_knowledge_ev_b",
  # "gamma_knowledge_subsidy_b",
  # "gamma_veh_count_b",
  # "gamma_veh_has_bev_b",
  # "gamma_veh_refuel_b",
  # "gamma_veh_range_b",

  # "gamma_EV_benefit_c",
  # "gamma_EV_anxiety_c",
  # "gamma_hhincome_c",
  # "gamma_ev_charge_c",
  # "gamma_ev_neighbor_c",
  # "gamma_knowledge_ev_c",
  # "gamma_knowledge_subsidy_c",
  # "gamma_veh_count_c",
  # "gamma_veh_has_bev_c",
  # "gamma_veh_refuel_c",
  # "gamma_veh_range_c"
)

# apollo_beta[!names(apollo_beta) %in% apollo_fixed] <-
#   runif(sum(!names(apollo_beta) %in% apollo_fixed), -1, 1)

apollo_beta = apollo_readBeta(
  apollo_beta,
  apollo_fixed,
  "car_lc_3c_indicator_1",
  overwriteFixed = FALSE
)

# apollo_beta = apollo_readBeta(
#   apollo_beta,
#   apollo_fixed,
#   "suv_lc_3c_indicator_1",
#   overwriteFixed = FALSE
# )

#### DEFINE LATENT CLASS COMPONENTS
apollo_lcPars = function(apollo_beta, apollo_inputs) {
  lcpars = list()
  lcpars[["b_no_choice"]] = list(b_no_choice_a, b_no_choice_b, b_no_choice_c)
  lcpars[["b_powertrainbev"]] = list(
    b_powertrainbev_a,
    b_powertrainbev_b,
    b_powertrainbev_c
  )

  lcpars[["b_powertrainhev"]] = list(
    b_powertrainhev_a,
    b_powertrainhev_b,
    b_powertrainhev_c
  )
  lcpars[["b_range_bev"]] = list(b_range_bev_a, b_range_bev_b, b_range_bev_c)
  lcpars[["b_mileage"]] = list(b_mileage_a, b_mileage_b, b_mileage_c)
  lcpars[["b_age"]] = list(b_age_a, b_age_b, b_age_c)
  lcpars[["b_operating_cost"]] = list(
    b_operating_cost_a,
    b_operating_cost_b,
    b_operating_cost_c
  )
  lcpars[["b_price"]] = list(b_price_a, b_price_b, b_price_c)

  ### Utilities of class allocation model: ow likely a person is to belong to each class based on indicators
  V = list()
  V[["class_a"]] = delta_a +
    gamma_EV_benefit_a * FA_EV_benefit +
    gamma_EV_anxiety_a * FA_EV_anxiety +
    gamma_hhincome_a * hhincome_num_10k +
    # gamma_ev_charge_a * (EV_charger == "yes") +
    gamma_ev_neighbor_a * (EV_neighbor == "yes") +
    gamma_knowledge_ev_a * (knowledge_ev == 1) +
    # gamma_knowledge_subsidy_a * (knowledge_subsidy == 1) +
    # gamma_veh_count_a * Veh_hh_count +
    # gamma_veh_has_bev_a * (Veh_hh_fuel == "has_bev") +
    # gamma_veh_refuel_a * Veh_primary_refuel_monthly +
    # gamma_veh_range_a * (Veh_primary_range / 100) +
    gamma_risk_taking_a * ATT_risktaker +
    # gamma_price_sensitivity_a * ATT_price_sensitive +
    # gamma_climate_concern_a * ATT_climate +
    gamma_evb_environment_positive_a * ATT_EVB_environment +
    gamma_evb_function_negative_a * ATT_EVB_function

  V[["class_b"]] = delta_b +
    gamma_EV_benefit_b * FA_EV_benefit +
    gamma_EV_anxiety_b * FA_EV_anxiety +
    gamma_hhincome_b * hhincome_num_10k +
    # gamma_ev_charge_b * (EV_charger == "yes") +
    gamma_ev_neighbor_b * (EV_neighbor == "yes") +
    gamma_knowledge_ev_b * (knowledge_ev == 1) +
    # gamma_knowledge_subsidy_b * (knowledge_subsidy == 1) +
    # gamma_veh_count_b * Veh_hh_count +
    # gamma_veh_has_bev_b * (Veh_hh_fuel == "has_bev") +
    # gamma_veh_refuel_b * Veh_primary_refuel_monthly +
    # gamma_veh_range_b * (Veh_primary_range / 100) +
    gamma_risk_taking_b * ATT_risktaker +
    # gamma_price_sensitivity_b * ATT_price_sensitive +
    # gamma_climate_concern_b * ATT_climate +
    gamma_evb_environment_positive_b * ATT_EVB_environment +
    gamma_evb_function_negative_b * ATT_EVB_function
  V[["class_c"]] = delta_c +
    gamma_EV_benefit_c * FA_EV_benefit +
    gamma_EV_anxiety_c * FA_EV_anxiety +
    gamma_hhincome_c * hhincome_num_10k +
    # gamma_ev_charge_c * (EV_charger == "yes") +
    gamma_ev_neighbor_c * (EV_neighbor == "yes") +
    gamma_knowledge_ev_c * (knowledge_ev == 1) +
    # gamma_knowledge_subsidy_c * (knowledge_subsidy == 1) +
    # gamma_veh_count_c * Veh_hh_count +
    # gamma_veh_has_bev_c * (Veh_hh_fuel == "has_bev") +
    # gamma_veh_refuel_c * Veh_primary_refuel_monthly +
    # gamma_veh_range_c * (Veh_primary_range / 100) +
    gamma_risk_taking_c * ATT_risktaker +
    # gamma_price_sensitivity_c * ATT_price_sensitive +
    # gamma_climate_concern_c * ATT_climate +
    gamma_evb_environment_positive_c * ATT_EVB_environment +
    gamma_evb_function_negative_c * ATT_EVB_function
  ### Settings for class allocation models
  classAlloc_settings = list(
    classes = c(class_a = 1, class_b = 2, class_c = 3),
    utilities = V
  )
  # computes the class membership probabilities (π) for each person
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
    avail = list(alt1 = 1, alt2 = 1, alt3 = 1, no_choice = 1), ## Availability of alternatives
    choiceVar = choice
  )

  ### Loop over classes
  for (s in 1:3) {
    ### Compute class-specific utilities
    V = list()
    V[["alt1"]] <-
      b_powertrainbev[[s]] *
      powertrainbev_1 +
      b_powertrainhev[[s]] * powertrainhev_1 +
      b_range_bev[[s]] * range_bev_1 +
      b_mileage[[s]] * mileage_1 +
      b_age[[s]] * age_1 +
      b_operating_cost[[s]] * operating_cost_1 +
      b_price[[s]] * price_1

    V[["alt2"]] <-
      b_powertrainbev[[s]] *
      powertrainbev_2 +
      b_powertrainhev[[s]] * powertrainhev_2 +
      b_range_bev[[s]] * range_bev_2 +
      b_mileage[[s]] * mileage_2 +
      b_age[[s]] * age_2 +
      b_operating_cost[[s]] * operating_cost_2 +
      b_price[[s]] * price_2

    V[["alt3"]] <-
      b_powertrainbev[[s]] *
      powertrainbev_3 +
      b_powertrainhev[[s]] * powertrainhev_3 +
      b_range_bev[[s]] * range_bev_3 +
      b_mileage[[s]] * mileage_3 +
      b_age[[s]] * age_3 +
      b_operating_cost[[s]] * operating_cost_3 +
      b_price[[s]] * price_3

    V[["no_choice"]] <- b_no_choice[[s]]

    # Model settings
    mnl_settings$utilities = V

    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_", s)]] = apollo_mnl(mnl_settings, functionality)

    ### Take product across observation for same individual
    P[[paste0("Class_", s)]] = apollo_panelProd(
      P[[paste0("Class_", s)]],
      apollo_inputs,
      functionality
    )
  }

  ### Compute latent class model probabilities
  lc_settings = list(inClassProb = P, classProb = pi_values)
  # mixes the class‐specific probabilities with class‐membership probabilities pi_values.
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)

  ### Prepare and return outputs of function: finalizes the probability vector
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#### MODEL ESTIMATION

### Estimate model
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

# Create a summary table for all 10 runs
model_summary <- data.frame(
  model_name = names(valid_models),
  # LLstart = sapply(valid_models, function(x) x$LL0),
  # LLfinal = sapply(valid_models, function(x) x$LLout),
  AIC = sapply(valid_models, function(x) unname(x$AIC)),
  BIC = sapply(valid_models, function(x) unname(x$BIC))
) %>%
  arrange((BIC))

# Identify best-fitting model
best_model_name <- model_summary$model_name[i]
model_final <- model_results[[best_model_name]]

### Show output in screen
# apollo_modelOutput(
#   lc_3c_indicator,
#   modelOutput_settings = list(printPVal = TRUE)
# )

### Save output to file(s)
apollo_saveOutput(
  model_final,
  saveOutput_settings = list(printPVal = 2)
)

### car
# saveRDS(
#   apollo_inputs,
#   file = here(
#     "code",
#     "output",
#     "model_output",
#     "vehicle_analysis",
#     "apollo",
#     "car_lc_3c_apollo_inputs.rds"
#   )
# )

# saveRDS(
#   apollo_probabilities,
#   file = here(
#     "code",
#     "output",
#     "model_output",
#     "vehicle_analysis",
#     "apollo",
#     "car_lc_3c_apollo_probabilities.rds"
#   )
# )

### SUV
# saveRDS(
#   apollo_inputs,
#   file = here(
#     "code",
#     "output",
#     "model_output",
#     "vehicle_analysis",
#     "apollo",
#     "suv_lc_3c_apollo_inputs.rds"
#   )
# )

# saveRDS(
#   apollo_probabilities,
#   file = here(
#     "code",
#     "output",
#     "model_output",
#     "vehicle_analysis",
#     "apollo",
#     "suv_lc_3c_apollo_probabilities.rds"
#   )
# )

#### CAR+SUV
saveRDS(
  apollo_inputs,
  file = here(
    "code",
    "output",
    "model_output",
    "vehicle_analysis",
    "apollo",
    "car_suv_lc_3c_apollo_inputs.rds"
  )
)

saveRDS(
  apollo_probabilities,
  file = here(
    "code",
    "output",
    "model_output",
    "vehicle_analysis",
    "apollo",
    "car_suv_lc_3c_apollo_probabilities.rds"
  )
)
