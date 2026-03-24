# rm(list = ls())

source(here::here('code', 'setup.R'))

data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery.parquet"
)) %>%
  filter(
    !is.na(hhincome_num) &
      !is.na(FA_EV_benefit) &
      !is.na(FA_EV_anxiety) &
      !is.na(hhincome_num_10k) &
      !is.na(EV_charger) &
      !is.na(EV_neighbor) &
      !is.na(knowledge_ev) &
      !is.na(knowledge_subsidy) &
      # !is.na(Veh_hh_count) &
      # !is.na(Veh_hh_fuel) &
      # !is.na(Veh_primary_refuel_monthly) &
      # !is.na(Veh_primary_range) &
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
### CAR
apollo_control = list(
  modelName = paste0("car_lc_3c_", i),
  modelDescr = "LC model with 3 classes with indicator",
  indivID = "respID",
  nCores = 2,
  panelData = TRUE,
  outputDirectory = paste0(here(), "/code/output/model_output/apollo/battery"),
  noValidation = TRUE
)

database <- data_model %>%
  filter(vehicle_typesuv == 0)

### SUV
# apollo_control = list(
#   modelName = paste0("suv_lc_3c_", i),
#   modelDescr = "LC model with 3 classes with indicator",
#   indivID = "respID",
#   nCores = 2,
#   panelData = TRUE,
#   outputDirectory = paste0(here(), "/code/output/model_output/apollo/battery"),
#   noValidation = TRUE
# )

# database <- data_model %>%
#   filter(vehicle_typesuv == 1)

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  # attributes for each class
  b_no_choice_a = 0,
  b_mileage_a = 0,
  b_range_year0_a = 0,
  b_degradation_a = 0,
  b_packreplace_a = 0,
  b_cellreplace_a = 0,
  b_price_a = 0,
  b_no_choice_b = 0,
  b_mileage_b = 0,
  b_range_year0_b = 0,
  b_degradation_b = 0,
  b_packreplace_b = 0,
  b_cellreplace_b = 0,
  b_price_b = 0,
  b_no_choice_c = 0,
  b_mileage_c = 0,
  b_range_year0_c = 0,
  b_degradation_c = 0,
  b_packreplace_c = 0,
  b_cellreplace_c = 0,
  b_price_c = 0,

  # base intercepts for class allocation model
  # coefficients for covariates in class‐allocation
  delta_a = 0,
  gamma_EV_benefit_a = 0,
  gamma_EV_anxiety_a = 0,
  gamma_hhincome_a = 0,
  gamma_ev_charge_a = 0,
  gamma_ev_neighbor_a = 0,
  gamma_knowledge_ev_a = 0,
  gamma_knowledge_subsidy_a = 0,
  gamma_evb_environment_disagree_a = 0,
  gamma_evb_environment_agree_a = 0,
  gamma_evb_function_disagree_a = 0,
  gamma_evb_function_agree_a = 0,
  delta_b = 0.1,
  gamma_EV_benefit_b = 0.1,
  gamma_EV_anxiety_b = 0.1,
  gamma_hhincome_b = 0.1,
  gamma_ev_charge_b = 0.1,
  gamma_ev_neighbor_b = 0.1,
  gamma_knowledge_ev_b = 0.1,
  gamma_knowledge_subsidy_b = 0.1,
  gamma_evb_environment_disagree_b = 0,
  gamma_evb_environment_agree_b = 0,
  gamma_evb_function_disagree_b = 0,
  gamma_evb_function_agree_b = 0,
  delta_c = 0.2,
  gamma_EV_benefit_c = 0.2,
  gamma_EV_anxiety_c = 0.2,
  gamma_hhincome_c = 0.2,
  gamma_ev_charge_c = 0.2,
  gamma_ev_neighbor_c = 0.2,
  gamma_knowledge_ev_c = 0.2,
  gamma_knowledge_subsidy_c = 0.2,
  gamma_evb_environment_disagree_c = 0,
  gamma_evb_environment_agree_c = 0,
  gamma_evb_function_disagree_c = 0,
  gamma_evb_function_agree_c = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c(
  "delta_a",
  "gamma_EV_benefit_a",
  "gamma_EV_anxiety_a",
  "gamma_hhincome_a",
  "gamma_ev_charge_a",
  "gamma_ev_neighbor_a",
  "gamma_knowledge_ev_a",
  "gamma_knowledge_subsidy_a",
  "gamma_evb_environment_disagree_a",
  "gamma_evb_environment_agree_a",
  "gamma_evb_function_disagree_a",
  "gamma_evb_function_agree_a"
  # "gamma_ev_charge_b",
  # "gamma_ev_neighbor_b",
  # "gamma_knowledge_ev_b",
  # "gamma_knowledge_subsidy_b",
  # "gamma_ev_charge_c",
  # "gamma_ev_neighbor_c",
  # "gamma_knowledge_ev_c",
  # "gamma_knowledge_subsidy_c"
  # "gamma_used_bev_b",
  # "gamma_EV_benefit_b",
  # "gamma_EV_anxiety_b",
  # "gamma_used_bev_c"
  # "gamma_EV_benefit_c",
  # "gamma_EV_anxiety_c"
)

# apollo_beta[!names(apollo_beta) %in% apollo_fixed] <-
#   runif(sum(!names(apollo_beta) %in% apollo_fixed), -1, 1)

apollo_beta = apollo_readBeta(
  apollo_beta,
  apollo_fixed,
  "car_lc_3c_1",
  overwriteFixed = FALSE
)

#### DEFINE LATENT CLASS COMPONENTS
apollo_lcPars = function(apollo_beta, apollo_inputs) {
  lcpars = list()
  lcpars[["b_no_choice"]] = list(b_no_choice_a, b_no_choice_b, b_no_choice_c)
  lcpars[["b_mileage"]] = list(b_mileage_a, b_mileage_b, b_mileage_c)
  lcpars[["b_range_year0"]] = list(
    b_range_year0_a,
    b_range_year0_b,
    b_range_year0_c
  )
  lcpars[["b_degradation"]] = list(
    b_degradation_a,
    b_degradation_b,
    b_degradation_c
  )
  lcpars[["b_packreplace"]] = list(
    b_packreplace_a,
    b_packreplace_b,
    b_packreplace_c
  )
  lcpars[["b_cellreplace"]] = list(
    b_cellreplace_a,
    b_cellreplace_b,
    b_cellreplace_c
  )
  lcpars[["b_price"]] = list(b_price_a, b_price_b, b_price_c)

  ### Utilities of class allocation model: ow likely a person is to belong to each class based on indicators
  V = list()
  V[["class_a"]] = delta_a +
    gamma_EV_benefit_a * FA_EV_benefit +
    gamma_EV_anxiety_a * FA_EV_anxiety +
    gamma_hhincome_a * hhincome_num_10k +
    gamma_ev_charge_a * (EV_charger == "yes") +
    gamma_ev_neighbor_a * (EV_neighbor == "yes") +
    gamma_knowledge_ev_a * (knowledge_ev == 1) +
    gamma_knowledge_subsidy_a * (knowledge_subsidy == 1) +
    gamma_evb_environment_disagree_a * (ATT_EVB_environment < 3) +
    gamma_evb_environment_agree_a * (ATT_EVB_environment > 3) +
    gamma_evb_function_disagree_a * (ATT_EVB_function < 3) +
    gamma_evb_function_agree_a * (ATT_EVB_function > 3)

  V[["class_b"]] = delta_b +
    gamma_EV_benefit_b * FA_EV_benefit +
    gamma_EV_anxiety_b * FA_EV_anxiety +
    gamma_hhincome_b * hhincome_num_10k +
    gamma_ev_charge_b * (EV_charger == "yes") +
    gamma_ev_neighbor_b * (EV_neighbor == "yes") +
    gamma_knowledge_ev_b * (knowledge_ev == 1) +
    gamma_knowledge_subsidy_b * (knowledge_subsidy == 1) +
    gamma_evb_environment_disagree_b * (ATT_EVB_environment < 3) +
    gamma_evb_environment_agree_b * (ATT_EVB_environment > 3) +
    gamma_evb_function_disagree_b * (ATT_EVB_function < 3) +
    gamma_evb_function_agree_b * (ATT_EVB_function > 3)

  V[["class_c"]] = delta_c +
    gamma_EV_benefit_c * FA_EV_benefit +
    gamma_EV_anxiety_c * FA_EV_anxiety +
    gamma_hhincome_c * hhincome_num_10k +
    gamma_ev_charge_c * (EV_charger == "yes") +
    gamma_ev_neighbor_c * (EV_neighbor == "yes") +
    gamma_knowledge_ev_c * (knowledge_ev == 1) +
    gamma_knowledge_subsidy_c * (knowledge_subsidy == 1) +
    gamma_evb_environment_disagree_c * (ATT_EVB_environment < 3) +
    gamma_evb_environment_agree_c * (ATT_EVB_environment > 3) +
    gamma_evb_function_disagree_c * (ATT_EVB_function < 3) +
    gamma_evb_function_agree_c * (ATT_EVB_function > 3)

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
      b_mileage[[s]] *
      mileage_1 +
      b_range_year0[[s]] * battery_range_year0_1 +
      b_degradation[[s]] * battery_degradation_1 +
      b_packreplace[[s]] * battery_refurbishpackreplace_1 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_1 +
      b_price[[s]] * price_1

    V[["alt2"]] <-
      b_mileage[[s]] *
      mileage_2 +
      b_range_year0[[s]] * battery_range_year0_2 +
      b_degradation[[s]] * battery_degradation_2 +
      b_packreplace[[s]] * battery_refurbishpackreplace_2 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_2 +
      b_price[[s]] * price_2

    V[["alt3"]] <-
      b_mileage[[s]] *
      mileage_3 +
      b_range_year0[[s]] * battery_range_year0_3 +
      b_degradation[[s]] * battery_degradation_3 +
      b_packreplace[[s]] * battery_refurbishpackreplace_3 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_3 +
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
apollo_modelOutput(
  model_final,
  modelOutput_settings = list(printPVal = TRUE)
)

### Save output to file(s)
apollo_saveOutput(
  model_final,
  saveOutput_settings = list(printPVal = 2)
)

saveRDS(
  apollo_inputs,
  file = here(
    "code",
    "output",
    "model_output",
    "apollo",
    "battery",
    "car_lc_3c_apollo_inputs.rds"
  )
)

saveRDS(
  apollo_probabilities,
  file = here(
    "code",
    "output",
    "model_output",
    "apollo",
    "battery",
    "car_lc_3c_apollo_probabilities.rds"
  )
)

# saveRDS(
#   apollo_inputs,
#   file = here(
#     "code",
#     "output",
#     "model_output",
#     "apollo",
#     "battery",
#     "suv_lc_3c_apollo_inputs.rds"
#   )
# )

# saveRDS(
#   apollo_probabilities,
#   file = here(
#     "code",
#     "output",
#     "model_output",
#     "apollo",
#     "battery",
#     "suv_lc_3c_apollo_probabilities.rds"
#   )
# )
