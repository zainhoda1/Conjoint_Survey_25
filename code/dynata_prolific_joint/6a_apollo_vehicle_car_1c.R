source(here::here('code', 'setup.R'))

# ----DCE only----
# Initialize
apollo_initialise()

# Define core controls
apollo_control = list(
  modelName = "LC_1c_indicator",
  modelDescr = "MNL model",
  indivID = "respID",
  panelData = TRUE,
  outputDirectory = paste0(here(), "/code/output/model_output/apollo/vehicle"),
  mixing = FALSE
)

database <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_vehicle.parquet"
)) %>%
  filter(vehicle_typesuv == 0)

#### 2. Parameter starting values
apollo_beta <- c(
  b_no_choice = 0,
  b_powertrainbev = 0,
  b_powertrainhev = 0,
  b_range_bev = 0,
  b_mileage = 0,
  b_age = 0,
  b_operating_cost = 0,
  b_price = 0
)


#### 3. No random parameters (simple MNL)
# Skip apollo_randCoeff

#### 4. Validate inputs
apollo_fixed <- c()
apollo_inputs <- apollo_validateInputs()

#### 5. Define the model and utilities
apollo_probabilities <- function(
  apollo_beta,
  apollo_inputs,
  functionality = "estimate"
) {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Define utility functions
  P <- list()
  # For each alternative, use the same variables (if attributes vary across alts)
  # If they are alt-specific columns, adjust accordingly.
  V <- list()
  V[["alt1"]] <-
    b_powertrainbev *
    powertrainbev_1 +
    b_powertrainhev * powertrainhev_1 +
    b_range_bev * range_bev_1 +
    b_mileage * mileage_1 +
    b_age * age_1 +
    b_operating_cost * operating_cost_1 +
    b_price * price_1

  V[["alt2"]] <-
    b_powertrainbev *
    powertrainbev_2 +
    b_powertrainhev * powertrainhev_2 +
    b_range_bev * range_bev_2 +
    b_mileage * mileage_2 +
    b_age * age_2 +
    b_operating_cost * operating_cost_2 +
    b_price * price_2

  V[["alt3"]] <-
    b_powertrainbev *
    powertrainbev_3 +
    b_powertrainhev * powertrainhev_3 +
    b_range_bev * range_bev_3 +
    b_mileage * mileage_3 +
    b_age * age_3 +
    b_operating_cost * operating_cost_3 +
    b_price * price_3

  V[["no_choice"]] <- b_no_choice

  # Model settings
  mnl_settings <- list(
    # altID = altID,
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3, no_choice = 4),
    avail = list(alt1 = 1, alt2 = 1, alt3 = 1, no_choice = 1), ## Availability of alternatives
    choiceVar = choice,
    utilities = V
  )

  # Compute probabilities
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  ### Take product across observation for same individual
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  ### Prepare and return outputs of function
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

#### 6. Estimate model
LC_1c_indicator <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(maxIterations = 1000)
)

#### 7. Output results
apollo_modelOutput(
  LC_1c_indicator,
  modelOutput_settings = list(printPVal = TRUE)
)

#### 7. Save results
apollo_saveOutput(
  LC_1c_indicator,
  saveOutput_settings = list(printPVal = 2)
)


#### 8. WTP - delta method
for (i in c(
  "b_no_choice",
  "b_powertrainbev",
  "b_powertrainhev",
  "b_range_bev",
  "b_mileage",
  "b_age",
  "b_operating_cost"
)) {
  deltaMethod_settings <- list(
    operation = "ratio",
    parName1 = i,
    parName2 = "b_price",
    multPar1 = -1
  )
  apollo_deltaMethod(LC_1c_indicator, deltaMethod_settings)
}
