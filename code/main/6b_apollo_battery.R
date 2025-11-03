source(here::here('code', 'setup.R'))

# ----Load the data set----

data_dce <- read_csv(here(
  "data",
  "main",
  "battery_choice_data.csv"
))

data_variable <- read_csv(here(
  "data",
  "main",
  "data_clean_variables.csv"
))

data_variable <- data_variable %>%
  select(
    psid,
    ends_with("_num"),
    ends_with("_cate"),
    starts_with("ATT_"),
    starts_with("FA_"),
    starts_with("knowledge_"),
    Veh_hh_fuel,
    starts_with("EV_")
  )

## ----Data Processing----
data_dce <- data_dce %>%
  mutate(
    veh_mileage = veh_mileage / 10000, #3 - 6
    veh_price = veh_price / 10000, # 0.5 - 6
    battery_range_year0 = battery_range_year0 / 100, # 1-3
    battery_range_year3 = battery_range_year3 / 100, # 1-3
    battery_range_year8 = battery_range_year8 / 100, # 0.5 - 2
    battery_degradation = (battery_degradation * 100) # percentage
  ) %>%
  select(
    -session_id,
    -starts_with("battery_health"),
    -starts_with("time"),
    -vehicle_type,
    -battery_condition
  )

## ---- Dummy encode----
data_dce_dummy <- cbc_encode(
  data_dce %>% select(-psid),
  coding = 'dummy',
  ref_levels = list(battery_refurbish = 'original')
) %>%
  as.data.frame()

data_dce_dummy <- cbind(data_dce_dummy, data_dce %>% select(psid))

data_covariate <- data_dce_dummy %>%
  left_join(data_variable, by = "psid")

# ----Apollo----
## ----Simple MNL----
### ----DCE only----

# Initialize
apollo_initialise()

# Define core controls
apollo_control = list(
  modelName = "MNL_DCE",
  modelDescr = "MNL model in WTP-space",
  indivID = "respID",
  panelData = TRUE,
  outputDirectory = paste0(here(), "/code/main/model_output/apollo"),
  mixing = FALSE
)

database <- data_covariate %>%
  group_by(respID, obsID) %>%
  mutate(choice_alt = altID[choice == 1]) %>%
  ungroup()

#### 2. Parameter starting values
# You can initialize them to 0 or to your logitr estimates.
apollo_beta <- c(
  b_mileage = 0,
  b_range_year0 = 0,
  b_degradation = 0,
  b_packreplace = 0,
  b_cellreplace = 0,
  b_no_choice = 0,
  mu_price = -1 # price coefficient (scale parameter)
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
    b_mileage *
    veh_mileage +
    b_range_year0 * battery_range_year0 +
    b_degradation * battery_degradation +
    b_packreplace * battery_refurbishpackreplace +
    b_cellreplace * battery_refurbishcellreplace +
    mu_price * veh_price

  V[["alt2"]] <-
    b_mileage *
    veh_mileage +
    b_range_year0 * battery_range_year0 +
    b_degradation * battery_degradation +
    b_packreplace * battery_refurbishpackreplace +
    b_cellreplace * battery_refurbishcellreplace +
    mu_price * veh_price

  V[["alt3"]] <-
    b_mileage *
    veh_mileage +
    b_range_year0 * battery_range_year0 +
    b_degradation * battery_degradation +
    b_packreplace * battery_refurbishpackreplace +
    b_cellreplace * battery_refurbishcellreplace +
    mu_price * veh_price

  V[["no_choice"]] <- b_no_choice

  # Model settings
  mnl_settings <- list(
    # altID = altID,
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3, no_choice = 4),
    avail = list(alt1 = 1, alt2 = 1, alt3 = 1, no_choice = 1), ## Availability of alternatives
    choiceVar = choice_alt,
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
mnl_wtp <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(maxIterations = 1000)
)

#### 7. Output results
apollo_modelOutput(mnl_wtp, modelOutput_settings = list(printPVal = TRUE))
