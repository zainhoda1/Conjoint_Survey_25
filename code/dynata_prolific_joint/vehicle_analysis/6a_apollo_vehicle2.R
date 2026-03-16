source(here::here('code', 'setup.R'))

# ----Data----
## ----Load dataset----

database <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_vehicle.parquet"
)) %>%
  filter(vehicle_typesuv == 0) %>%
  filter(!is.na(hhincome_num))


# ---- ****CAR**** ----
## ----Preference space----
### ----DCE only----
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

database <- database_all %>%
  filter(vehicle_typesuv == 0)
# mean(data_covariate_num$next_veh_fuel_used_bev)

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


### ----Latent class (c=3)----
# Create an empty list to store the results of each model run
model_results <- list()
i = 1
# for (i in c(1:10)) {
# Initialize
apollo_initialise()

# Define core controls
apollo_control = list(
  modelName = paste0("LC_3c_indicator_", i),
  modelDescr = "LC model with 3 classes with indicator",
  indivID = "respID",
  nCores = 2,
  panelData = TRUE,
  outputDirectory = paste0(here(), "/code/output/model_output/apollo/vehicle")
)

database <- database_all %>%
  filter(vehicle_typesuv == 0) %>%
  filter(!is.na(hhincome_num))

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
  gamma_veh_budget_a = 0,
  delta_b = 0,
  gamma_EV_benefit_b = 0,
  gamma_EV_anxiety_b = 0,
  gamma_hhincome_b = 0,
  gamma_veh_budget_b = 0,
  delta_c = 0,
  gamma_EV_benefit_c = 0,
  gamma_EV_anxiety_c = 0,
  gamma_hhincome_c = 0,
  gamma_veh_budget_c = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c(
  "delta_a",
  "gamma_EV_benefit_a",
  "gamma_EV_anxiety_a",
  "gamma_hhincome_a",
  "gamma_veh_budget_a"
  # "gamma_EV_benefit_b",
  # "gamma_EV_anxiety_b",
  # "gamma_EV_benefit_c",
  # "gamma_EV_anxiety_c"
  # ,
  #
  # "b_range_bev_a",
  # "b_range_bev_b",
  # "b_range_bev_c",
)

# apollo_beta[!names(apollo_beta) %in% apollo_fixed] <-
#   runif(sum(!names(apollo_beta) %in% apollo_fixed), -1, 1)

apollo_beta = apollo_readBeta(
  apollo_beta,
  apollo_fixed,
  "lc_2c_indicator",
  overwriteFixed = FALSE
)

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
    gamma_veh_budget_a * next_veh_budget_k
  V[["class_b"]] = delta_b +
    gamma_EV_benefit_b * FA_EV_benefit +
    gamma_EV_anxiety_b * FA_EV_anxiety +
    gamma_hhincome_b * hhincome_num_10k +
    gamma_veh_budget_b * next_veh_budget_k
  V[["class_c"]] = delta_c +
    gamma_EV_benefit_c * FA_EV_benefit +
    gamma_EV_anxiety_c * FA_EV_anxiety +
    gamma_hhincome_c * hhincome_num_10k +
    gamma_veh_budget_c * next_veh_budget_k

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
model_results[[paste0("LC_3c_indicator_", i)]] = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(maxIterations = 5000)
)
# }

# Filter to only include successfully estimated models
# valid_models <- model_results[
#   !sapply(model_results, function(m) m$successfulEstimation == TRUE)
# ]
#
# # Create a summary table for all 10 runs
# model_summary <- data.frame(
#   model_name = names(valid_models),
#   # LLstart = sapply(valid_models, function(x) x$LL0),
#   # LLfinal = sapply(valid_models, function(x) x$LLout),
#   AIC = sapply(valid_models, function(x) unname(x$AIC)),
#   BIC = sapply(valid_models, function(x) unname(x$BIC))
# ) %>%
#   arrange((BIC))

# Identify best-fitting model
# best_model_name <- model_summary$model_name[i]
# lc_3c_indicator <- model_results[[best_model_name]]

lc_3c_indicator <- model_results[["LC_3c_indicator_1"]]
### Show output in screen
apollo_modelOutput(
  lc_3c_indicator,
  modelOutput_settings = list(printPVal = TRUE)
)

### Save output to file(s)
apollo_saveOutput(
  lc_3c_indicator,
  saveOutput_settings = list(printPVal = 2)
)


##### POST-PROCESSING

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
# apollo_sink()

#---- OUT OF SAMPLE TESTING

apollo_outOfSample(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)


#---- BOOTSTRAP ESTIMATION

apollo_bootstrap(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  bootstrap_settings = list(nRep = 3)
)


####### POSTERIOR ANALYSIS

### Compute unconditional estimates (averaged over classes) of parameters.
### Unconditional means averaged across classes using the population-level class probabilities
unconditionals = apollo_unconditionals(
  lc_3c_indicator,
  apollo_probabilities,
  apollo_inputs
)


# computes value of travel time (VTT) for each class (ratio of beta_tt to beta_tc).
## for each class

# List of attributes for which we want WTP
attributes <- c(
  "no_choice",
  "powertrainbev",
  "powertrainhev",
  "range_bev",
  "mileage",
  "age",
  "operating_cost"
)

# Number of classes
n_classes <- length(unconditionals[["pi_values"]])

# Create an empty list to store WTPs
wtp_list <- list()

# Loop over attributes
for (attr in attributes) {
  wtp_attr <- numeric(n_classes) # store class-specific WTP
  for (class in 1:n_classes) {
    beta_attr <- unconditionals[[paste0("b_", attr)]][[class]]
    beta_price <- unconditionals[["b_price"]][[class]]
    wtp_attr[class] <- beta_attr / (-beta_price) # WTP = beta_attr / beta_price
  }
  wtp_list[[attr]] <- wtp_attr
}

# Convert to a data frame for easy viewing
wtp_df <- as.data.frame(wtp_list)
rownames(wtp_df) <- paste0("class_", 1:n_classes)
wtp_df

### Compute conditional class probability that each individual belongs to each class, given their observed choices
### Conditional means individualized using each person’s posterior
### These conditional probabilities let you assign individuals to classes or at least identify which class they’re more likely to belong to.
conditionals = apollo_conditionals(
  lc_3c_indicator,
  apollo_probabilities,
  apollo_inputs
)

conditionals_fixed <- conditionals %>%
  mutate(
    class_allocation = case_when(
      pmax(X1, X2, X3) == X1 ~ "X1",
      pmax(X1, X2, X3) == X2 ~ "X2",
      pmax(X1, X2, X3) == X3 ~ "X3"
    )
  )

data_output <- database %>%
  left_join(
    conditionals_fixed %>% select(ID, class_allocation),
    by = c("respID" = "ID")
  )

class_character <- data_output %>%
  group_by(class_allocation) %>%
  summarise(across(
    c(
      ATT_EVB_environment,
      ATT_EVB_function,
      FA_EV_benefit,
      FA_EV_anxiety,
      next_veh_fuel_used_bev,
      next_veh_fuel_used_phev
    ), # replace with your variable names
    ~ mean(.x, na.rm = TRUE)
  ))


### ----Latent class (c=3)----
# Create an empty list to store the results of each model run
model_results <- list()
i = 2
# for (i in c(1:10)) {
# Initialize
apollo_initialise()

# Define core controls
apollo_control = list(
  modelName = paste0("LC_3c_indicator_", i),
  modelDescr = "LC model with 3 classes with indicator",
  indivID = "respID",
  nCores = 2,
  panelData = TRUE,
  outputDirectory = paste0(here(), "/code/output/model_output/apollo/vehicle")
)

database <- database

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
  delta_a = 0,
  # coefficients for covariates in class‐allocation
  gamma_used_bev_a = 0,
  gamma_EV_benefit_a = 0,
  gamma_EV_anxiety_a = 0,
  delta_b = 0,
  gamma_used_bev_b = 0,
  gamma_EV_benefit_b = 0,
  gamma_EV_anxiety_b = 0,
  delta_c = 0,
  gamma_used_bev_c = 0,
  gamma_EV_benefit_c = 0,
  gamma_EV_anxiety_c = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c(
  "delta_a",
  "gamma_used_bev_a",
  "gamma_EV_benefit_a",
  "gamma_EV_anxiety_a"
  # "gamma_used_bev_b",
  # "gamma_EV_benefit_b",
  # "gamma_EV_anxiety_b",
  # "gamma_used_bev_c"
  # "gamma_EV_benefit_c",
  # "gamma_EV_anxiety_c"
  # ,
  #
  # "b_range_bev_a",
  # "b_range_phev_a",
  # "b_range_bev_b",
  # "b_range_phev_b",
  # "b_range_bev_c",
  # "b_range_phev_c"
)

# apollo_beta[!names(apollo_beta) %in% apollo_fixed] <-
#   runif(sum(!names(apollo_beta) %in% apollo_fixed), -1, 1)

apollo_beta = apollo_readBeta(
  apollo_beta,
  apollo_fixed,
  "lc_3c_indicator_1",
  overwriteFixed = FALSE
)

#### DEFINE LATENT CLASS COMPONENTS
apollo_lcPars = function(apollo_beta, apollo_inputs) {
  lcpars = list()
  lcpars[["b_no_choice"]] = list(b_no_choice_a, b_no_choice_b, b_no_choice_c)
  lcpars[["b_powertrainbev"]] = list(
    b_powertrainbev_a,
    b_powertrainbev_b,
    b_powertrainbev_c
  )
  lcpars[["b_powertrainphev"]] = list(
    b_powertrainphev_a,
    b_powertrainphev_b,
    b_powertrainphev_c
  )
  lcpars[["b_powertrainhev"]] = list(
    b_powertrainhev_a,
    b_powertrainhev_b,
    b_powertrainhev_c
  )
  lcpars[["b_range_bev"]] = list(b_range_bev_a, b_range_bev_b, b_range_bev_c)
  lcpars[["b_range_phev"]] = list(
    b_range_phev_a,
    b_range_phev_b,
    b_range_phev_c
  )
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
    gamma_EV_anxiety_a * FA_EV_anxiety
  V[["class_b"]] = delta_b +
    gamma_used_bev_b * next_veh_fuel_used_bev +
    gamma_EV_benefit_b * FA_EV_benefit +
    gamma_EV_anxiety_b * FA_EV_anxiety
  V[["class_c"]] = delta_c +
    gamma_used_bev_c * next_veh_fuel_used_bev +
    gamma_EV_benefit_c * FA_EV_benefit +
    gamma_EV_anxiety_c * FA_EV_anxiety

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
      b_powertrainphev[[s]] * powertrainphev_1 +
      b_powertrainhev[[s]] * powertrainhev_1 +
      b_range_bev[[s]] * range_bev_1 +
      b_range_phev[[s]] * range_phev_1 +
      b_mileage[[s]] * mileage_1 +
      b_age[[s]] * age_1 +
      b_operating_cost[[s]] * operating_cost_1 +
      b_price[[s]] * price_1

    V[["alt2"]] <-
      b_powertrainbev[[s]] *
      powertrainbev_2 +
      b_powertrainphev[[s]] * powertrainphev_2 +
      b_powertrainhev[[s]] * powertrainhev_2 +
      b_range_bev[[s]] * range_bev_2 +
      b_range_phev[[s]] * range_phev_2 +
      b_mileage[[s]] * mileage_2 +
      b_age[[s]] * age_2 +
      b_operating_cost[[s]] * operating_cost_2 +
      b_price[[s]] * price_2

    V[["alt3"]] <-
      b_powertrainbev[[s]] *
      powertrainbev_3 +
      b_powertrainphev[[s]] * powertrainphev_3 +
      b_powertrainhev[[s]] * powertrainhev_3 +
      b_range_bev[[s]] * range_bev_3 +
      b_range_phev[[s]] * range_phev_3 +
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
model_results[[paste0("LC_3c_indicator_", i)]] = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(maxIterations = 5000)
)
# }

# # Filter to only include successfully estimated models
# valid_models <- model_results[
#   !sapply(model_results, function(m) m$successfulEstimation == TRUE)
# ]
#
# # Create a summary table for all 10 runs
# model_summary <- data.frame(
#   model_name = names(valid_models),
#   # LLstart = sapply(valid_models, function(x) x$LL0),
#   # LLfinal = sapply(valid_models, function(x) x$LLout),
#   AIC = sapply(valid_models, function(x) unname(x$AIC)),
#   BIC = sapply(valid_models, function(x) unname(x$BIC))
# ) %>%
#   arrange((BIC))

# Identify best-fitting model
best_model_name <- model_summary$model_name[i]
lc_3c_indicator <- model_results[[best_model_name]]

### Show output in screen
apollo_modelOutput(
  lc_3c_indicator,
  modelOutput_settings = list(printPVal = TRUE)
)

### Save output to file(s)
apollo_saveOutput(
  lc_3c_indicator,
  saveOutput_settings = list(printPVal = 2)
)


##### POST-PROCESSING

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
# apollo_sink()

#---- OUT OF SAMPLE TESTING

apollo_outOfSample(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)


#---- BOOTSTRAP ESTIMATION

apollo_bootstrap(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  bootstrap_settings = list(nRep = 3)
)


####### POSTERIOR ANALYSIS

### Compute unconditional estimates (averaged over classes) of parameters.
### Unconditional means averaged across classes using the population-level class probabilities
unconditionals = apollo_unconditionals(
  lc_3c_indicator,
  apollo_probabilities,
  apollo_inputs
)


# computes value of travel time (VTT) for each class (ratio of beta_tt to beta_tc).
## for each class

# List of attributes for which we want WTP
attributes <- c(
  "no_choice",
  "powertrainbev",
  "powertrainphev",
  "powertrainhev",
  "range_bev",
  "range_phev",
  "mileage",
  "age",
  "operating_cost"
)

# Number of classes
n_classes <- length(unconditionals[["pi_values"]])

# Create an empty list to store WTPs
wtp_list <- list()

# Loop over attributes
for (attr in attributes) {
  wtp_attr <- numeric(n_classes) # store class-specific WTP
  for (class in 1:n_classes) {
    beta_attr <- unconditionals[[paste0("b_", attr)]][[class]]
    beta_price <- unconditionals[["b_price"]][[class]]
    wtp_attr[class] <- beta_attr / (-beta_price) # WTP = beta_attr / beta_price
  }
  wtp_list[[attr]] <- wtp_attr
}

# Convert to a data frame for easy viewing
wtp_df <- as.data.frame(wtp_list)
rownames(wtp_df) <- paste0("class_", 1:n_classes)
wtp_df

### Compute conditional class probability that each individual belongs to each class, given their observed choices
### Conditional means individualized using each person’s posterior
### These conditional probabilities let you assign individuals to classes or at least identify which class they’re more likely to belong to.
conditionals = apollo_conditionals(
  lc_mnl_2c_indicator,
  apollo_probabilities,
  apollo_inputs
)

conditionals_fixed <- conditionals %>%
  mutate(
    class_allocation = case_when(
      pmax(X1, X2) == X1 ~ "X1",
      pmax(X1, X2) == X2 ~ "X2"
    )
  )

data_output <- database %>%
  left_join(
    conditionals_fixed %>% select(ID, class_allocation),
    by = c("respID" = "ID")
  )

class_character <- data_output %>%
  group_by(class_allocation) %>%
  summarise(across(
    c(
      ATT_EVB_environment,
      ATT_EVB_function,
      FA_EV_benefit,
      FA_EV_anxiety,
      next_veh_fuel_used_bev,
    ), # replace with your variable names
    ~ mean(.x, na.rm = TRUE)
  ))


### ----Latent class (c=3) reducedsample----
# Create an empty list to store the results of each model run
model_results <- list()
i = 2
# for (i in c(1:10)) {
# Initialize
apollo_initialise()

# Define core controls
apollo_control = list(
  # modelName = paste0("LC_3c_indicator_", i),
  modelName = "LC_3c_indicator_reducedsample",
  modelDescr = "LC model with 3 classes with indicator",
  indivID = "respID",
  nCores = 2,
  panelData = TRUE,
  outputDirectory = paste0(here(), "/code/output/model_output/apollo/vehicle")
)

database <- database

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  # attributes for each class
  b_no_choice_a = 0,
  b_powertrainbev_a = 0,
  b_powertrainphev_a = 0,
  b_powertrainhev_a = 0,
  b_range_bev_a = 0,
  b_range_phev_a = 0,
  b_mileage_a = 0,
  b_age_a = 0,
  b_operating_cost_a = 0,
  b_price_a = 0,
  b_no_choice_b = 0,
  b_powertrainbev_b = 0,
  b_powertrainphev_b = 0,
  b_powertrainhev_b = 0,
  b_range_bev_b = 0,
  b_range_phev_b = 0,
  b_mileage_b = 0,
  b_age_b = 0,
  b_operating_cost_b = 0,
  b_price_b = 0,
  b_no_choice_c = 0,
  b_powertrainbev_c = 0,
  b_powertrainphev_c = 0,
  b_powertrainhev_c = 0,
  b_range_bev_c = 0,
  b_range_phev_c = 0,
  b_mileage_c = 0,
  b_age_c = 0,
  b_operating_cost_c = 0,
  b_price_c = 0,

  # base intercepts for class allocation model
  delta_a = 0,
  # coefficients for covariates in class‐allocation
  gamma_hhincome_a = 0,
  gamma_used_bev_a = 0,
  gamma_EV_benefit_a = 0,
  gamma_EV_anxiety_a = 0,
  delta_b = 0,
  gamma_hhincome_b = 0,
  gamma_used_bev_b = 0,
  gamma_EV_benefit_b = 0,
  gamma_EV_anxiety_b = 0,
  delta_c = 0,
  gamma_hhincome_c = 0,
  gamma_used_bev_c = 0,
  gamma_EV_benefit_c = 0,
  gamma_EV_anxiety_c = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c(
  "delta_a",
  "gamma_hhincome_a",
  "gamma_used_bev_a",
  "gamma_EV_benefit_a",
  "gamma_EV_anxiety_a",
  "gamma_used_bev_b",
  # "gamma_EV_benefit_b",
  # "gamma_EV_anxiety_b",
  "gamma_used_bev_c"
  # "gamma_EV_benefit_c",
  # "gamma_EV_anxiety_c"
  # ,
  #
  # "b_range_bev_a",
  # "b_range_phev_a",
  # "b_range_bev_b",
  # "b_range_phev_b",
  # "b_range_bev_c",
  # "b_range_phev_c"
)

# apollo_beta[!names(apollo_beta) %in% apollo_fixed] <-
#   runif(sum(!names(apollo_beta) %in% apollo_fixed), -1, 1)

apollo_beta = apollo_readBeta(
  apollo_beta,
  apollo_fixed,
  "lc_3c_indicator",
  overwriteFixed = FALSE
)

#### DEFINE LATENT CLASS COMPONENTS
apollo_lcPars = function(apollo_beta, apollo_inputs) {
  lcpars = list()
  lcpars[["b_no_choice"]] = list(b_no_choice_a, b_no_choice_b, b_no_choice_c)
  lcpars[["b_powertrainbev"]] = list(
    b_powertrainbev_a,
    b_powertrainbev_b,
    b_powertrainbev_c
  )
  lcpars[["b_powertrainphev"]] = list(
    b_powertrainphev_a,
    b_powertrainphev_b,
    b_powertrainphev_c
  )
  lcpars[["b_powertrainhev"]] = list(
    b_powertrainhev_a,
    b_powertrainhev_b,
    b_powertrainhev_c
  )
  lcpars[["b_range_bev"]] = list(b_range_bev_a, b_range_bev_b, b_range_bev_c)
  lcpars[["b_range_phev"]] = list(
    b_range_phev_a,
    b_range_phev_b,
    b_range_phev_c
  )
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
    gamma_hhincome_a * hhincome_num +
    gamma_EV_benefit_a * FA_EV_benefit +
    gamma_EV_anxiety_a * FA_EV_anxiety
  V[["class_b"]] = delta_b +
    gamma_hhincome_b * hhincome_num +
    gamma_used_bev_b * next_veh_fuel_used_bev +
    gamma_EV_benefit_b * FA_EV_benefit +
    gamma_EV_anxiety_b * FA_EV_anxiety
  V[["class_c"]] = delta_c +
    gamma_hhincome_c * hhincome_num +
    gamma_used_bev_c * next_veh_fuel_used_bev +
    gamma_EV_benefit_c * FA_EV_benefit +
    gamma_EV_anxiety_c * FA_EV_anxiety

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
      b_powertrainphev[[s]] * powertrainphev_1 +
      b_powertrainhev[[s]] * powertrainhev_1 +
      b_range_bev[[s]] * range_bev_1 +
      b_range_phev[[s]] * range_phev_1 +
      b_mileage[[s]] * mileage_1 +
      b_age[[s]] * age_1 +
      b_operating_cost[[s]] * operating_cost_1 +
      b_price[[s]] * price_1

    V[["alt2"]] <-
      b_powertrainbev[[s]] *
      powertrainbev_2 +
      b_powertrainphev[[s]] * powertrainphev_2 +
      b_powertrainhev[[s]] * powertrainhev_2 +
      b_range_bev[[s]] * range_bev_2 +
      b_range_phev[[s]] * range_phev_2 +
      b_mileage[[s]] * mileage_2 +
      b_age[[s]] * age_2 +
      b_operating_cost[[s]] * operating_cost_2 +
      b_price[[s]] * price_2

    V[["alt3"]] <-
      b_powertrainbev[[s]] *
      powertrainbev_3 +
      b_powertrainphev[[s]] * powertrainphev_3 +
      b_powertrainhev[[s]] * powertrainhev_3 +
      b_range_bev[[s]] * range_bev_3 +
      b_range_phev[[s]] * range_phev_3 +
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
lc_3c_indicator_reducedsample = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(maxIterations = 5000)
)
# }

# # Filter to only include successfully estimated models
# valid_models <- model_results[
#   !sapply(model_results, function(m) m$successfulEstimation == TRUE)
# ]
#
# # Create a summary table for all 10 runs
# model_summary <- data.frame(
#   model_name = names(valid_models),
#   # LLstart = sapply(valid_models, function(x) x$LL0),
#   # LLfinal = sapply(valid_models, function(x) x$LLout),
#   AIC = sapply(valid_models, function(x) unname(x$AIC)),
#   BIC = sapply(valid_models, function(x) unname(x$BIC))
# ) %>%
#   arrange((BIC))

# Identify best-fitting model
# best_model_name <- model_summary$model_name[i]
# lc_3c_indicator <- model_results[[best_model_name]]

### Show output in screen
apollo_modelOutput(
  lc_3c_indicator_reducedsample,
  modelOutput_settings = list(printPVal = TRUE)
)

### Save output to file(s)
apollo_saveOutput(
  lc_3c_indicator_reducedsample,
  saveOutput_settings = list(printPVal = 2)
)


##### POST-PROCESSING

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
# apollo_sink()

#---- OUT OF SAMPLE TESTING

apollo_outOfSample(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)


#---- BOOTSTRAP ESTIMATION

apollo_bootstrap(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  bootstrap_settings = list(nRep = 3)
)


####### POSTERIOR ANALYSIS

### Compute unconditional estimates (averaged over classes) of parameters.
### Unconditional means averaged across classes using the population-level class probabilities
unconditionals = apollo_unconditionals(
  lc_3c_indicator_reducedsample,
  apollo_probabilities,
  apollo_inputs
)


# computes value of travel time (VTT) for each class (ratio of beta_tt to beta_tc).
## for each class

# List of attributes for which we want WTP
attributes <- c(
  "no_choice",
  "powertrainbev",
  "powertrainphev",
  "powertrainhev",
  "range_bev",
  "range_phev",
  "mileage",
  "age",
  "operating_cost"
)

# Number of classes
n_classes <- length(unconditionals[["pi_values"]])

# Create an empty list to store WTPs
wtp_list <- list()

# Loop over attributes
for (attr in attributes) {
  wtp_attr <- numeric(n_classes) # store class-specific WTP
  for (class in 1:n_classes) {
    beta_attr <- unconditionals[[paste0("b_", attr)]][[class]]
    beta_price <- unconditionals[["b_price"]][[class]]
    wtp_attr[class] <- beta_attr / (-beta_price) # WTP = beta_attr / beta_price
  }
  wtp_list[[attr]] <- wtp_attr
}

# Convert to a data frame for easy viewing
wtp_df <- as.data.frame(wtp_list)
rownames(wtp_df) <- paste0("class_", 1:n_classes)
wtp_df

### Compute conditional class probability that each individual belongs to each class, given their observed choices
### Conditional means individualized using each person’s posterior
### These conditional probabilities let you assign individuals to classes or at least identify which class they’re more likely to belong to.
conditionals = apollo_conditionals(
  lc_3c_indicator_reducedsample,
  apollo_probabilities,
  apollo_inputs
)

conditionals_fixed <- conditionals %>%
  mutate(
    class_allocation = case_when(
      pmax(X1, X2, X3) == X1 ~ "X1",
      pmax(X1, X2, X3) == X2 ~ "X2",
      pmax(X1, X2, X3) == X3 ~ "X3"
    )
  )

data_output <- database %>%
  left_join(
    conditionals_fixed %>% select(ID, class_allocation),
    by = c("respID" = "ID")
  )

class_character <- data_output %>%
  group_by(class_allocation) %>%
  summarise(across(
    c(
      hhincome_num,
      FA_EV_benefit,
      FA_EV_anxiety,
      next_veh_fuel_new_bev,
      next_veh_fuel_used_bev,
    ), # replace with your variable names
    ~ mean(.x, na.rm = TRUE)
  ))


## ----Combine Output----
model_combine <- data.frame()
### Read the entire text file
for (i in c(1:3)) {
  model_coef <- read_csv(paste0(
    here::here(),
    "/code/output/model_output/apollo/vehicle",
    "/LC_",
    i,
    "c_indicator_estimates.csv"
  )) %>%
    select(1, Estimate, starts_with("Rob."), -contains("t-ratio")) %>%
    as.data.frame() %>%
    setNames(c("variables/fit_indices", "est", "std_err", "p_val")) %>%
    mutate(
      sig = case_when(
        p_val < 0.001 ~ "***",
        p_val < 0.01 ~ "**",
        p_val < 0.05 ~ "*",
        p_val < 0.1 ~ ".",
        T ~ " "
      )
      # ,
      # change_odds=paste0(round((OR-1)*100,0),"%")
    ) %>%
    setNames(c(
      "variables/fit_indices",
      paste0("est_m", i),
      paste0("std_err_m", i),
      paste0("p_val_m", i),
      paste0("sig_m", i)
      # ,paste0("change_odds_m",i)
    ))

  model_fit <- as.data.frame(readLines(paste0(
    here::here(),
    "/code/output/model_output/apollo/vehicle",
    "/LC_",
    i,
    "c_indicator_output.txt"
  ))) %>%
    slice(c(15, 16, 22:31)) %>%
    setNames("fit") %>%
    separate(
      fit,
      into = c("variables/fit_indices", "fit_indice"),
      sep = ": "
    ) %>%
    setNames(c("variables/fit_indices", paste0("fit_indice_m", i)))

  model <- bind_rows(model_coef, model_fit)

  if (nrow(model_combine) == 0) {
    model_combine <- model
  } else {
    model_combine <- model_combine %>%
      full_join(model, by = "variables/fit_indices")
  }
}


# Create an empty row (matching column structure)
empty_row <- model_combine[1:2, ] # Copy structure
empty_row[,] <- NA # Set all values to NA

model_combine_output <- model_combine %>%
  slice(-c(11:22)) %>%
  bind_rows(empty_row, slice(model_combine, c(11:22))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# model_combine_output<-model_combine_output %>%
#   mutate_all(~replace(., is.na(.), NA))

write.xlsx(
  model_combine_output,
  paste0(
    here::here(),
    "/code/output/model_output/apollo/vehicle",
    "/0_vehicle_model_combine_output.xlsx"
  ),
  sheetName = "coef_fit",
  row.names = F
)
## ----WTP space----

# Initialize
apollo_initialise()

# Define core controls
apollo_control = list(
  modelName = "MNL_DCE",
  modelDescr = "MNL model in WTP-space",
  indivID = "respID",
  panelData = TRUE,
  outputDirectory = paste0(here(), "/code/output/model_output/apollo"),
  mixing = FALSE
)

database <- data_covariate %>%
  group_by(respID, obsID) %>%
  mutate(choice_alt = altID[choice == 1]) %>%
  ungroup()

#### 2. Parameter starting values
# You can initialize them to 0 or to your logitr estimates.
apollo_beta <- c(
  wtp_mileage = 0,
  wtp_range_year0 = 0,
  wtp_degradation = 0,
  wtp_packreplace = 0,
  wtp_cellreplace = 0,
  b_no_choice = 0,
  b_price = 1 # price coefficient (scale parameter)
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
    b_price *
    (veh_price +
      wtp_mileage * veh_mileage +
      wtp_range_year0 * battery_range_year0 +
      wtp_degradation * battery_degradation +
      wtp_packreplace * battery_refurbishpackreplace +
      wtp_cellreplace * battery_refurbishcellreplace)

  V[["alt2"]] <-
    b_price *
    (veh_price +
      wtp_mileage * veh_mileage +
      wtp_range_year0 * battery_range_year0 +
      wtp_degradation * battery_degradation +
      wtp_packreplace * battery_refurbishpackreplace +
      wtp_cellreplace * battery_refurbishcellreplace)

  V[["alt3"]] <-
    b_price *
    (veh_price +
      wtp_mileage * veh_mileage +
      wtp_range_year0 * battery_range_year0 +
      wtp_degradation * battery_degradation +
      wtp_packreplace * battery_refurbishpackreplace +
      wtp_cellreplace * battery_refurbishcellreplace)

  V[["no_choice"]] <- b_price * b_no_choice

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
