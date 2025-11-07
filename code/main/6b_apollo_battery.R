source(here::here('code', 'setup.R'))

# ----Data----
## ----Load dataset----

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
    starts_with("EV_"),
    starts_with("next_veh_fuel_")
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
  mutate(
    veh_price_cate = case_when(
      is.na(veh_price) ~ NA,
      veh_price < 1.5 ~ "price_1",
      veh_price < 2.5 ~ "price_2",
      veh_price < 3.5 ~ "price_3",
      veh_price < 4.5 ~ "price_4",
      veh_price < 5.5 ~ "price_5",
      T ~ "price_6"
    )
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
  data_dce %>% select(-c(psid, veh_price)),
  coding = 'dummy',
  ref_levels = list(
    battery_refurbish = 'original',
    veh_price_cate = 'price_1'
  )
) %>%
  as.data.frame()

data_dce_dummy <- cbind(
  data_dce_dummy,
  data_dce %>%
    select(psid, veh_price) %>%
    mutate(veh_price = case_when(is.na(veh_price) ~ 0, T ~ veh_price))
)

data_dce_dummy_apollo <- data_dce_dummy %>%
  select(
    psid,
    respID,
    qID,
    altID,
    choice,
    veh_mileage,
    battery_range_year0,
    battery_degradation,
    battery_refurbishcellreplace,
    battery_refurbishpackreplace,
    veh_price,
  ) %>%
  pivot_wider(
    id_cols = c(psid, respID, qID),
    names_from = altID,
    values_from = c(
      veh_mileage,
      battery_range_year0,
      battery_degradation,
      battery_refurbishcellreplace,
      battery_refurbishpackreplace,
      veh_price,
      choice
    ),
    names_glue = "{.value}_{altID}"
  ) %>%
  mutate(
    choice = case_when(
      choice_1 == 1 ~ 1,
      choice_2 == 1 ~ 2,
      choice_3 == 1 ~ 3,
      choice_4 == 1 ~ 4
    )
  ) %>%
  select(-c(choice_1, choice_2, choice_3, choice_4))


data_covariate <- data_dce_dummy_apollo %>%
  left_join(data_variable, by = "psid")
# n_distinct(data_covariate$psid) #373

data_covariate_num <- data_covariate %>%
  mutate(
    across(
      any_of(c(
        "ATT_EVB_environment",
        "ATT_EVB_function",
        "ATT_techsavvy",
        "ATT_risktaker"
      )),
      ~ case_when(
        grepl("strongly_disagree", ., ignore.case = TRUE) ~ 1,
        grepl("somewhat_disagree", ., ignore.case = TRUE) ~ 2,
        grepl("neutral", ., ignore.case = TRUE) ~ 3,
        grepl("somewhat_agree", ., ignore.case = TRUE) ~ 4,
        grepl("strongly_agree", ., ignore.case = TRUE) ~ 5,
        TRUE ~ NA
      )
    ),
    across(
      any_of(c(
        "next_veh_fuel_used_bev"
      )),
      ~ case_when(
        grepl("very_unlikely", ., ignore.case = TRUE) ~ 1,
        grepl("somewhat_unlikely", ., ignore.case = TRUE) ~ 2,
        grepl("neutral", ., ignore.case = TRUE) ~ 3,
        grepl("somewhat_likely", ., ignore.case = TRUE) ~ 4,
        grepl("very_likely", ., ignore.case = TRUE) ~ 5,
        TRUE ~ NA
      )
    )
  )

data_covariate_dummy <- cbc_encode(
  data_covariate %>%
    select(!psid) %>%
    mutate(
      obsID = row_number(),
      altID = row_number(),
      profileID = row_number()
    ) %>%
    mutate(
      knowledge_gas = as.character(knowledge_gas),
      knowledge_plugin = as.character(knowledge_plugin),
      knowledge_ev = as.character(knowledge_ev),
      knowledge_subsidy = as.character(knowledge_subsidy)
    ),
  coding = 'dummy',
  ref_levels = list(
    gender_cate = 'female',
    ethnicity_cate = 'hispanic',
    race_cate = 'white_only',
    education_cate = 'high_school',
    student_cate = 'student',
    employment_cate = 'full_time',
    hhtenure_cate = 'own',
    hhtype_cate = 'sf_detached',
    ATT_EVB_environment = 'neutral',
    ATT_EVB_function = 'neutral',
    ATT_techsavvy = 'neutral',
    ATT_risktaker = 'neutral',
    ATT_climate = 'not_at_all',
    ATT_political = 'conservative',
    ATT_voting = 'democratic',
    Veh_hh_fuel = 'icev_only',
    EV_charger = 'no',
    EV_neighbor = 'no',
    knowledge_gas = '0',
    knowledge_plugin = '0',
    knowledge_ev = '0',
    knowledge_subsidy = '0'
  )
) %>%
  as.data.frame()

# ---- ****Apollo**** ----
## ----Preference space----
### ----DCE only----
# Initialize
apollo_initialise()

# Define core controls
apollo_control = list(
  modelName = "MNL_DCE_pref",
  modelDescr = "MNL model",
  indivID = "respID",
  panelData = TRUE,
  outputDirectory = paste0(here(), "/code/main/model_output/apollo"),
  mixing = FALSE
)

database <- data_dce_dummy_apollo

#### 2. Parameter starting values
apollo_beta <- c(
  b_mileage = 0,
  b_price = 0,
  b_range_year0 = 0,
  b_degradation = 0,
  b_packreplace = 0,
  b_cellreplace = 0,
  asc_no_choice = 0
  # price coefficient (scale parameter)
)


#### 3. No random parameters (simple MNL)
# Skip apollo_randCoeff

#### 4. Validate inputs
apollo_fixed <- c(
  # "asc_no_choice"
)
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
    veh_mileage_1 +
    b_range_year0 * battery_range_year0_1 +
    b_degradation * battery_degradation_1 +
    b_packreplace * battery_refurbishpackreplace_1 +
    b_cellreplace * battery_refurbishcellreplace_1 +
    b_price * veh_price_1

  V[["alt2"]] <-
    b_mileage *
    veh_mileage_2 +
    b_range_year0 * battery_range_year0_2 +
    b_degradation * battery_degradation_2 +
    b_packreplace * battery_refurbishpackreplace_2 +
    b_cellreplace * battery_refurbishcellreplace_2 +
    b_price * veh_price_2

  V[["alt3"]] <-
    b_mileage *
    veh_mileage_3 +
    b_range_year0 * battery_range_year0_3 +
    b_degradation * battery_degradation_3 +
    b_packreplace * battery_refurbishpackreplace_3 +
    b_cellreplace * battery_refurbishcellreplace_3 +
    b_price * veh_price_3

  V[["no_choice"]] <- asc_no_choice

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
mnl_pref_apollo <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(maxIterations = 1000)
)

#### 7. Output results
apollo_modelOutput(
  mnl_pref_apollo,
  modelOutput_settings = list(printPVal = TRUE)
)

#### 8. WTP - delta method
for (i in c(
  "b_mileage",
  "b_range_year0",
  "b_degradation",
  "b_packreplace",
  "b_cellreplace",
  "asc_no_choice"
)) {
  deltaMethod_settings <- list(
    operation = "ratio",
    parName1 = i,
    parName2 = "b_price",
    multPar1 = -1
  )
  apollo_deltaMethod(mnl_pref_apollo, deltaMethod_settings)
}

### ----Latent class (c=2)----
# Initialize
apollo_initialise()

# Define core controls
apollo_control = list(
  modelName = "LC_DCE_2c_indicator",
  modelDescr = "LC model with 2 classes with indicator (ATT and knowledge)",
  indivID = "respID",
  nCores = 2,
  panelData = TRUE,
  outputDirectory = paste0(here(), "/code/main/model_output/apollo")
)


### Loading data from package
database = data_covariate_num %>%
  filter(
    !is.na(next_veh_fuel_used_bev) &
      !is.na(ATT_EVB_environment) &
      !is.na(ATT_EVB_function) &
      !is.na(FA_EV_benefit) &
      !is.na(FA_EV_anxiety)
    # !is.na(knowledge_gas) &
    # !is.na(knowledge_plugin) &
    # !is.na(knowledge_ev) &
    # !is.na(knowledge_subsidy)
  )


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  # lternative‐specific constants for each class
  asc_no_choice = 0,
  # attributes for each class
  b_mileage_a = 0,
  b_mileage_b = 0,
  b_range_year0_a = 0,
  b_range_year0_b = 0,
  b_degradation_a = 0,
  b_degradation_b = 0,
  b_packreplace_a = 0,
  b_packreplace_b = 0,
  b_cellreplace_a = 0,
  b_cellreplace_b = 0,
  b_price_a = 0,
  b_price_b = 0,

  # base intercepts for class allocation model
  delta_a = 0,
  delta_b = 0,
  # coefficients for covariates in class‐allocation
  gamma_used_bev_a = 0,
  gamma_EVB_environ_a = 0,
  gamma_EVB_function_a = 0,
  gamma_EV_benefit_a = 0,
  gamma_EV_anxiety_a = 0,
  # gamma_knowledge_gas_a = 0,
  # gamma_knowledge_plugin_a = 0,
  # gamma_knowledge_ev_a = 0,
  # gamma_knowledge_subsidy_a = 0,
  gamma_used_bev_b = 0,
  gamma_EVB_environ_b = 0,
  gamma_EVB_function_b = 0,
  gamma_EV_benefit_b = 0,
  gamma_EV_anxiety_b = 0
  # gamma_knowledge_gas_b = 0,
  # gamma_knowledge_plugin_b = 0,
  # gamma_knowledge_ev_b = 0,
  # gamma_knowledge_subsidy_b = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c(
  "asc_no_choice",
  "delta_a",
  "gamma_used_bev_a",
  "gamma_EVB_environ_a",
  "gamma_EVB_function_a",
  "gamma_EV_benefit_a",
  "gamma_EV_anxiety_a"
  # "gamma_knowledge_gas_a",
  # "gamma_knowledge_plugin_a",
  # "gamma_knowledge_ev_a",
  # "gamma_knowledge_subsidy_a"
)


#### DEFINE LATENT CLASS COMPONENTS

apollo_lcPars = function(apollo_beta, apollo_inputs) {
  lcpars = list()
  lcpars[["b_mileage"]] = list(b_mileage_a, b_mileage_b)
  lcpars[["b_range_year0"]] = list(b_range_year0_a, b_range_year0_b)
  lcpars[["b_degradation"]] = list(b_degradation_a, b_degradation_b)
  lcpars[["b_packreplace"]] = list(b_packreplace_a, b_packreplace_b)
  lcpars[["b_cellreplace"]] = list(b_cellreplace_a, b_cellreplace_b)
  lcpars[["b_price"]] = list(b_price_a, b_price_b)

  ### Utilities of class allocation model: ow likely a person is to belong to each class based on indicators
  V = list()
  V[["class_a"]] = delta_a +
    gamma_used_bev_a * next_veh_fuel_used_bev +
    gamma_EVB_environ_a * ATT_EVB_environment +
    gamma_EVB_function_a * ATT_EVB_function +
    gamma_EV_benefit_a * FA_EV_benefit +
    gamma_EV_anxiety_a * FA_EV_anxiety
  # gamma_knowledge_gas_a * knowledge_gas +
  # gamma_knowledge_plugin_a * knowledge_plugin +
  # gamma_knowledge_ev_a * knowledge_ev +
  # gamma_knowledge_subsidy_a * knowledge_subsidy
  V[["class_b"]] = delta_b +
    gamma_used_bev_b * next_veh_fuel_used_bev +
    gamma_EVB_environ_b * ATT_EVB_environment +
    gamma_EVB_function_b * ATT_EVB_function +
    gamma_EV_benefit_b * FA_EV_benefit +
    gamma_EV_anxiety_b * FA_EV_anxiety
  # gamma_knowledge_gas_b * knowledge_gas +
  # gamma_knowledge_plugin_b * knowledge_plugin +
  # gamma_knowledge_ev_b * knowledge_ev +
  # gamma_knowledge_subsidy_b * knowledge_subsidy

  ### Settings for class allocation models
  classAlloc_settings = list(
    classes = c(class_a = 1, class_b = 2),
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
  for (s in 1:2) {
    ### Compute class-specific utilities
    V = list()
    V[["alt1"]] <-
      b_mileage[[s]] *
      veh_mileage_1 +
      b_range_year0[[s]] * battery_range_year0_1 +
      b_degradation[[s]] * battery_degradation_1 +
      b_packreplace[[s]] * battery_refurbishpackreplace_1 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_1 +
      b_price[[s]] * veh_price_1

    V[["alt2"]] <-
      b_mileage[[s]] *
      veh_mileage_2 +
      b_range_year0[[s]] * battery_range_year0_2 +
      b_degradation[[s]] * battery_degradation_2 +
      b_packreplace[[s]] * battery_refurbishpackreplace_2 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_2 +
      b_price[[s]] * veh_price_2

    V[["alt3"]] <-
      b_mileage[[s]] *
      veh_mileage_3 +
      b_range_year0[[s]] * battery_range_year0_3 +
      b_degradation[[s]] * battery_degradation_3 +
      b_packreplace[[s]] * battery_refurbishpackreplace_3 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_3 +
      b_price[[s]] * veh_price_3

    V[["no_choice"]] <- asc_no_choice

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
lc_mnl_2c_indicator = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

### Show output in screen
apollo_modelOutput(
  lc_mnl_2c_indicator,
  modelOutput_settings = list(printPVal = TRUE)
)

### Save output to file(s)
apollo_saveOutput(
  lc_mnl_2c_indicator,
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
  lc_mnl_2c_indicator,
  apollo_probabilities,
  apollo_inputs
)


# computes value of travel time (VTT) for each class (ratio of beta_tt to beta_tc).
## for each class

# List of attributes for which we want WTP
attributes <- c(
  "mileage",
  "range_year0",
  "degradation",
  "packreplace",
  "cellreplace"
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

## for each individual
mileage_unconditional = unconditionals[["pi_values"]][[1]] *
  vtt_class_a +
  unconditionals[["pi_values"]][[2]] * vtt_class_b


### Compute conditional class probability that each individual belongs to each class, given their observed choices
### Conditional means individualized using each person’s posterior
### These conditional probabilities let you assign individuals to classes or at least identify which class they’re more likely to belong to.
conditionals = apollo_conditionals(
  lc_mnl_2c_indicator,
  apollo_probabilities,
  apollo_inputs
)

summary(conditionals)
summary(as.data.frame(unconditionals[["pi_values"]]))

vtt_conditional = conditionals[, 2] *
  vtt_class_a +
  conditionals[, 3] * vtt_class_b

summary(vtt_unconditional)
summary(vtt_conditional)

### Take first value of covariates for each person
EVB_environ_n = apollo_firstRow(database$ATT_EVB_environment, apollo_inputs)

### Compute posterior values for covariates
post_EVB_environ_n = colSums(EVB_environ_n * conditionals[, 2:3]) /
  colSums(conditionals[, 2:3])

post_commute


#---- switch off writing to file

### ----Latent class (c=3)----
# Initialize
apollo_initialise()

# Define core controls
apollo_control = list(
  modelName = "LC_DCE_3c_indicator",
  modelDescr = "LC model with 3 classes with indicator (ATT and knowledge)",
  indivID = "respID",
  nCores = 2,
  panelData = TRUE,
  outputDirectory = paste0(here(), "/code/main/model_output/apollo")
)


### Loading data from package
database = data_covariate_num %>%
  filter(
    !is.na(next_veh_fuel_used_bev) &
      !is.na(ATT_EVB_environment) &
      !is.na(ATT_EVB_function) &
      !is.na(FA_EV_benefit) &
      !is.na(FA_EV_anxiety)
    # !is.na(knowledge_gas) &
    # !is.na(knowledge_plugin) &
    # !is.na(knowledge_ev) &
    # !is.na(knowledge_subsidy)
  )


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  # lternative‐specific constants for each class
  asc_no_choice = 0,
  # attributes for each class
  b_mileage_a = 0,
  b_range_year0_a = 0,
  b_degradation_a = 0,
  b_packreplace_a = 0,
  b_cellreplace_a = 0,
  b_price_a = 0,
  b_mileage_b = 0,
  b_range_year0_b = 0,
  b_degradation_b = 0,
  b_packreplace_b = 0,
  b_cellreplace_b = 0,
  b_price_b = 0,
  b_mileage_c = 0,
  b_range_year0_c = 0,
  b_degradation_c = 0,
  b_packreplace_c = 0,
  b_cellreplace_c = 0,
  b_price_c = 0,

  # base intercepts for class allocation model
  delta_a = 0,
  delta_b = 0,
  delta_c = 0,
  # coefficients for covariates in class‐allocation
  gamma_used_bev_a = 0,
  gamma_EVB_environ_a = 0,
  gamma_EVB_function_a = 0,
  gamma_EV_benefit_a = 0,
  gamma_EV_anxiety_a = 0,
  # gamma_knowledge_gas_a = 0,
  # gamma_knowledge_plugin_a = 0,
  # gamma_knowledge_ev_a = 0,
  # gamma_knowledge_subsidy_a = 0,
  gamma_used_bev_b = 0,
  gamma_EVB_environ_b = 0,
  gamma_EVB_function_b = 0,
  gamma_EV_benefit_b = 0,
  gamma_EV_anxiety_b = 0,
  # gamma_knowledge_gas_b = 0,
  # gamma_knowledge_plugin_b = 0,
  # gamma_knowledge_ev_b = 0,
  # gamma_knowledge_subsidy_b = 0
  gamma_used_bev_c = 0,
  gamma_EVB_environ_c = 0,
  gamma_EVB_function_c = 0,
  gamma_EV_benefit_c = 0,
  gamma_EV_anxiety_c = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c(
  "asc_no_choice",
  "delta_a",
  "gamma_used_bev_a",
  "gamma_used_bev_b",
  "gamma_used_bev_c",
  "gamma_EVB_environ_a",
  "gamma_EVB_environ_b",
  "gamma_EVB_environ_c",
  "gamma_EVB_function_a",
  "gamma_EVB_function_b",
  "gamma_EVB_function_c",
  "gamma_EV_benefit_a",
  "gamma_EV_anxiety_a"
  # "gamma_knowledge_gas_a",
  # "gamma_knowledge_plugin_a",
  # "gamma_knowledge_ev_a",
  # "gamma_knowledge_subsidy_a"
)


#### DEFINE LATENT CLASS COMPONENTS

apollo_lcPars = function(apollo_beta, apollo_inputs) {
  lcpars = list()
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
    gamma_used_bev_a * next_veh_fuel_used_bev +
    gamma_EVB_environ_a * ATT_EVB_environment +
    gamma_EVB_function_a * ATT_EVB_function +
    gamma_EV_benefit_a * FA_EV_benefit +
    gamma_EV_anxiety_a * FA_EV_anxiety
  # gamma_knowledge_gas_a * knowledge_gas +
  # gamma_knowledge_plugin_a * knowledge_plugin +
  # gamma_knowledge_ev_a * knowledge_ev +
  # gamma_knowledge_subsidy_a * knowledge_subsidy
  V[["class_b"]] = delta_b +
    gamma_used_bev_b * next_veh_fuel_used_bev +
    gamma_EVB_environ_b * ATT_EVB_environment +
    gamma_EVB_function_b * ATT_EVB_function +
    gamma_EV_benefit_b * FA_EV_benefit +
    gamma_EV_anxiety_b * FA_EV_anxiety
  # gamma_knowledge_gas_b * knowledge_gas +
  # gamma_knowledge_plugin_b * knowledge_plugin +
  # gamma_knowledge_ev_b * knowledge_ev +
  # gamma_knowledge_subsidy_b * knowledge_subsidy

  V[["class_c"]] = delta_c +
    gamma_used_bev_c * next_veh_fuel_used_bev +
    gamma_EVB_environ_c * ATT_EVB_environment +
    gamma_EVB_function_c * ATT_EVB_function +
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
      b_mileage[[s]] *
      veh_mileage_1 +
      b_range_year0[[s]] * battery_range_year0_1 +
      b_degradation[[s]] * battery_degradation_1 +
      b_packreplace[[s]] * battery_refurbishpackreplace_1 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_1 +
      b_price[[s]] * veh_price_1

    V[["alt2"]] <-
      b_mileage[[s]] *
      veh_mileage_2 +
      b_range_year0[[s]] * battery_range_year0_2 +
      b_degradation[[s]] * battery_degradation_2 +
      b_packreplace[[s]] * battery_refurbishpackreplace_2 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_2 +
      b_price[[s]] * veh_price_2

    V[["alt3"]] <-
      b_mileage[[s]] *
      veh_mileage_3 +
      b_range_year0[[s]] * battery_range_year0_3 +
      b_degradation[[s]] * battery_degradation_3 +
      b_packreplace[[s]] * battery_refurbishpackreplace_3 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_3 +
      b_price[[s]] * veh_price_3

    V[["no_choice"]] <- asc_no_choice

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
lc_mnl_4c_indicator = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(maxIterations = 1000)
)

### Show output in screen
apollo_modelOutput(
  lc_mnl_4c_indicator,
  modelOutput_settings = list(printPVal = TRUE)
)

### Save output to file(s)
apollo_saveOutput(
  lc_mnl_4c_indicator,
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
  lc_mnl_4c_indicator,
  apollo_probabilities,
  apollo_inputs
)


# computes value of travel time (VTT) for each class (ratio of beta_tt to beta_tc).
## for each class

# List of attributes for which we want WTP
attributes <- c(
  "mileage",
  "range_year0",
  "degradation",
  "packreplace",
  "cellreplace"
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

## for each individual
mileage_unconditional = unconditionals[["pi_values"]][[1]] *
  vtt_class_a +
  unconditionals[["pi_values"]][[2]] * vtt_class_b


### Compute conditional class probability that each individual belongs to each class, given their observed choices
### Conditional means individualized using each person’s posterior
### These conditional probabilities let you assign individuals to classes or at least identify which class they’re more likely to belong to.
conditionals = apollo_conditionals(
  lc_mnl_3c_indicator,
  apollo_probabilities,
  apollo_inputs
)

summary(conditionals)
summary(as.data.frame(unconditionals[["pi_values"]]))

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
      next_veh_fuel_used_bev
    ), # replace with your variable names
    ~ mean(.x, na.rm = TRUE)
  ))

ATT_EVB_environment +
  gamma_EVB_function_c * ATT_EVB_function +
  gamma_EV_benefit_c * FA_EV_benefit +
  gamma_EV_anxiety_c * FA_EV_anxiety

vtt_conditional = conditionals[, 2] *
  vtt_class_a +
  conditionals[, 3] * vtt_class_b

summary(vtt_unconditional)
summary(vtt_conditional)

### Take first value of covariates for each person
EVB_environ_n = apollo_firstRow(database$ATT_EVB_environment, apollo_inputs)

### Compute posterior values for covariates
post_EVB_environ_n = colSums(EVB_environ_n * conditionals[, 2:3]) /
  colSums(conditionals[, 2:3])

post_commute


#---- switch off writing to file

### ----Latent class (c=4)----
# Initialize
apollo_initialise()

# Define core controls
apollo_control = list(
  modelName = "LC_DCE_4c_indicator",
  modelDescr = "LC model with 4 classes with indicator (ATT and knowledge)",
  indivID = "respID",
  nCores = 2,
  panelData = TRUE,
  outputDirectory = paste0(here(), "/code/main/model_output/apollo")
)


### Loading data from package
database = data_covariate_num %>%
  filter(
    !is.na(next_veh_fuel_used_bev) &
      !is.na(ATT_EVB_environment) &
      !is.na(ATT_EVB_function) &
      !is.na(FA_EV_benefit) &
      !is.na(FA_EV_anxiety)
    # !is.na(knowledge_gas) &
    # !is.na(knowledge_plugin) &
    # !is.na(knowledge_ev) &
    # !is.na(knowledge_subsidy)
  )


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  # lternative‐specific constants for each class
  asc_no_choice = 0,
  # attributes for each class
  b_mileage_a = 0,
  b_range_year0_a = 0,
  b_degradation_a = 0,
  b_packreplace_a = 0,
  b_cellreplace_a = 0,
  b_price_a = 0,
  b_mileage_b = 0,
  b_range_year0_b = 0,
  b_degradation_b = 0,
  b_packreplace_b = 0,
  b_cellreplace_b = 0,
  b_price_b = 0,
  b_mileage_c = 0,
  b_range_year0_c = 0,
  b_degradation_c = 0,
  b_packreplace_c = 0,
  b_cellreplace_c = 0,
  b_price_c = 0,
  b_mileage_d = 0,
  b_range_year0_d = 0,
  b_degradation_d = 0,
  b_packreplace_d = 0,
  b_cellreplace_d = 0,
  b_price_d = 0,

  # base intercepts for class allocation model
  delta_a = 0,
  delta_b = 0,
  delta_c = 0,
  delta_d = 0,
  # coefficients for covariates in class‐allocation
  gamma_used_bev_a = 0,
  gamma_EVB_environ_a = 0,
  gamma_EVB_function_a = 0,
  gamma_EV_benefit_a = 0,
  gamma_EV_anxiety_a = 0,
  # gamma_knowledge_gas_a = 0,
  # gamma_knowledge_plugin_a = 0,
  # gamma_knowledge_ev_a = 0,
  # gamma_knowledge_subsidy_a = 0,
  gamma_used_bev_b = 0,
  gamma_EVB_environ_b = 0,
  gamma_EVB_function_b = 0,
  gamma_EV_benefit_b = 0,
  gamma_EV_anxiety_b = 0,
  # gamma_knowledge_gas_b = 0,
  # gamma_knowledge_plugin_b = 0,
  # gamma_knowledge_ev_b = 0,
  # gamma_knowledge_subsidy_b = 0
  gamma_used_bev_c = 0,
  gamma_EVB_environ_c = 0,
  gamma_EVB_function_c = 0,
  gamma_EV_benefit_c = 0,
  gamma_EV_anxiety_c = 0,

  gamma_used_bev_d = 0,
  gamma_EVB_environ_d = 0,
  gamma_EVB_function_d = 0,
  gamma_EV_benefit_d = 0,
  gamma_EV_anxiety_d = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c(
  "asc_no_choice",
  "delta_a",
  "gamma_used_bev_a",
  "gamma_used_bev_b",
  "gamma_used_bev_c",
  "gamma_used_bev_d",
  "gamma_EVB_environ_a",
  "gamma_EVB_environ_b",
  "gamma_EVB_environ_c",
  "gamma_EVB_environ_d",
  "gamma_EVB_function_a",
  "gamma_EVB_function_b",
  "gamma_EVB_function_c",
  "gamma_EVB_function_d",
  "gamma_EV_benefit_a",
  "gamma_EV_benefit_b",
  "gamma_EV_benefit_c",
  "gamma_EV_benefit_d",
  "gamma_EV_anxiety_a",
  "gamma_EV_anxiety_b",
  "gamma_EV_anxiety_c",
  "gamma_EV_anxiety_d"
  # "gamma_knowledge_gas_a",
  # "gamma_knowledge_plugin_a",
  # "gamma_knowledge_ev_a",
  # "gamma_knowledge_subsidy_a"
)


#### DEFINE LATENT CLASS COMPONENTS

apollo_lcPars = function(apollo_beta, apollo_inputs) {
  lcpars = list()
  lcpars[["b_mileage"]] = list(
    b_mileage_a,
    b_mileage_b,
    b_mileage_c,
    b_mileage_d
  )
  lcpars[["b_range_year0"]] = list(
    b_range_year0_a,
    b_range_year0_b,
    b_range_year0_c,
    b_range_year0_d
  )
  lcpars[["b_degradation"]] = list(
    b_degradation_a,
    b_degradation_b,
    b_degradation_c,
    b_degradation_d
  )
  lcpars[["b_packreplace"]] = list(
    b_packreplace_a,
    b_packreplace_b,
    b_packreplace_c,
    b_packreplace_d
  )
  lcpars[["b_cellreplace"]] = list(
    b_cellreplace_a,
    b_cellreplace_b,
    b_cellreplace_c,
    b_cellreplace_d
  )
  lcpars[["b_price"]] = list(b_price_a, b_price_b, b_price_c, b_price_d)

  ### Utilities of class allocation model: ow likely a person is to belong to each class based on indicators
  V = list()
  V[["class_a"]] = delta_a +
    gamma_used_bev_a * next_veh_fuel_used_bev +
    gamma_EVB_environ_a * ATT_EVB_environment +
    gamma_EVB_function_a * ATT_EVB_function +
    gamma_EV_benefit_a * FA_EV_benefit +
    gamma_EV_anxiety_a * FA_EV_anxiety
  # gamma_knowledge_gas_a * knowledge_gas +
  # gamma_knowledge_plugin_a * knowledge_plugin +
  # gamma_knowledge_ev_a * knowledge_ev +
  # gamma_knowledge_subsidy_a * knowledge_subsidy
  V[["class_b"]] = delta_b +
    gamma_used_bev_b * next_veh_fuel_used_bev +
    gamma_EVB_environ_b * ATT_EVB_environment +
    gamma_EVB_function_b * ATT_EVB_function +
    gamma_EV_benefit_b * FA_EV_benefit +
    gamma_EV_anxiety_b * FA_EV_anxiety
  # gamma_knowledge_gas_b * knowledge_gas +
  # gamma_knowledge_plugin_b * knowledge_plugin +
  # gamma_knowledge_ev_b * knowledge_ev +
  # gamma_knowledge_subsidy_b * knowledge_subsidy

  V[["class_c"]] = delta_c +
    gamma_used_bev_c * next_veh_fuel_used_bev +
    gamma_EVB_environ_c * ATT_EVB_environment +
    gamma_EVB_function_c * ATT_EVB_function +
    gamma_EV_benefit_c * FA_EV_benefit +
    gamma_EV_anxiety_c * FA_EV_anxiety

  V[["class_d"]] = delta_d +
    gamma_used_bev_d * next_veh_fuel_used_bev +
    gamma_EVB_environ_d * ATT_EVB_environment +
    gamma_EVB_function_d * ATT_EVB_function +
    gamma_EV_benefit_d * FA_EV_benefit +
    gamma_EV_anxiety_d * FA_EV_anxiety

  ### Settings for class allocation models
  classAlloc_settings = list(
    classes = c(class_a = 1, class_b = 2, class_c = 3, class_d = 4),
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
  for (s in 1:4) {
    ### Compute class-specific utilities
    V = list()
    V[["alt1"]] <-
      b_mileage[[s]] *
      veh_mileage_1 +
      b_range_year0[[s]] * battery_range_year0_1 +
      b_degradation[[s]] * battery_degradation_1 +
      b_packreplace[[s]] * battery_refurbishpackreplace_1 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_1 +
      b_price[[s]] * veh_price_1

    V[["alt2"]] <-
      b_mileage[[s]] *
      veh_mileage_2 +
      b_range_year0[[s]] * battery_range_year0_2 +
      b_degradation[[s]] * battery_degradation_2 +
      b_packreplace[[s]] * battery_refurbishpackreplace_2 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_2 +
      b_price[[s]] * veh_price_2

    V[["alt3"]] <-
      b_mileage[[s]] *
      veh_mileage_3 +
      b_range_year0[[s]] * battery_range_year0_3 +
      b_degradation[[s]] * battery_degradation_3 +
      b_packreplace[[s]] * battery_refurbishpackreplace_3 +
      b_cellreplace[[s]] * battery_refurbishcellreplace_3 +
      b_price[[s]] * veh_price_3

    V[["no_choice"]] <- asc_no_choice

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
lc_mnl_4c_indicator = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(maxIterations = 5000)
)

### Show output in screen
apollo_modelOutput(
  lc_mnl_4c_indicator,
  modelOutput_settings = list(printPVal = TRUE)
)

### Save output to file(s)
apollo_saveOutput(
  lc_mnl_4c_indicator,
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
  lc_mnl_4c_indicator,
  apollo_probabilities,
  apollo_inputs
)


# computes value of travel time (VTT) for each class (ratio of beta_tt to beta_tc).
## for each class

# List of attributes for which we want WTP
attributes <- c(
  "mileage",
  "range_year0",
  "degradation",
  "packreplace",
  "cellreplace"
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
    wtp_attr[class] <- beta_attr / (beta_price) # WTP = beta_attr / beta_price
  }
  wtp_list[[attr]] <- wtp_attr
}

# Convert to a data frame for easy viewing
wtp_df <- as.data.frame(wtp_list)
rownames(wtp_df) <- paste0("class_", 1:n_classes)
wtp_df

## for each individual
mileage_unconditional = unconditionals[["pi_values"]][[1]] *
  vtt_class_a +
  unconditionals[["pi_values"]][[2]] * vtt_class_b


### Compute conditional class probability that each individual belongs to each class, given their observed choices
### Conditional means individualized using each person’s posterior
### These conditional probabilities let you assign individuals to classes or at least identify which class they’re more likely to belong to.
conditionals = apollo_conditionals(
  lc_mnl_4c_indicator,
  apollo_probabilities,
  apollo_inputs
)

summary(conditionals)
summary(as.data.frame(unconditionals[["pi_values"]]))

conditionals_fixed <- conditionals %>%
  mutate(
    class_allocation = case_when(
      pmax(X1, X2, X3, X4) == X1 ~ "X1",
      pmax(X1, X2, X3, X4) == X2 ~ "X2",
      pmax(X1, X2, X3, X4) == X3 ~ "X3",
      pmax(X1, X2, X3, X4) == X4 ~ "X4"
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
      next_veh_fuel_used_bev
    ), # replace with your variable names
    ~ mean(.x, na.rm = TRUE)
  ))

ATT_EVB_environment +
  gamma_EVB_function_c * ATT_EVB_function +
  gamma_EV_benefit_c * FA_EV_benefit +
  gamma_EV_anxiety_c * FA_EV_anxiety

vtt_conditional = conditionals[, 2] *
  vtt_class_a +
  conditionals[, 3] * vtt_class_b

summary(vtt_unconditional)
summary(vtt_conditional)

### Take first value of covariates for each person
EVB_environ_n = apollo_firstRow(database$ATT_EVB_environment, apollo_inputs)

### Compute posterior values for covariates
post_EVB_environ_n = colSums(EVB_environ_n * conditionals[, 2:3]) /
  colSums(conditionals[, 2:3])

post_commute


#---- switch off writing to file

## ----WTP space----

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
  wtp_mileage = 0,
  wtp_range_year0 = 0,
  wtp_degradation = 0,
  wtp_packreplace = 0,
  wtp_cellreplace = 0,
  asc_no_choice = 0,
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

  V[["no_choice"]] <- b_price * asc_no_choice

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
