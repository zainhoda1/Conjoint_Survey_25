source(here::here('code', 'setup.R'))

### ----Latent class (c=2)----
# Create an empty list to store the results of each model run
model_results <- list()

for (i in c(1:5)) {
  # Initialize
  apollo_initialise()

  # Define core controls
  apollo_control = list(
    modelName = paste0("LC_2c_indicator_", i),
    modelDescr = "LC model with 2 classes with indicator",
    indivID = "respID",
    nCores = 2,
    panelData = TRUE,
    outputDirectory = paste0(here(), "/code/output/model_output/apollo/vehicle")
  )

  ## ----Load dataset----

  database <- read_parquet(here(
    "data",
    "dynata_prolific_joint",
    "data_apollo_vehicle.parquet"
  )) %>%
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

    # base intercepts for class allocation model
    delta_a = 0,
    gamma_EV_benefit_a = 0,
    gamma_EV_anxiety_a = 0,
    gamma_hhincome_a = 0,
    gamma_veh_budget_a = 0,
    delta_b = 0,
    gamma_EV_benefit_b = 0,
    gamma_EV_anxiety_b = 0,
    gamma_hhincome_b = 0,
    gamma_veh_budget_b = 0
  )

  ### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
  apollo_fixed = c(
    "delta_a",
    "gamma_EV_benefit_a",
    "gamma_EV_anxiety_a",
    "gamma_hhincome_a",
    "gamma_veh_budget_a",
    "gamma_veh_budget_b"
    # "gamma_EV_benefit_b",
    # "gamma_EV_anxiety_b"
  )

  # apollo_beta = apollo_readBeta(
  #   apollo_beta,
  #   apollo_fixed,
  #   "lc_2c_indicator_1",
  #   overwriteFixed = FALSE
  # )

  #### DEFINE LATENT CLASS COMPONENTS
  apollo_lcPars = function(apollo_beta, apollo_inputs) {
    lcpars = list()
    lcpars[["b_no_choice"]] = list(b_no_choice_a, b_no_choice_b)
    lcpars[["b_powertrainbev"]] = list(b_powertrainbev_a, b_powertrainbev_b)
    lcpars[["b_powertrainhev"]] = list(b_powertrainhev_a, b_powertrainhev_b)
    lcpars[["b_range_bev"]] = list(b_range_bev_a, b_range_bev_b)
    lcpars[["b_mileage"]] = list(b_mileage_a, b_mileage_b)
    lcpars[["b_age"]] = list(b_age_a, b_age_b)
    lcpars[["b_operating_cost"]] = list(b_operating_cost_a, b_operating_cost_b)
    lcpars[["b_price"]] = list(b_price_a, b_price_b)

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
  model_results[[paste0("LC_2c_indicator_", i)]] = apollo_estimate(
    apollo_beta,
    apollo_fixed,
    apollo_probabilities,
    apollo_inputs
  )
}

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
  arrange(desc(BIC))

# Identify best-fitting model
best_model_name <- model_summary$model_name[1]
lc_2c_indicator <- model_results[[best_model_name]]

### Show output in screen
apollo_modelOutput(
  lc_2c_indicator,
  modelOutput_settings = list(printPVal = TRUE)
)

### Save output to file(s)
apollo_saveOutput(
  lc_2c_indicator,
  saveOutput_settings = list(printPVal = 2)
)


##### POST-PROCESSING

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()

#---- OUT OF SAMPLE TESTING

# apollo_outOfSample(
#   apollo_beta,
#   apollo_fixed,
#   apollo_probabilities,
#   apollo_inputs
# )

#---- BOOTSTRAP ESTIMATION

# apollo_bootstrap(
#   apollo_beta,
#   apollo_fixed,
#   apollo_probabilities,
#   apollo_inputs,
#   bootstrap_settings = list(nRep = 3)
# )

####### POSTERIOR ANALYSIS

### Compute unconditional estimates (averaged over classes) of parameters.
### Unconditional means averaged across classes using the population-level class probabilities
unconditionals = apollo_unconditionals(
  lc_2c_indicator,
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
  lc_2c_indicator,
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
      # Active indicators
      FA_EV_benefit,
      FA_EV_anxiety,
      next_veh_fuel_used_bev,
      hhincome_num,
      next_veh_budget
    ), # replace with your variable names
    ~ mean(.x, na.rm = TRUE)
  ))
class_character
