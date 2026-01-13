source(here::here('code', 'setup.R'))

# ----Data----
## ----Load dataset----
data_dce <- read_csv(here(
  "data",
  "main",
  "vehicle_choice_data.csv"
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

# head(data)

# glimpse(data)
## ----Processing----
data_dce <- data_dce %>%
  mutate(
    price = price / 10000, # 0.4-6
    range_bev = range_bev / 100, # 0.5 - 2.5
    range_phev = range_phev / 10, # 1 - 4
    mileage = mileage * 10, # 2 - 6
    age = age * 10, # 2 - 8
    operating_cost = operating_cost # 3 - 18,
  ) %>%
  select(-range, -operating_cost_text, -session_id, -vehicle_type)


# ----Apollo----
## ----Simple MNL----
### ----DCE only----

# Initialize
apollo_initialise()

# Define core controls
apollo_control = list(
  modelName = "MNL_DCE",
  modelDescr = "MNL_DCE",
  indivID = "respID",
  mixing = FALSE
)

database <- data_DCE
# example: get vector of distinct alternative names present in data
alts <- sort(unique(database$profileID)) # e.g., c("A","B","C","D")

# if your df_long already has a column 'avail' (0/1) per row,
# and each row corresponds to one alt, we can create avail list from that:
avail_list <- lapply(alts, function(a) {
  as.numeric(database$profileID == a)
})
names(avail_list) <- alts

apollo_beta <- c(
  b_mileage = 0,
  b_price = 0,
  b_range_y0 = 0,
  b_degradation = 0,
  b_refurbishpack = 0,
  b_refurbishcell = 0,
  b_nochoice = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


#### GROUP AND VALIDATE INPUTS

apollo_inputs = apollo_validateInputs()


#### DEFINE MODEL AND LIKELIHOOD FUNCTION

apollo_probabilities <- function(
  apollo_beta,
  apollo_inputs,
  functionality = "estimate"
) {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()

  ### List of utilities
  V <- list()

  # V needs an entry per alt name; use the long-data attributes

  # each row of df_long will have price/range/charge_time for that alt
  V[["A"]] <- b_mileage *
    veh_mileage +
    b_price * veh_price +
    b_range_y0 * battery_range_year0 +
    b_degradation * battery_degradation +
    b_refurbishpack * (battery_refurbish == "packreplace") +
    b_refurbishcell * (battery_refurbish == "cellreplace") +
    b_nochoice * (choice_4 == 1)

  V[["B"]] <- b_mileage *
    veh_mileage +
    b_price * veh_price +
    b_range_y0 * battery_range_year0 +
    b_degradation * battery_degradation +
    b_refurbishpack * (battery_refurbish == "packreplace") +
    b_refurbishcell * (battery_refurbish == "cellreplace") +
    b_nochoice * (choice_4 == 1)

  V[["C"]] <- b_mileage *
    veh_mileage +
    b_price * veh_price +
    b_range_y0 * battery_range_year0 +
    b_degradation * battery_degradation +
    b_refurbishpack * (battery_refurbish == "packreplace") +
    b_refurbishcell * (battery_refurbish == "cellreplace") +
    b_nochoice * (choice_4 == 1)

  # MNL model
  P <- apollo_mnl(
    V = V,
    choiceVar = choice,
    avail = avail_list
  )

  P <- apollo_panelProd(P, apollo_inputs)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


#### MODEL ESTIMATION

model_MNL_DCE = apollo_estimate(apollo_beta, apollo_probabilities)
