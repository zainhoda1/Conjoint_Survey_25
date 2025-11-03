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

## ----Dummy encode----

data_dce_dummy <- cbc_encode(
  data_dce %>%
    select(!psid),
  coding = 'dummy',
  ref_levels = list(powertrain = 'gas')
) %>%
  as.data.frame()

data_dce_dummy <- cbind(data_dce_dummy, data_dce %>% select(psid))

data_covariate <- data_dce_dummy %>%
  left_join(data_variable, by = "psid")
n_distinct(data_covariate$psid) #334

# table variables
# data_covariate %>%
#   select(where(is.character)) %>% # select all character variables
#   map(~ table(.))

data_covariate <- cbc_encode(
  data_covariate %>%
    select(!psid) %>%
    mutate(
      knowledge_gas = as.character(knowledge_gas),
      knowledge_plugin = as.character(knowledge_plugin),
      knowledge_ev = as.character(knowledge_ev),
      knowledge_subsidy = as.character(knowledge_subsidy)
    ),
  coding = 'dummy',
  # categorical_attrs =
  #   c(
  #   "Veh_hh_fuel",
  #   "gender_cate",
  #   "ethnicity_cate"
  #   # names(select(
  #   #   data_covariate,
  #   #   ends_with("_cate"),
  #   #   starts_with("ATT_"),
  #   #   starts_with("knowledge_"),
  #   #   starts_with("EV_")
  #   # ))
  # )),
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

# glimpse(data)

# Estimate MNL model

# ---- ****DCE ONLY**** ----
## --- WTP Space ----

mnl_wtp <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "mileage",
    "age",
    "operating_cost",
    "range_bev",
    "range_phev",
    "powertrainbev",
    "powertrainphev",
    "powertrainhev",
    "no_choice"
  ),
  scalePar = "price",
  numMultiStarts = 50,
  numCores = 1
)

# View summary of results
summary(mnl_wtp)
# Check the 1st order condition: Is the gradient at the solution zero?
mnl_wtp$gradient
# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_wtp$hessian)$values

# ---- ****DCE ONLY + Covariates**** ----
## --- WTP Space ----
### --- FA_EV ----
mnl_wtp_FA_EV <- logitr(
  data = data_covariate,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    # attributes
    "mileage",
    "age",
    "operating_cost",
    "range_bev",
    "range_phev",
    "powertrainbev",
    "powertrainphev",
    "powertrainhev",
    "no_choice",
    # EV attitudes
    "FA_EV_benefit",
    "FA_EV_anxiety",

    # "knowledge_gas1",
    # "knowledge_gas2"
    # "knowledge_plugin1",
    # "knowledge_plugin2",
    # "knowledge_ev1",
    "knowledge_subsidy1"
  ),
  scalePar = "price",
  numMultiStarts = 50,
  numCores = 1
)

# View summary of results
summary(mnl_wtp_FA_EV)
mnl_wtp$gradient
eigen(mnl_wtp$hessian)$values

### --- FA_EV + ATT ----
mnl_wtp_ATT <- logitr(
  data = data_covariate,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    # attributes
    "mileage",
    "age",
    "operating_cost",
    "range_bev",
    "range_phev",
    "powertrainbev",
    "powertrainphev",
    "powertrainhev",
    "no_choice",
    # attitudes
    "FA_EV_benefit",
    "FA_EV_anxiety",
    "age_num"

    # "gender_catemale"

    # "ATT_climatea_little",
    # "ATT_climatemoderate",
    # "ATT_climatea_lot",
    # "ATT_climategreat_deal"

    # "knowledge_gas1",
    # "knowledge_gas2"
    # "knowledge_plugin1",
    # "knowledge_plugin2",
    # "knowledge_ev1",
    # "knowledge_subsidy1"
  ),
  scalePar = "price",
  options = list(
    numDerivs = TRUE, # compute numerical derivatives for SEs
    robust = TRUE, # optional: compute robust SEs (recommended)
    startVals = "wtp" # start from reasonable WTP values
  ),
  numMultiStarts = 50,
  numCores = 1
)
summary(mnl_wtp_ATT)


set.seed(5678)
mnl_wtp_unweighted <- logitr(
  data = cars_us,
  outcome = "choice",
  obsID = "obsnum",
  pars = c(
    "hev",
    "phev10",
    "phev20",
    "phev40",
    "bev75",
    "bev100",
    "bev150",
    "american",
    "japanese",
    "chinese",
    "skorean",
    "phevFastcharge",
    "bevFastcharge",
    "opCost",
    "accelTime"
  ),
  scalePar = "price",
  robust = TRUE,
  numMultiStarts = 10,
  numCores = 1
)


summary(mnl_wtp_unweighted)
view(cars_us)
