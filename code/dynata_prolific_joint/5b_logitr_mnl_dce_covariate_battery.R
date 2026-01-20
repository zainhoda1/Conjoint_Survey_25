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
  as.data.frame() %>%
  clean_names()

data_dce_dummy <- cbind(data_dce_dummy, data_dce %>% select(psid))

data_covariate <- data_dce_dummy %>%
  left_join(data_variable, by = "psid")
# n_distinct(data_covariate$psid) #373

# --- Only DCE ----
mnl_wtp <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obs_id",
  pars = c(
    "veh_mileage",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice"
  ),
  scalePar = "veh_price",
  numMultiStarts = 10,
  numCores = 1
)

# View summary of results
summary(mnl_wtp)
# Check the 1st order condition: Is the gradient at the solution zero?
mnl_wtp$gradient
# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_wtp$hessian)$values
#coefficient
mnl_wtp_coef <- mnl_wtp$coefficients


# ---- Estimate MXL model ----
## --- WTP Space ----
### --- full ----
mxl_wtp_full <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obs_id",
  pars = c(
    "veh_mileage",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice"
  ),
  scalePar = "veh_price",
  randPars = c(
    veh_mileage = "n",
    battery_range_year0 = "n",
    battery_degradation = "n",
    battery_refurbishpackreplace = "n",
    battery_refurbishcellreplace = "n"
  ),
  numMultiStarts = 10,
  numCores = 1
)

summary(mxl_wtp_full)

### --- reduced ----
mxl_wtp_reduced <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obs_id",
  pars = c(
    "veh_mileage",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice"
  ),
  scalePar = "veh_price",
  randPars = c(
    # veh_mileage = "n",
    battery_range_year0 = "n",
    battery_degradation = "n"
    # battery_refurbishpackreplace = "n",
    # battery_refurbishcellreplace = "n"
  ),
  numMultiStarts = 10,
  numCores = 1
)

summary(mxl_wtp_reduced)

# ---- ****DCE ONLY + Covariates**** ----
### --- DCE + knowledge ----
mnl_wtp_FA <- logitr(
  data = data_covariate,
  outcome = "choice",
  obsID = "obs_id",
  pars = c(
    "veh_mileage",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice",
    "FA_EV_benefit"
  ),
  scalePar = "veh_price",
  # startVals = mnl_wtp_coef,
  numMultiStarts = 100,
  numCores = 1,
  options = list(
    numDerivs = TRUE, # compute numerical derivatives for SEs
    robust = TRUE, # optional: compute robust SEs (recommended)
    startVals = "mnl_wtp" # start from reasonable WTP values
  ),
)

# View summary of results
summary(mnl_wtp_FA)
# Check the 1st order condition: Is the gradient at the solution zero?
mnl_wtp$gradient
# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_wtp$hessian)$values
# ---- Archive code ----
## ---- Model (range:yr8) ----
# First create some dummy coded variables for categorical variables
#data <- dummy_cols(data, c('battery_refurbish', 'degradation_high'))

# Clean up names of created variables

# glimpse(data)

# model1 <- logitr(
#   data = data,
#   outcome = "choice",
#   obsID = "obs_id",
#   pars = c(
#     "veh_mileage",
#     "veh_price",
#     #### ranges are highly correlated, so only include one year.
#     #### Keeping year8 generates higher R2
#     # "battery_range_year3",
#     "battery_range_year8",
#     "battery_refurbishpackreplace",
#     "battery_refurbishcellreplace",
#     "no_choice"
#   )
# )
#
#
# # View summary of results
# summary(model1)
#
# # Check the 1st order condition: Is the gradient at the solution zero?
# model1$gradient
#
# # 2nd order condition: Is the hessian negative definite?
# # (If all the eigenvalues are negative, the hessian is negative definite)
# eigen(model1$hessian)$values
