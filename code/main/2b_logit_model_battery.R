source(here::here('code', 'setup.R'))

# ----Load the data set----

data <- read_csv(here(
  "data",
  "main",
  "battery_choice_data.csv"
))
#head(data)

#n_distinct(data$resp_id)  #373

# glimpse(data)

# ----Data Processing----
data <- data %>%
  mutate(
    veh_mileage = veh_mileage / 10000, #3 - 6
    veh_price = veh_price / 10000, # 0.5 - 6
    battery_range_year0 = battery_range_year0 / 100, # 1-3
    battery_range_year3 = battery_range_year3 / 100, # 1-3
    battery_range_year8 = battery_range_year8 / 100, # 0.5 - 2
    battery_degradation = (battery_degradation * 100) # percentage
  ) %>%
  select(
    -starts_with("battery_health"),
    -starts_with("time"),
    -vehicle_type,
    -battery_condition
  )

# Dummy encode
data <- cbc_encode(
  data,
  coding = 'dummy',
  ref_levels = list(battery_refurbish = 'original')
) %>%
  as.data.frame() %>%
  clean_names()

# ---- Estimate MNL model (range:yr0 + degradation) ----
## --- Preference Space ----
mnl_pref <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obs_id",
  pars = c(
    "veh_mileage",
    "veh_price",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice"
  ),
  numMultiStarts = 10,
  numCores = 1
)

# View summary of results
summary(mnl_pref)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_pref$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_pref$hessian)$values

### ---- WTP calculation ----
wtp(mnl_pref, scalePar = "veh_price")

## --- WTP Space ----
mnl_wtp <- logitr(
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

## ---- WTP comparison ----
wtpCompare(mnl_pref, mnl_wtp, scalePar = "veh_price")

# Based on WTP space,

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

# # ---- Archive code ----
# ## ---- Model (range:yr8) ----
# # First create some dummy coded variables for categorical variables
# #data <- dummy_cols(data, c('battery_refurbish', 'degradation_high'))
#
# # Clean up names of created variables
#
# # glimpse(data)
#
#
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
