# Estimate multinomial logit (MNL) models

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(cbcTools)
library(janitor)
library(here)
options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:

data <- read_csv(here(
  "code files",
  "pilot",
  "data",
  "battery_choice_data.csv"
))
#head(data)

data_covariates <- read_csv(here(
  "code files",
  "pilot",
  "Battery_analysis",
  "data_covariates.csv"
))


glimpse(data)


data <- data %>%
  mutate(
    veh_mileage = veh_mileage / 10000, #3 - 6
    veh_price = veh_price / 10000, # 0.5 - 6
    battery_range_year0 = battery_range_year0 / 100, # 1-3
    battery_range_year3 = battery_range_year3 / 100, # 1-3
    battery_range_year8 = battery_range_year8 / 100, # 0.5 - 2
    battery_degradation = (battery_degradation * 10)
  ) %>%
  select(
    -starts_with("battery_health"),
    -starts_with("time"),
    -session_id,
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


data<-data %>%
  mutate(battery_respID=battery_resp_id) %>% 
  left_join(data_covariates %>% 
              select(battery_respID, ends_with("_num"), ends_with("cate")),
            by="battery_respID")


# Estimate MNL model

# First create some dummy coded variables for categorical variables
#data <- dummy_cols(data, c('battery_refurbish', 'degradation_high'))

# Clean up names of created variables

glimpse(data)

# Estimate the model
model1 <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obs_id",
  pars = c(
    "veh_mileage",
    "veh_price",
    #### ranges are highly correlated, so only include one year.
    #### Keeping year8 generates higher R2
    # "battery_range_year3",
    "battery_range_year8", 
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice"
  )
)


# View summary of results
summary(model1)

# Check the 1st order condition: Is the gradient at the solution zero?
model1$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values

# Estimate the model
model2 <- logitr(
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
  )
)

# View summary of results
summary(model2)


# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values




# # Estimate the model
# model3 <- logitr(
#   data = data,
#   outcome = "choice",
#   obsID = "obs_id",
#   pars = c(
#     "veh_mileage",
#     "veh_price",
#     "battery_range_year0",
#     "battery_degradation",
#     "battery_refurbishpackreplace",
#     "battery_refurbishcellreplace",
#     "no_choice",
#     "age_num",
#     "hhincome_num",
#     "hhsize_num"
#   )
# )
# 
# # View summary of results
# summary(model3)
# 
# 
# # Check the 1st order condition: Is the gradient at the solution zero?
# model$gradient
# 
# # 2nd order condition: Is the hessian negative definite?
# # (If all the eigenvalues are negative, the hessian is negative definite)
# eigen(model$hessian)$values
