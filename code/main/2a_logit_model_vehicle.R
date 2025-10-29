source(here::here('code', 'setup.R'))

# --------------------------------------------------------------------------
# Load the data set:

data <- read_csv(here(
  "data",
  "main",
  "vehicle_choice_data.csv"
))
head(data)

#n_distinct(data$session_id)  #334

glimpse(data)

data <- data %>%
  mutate(
    price = price / 10000, # 0.5-6
    range_bev = range_bev / 100, # 0.5 - 2.5
    range_phev = range_phev / 10, # 1 - 4
    mileage = mileage * 10, # 2 - 6
    age = age * 10, # 2 - 8
    operating_cost = operating_cost / 10 # 0.3 - 2.5,
  ) %>%
  select(-range, -operating_cost_text, -session_id, -vehicle_type)

# Dummy encode
data <- cbc_encode(data, coding = 'dummy', ref_levels = list(powertrain='gas')) %>%
  as.data.frame()

glimpse(data)


# Estimate MNL model

# First create some dummy coded variables for categorical variables
#data <- dummy_cols(data, c('powertrain'))

data %>%
  count(choice, qID)

data %>%
  count(no_choice, choice)

# Estimate the model
model <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "price",
    "mileage",
    "age",
    "operating_cost",
    "range_bev",
    "range_phev",
    "powertrainbev",
    "powertrainphev",
    "powertrainhev",
    "no_choice"
  )
)


# View summary of results
summary(model)

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values

