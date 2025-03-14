# Estimate mixed logit (MXL) models

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(fastDummies)
library(janitor)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
pathToData <- here('data', "generated_data.csv")
data <- read_csv(pathToData)
head(data)


# Variables:
# "respID"      = Identifies each survey respondent
# "qID"         = Identifies each question for each survey respondent
# "altID"       = Identifies the alternative in each unique choice observation
# "obsID"       = Identifies each unique choice observation
# "choice"      = 1 if the alternative is chosen, 0 otherwise
# "price"       = Purchase price in thousands of dollars (15, 20, 25)
# "fuelEconomy" = Fuel economy in miles per gallon of gasoline (20, 25, 30)
# "accelTime"   = 0 to 60 mph acceleration time in seconds (6, 7, 8)
# "powertrain"  = Indicates if the car is electric or gas

# -----------------------------------------------------------------------------
# Estimate preference space MXL model with linear price, fuelEconomy, and accelTime



# Create dummy coded variables

data_dummy <- dummy_cols(
  data, c('powertrain'))
head(data_dummy)

# Clean up names of created variables
data_dummy1 <- clean_names(data_dummy)


# Estimate the model
mxl_pref <- logitr(
  data    = data_dummy1,
  outcome = "choice",
  obsID   = "obs_id",
  pars    = c("price","range", "mileage", "my", "operating_cost",
    "powertrain_electric","powertrain_hybrid" ,"powertrain_plug_in_hybrid"),
  randPars = c(range = 'n', mileage = 'n', my = 'n',
               operating_cost = 'n', powertrain_electric = 'n',
               powertrain_hybrid = 'n',powertrain_plug_in_hybrid = 'n'),
  drawType = "sobol",
  numDraws = 200
)

# View summary of results
summary(mxl_pref)

# Check the 1st order condition: Is the gradient at the solution zero?
mxl_pref$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mxl_pref$hessian)$values

# -----------------------------------------------------------------------------
# Estimate WTP space MXL model with linear price, fuelEconomy, and accelTime

# Estimate the model
mxl_wtp <- logitr(
  data    = data_dummy1,
  outcome = "choice",
  obsID   = "obs_id",
  pars    = c("range", "mileage", "my", "operating_cost",
              "powertrain_electric","powertrain_hybrid" ,"powertrain_plug_in_hybrid"),
  scalePar = 'price',
  randPars = c(range = 'n', mileage = 'n', my = 'n',
               operating_cost = 'n', powertrain_electric = 'n',
               powertrain_hybrid = 'n',powertrain_plug_in_hybrid = 'n'),
  drawType = "sobol",
  numDraws = 200
)

# View summary of results
summary(mxl_wtp)

# Check the 1st order condition: Is the gradient at the solution zero?
mxl_wtp$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mxl_wtp$hessian)$values

# -----------------------------------------------------------------------------
# Save model objects

save(
  mxl_pref,
  mxl_wtp,
  file = here("models", "mxl.RData")
)
