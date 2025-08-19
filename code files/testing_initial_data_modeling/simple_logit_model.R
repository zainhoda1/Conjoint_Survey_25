# Estimate multinomial logit (MNL) models

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(janitor)
library(here)
options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:

data <- read_csv(here("code files", "modeling",  "choice_data.csv"))
head(data)

# Estimate MNL model

# First create some dummy coded variables for categorical variables
data <- dummy_cols(data, c('powertrain'))

# Clean up names of created variables
data <- clean_names(data)

# Estimate the model
model <- logitr(
  data    = data,
  outcome = "choice",
  obsID   = "obs_id",
  pars = c(
    "price",
    "powertrain_battery_electric",
    "powertrain_gas_hybrid", "powertrain_plug_in_hybrid")
)


# View summary of results
summary(model)

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values
