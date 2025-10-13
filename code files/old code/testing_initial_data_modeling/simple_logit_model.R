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

data <- read_csv(here("code files", "old code", "testing_initial_data_modeling",  "vehicle_choice_data.csv"))
head(data)

glimpse(data)

data <- data %>%
  #select(price, mileage, age, operating_cost, )
  mutate(price = price/10000,  # 0.5-6
         range_bev = range_bev/100,  # 0.5 - 2.5
         range_phev = range_phev /10,  # 1 - 4 
         mileage = mileage * 10,  # 2 - 6
         age = age * 10, # 2 - 8
        operating_cost = operating_cost/10 # 0.3 - 2.5
         )


# Estimate MNL model

# First create some dummy coded variables for categorical variables
#data <- dummy_cols(data, c('powertrain'))

# Clean up names of created variables
data <- clean_names(data)

# Estimate the model
model <- logitr(
  data    = data,
  outcome = "choice",
  obsID   = "obs_id",
  pars = c(
    "price",
    "mileage",
    "age",
    "operating_cost",
    "range_bev",
    "range_phev",
    "powertrainbev",
    "powertrainhev",
    "powertrainphev"
    )
)




# View summary of results
summary(model)

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values
