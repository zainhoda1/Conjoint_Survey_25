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

data <- read_csv(here("code files", "testing_initial_data_modeling",  "battery_choice_data.csv"))
head(data)

patterns <- c("_5.png", "_6.png", "_7.png", "_8.png" )

data <- data %>% 
  mutate(degradation_high = 
           ifelse(str_detect(image_degradation, paste(patterns, collapse = "|")), 1, 0)
  )


data <- data %>%
  mutate(
    veh_mileage = veh_mileage/1000,
    veh_price = veh_price/1000,
    battery_degradation = battery_degradation * 100,
    battery_range_year0 = battery_range_year0 /10
         ) %>% 
  select(-image_degradation, -image_refurbishment)

# Estimate MNL model

# First create some dummy coded variables for categorical variables
data <- dummy_cols(data, c('battery_refurbish', 'degradation_high'))

# Clean up names of created variables
data <- clean_names(data)

# Estimate the model
model1 <- logitr(
  data    = data 
  #%>% 
   # filter(q_id != 4) %>%
  #  filter(q_id != 6)
  ,
  outcome = "choice",
  obsID   = "obs_id",
  pars = c(
    "veh_price",
    "veh_mileage", 
    "battery_degradation", 
    "battery_range_year3", 
    "battery_refurbish_cellreplace",
    "battery_refurbish_packreplace"
  )
)


# View summary of results
summary(model1)


# Estimate the model
model2 <- logitr(
  data    = data 
  #%>% 
  # filter(q_id != 4) %>%
  #  filter(q_id != 6)
  ,
  outcome = "choice",
  obsID   = "obs_id",
  pars = c(
    "veh_price",
    "veh_mileage", 
    "battery_range_year0", 
    "battery_range_year3", 
    "battery_range_year8", 
    "battery_refurbish_cellreplace",
    "battery_refurbish_packreplace"
  )
)

# View summary of results
summary(model2)

# Estimate the model
model3 <- logitr(
  data    = data 
  #%>% 
  # filter(q_id != 4) %>%
  #  filter(q_id != 6)
  ,
  outcome = "choice",
  obsID   = "obs_id",
  pars = c(
    "degradation_high_1",
    "veh_price",
    "veh_mileage", 
    "battery_range_year0", 
    "battery_refurbish_cellreplace",
    "battery_refurbish_packreplace"
  )
)

# View summary of results
summary(model3)


# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values
