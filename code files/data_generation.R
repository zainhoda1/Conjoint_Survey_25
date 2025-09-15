# Make conjoint surveys and simulate choice data using {cbcTools}

# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)
library(logitr)

options(dplyr.width = Inf)

# First, define the attributes and levels:

profiles_used <- cbc_profiles(
  powertrain = c('Gasoline', 'Electric', 'Plug-in Hybrid', 'Hybrid'),
  price = seq(0.8, 1.1, 0.1),
  range_bev = c(0, seq(50, 250, 25)),
  range_phev = c(0, seq(10, 40, 10)),
  mileage = seq(20000, 60000, 5000),
  my = seq(2015, 2023),
  operating_cost = seq(3, 21, 3)
)

# Resrictions:

profiles_used_restricted <- profiles_used
profiles_used_restricted <- cbc_restrict(
  profiles_used,
  # BEV range restrictions
  (powertrain == "Gasoline") & (range_bev != 0),
  (powertrain == "Hybrid") & (range_bev != 0),
  (powertrain == "Plug-in Hybrid") & (range_bev != 0),
  (powertrain == "Electric") & (range_bev < 50),
  # PHEV range restrictions
  (powertrain == "Gasoline") & (range_phev != 0),
  (powertrain == "Hybrid") & (range_phev != 0),
  (powertrain == "Electric") & (range_phev != 0),
  (powertrain == "Plug-in Hybrid") & (range_phev > 40),
  (powertrain == "Plug-in Hybrid") & (range_phev == 0),
  # Gas efficiency restrictions
  (powertrain == "Gasoline") & (operating_cost < 9),
  (powertrain != "Gasoline") & (operating_cost >= 18)
)

# Make a randomized full-factorial design:

design_rand <- cbc_design(
  profiles = profiles_used_restricted,
  n_resp = 4000, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE
)

head(design_rand)

design_rand$range_bev[design_rand$powertrainElectric != 1] <- 0
design_rand$range_phev[design_rand$`powertrainPlug-in Hybrid` != 1] <- 0

# Check that counts by powertrain are even
design_rand %>%
  tibble() %>%
  select(starts_with('power'), no_choice) %>%
  janitor::clean_names() %>%
  count(
    powertrain_electric,
    powertrain_plug_in_hybrid,
    powertrain_hybrid,
    no_choice
  )


#Checking counts:
#cbc_balance(design_rand)
#cbc_overlap(design_rand)

#Simulate random choices

data_rand <- cbc_choices(
  design = design_rand,
  obsID = "obsID"
)

# Adding price values (based on options shown to user)
my_vec <- c()
user_price_options <- seq(5000, 32500, 2500)
respID <- unique(data_rand$respID)
sampled_prices <- data.frame(
  respID = respID,
  user_price = sample(user_price_options, size = length(respID), replace = TRUE)
)

data_rand = left_join(data_rand, sampled_prices)

# for (i in seq(1,nrow(data_rand)/18,1))
# {
#   val <- sample(user_price_options,1)
#   my_vec <- c(my_vec , rep(val, times = 18))
# }

data_rand$price = data_rand$user_price * data_rand$price


# Save data
write_csv(data_rand, here('data', 'generated_data.csv'))
