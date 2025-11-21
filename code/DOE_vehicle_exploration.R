# --- Car Survey ----

# Make conjoint surveys using the cbcTools package

# Install packages
# install.packages("remotes")
# install.packages("tidyverse")
# remotes::install_github("jhelvy/cbcTools")

# Load libraries

library(tidyverse)
library(cbcTools)
library(logitr)
library(here)
library(data.table)
# library(readxl)

## --- Quantiles of MPG  ----
#for the 2024 model year vehicles by for different powertrains and vehicle types. Data source: EPA or fuel_economic_gov

dt_mpg <- read.csv(here('data', 'mpg_by_segment_fuel.csv')) %>%
  # kwh per 100 miles --> kwh per mile
  mutate(
    kwh_q10 = kwh_q10 / 100,
    kwh_q25 = kwh_q25 / 100,
    kwh_q50 = kwh_q50 / 100,
    kwh_q75 = kwh_q75 / 100,
    kwh_q90 = kwh_q90 / 100
  )


# electricity rate per kwh https://www.chooseenergy.com/electricity-rates-by-state/ March 2025
electricity_rate_low <- 0.11
electricity_rate_avg <- 0.17
electricity_rate_high <- 0.33

# gas rate per gallon https://www.chooseenergy.com/data-center/cost-of-driving-by-state May 2025
gasoline_rate_low <- 2.66
gasoline_rate_avg <- 3.18
gasoline_rate_high <- 4.77

# PHEV utility factors https://docs.nrel.gov/docs/gen/fy07/41341.pdf
phev_uf_car <- 0.668
phev_uf_suv <- 0.487

# MPGe- One gallon of gasoline is equivalent to 33.7 kilowatt-hours (kWh) of electricity https://www.bluegrassauto.com/hybrid-and-electric-vehicle-comparisons/
gas_electricity <- 33.7

# # PHEV: Assume total range = 300 miles, electric range = 40 miles,
# phev_total_range<- 300
# phev_e_range<- 40

dt_mpg <- dt_mpg %>%
  mutate(
    cents_mile_min = case_when(
      powertrain == "bev" ~ electricity_rate_low * (kwh_q10) * 100,
      powertrain %in% c("cv", "hev") ~ gasoline_rate_low / mpg_q90 * 100,
      
      powertrain == "phev" & vehicle_type == "car" ~
        (phev_uf_car *
           (electricity_rate_low * (kwh_q10)) +
           (1 - phev_uf_car) * (gasoline_rate_low / mpg_q90)) *
        100,
      powertrain == "phev" & vehicle_type == "suv" ~
        (phev_uf_suv *
           (electricity_rate_low * (kwh_q10)) +
           (1 - phev_uf_suv) * (gasoline_rate_low / mpg_q90)) *
        100
    ),
    cents_mile_max = case_when(
      powertrain == "bev" ~ electricity_rate_high * (kwh_q90) * 100,
      powertrain %in% c("cv", "hev") ~ gasoline_rate_high / mpg_q10 * 100,
      
      powertrain == "phev" & vehicle_type == "car" ~
        (phev_uf_car *
           (electricity_rate_high * (kwh_q90)) +
           (1 - phev_uf_car) * (gasoline_rate_high / mpg_q10)) *
        100,
      powertrain == "phev" & vehicle_type == "suv" ~
        (phev_uf_suv *
           (electricity_rate_high * (kwh_q90)) +
           (1 - phev_uf_suv) * (gasoline_rate_high / mpg_q10)) *
        100
    )
  ) %>%
  mutate(
    MPGe_min = case_when(
      powertrain %in% c("bev", "cv", "hev") ~ mpg_q10,
      
      powertrain == "phev" & vehicle_type == "car" ~
        (1 /
           (phev_uf_car *
              (kwh_q10 / gas_electricity) +
              (1 - phev_uf_car) * (1 / mpg_q90))),
      powertrain == "phev" & vehicle_type == "suv" ~
        (1 /
           (phev_uf_suv *
              (kwh_q10 / gas_electricity) +
              (1 - phev_uf_suv) * (1 / mpg_q90)))
    ),
    
    MPGe_max = case_when(
      powertrain %in% c("bev", "cv", "hev") ~ mpg_q90,
      
      powertrain == "phev" & vehicle_type == "car" ~
        (1 /
           (phev_uf_car *
              (kwh_q90 / gas_electricity) +
              (1 - phev_uf_car) * (1 / mpg_q10))),
      powertrain == "phev" & vehicle_type == "suv" ~
        (1 /
           (phev_uf_suv *
              (kwh_q90 / gas_electricity) +
              (1 - phev_uf_suv) * (1 / mpg_q10)))
    )
  )


dt_mpg <- dt_mpg %>%
  rowwise() %>%
  mutate(
    quintiles = list(seq(
      from = cents_mile_min,
      to = cents_mile_max,
      length.out = 20
    ))
  ) %>%
  unnest_wider(quintiles, names_sep = "_") %>%
  rename_with(~ paste0("cents_mile_value_", 1:20), starts_with("quintiles"))

dt_mpg <- dt_mpg %>%
  rowwise() %>%
  mutate(
    quintiles = list(seq(from = MPGe_max, to = MPGe_min, length.out = 20))
  ) %>%
  unnest_wider(quintiles, names_sep = "_") %>%
  rename_with(~ paste0("MPGe_value_", 1:20), starts_with("quintiles"))
dt_mpg <- dt_mpg %>%
  mutate(across(starts_with("cents_mile_value_"), ~ round(.x, 0)))


# write.csv(dt_mpg, here("survey","data","mpg_by_segment_fuel_cost_final.csv"),row.names = F)

dt_mpg_expanded <- dt_mpg %>%
  ## Display all possible values
  # mutate(
  #   cents_mile_value = map2(cents_mile_min_round, cents_mile_max_round, ~ .x:.y)
  # ) %>%
  # unnest(cents_mile_value) %>%
  
  ## Only display min, avg, max values
  pivot_longer(
    starts_with("cents_mile_value_") | starts_with("MPGe_value_"),
    names_to = c(".value", "rank"),
    names_pattern = "(.*)_value_(\\d+)"
  ) %>%
  mutate(
    cents_mile = round(cents_mile, 0),
    MPGe = format(round(MPGe, 1), nsmall = 1)
  ) %>%
  select(vehicle_type, powertrain, cents_mile, MPGe) %>%
  mutate(
    operating_cost_text = paste0(
      cents_mile,
      " cents per mile",
      " (",
      MPGe,
      " MPG equivalent)"
    )
  ) %>%
  select(-MPGe)


dt_mpg_expanded <- dt_mpg_expanded %>%
  group_by(vehicle_type, powertrain, cents_mile) %>%
  slice_head(n = 1) %>% # or slice_tail(n = 1)
  ungroup()


cost_list <- dt_mpg_expanded %>%
  group_by(vehicle_type, powertrain) %>%
  summarise(cents_mile_list = list(cents_mile), .groups = 'drop')


dt_mpg_expanded$powertrain[dt_mpg_expanded$powertrain == 'cv'] = 'gas'

dt_mpg_expanded <- dt_mpg_expanded %>%
  mutate(
    operating_cost_text = str_replace(operating_cost_text, " \\(", "<br> (")
  )


# checking dt_mpg_expanded:

dt_mpg_expanded %>%
  group_by(powertrain, vehicle_type) %>%
  summarise(
    min_cents_mile = min(cents_mile, na.rm = TRUE),
    max_cents_mile = max(cents_mile, na.rm = TRUE),
    .groups = 'drop'
  )

dt_mpg_expanded %>%
  group_by(powertrain, vehicle_type) %>%
  summarise(counts = n(), .groups = "drop")



## --- DCE   ----
###---- For car----

n_respondents = 100

# Define profiles with attributes and levels
profiles_car_low <- cbc_profiles(
  powertrain = c('gas', 'bev','hev'),
  price = seq(1.0, 2.0, 0.2), # unit: 10000
  range_bev = c(0, seq(0.5, 1.5, 0.5)), # unit: 100
  mileage = seq(2, 6, 0.5), # unit: 10000
  age = seq(0.2, 1.0, 0.2), # make_year changed to age / unit: 10
  operating_cost = seq(0.3, 1.8, 0.3) # unit: 10
)

# Restrictions 

profiles_restricted_car <- cbc_restrict(
  profiles_car_low,
  # BEV range restrictions
  (powertrain == "gas") & (range_bev != 0),
  (powertrain == "hev") & (range_bev != 0),
  (powertrain == "bev") & (range_bev == 0),
  # Gas efficiency restrictions
  (powertrain == "gas") & (operating_cost < 0.8),
  (powertrain == "bev") & (operating_cost > 1.2),
  (powertrain == "hev") & (operating_cost < 0.6), 
  (powertrain == "hev") & (operating_cost > 1.2)
)

## Set up priors

priors_fixed_parameter_car <- cbc_priors(
  profiles = profiles_restricted_car,
  powertrain = c("bev" = -1.0,  "hev" = 0.1),
  price = -0.2,
  range_bev = 0.1,
  mileage = -0.5,
  age = -0.2,
  operating_cost = -0.3,
  no_choice = 0.5
)

## Generate Designs

design_car_random <- cbc_design(
  profiles = profiles_restricted_car,
  method = 'random',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_parameter_car,
  balance_by = c('powertrain'),
  remove_dominant = FALSE
)

design_car_minoverlap <- cbc_design(
  profiles = profiles_car_low,
  method = 'minoverlap',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_parameter_car,
  balance_by = c('powertrain'),
  remove_dominant = FALSE
)

design_car_shortcut <- cbc_design(
  profiles = profiles_car_low,
  method = 'shortcut',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_parameter_car,
  balance_by = c('powertrain'),
  remove_dominant = FALSE
)

design_car_random_label <- cbc_design(
  profiles = profiles_car_low,
  method = 'random',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_parameter_car,
  remove_dominant = FALSE,
  label= 'powertrain'
)

design_car_minoverlap_label <- cbc_design(
  profiles = profiles_car_low,
  method = 'minoverlap',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_parameter_car,
  remove_dominant = FALSE,
  label= 'powertrain'
)

design_car_shortcut_label <- cbc_design(
  profiles = profiles_car_low,
  method = 'shortcut',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_parameter_car,
  remove_dominant = FALSE,
  label= 'powertrain'
)

cbc_compare(
  "Random" = design_car_random,
  "Shortcut" = design_car_shortcut,
  "Min Overlap" = design_car_minoverlap,
  "Random_label" = design_car_random_label,
  "Shortcut_label" = design_car_shortcut_label,
  "Min Overlap_label" = design_car_minoverlap_label
)


saveRDS(design_car_random, here('data', 'design_car_random.Rds'))
saveRDS(design_car_shortcut, here('data', 'design_car_shortcut.Rds'))
saveRDS(design_car_minoverlap, here('data', 'design_car_minoverlap.Rds'))
saveRDS(design_car_random_label, here('data', 'design_car_random_label.Rds'))
saveRDS(design_car_shortcut_label, here('data', 'design_car_shortcut_label.Rds'))
saveRDS(design_car_minoverlap_label, here('data', 'design_car_minoverlap_label.Rds'))




cbc_inspect(design_car_random)


design_car_random <- cbc_encode(design_car_random, coding = "dummy")

choices_priors <- cbc_choices(design_car_random, priors = priors_fixed_parameter_car)

model <- logitr(
  data = choices_priors, 
  outcome = 'choice', 
  obsID = 'obsID', 
  pars = c(
    'price', 'range_bev', 'mileage', 'age', 'operating_cost',
    'powertrainbev', 'powertrainhev')
)

summary(model)

power <- cbc_power(choices_priors)

plot(power, type = "power", power_threshold = 0.9)
summary(power, power_threshold = 0.9)

plot(power, type = "se")


#############################################
