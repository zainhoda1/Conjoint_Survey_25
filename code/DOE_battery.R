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

n_respondents = 500

# ---- Battery Survey----
## ---- profile ----
### ----car----
#### ---- budget <= 20k----
profiles_car_low <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(1.0, 2.0, 0.2), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = c(0.5, 1.0, 1.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)

#### ----budget > 20k----
profiles_car_high <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(2.0, 4.0, 0.5), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = c(1.0, 1.5, 2.0, 2.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)

###----SUV----
####----budget <= 20k----
profiles_suv_low <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(1.5, 2.5, 0.2), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = c(1.5, 2.0, 2.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)

####---- budget > 20k----
profiles_suv_high <- cbc_profiles(
  veh_mileage = seq(2, 6, 0.5), # unit: 10000
  veh_price = seq(2.5, 4.5, 0.5), # unit: 10000
  battery_refurbish = c('original', 'cellreplace', 'packreplace'),
  battery_range_year0 = c(2.0, 3.0, 3.5), # unit: 100
  battery_degradation = seq(1, 8, 1) # %
)

## ---- Restrictions----

## ---- Set up priors----

priors_fixed_car_low <- cbc_priors(
  profiles = profiles_car_low,
  veh_mileage = -0.1,
  veh_price = -1.2,
  battery_refurbish = c(-0.3, -0.25),
  battery_range_year0 = 0.4,
  battery_degradation = -0.3,
  no_choice = -2.0
)

priors_fixed_car_high <- cbc_priors(
  profiles = profiles_car_high,
  veh_mileage = -0.2, # Each 10000 mile increase reduces utility by 0.2
  veh_price = -1.0, # Each $10000 increase reduces utility by 1
  battery_refurbish = c(-0.4, -0.35), # Cell refurbishment least preferred
  battery_range_year0 = 0.3, # Each 100 mile of range adds utility by 0.3
  battery_degradation = -0.2, # Each 1% of degradation increases subtracts utility by 0.2
  no_choice = -2.0 # There is a strong positive preference for EV, so positive for "no_choice"
)


priors_fixed_suv_low <- cbc_priors(
  profiles = profiles_suv_low,
  veh_mileage = -0.15,
  veh_price = -1.1,
  battery_refurbish = c(-0.35, -0.3),
  battery_range_year0 = 0.45,
  battery_degradation = -0.35,
  no_choice = -2.0
)

priors_fixed_suv_high <- cbc_priors(
  profiles = profiles_suv_high,
  veh_mileage = -0.25,
  veh_price = -0.9,
  battery_refurbish = c(-0.45, -0.4),
  battery_range_year0 = 0.35,
  battery_degradation = -0.25,
  no_choice = -2.0
)

##---- Generate Designs----
###---- car-low----
design_car_low_random <- cbc_design(
  profiles = profiles_car_low,
  method = 'random',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_car_low,
  remove_dominant = TRUE
)

design_car_low_minoverlap <- cbc_design(
  profiles = profiles_car_low,
  method = 'minoverlap',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_car_low,
  remove_dominant = TRUE
)

design_car_low_shortcut <- cbc_design(
  profiles = profiles_car_low,
  method = 'shortcut',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_car_low,
  remove_dominant = TRUE
)

cbc_compare(
  "Random" = design_car_low_random,
  "Shortcut" = design_car_low_shortcut,
  "Min Overlap" = design_car_low_minoverlap
)

saveRDS(
  design_car_low_random,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_car_low_random.Rds'
  )
)
saveRDS(
  design_car_low_shortcut,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_car_low_shortcut.Rds'
  )
)
saveRDS(
  design_car_low_minoverlap,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_car_low_minoverlap.Rds'
  )
)


###---- car-high----
design_car_high_random <- cbc_design(
  profiles = profiles_car_high,
  method = 'random',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_car_high,
  remove_dominant = TRUE
)

design_car_high_minoverlap <- cbc_design(
  profiles = profiles_car_high,
  method = 'minoverlap',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_car_high,
  remove_dominant = TRUE
)

design_car_high_shortcut <- cbc_design(
  profiles = profiles_car_high,
  method = 'shortcut',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_car_high,
  remove_dominant = TRUE
)

cbc_compare(
  "Random" = design_car_high_random,
  "Shortcut" = design_car_high_shortcut,
  "Min Overlap" = design_car_high_minoverlap
)

saveRDS(
  design_car_high_random,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_car_high_random.Rds'
  )
)
saveRDS(
  design_car_high_shortcut,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_car_high_shortcut.Rds'
  )
)
saveRDS(
  design_car_high_minoverlap,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_car_high_minoverlap.Rds'
  )
)


###---- SUV-low----
design_suv_low_random <- cbc_design(
  profiles = profiles_suv_low,
  method = 'random',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_suv_low,
  remove_dominant = TRUE
)

design_suv_low_minoverlap <- cbc_design(
  profiles = profiles_suv_low,
  method = 'minoverlap',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_suv_low,
  remove_dominant = TRUE
)

design_suv_low_shortcut <- cbc_design(
  profiles = profiles_suv_low,
  method = 'shortcut',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_suv_low,
  remove_dominant = TRUE
)

cbc_compare(
  "Random" = design_suv_low_random,
  "Shortcut" = design_suv_low_shortcut,
  "Min Overlap" = design_suv_low_minoverlap
)

saveRDS(
  design_suv_low_random,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_suv_low_random.Rds'
  )
)
saveRDS(
  design_suv_low_shortcut,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_suv_low_shortcut.Rds'
  )
)
saveRDS(
  design_suv_low_minoverlap,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_suv_low_minoverlap.Rds'
  )
)


###---- SUV-high----
design_suv_high_random <- cbc_design(
  profiles = profiles_suv_high,
  method = 'random',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_suv_high,
  remove_dominant = TRUE
)

design_suv_high_minoverlap <- cbc_design(
  profiles = profiles_suv_high,
  method = 'minoverlap',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_suv_high,
  remove_dominant = TRUE
)

design_suv_high_shortcut <- cbc_design(
  profiles = profiles_suv_high,
  method = 'shortcut',
  n_resp = n_respondents, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_suv_high,
  remove_dominant = TRUE
)

cbc_compare(
  "Random" = design_suv_high_random,
  "Shortcut" = design_suv_high_shortcut,
  "Min Overlap" = design_suv_high_minoverlap
)

saveRDS(
  design_suv_high_random,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_suv_high_random.Rds'
  )
)
saveRDS(
  design_suv_high_shortcut,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_suv_high_shortcut.Rds'
  )
)
saveRDS(
  design_suv_high_minoverlap,
  here(
    'data',
    'doe',
    'design_new',
    'profiles',
    'battery_design_suv_high_minoverlap.Rds'
  )
)


##---- Inspect Design----
# cbc_inspect(design_car_low_random)
# cbc_inspect(design_car_low_shortcut)
# cbc_inspect(design_car_low_minoverlap)

##---- Simulate Choices----
# choices_car_low_random <- cbc_choices(
#   design_car_low_random,
#   priors = priors_fixed_car_low
# )
# choices_car_low_shortcut <- cbc_choices(
#   design_car_low_shortcut,
#   priors = priors_fixed_car_low
# )
choices_car_low_minoverlap <- cbc_choices(
  design_car_low_minoverlap,
  priors = priors_fixed_car_low
)
choices_car_high_minoverlap <- cbc_choices(
  design_car_high_minoverlap,
  priors = priors_fixed_car_high
)
choices_suv_low_minoverlap <- cbc_choices(
  design_suv_low_minoverlap,
  priors = priors_fixed_suv_low
)
choices_suv_high_minoverlap <- cbc_choices(
  design_suv_high_minoverlap,
  priors = priors_fixed_suv_high
)

##---- Assess Power----

# design_car_low_random <- cbc_encode(design_car_low_random, coding = "dummy")
# design_car_low_shortcut <- cbc_encode(design_car_low_shortcut, coding = "dummy")
design_car_low_minoverlap <- cbc_encode(
  design_car_low_minoverlap,
  coding = "dummy"
)
design_car_high_minoverlap <- cbc_encode(
  design_car_high_minoverlap,
  coding = "dummy"
)
design_suv_low_minoverlap <- cbc_encode(
  design_suv_low_minoverlap,
  coding = "dummy"
)
design_suv_high_minoverlap <- cbc_encode(
  design_suv_high_minoverlap,
  coding = "dummy"
)

# power_car_low_random <- cbc_power(
#   data = choices_car_low_random,
#   outcome = "choice",
#   obsID = "obsID",
#   n_q = 6,
#   n_breaks = 10
# )
#
# power_car_low_shortcut <- cbc_power(
#   data = choices_car_low_shortcut,
#   outcome = "choice",
#   obsID = "obsID",
#   n_q = 6,
#   n_breaks = 10
# )

power_car_low_minoverlap <- cbc_power(
  data = choices_car_low_minoverlap,
  outcome = "choice",
  obsID = "obsID",
  n_q = 6,
  n_breaks = 10
)
power_car_high_minoverlap <- cbc_power(
  data = choices_car_high_minoverlap,
  outcome = "choice",
  obsID = "obsID",
  n_q = 6,
  n_breaks = 10
)
power_suv_low_minoverlap <- cbc_power(
  data = choices_suv_low_minoverlap,
  outcome = "choice",
  obsID = "obsID",
  n_q = 6,
  n_breaks = 10
)
power_suv_high_minoverlap <- cbc_power(
  data = choices_suv_high_minoverlap,
  outcome = "choice",
  obsID = "obsID",
  n_q = 6,
  n_breaks = 10
)


# par(mfrow = c(1, 3))
# plot(power_car_low_random, type = "power", power_threshold = 0.9)
# plot(power_car_low_shortcut, type = "power", power_threshold = 0.9)
# plot(power_car_low_minoverlap, type = "power", power_threshold = 0.9)
# par(mfrow = c(1, 1))

# plot(power_car_low_random, type = "se", ylim = c(0, 0.5))
# plot(power_car_low_shortcut, type = "se", ylim = c(0, 0.5))
plot(power_car_low_minoverlap, type = "se", ylim = c(0, 0.5))
plot(power_car_high_minoverlap, type = "se", ylim = c(0, 0.5))
plot(power_suv_low_minoverlap, type = "se", ylim = c(0, 0.5))
plot(power_suv_high_minoverlap, type = "se", ylim = c(0, 0.5))

# summary(power_car_low_random, power_threshold = 0.9)

## ---- Modeling----
# model_car_low_random <- logitr(
#   data = choices_car_low_random,
#   outcome = 'choice',
#   obsID = 'obsID',
#   pars = c(
#     'veh_price',
#     'veh_mileage',
#     'battery_refurbishcellreplace',
#     'battery_refurbishpackreplace',
#     'battery_range_year0',
#     'battery_degradation',
#     'no_choice'
#   )
# )
#
# summary(model_car_low_random)
# wtp(model_car_low_random, scalePar = "veh_price")
#
#
# model_car_low_shortcut <- logitr(
#   data = choices_car_low_shortcut,
#   outcome = 'choice',
#   obsID = 'obsID',
#   pars = c(
#     'veh_price',
#     'veh_mileage',
#     'battery_refurbishcellreplace',
#     'battery_refurbishpackreplace',
#     'battery_range_year0',
#     'battery_degradation',
#     'no_choice'
#   )
# )
#
# summary(model_car_low_shortcut)
# wtp(model_car_low_shortcut, scalePar = "veh_price")

model_car_low_minoverlap <- logitr(
  data = choices_car_low_minoverlap,
  outcome = 'choice',
  obsID = 'obsID',
  pars = c(
    'veh_price',
    'veh_mileage',
    'battery_refurbishcellreplace',
    'battery_refurbishpackreplace',
    'battery_range_year0',
    'battery_degradation',
    'no_choice'
  )
)

summary(model_car_low_minoverlap)
wtp(model_car_low_minoverlap, scalePar = "veh_price")

model_car_high_minoverlap <- logitr(
  data = choices_car_high_minoverlap,
  outcome = 'choice',
  obsID = 'obsID',
  pars = c(
    'veh_price',
    'veh_mileage',
    'battery_refurbishcellreplace',
    'battery_refurbishpackreplace',
    'battery_range_year0',
    'battery_degradation',
    'no_choice'
  )
)

summary(model_car_high_minoverlap)
wtp(model_car_high_minoverlap, scalePar = "veh_price")

model_suv_low_minoverlap <- logitr(
  data = choices_suv_low_minoverlap,
  outcome = 'choice',
  obsID = 'obsID',
  pars = c(
    'veh_price',
    'veh_mileage',
    'battery_refurbishcellreplace',
    'battery_refurbishpackreplace',
    'battery_range_year0',
    'battery_degradation',
    'no_choice'
  )
)

summary(model_suv_low_minoverlap)
wtp(model_suv_low_minoverlap, scalePar = "veh_price")

model_suv_high_minoverlap <- logitr(
  data = choices_suv_high_minoverlap,
  outcome = 'choice',
  obsID = 'obsID',
  pars = c(
    'veh_price',
    'veh_mileage',
    'battery_refurbishcellreplace',
    'battery_refurbishpackreplace',
    'battery_range_year0',
    'battery_degradation',
    'no_choice'
  )
)

summary(model_suv_high_minoverlap)
wtp(model_suv_high_minoverlap, scalePar = "veh_price")

## ---- Output----
design_battery <- rbind(
  design_car_low_minoverlap %>% mutate(vehicle_type = "car", budget = "low"),
  design_car_high_minoverlap %>%
    mutate(vehicle_type = "car", budget = "high"),
  design_suv_low_minoverlap %>% mutate(vehicle_type = "suv", budget = "low"),
  design_suv_high_minoverlap %>% mutate(vehicle_type = "suv", budget = "high")
)

design_rand_output_battery <- design_battery %>%
  mutate(
    price = veh_price * 10000,
    mileage = veh_mileage * 10000,
    battery_degradation = battery_degradation / 100,
    battery_range_year0 = battery_range_year0 * 100
  ) %>%
  mutate(
    battery_health_year0 = paste0(round(1 * 100, 0), "%"),
    battery_health_year3 = (1 - battery_degradation)^3,
    battery_health_year8 = (1 - battery_degradation)^8, # round to the closest 5
    battery_range_year3 = round(
      battery_range_year0 * battery_health_year3 / 5
    ) *
      5,
    battery_range_year8 = round(
      battery_range_year0 * battery_health_year8 / 5
    ) *
      5,
    battery_health_year3 = paste0(
      round((1 - battery_degradation)^3 * 100, 0),
      "%"
    ),
    battery_health_year8 = paste0(
      round((1 - battery_degradation)^8 * 100, 0),
      "%"
    )
  ) %>%
  mutate(
    battery_condition = case_when(
      no_choice == 1 ~ NA,
      battery_refurbishcellreplace == 1 ~ 'Refurbished Cell-Replaced',
      battery_refurbishpackreplace == 1 ~ 'Refurbished Pack-Replaced',
      TRUE ~ 'Original Battery'
    )
  ) %>%
  select(
    -battery_refurbishcellreplace,
    -battery_refurbishpackreplace,
    -veh_mileage,
    -veh_price
  )


#write.csv(design_rand_output_battery, here('survey', 'data', 'battery_choice_questions.csv'))

arrow::write_parquet(
  design_rand_output_battery,
  here('survey', 'data', 'design_battery.parquet')
)

saveRDS(
  design_rand_output_battery,
  here('data', 'design_battery.Rds')
)
