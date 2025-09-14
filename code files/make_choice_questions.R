# Make conjoint surveys using the cbcTools package

# Install packages
# install.packages("remotes")
# install.packages("tidyverse")
# remotes::install_github("jhelvy/cbcTools")

# Load libraries
library(cbcTools)
library(tidyverse)
library(here)
library(data.table)
library(cbcTools)
library(janitor)


### For car 
# Define profiles with attributes and levels
profiles <- cbc_profiles(
  powertrain     = c('Conventional','Gas hybrid', 'Plug-in hybrid','Battery electric'),
  price          = seq(0.8, 1.1, 0.1),
  range          = seq(50, 250, 25), # x 100
  mileage        = seq(20000, 60000, 5000), # x 10,000
  make_year      = seq(2015, 2023),  # (2015 - 2023)
  operating_cost = seq(3, 21, 3)
) 


## Resrictions:


profiles_restricted <- cbc_restrict(
  profiles,
  (powertrain == "Gasoline") & (operating_cost < 9),
  (powertrain != "Gasoline") & (operating_cost >= 18)
)



# Make a basic survey using the full factorial of all profiles
design <- cbc_design(
  profiles = profiles,
  n_resp   = 4000, # Number of respondents # original 2000 # higher number than respondents
  n_alts   = 3,    # Number of alternatives per question
  n_q      = 6,    # Number of questions per respondent
  no_choice = TRUE
)


design$range[design$'powertrainBattery electric' !='1'] <- 300


duplicates <- design[duplicated(design[c('respID',
                                         'qID',
                                         'powertrainGas hybrid',
                                         'powertrainPlug-in hybrid',
                                         'powertrainBattery electric',
                                         'price',
                                         'range',
                                        'mileage',
                                        'make_year',
                                       'operating_cost' )]), ]

design <- design[!duplicated(design[c( 'respID',
                                       'qID',
                                       'powertrainGas hybrid',
                                       'powertrainPlug-in hybrid',
                                       'powertrainBattery electric',
                                       'price',
                                       'range',
                                       'mileage',
                                       'make_year',
                                       'operating_cost')]), ]
 

cbc_inspect(design)


choices <- cbc_choices(design)

design <- design %>%
  rename(powertrain_gas_hybrid = `powertrainGas hybrid`,
         powertrain_plug_in_hybrid = `powertrainPlug-in hybrid`,
         powertrain_battery_electric = `powertrainBattery electric`)


design <- design %>%
  mutate(range = case_when(
    powertrain_gas_hybrid == 1 ~ "300 miles on 1 tank",
    powertrain_plug_in_hybrid == 1 ~ "300 miles on 1 tank \n(first 40 miles electric)",
    powertrain_battery_electric == 1  ~ paste0(design$range, " miles on full charge"),
    (powertrain_battery_electric == 0 &
       powertrain_plug_in_hybrid == 0 &
       powertrain_gas_hybrid == 0 &
       no_choice == 0)~ "300 miles on 1 tank"
  ),
  powertrain = case_when(
    powertrain_gas_hybrid == 1 ~ "Gas hybrid",
    powertrain_plug_in_hybrid == 1 ~ "Plug-in hybrid",
    powertrain_battery_electric == 1  ~ 'Battery electric',
    (powertrain_battery_electric == 0 &
       powertrain_plug_in_hybrid == 0 &
       powertrain_gas_hybrid == 0 &
       no_choice == 0)~ "Conventional"
  )
  )


design_car<-design %>% mutate(vehicle_type="car")


# Save design
write_csv(design_car, here('survey_updated_Dynata','data', 'testing_choice_questions.csv'))


power <- cbc_power(choices)


plot(power, type = "power", power_threshold = 0.9)

summary(power, power_threshold = 0.9)







