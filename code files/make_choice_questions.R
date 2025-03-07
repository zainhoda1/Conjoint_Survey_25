# Make conjoint surveys using the cbcTools package

# Install packages
# install.packages("remotes")
# install.packages("tidyverse")
# remotes::install_github("jhelvy/cbcTools")

# Load libraries
library(cbcTools)
library(tidyverse)
library(here)

# Define profiles with attributes and levels
profiles_used <- cbc_profiles(
  powertrain     = c('Gasoline', 'Electric', 'Plug-in Hybrid', 'Hybrid'),
  price          = seq(0.8, 1.1, 0.1),
  range          = seq(50, 250, 25),
  mileage        = seq(20000, 60000, 5000),
  make_year      = seq(2015, 2023),
  operating_cost = seq(3, 21, 3)
)

profiles_used_restricted <- cbc_restrict(
  profiles_used, 
  (powertrain == "Gasoline") & (operating_cost < 9), 
  (powertrain != "Gasoline") & (operating_cost >= 18)
)

# Check powertrain counts
profiles_used_restricted %>% 
  count(powertrain, operating_cost)


# Make a basic survey using the full factorial of all profiles
design <- cbc_design(
  profiles = profiles_used_restricted,
  n_resp   = 2000, # Number of respondents
  n_alts   = 3,    # Number of alternatives per question
  n_q      = 6     # Number of questions per respondent
)

head(design) # preview

design$range[design$powertrain !='Electric'] <- 300

#design$range[design$powertrain =='Gasoline'] <- 'NA'

duplicates <- design[duplicated(design[c('respID', 'qID', 'powertrain', 'price',
                                       'range', 'mileage', 'operating_cost' )]), ]

design <- design[!duplicated(design[c('respID', 'qID', 'powertrain', 'price',
                            'range', 'mileage', 'operating_cost' )]), ]

#test <- design %>% filter(respID == 342)

id_to_remove <- duplicates$respID

design <- design %>% 
  filter(!respID %in% id_to_remove)

design$operating_cost =  paste0(design$operating_cost, " cents per mile", "\n", 
                            "("  ,round(330/design$operating_cost, 1), " MPG equivalent)")


design <- design %>% 
  mutate(range = case_when(
    powertrain == "Hybrid" ~ "300 mile range on 1 tank",
    powertrain == "Gasoline" ~ "300 mile range on 1 tank",
    powertrain == "Plug-in Hybrid" ~ "300 mile range on 1 tank \n(first 40 miles electric)",
    powertrain == "Electric"  ~ paste0(design$range, " mile range on full charge")
  ))


# Save design
write_csv(design, here('data', 'choice_questions.csv'))


