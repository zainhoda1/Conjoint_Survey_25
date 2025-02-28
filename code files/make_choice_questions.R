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
  mileage        = seq(20, 60, 5),
  my             = seq(2015, 2023),
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

# profiles_new <- cbc_profiles(
#   powertrain     = c('Gasoline', 'Electric', 'Plug-in Hybrid'),
#   price          = seq(0.8, 1.1, 0.1),
#   range          = seq(50, 250, 25),
#   mileage        = seq(20, 60, 5),
#   operating_cost = seq(6, 21, 3)
# )

# Make a basic survey using the full factorial of all profiles
design <- cbc_design(
  profiles = profiles_used_restricted,
  n_resp   = 2000, # Number of respondents
  n_alts   = 3,    # Number of alternatives per question
  n_q      = 6     # Number of questions per respondent
)

head(design) # preview

# Add image names matched to the apple type
# (we'll use these to display images in the survey)
# image_names <- data.frame(
#   type = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#   image = c('fuji.jpg', 'gala.jpg', 'honeycrisp.jpg', 'pinkLady.jpg',
#             'redDelicious.jpg')
# )
# design <- design %>%
#   left_join(image_names, by = "type")
# head(design) # preview


design$range[design$powertrain =='Gasoline'] <- 'NA'

duplicates <- design[duplicated(design[c('respID', 'qID', 'powertrain', 'price',
                                       'range', 'mileage', 'operating_cost' )]), ]

design <- design[!duplicated(design[c('respID', 'qID', 'powertrain', 'price',
                            'range', 'mileage', 'operating_cost' )]), ]

#test <- design %>% filter(respID == 342)

id_to_remove <- duplicates$respID

design <- design %>% 
  filter(!respID %in% id_to_remove)


# Save design
write_csv(design, here('survey','data', 'choice_questions.csv'))


