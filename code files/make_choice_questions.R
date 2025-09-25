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
# library(readxl)

# quantiles of MPG for the 2024 model year vehicles by for different powertrains and vehicle types. Data source: EPA or fuel_economic_gov
dt_mpg<-as.data.frame(read.csv(paste0(here(),"/survey_updated_dynata/data/mpg_by_segment_fuel.csv"))) %>% 
  # kwh per 100 miles --> kwh per mile
  mutate(kwh_q10=kwh_q10/100,
         kwh_q25=kwh_q25/100,
         kwh_q50=kwh_q50/100,
         kwh_q75=kwh_q75/100,
         kwh_q90=kwh_q90/100
  )


# electricity rate per kwh https://www.chooseenergy.com/electricity-rates-by-state/ March 2025
electricity_rate_low <- 0.11
electricity_rate_avg <- 0.17
electricity_rate_high <-0.33

# gas rate per gallon https://www.chooseenergy.com/data-center/cost-of-driving-by-state May 2025
gasoline_rate_low <- 2.66
gasoline_rate_avg <- 3.18
gasoline_rate_high <- 4.77

# PHEV utility factors https://docs.nrel.gov/docs/gen/fy07/41341.pdf
phev_uf_car<- 0.668
phev_uf_suv<- 0.487

# MPGe- One gallon of gasoline is equivalent to 33.7 kilowatt-hours (kWh) of electricity https://www.bluegrassauto.com/hybrid-and-electric-vehicle-comparisons/
gas_electricity<- 33.7

# # PHEV: Assume total range = 300 miles, electric range = 40 miles,
# phev_total_range<- 300
# phev_e_range<- 40


dt_mpg<-dt_mpg %>% 
  mutate(
    cents_mile_min=case_when(
      powertrain=="bev" ~ electricity_rate_low*(kwh_q10)*100,
      powertrain %in% c("cv","hev") ~ gasoline_rate_low/mpg_q90*100,
      
      powertrain=="phev" & vehicle_type=="car" ~ (phev_uf_car*(electricity_rate_low*(kwh_q10))+(1-phev_uf_car)*(gasoline_rate_low/mpg_q90))*100,
      powertrain=="phev" & vehicle_type=="suv" ~ (phev_uf_suv*(electricity_rate_low*(kwh_q10))+(1-phev_uf_suv)*(gasoline_rate_low/mpg_q90))*100
    ),
    cents_mile_max=case_when(
      powertrain=="bev" ~ electricity_rate_high*(kwh_q90)*100,
      powertrain %in% c("cv","hev") ~ gasoline_rate_high/mpg_q10*100,
      
      powertrain=="phev" & vehicle_type=="car" ~ (phev_uf_car*(electricity_rate_high*(kwh_q90))+(1-phev_uf_car)*(gasoline_rate_high/mpg_q10))*100,
      powertrain=="phev" & vehicle_type=="suv" ~ (phev_uf_suv*(electricity_rate_high*(kwh_q90))+(1-phev_uf_suv)*(gasoline_rate_high/mpg_q10))*100
    )
    
  ) %>%
  mutate(MPGe_min=case_when(
    powertrain %in% c("bev","cv","hev") ~ mpg_q10,
    
    powertrain=="phev" & vehicle_type=="car" ~ (1 / (phev_uf_car * (kwh_q10 / gas_electricity) + (1 - phev_uf_car) * (1 / mpg_q90))),
    powertrain=="phev" & vehicle_type=="suv" ~ (1 / (phev_uf_suv * (kwh_q10 / gas_electricity) + (1 - phev_uf_suv) * (1 / mpg_q90)))
  ),
  
  MPGe_max=case_when(
    powertrain %in% c("bev","cv","hev") ~ mpg_q90,
    
    powertrain=="phev" & vehicle_type=="car" ~ (1 / (phev_uf_car * (kwh_q90 / gas_electricity) + (1 - phev_uf_car) * (1 / mpg_q10))),
    powertrain=="phev" & vehicle_type=="suv" ~ (1 / (phev_uf_suv * (kwh_q90 / gas_electricity) + (1 - phev_uf_suv) * (1 / mpg_q10)))
  )
  
  ) 


dt_mpg<-dt_mpg %>% 
  rowwise() %>%
  mutate(
    quintiles = list(seq(from = cents_mile_min, to = cents_mile_max, length.out = 5))
  ) %>%
  unnest_wider(quintiles, names_sep = "_") %>%
  rename_with(~ paste0("cents_mile_value_", 1:5), starts_with("quintiles")) 

dt_mpg<-dt_mpg%>% 
  rowwise() %>%
  mutate(
    quintiles = list(seq(from = MPGe_max, to = MPGe_min, length.out = 5))
  ) %>%
  unnest_wider(quintiles, names_sep = "_") %>%
  rename_with(~ paste0("MPGe_value_", 1:5), starts_with("quintiles"))


# write.csv(dt_mpg, here("survey","data","mpg_by_segment_fuel_cost_final.csv"),row.names = F)    

dt_mpg_expanded <- dt_mpg %>%
  ## Display all possible values
  # mutate(
  #   cents_mile_value = map2(cents_mile_min_round, cents_mile_max_round, ~ .x:.y)
  # ) %>%
  # unnest(cents_mile_value) %>%
  
  ## Only display min, avg, max values
  pivot_longer(starts_with("cents_mile_value_") | starts_with("MPGe_value_"),
               names_to = c(".value", "rank"),
               names_pattern = "(.*)_value_(\\d+)"
  ) %>%
  mutate(cents_mile=round(cents_mile,0),
         MPGe=format(round(MPGe,1), nsmall=1)) %>% 
  select(vehicle_type,powertrain,cents_mile, MPGe) %>%
  mutate(operating_cost_text =  paste0(cents_mile, " cents per mile", 
                                       " ("  ,MPGe, " MPG equivalent)")) %>% 
  select(-MPGe)


cost_list <- dt_mpg_expanded %>%
  group_by(vehicle_type, powertrain) %>%
  summarise(cents_mile_list = list(cents_mile), .groups = 'drop')



dt_mpg_expanded$powertrain[dt_mpg_expanded$powertrain == 'cv'] = 'gas'

dt_mpg_expanded <- dt_mpg_expanded %>%
  mutate(operating_cost_text = str_replace(operating_cost_text, " \\(", "<br> ("))



### For car 



# Define profiles with attributes and levels
profiles <- cbc_profiles(
  powertrain     = c('gas', 'bev', 'phev', 'hev'),
  price          = seq(0.8, 1.1, 0.1),
  range_bev = c(0, seq(0.5, 2.5, 0.5)), # x 100
  range_phev = c(0, seq(0.1, 0.4, 1)), # x 100
  mileage        = seq(2, 6, 0.5), # x 10000
  age            = seq(2, 10),  # make_year changed to age
  operating_cost = seq(dt_mpg_expanded %>%
                         filter(vehicle_type == "car") %>%
                         select(starts_with("cents")) %>%
                         unlist() %>%
                         min(na.rm = TRUE),
                       dt_mpg_expanded %>%
                         filter(vehicle_type == "car") %>%
                         select(starts_with("cents")) %>%
                         unlist() %>%
                         max(na.rm = TRUE)+1)
) 


# Restrictions

profiles_restricted <- cbc_restrict(
  profiles,
  # BEV range restrictions
  (powertrain == "gas") & (range_bev != 0),
  (powertrain == "hev") & (range_bev != 0),
  (powertrain == "phev") & (range_bev != 0),
  (powertrain == "bev") & (range_bev == 0),
  # PHEV range restrictions
  (powertrain == "gas") & (range_phev != 0),
  (powertrain == "hev") & (range_phev != 0),
  (powertrain == "bev") & (range_phev != 0),
  (powertrain == "phev") & (range_phev == 0),
  # Gas efficiency restrictions
  #(powertrain == "gas") & (operating_cost < 9),
  (powertrain == "bev") & !(operating_cost %in% c(3,5,7,9,12)),
  (powertrain == "hev") & !(operating_cost %in% c(5,7,9,11,12)), 
  (powertrain == "phev") & !(operating_cost %in% c(3,6,8,10,12)), 
  (powertrain == "gas") & !(operating_cost %in% c(8,12,16,20,24)) 

)


## Set up priors

priors_fixed_parameter <- cbc_priors(
  profiles = profiles_restricted,
  # powertrain: categorical (effects coded or dummy)
  powertrain = c("bev" = -1.0, "phev" = -0.5,  "hev" = 0.1),
  price = -0.2,
  range_bev = 0.1,
  range_phev = 0.1,
  mileage = -0.5,
  age = -0.2,
  operating_cost = -0.3,
  no_choice = 0.5
)

## Generate Designs

design <- cbc_design(
  profiles = profiles_restricted,
  method = 'random',
  n_resp = 4000, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_parameter,
  balance_by = c('powertrain', 'operating_cost'),
  remove_dominant = FALSE
)


design <- design %>% 
  mutate(powertrain = case_when(powertrainphev == 1 ~ 'phev',
                                powertrainhev == 1 ~ 'hev',
                                powertrainbev == 1 ~ 'bev',
                                ( powertrainphev == 0 & 
                                  powertrainhev == 0 &
                                  powertrainbev == 0 &
                                  no_choice == 0) ~ 'gas'
                                ),
         range_phev = range_phev * 100,
         range_bev = range_bev * 100,
         range = case_when(powertrainphev == 1 ~ paste0(range_phev, ' miles on a full charge'),
                               powertrainbev == 1 ~ paste0(range_bev,' miles on a full charge'),
                               TRUE ~ NA),
         vehicle_type = 'car'
         )

by <- join_by(vehicle_type, powertrain,  operating_cost == cents_mile)
design_car <- left_join(design, dt_mpg_expanded, by)





################################################################################################



### For SUV 


# Define profiles with attributes and levels
profiles <- cbc_profiles(
  powertrain     = c('gas', 'bev', 'phev', 'hev'),
  price          = seq(0.8, 1.1, 0.1),
  range_bev = c(0, seq(0.5, 2.5, 0.5)), # x 100
  range_phev = c(0, seq(0.1, 0.4, 1)), # x 100
  mileage        = seq(2, 6, 0.5), # x 10000
  age            = seq(2, 10),  # make_year changed to age
  operating_cost = seq(dt_mpg_expanded %>%
                         filter(vehicle_type == "suv") %>%
                         select(starts_with("cents")) %>%
                         unlist() %>%
                         min(na.rm = TRUE),
                       dt_mpg_expanded %>%
                         filter(vehicle_type == "suv") %>%
                         select(starts_with("cents")) %>%
                         unlist() %>%
                         max(na.rm = TRUE)+1)
) 


# Restrictions

profiles_restricted <- cbc_restrict(
  profiles,
  # BEV range restrictions
  (powertrain == "gas") & (range_bev != 0),
  (powertrain == "hev") & (range_bev != 0),
  (powertrain == "phev") & (range_bev != 0),
  (powertrain == "bev") & (range_bev == 0),
  # PHEV range restrictions
  (powertrain == "gas") & (range_phev != 0),
  (powertrain == "hev") & (range_phev != 0),
  (powertrain == "bev") & (range_phev != 0),
  (powertrain == "phev") & (range_phev == 0),
  # Gas efficiency restrictions
  #(powertrain == "gas") & (operating_cost < 9),
  (powertrain == "bev") & !(operating_cost %in% c(3,6,8,11,14)),
  (powertrain == "hev") & !(operating_cost %in% c(7,10,13,17,20)), 
  (powertrain == "phev") & !(operating_cost %in% c(8,12,16,19,23)), 
  (powertrain == "gas") & !(operating_cost %in% c(9,13,17,21,25)) 
)


## Set up priors


priors_fixed_parameter <- cbc_priors(
  profiles = profiles_restricted,
  # powertrain: categorical (effects coded or dummy)
  powertrain = c("bev" = -1.0, "phev" = -0.5,  "hev" = 0.1),
  price = -0.2,
  range_bev = 0.1,
  range_phev = 0.1,
  mileage = -0.5,
  age = -0.2,
  operating_cost = -0.3,
  no_choice = 0.5
)


## Generate Designs

design <- cbc_design(
  profiles = profiles_restricted,
  method = 'random',
  n_resp = 4000, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = priors_fixed_parameter,
  balance_by = c('powertrain', 'operating_cost'),
  remove_dominant = FALSE
)


design <- design %>% 
  mutate(powertrain = case_when(powertrainphev == 1 ~ 'phev',
                                powertrainhev == 1 ~ 'hev',
                                powertrainbev == 1 ~ 'bev',
                                ( powertrainphev == 0 & 
                                    powertrainhev == 0 &
                                    powertrainbev == 0 &
                                    no_choice == 0) ~ 'gas'
  ),
  range_phev = range_phev * 100,
  range_bev = range_bev * 100,
  range = case_when(powertrainphev == 1 ~ paste0(range_phev, ' miles on a full charge'),
                    powertrainbev == 1 ~ paste0(range_bev,' miles on a full charge'),
                    TRUE ~ NA),
  vehicle_type = 'suv'
  )




by <- join_by(vehicle_type, powertrain,  operating_cost == cents_mile)
design_suv <- left_join(design, dt_mpg_expanded, by)



################################################################################################




design_combined<-rbind(design_car,design_suv)


# Save design
write_csv(design_combined, here('survey_updated_dynata','data', 'choice_questions.csv'))


