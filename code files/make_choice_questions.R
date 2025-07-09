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
dt_mpg<-as.data.frame(read.csv(paste0(here(),"/survey/data/mpg_by_segment_fuel.csv"))) %>% 
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

### For car 
# Define profiles with attributes and levels
profiles_used <- cbc_profiles(
  powertrain     = c('Conventional','Gas hybrid', 'Plug-in hybrid','Battery electric'),
  price          = seq(0.8, 1.1, 0.1),
  range          = seq(50, 250, 25),
  mileage        = seq(20000, 60000, 5000),
  make_year      = seq(2015, 2023), 
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

profiles_used <-profiles_used %>% 
  mutate(keep_row = case_when(
    powertrain == "Conventional"   & operating_cost %in% unlist(cost_list %>% filter(vehicle_type == "car", powertrain == "cv")   %>% pull(cents_mile_list)) ~ TRUE,
    powertrain == "Gas hybrid"     & operating_cost %in% unlist(cost_list %>% filter(vehicle_type == "car", powertrain == "hev")  %>% pull(cents_mile_list)) ~ TRUE,
    powertrain == "Plug-in hybrid" & operating_cost %in% unlist(cost_list %>% filter(vehicle_type == "car", powertrain == "phev") %>% pull(cents_mile_list)) ~ TRUE,
    powertrain == "Battery electric" & operating_cost %in% unlist(cost_list %>% filter(vehicle_type == "car", powertrain == "bev") %>% pull(cents_mile_list)) ~ TRUE,
    TRUE ~ FALSE
  ))




profiles_used_restricted <- profiles_used %>% filter(keep_row)

# Check powertrain counts
# profiles_used_restricted %>% 
#   count(powertrain, operating_cost)
# 
# dt_mpg_expanded %>%
#   filter(vehicle_type=="car") %>% 
#   count(powertrain, cents_mile)


# Make a basic survey using the full factorial of all profiles
design <- cbc_design(
  profiles = profiles_used_restricted,
  n_resp   = 2000, # Number of respondents # original 2000 # higher number than respondents
  n_alts   = 3,    # Number of alternatives per question
  n_q      = 6     # Number of questions per respondent
)

# head(design) # preview
design %>%
  count(powertrain, operating_cost)

design$range[design$powertrain !='Battery electric'] <- 300


# duplicates <- design[duplicated(design[c('respID', 'qID', 'powertrain', 'price',
#                                        'range', 'mileage', 'operating_cost' )]), ]
# 
# design <- design[!duplicated(design[c('respID', 'qID', 'powertrain', 'price',
#                             'range', 'mileage', 'operating_cost' )]), ]
#test <- design %>% filter(respID == 342)
# id_to_remove <- duplicates$respID
# design <- design %>% 
#   filter(!respID %in% id_to_remove)

design <- design %>% 
  left_join(dt_mpg_expanded %>% 
              filter(vehicle_type=="car") %>%
              select(powertrain,cents_mile,operating_cost_text) %>% 
              mutate(powertrain=case_when(powertrain=="cv"~"Conventional",
                                          powertrain=="hev" ~ "Gas hybrid",
                                          powertrain=="phev" ~ "Plug-in hybrid",
                                          powertrain=="bev" ~ "Battery electric")), 
            by=c("powertrain"="powertrain","operating_cost"="cents_mile")) %>% 
  mutate(operating_cost=operating_cost_text) %>%
select(-c(operating_cost_text, keep_row))
  


design <- design %>% 
  mutate(range = case_when(
    powertrain == "Gas hybrid" ~ "300 miles on 1 tank",
    powertrain == "Conventional" ~ "300 miles on 1 tank",
    powertrain == "Plug-in hybrid" ~ "300 miles on 1 tank \n(first 40 miles electric)",
    powertrain == "Battery electric"  ~ paste0(design$range, " miles on full charge")
  ))

design_car<-design %>% mutate(vehicle_type="car")


### For suv 
# Define profiles with attributes and levels
profiles_used <- cbc_profiles(
  powertrain     = c('Conventional','Gas hybrid', 'Plug-in hybrid','Battery electric'),
  price          = seq(0.8, 1.1, 0.1),
  range          = seq(50, 250, 25),
  mileage        = seq(20000, 60000, 5000),
  make_year      = seq(2015, 2023), 
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

profiles_used <-profiles_used %>% 
  mutate(keep_row = case_when(
    powertrain == "Conventional"   & operating_cost %in% unlist(cost_list %>% filter(vehicle_type == "suv", powertrain == "cv")   %>% pull(cents_mile_list)) ~ TRUE,
    powertrain == "Gas hybrid"     & operating_cost %in% unlist(cost_list %>% filter(vehicle_type == "suv", powertrain == "hev")  %>% pull(cents_mile_list)) ~ TRUE,
    powertrain == "Plug-in hybrid" & operating_cost %in% unlist(cost_list %>% filter(vehicle_type == "suv", powertrain == "phev") %>% pull(cents_mile_list)) ~ TRUE,
    powertrain == "Battery electric" & operating_cost %in% unlist(cost_list %>% filter(vehicle_type == "suv", powertrain == "bev") %>% pull(cents_mile_list)) ~ TRUE,
    TRUE ~ FALSE
  ))




profiles_used_restricted <- profiles_used %>% filter(keep_row)

# Check powertrain counts
# profiles_used_restricted %>% 
#   count(powertrain, operating_cost)
# 
# dt_mpg_expanded %>%
#   filter(vehicle_type=="suv") %>% 
#   count(powertrain, cents_mile)


# Make a basic survey using the full factorial of all profiles
design <- cbc_design(
  profiles = profiles_used_restricted,
  n_resp   = 2000, # Number of respondents # original 2000 # higher number than respondents
  n_alts   = 3,    # Number of alternatives per question
  n_q      = 6     # Number of questions per respondent
)

# head(design) # preview
design %>%
  count(powertrain, operating_cost)

design$range[design$powertrain !='Battery electric'] <- 300


# duplicates <- design[duplicated(design[c('respID', 'qID', 'powertrain', 'price',
#                                          'range', 'mileage', 'operating_cost' )]), ]
# 
# design <- design[!duplicated(design[c('respID', 'qID', 'powertrain', 'price',
#                                       'range', 'mileage', 'operating_cost' )]), ]

#test <- design %>% filter(respID == 342)
# id_to_remove <- duplicates$respID
# design <- design %>% 
#   filter(!respID %in% id_to_remove)

design <- design %>% 
  left_join(dt_mpg_expanded %>% 
              filter(vehicle_type=="suv") %>%
              select(powertrain,cents_mile,operating_cost_text) %>% 
              mutate(powertrain=case_when(powertrain=="cv"~"Conventional",
                                          powertrain=="hev" ~ "Gas hybrid",
                                          powertrain=="phev" ~ "Plug-in hybrid",
                                          powertrain=="bev" ~ "Battery electric")), 
            by=c("powertrain"="powertrain","operating_cost"="cents_mile")) %>% 
  mutate(operating_cost=operating_cost_text) %>%
  select(-c(operating_cost_text, keep_row))



design <- design %>% 
  mutate(range = case_when(
    powertrain == "Gas hybrid" ~ "300 miles on 1 tank",
    powertrain == "Conventional" ~ "300 miles on 1 tank",
    powertrain == "Plug-in hybrid" ~ "300 miles on 1 tank \n(first 40 miles electric)",
    powertrain == "Battery electric"  ~ paste0(design$range, " miles on full charge")
  ))

design_suv<-design %>% mutate(vehicle_type="suv")

design_combined<-rbind(design_car,design_suv)


# Save design
write_csv(design_combined, here('survey','data', 'choice_questions.csv'))


