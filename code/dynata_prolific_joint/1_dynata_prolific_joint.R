# After running both Dynata and Prolific
source(here::here('code', 'setup.R'))

# --- DCE data set----

data_prolific_vehicle <- read_parquet(here(
  "data",
  "prolific_testing",
  "choice_data_vehicle.parquet"
)) %>%
  mutate(data_source = "prolific")

data_prolific_battery <- read_parquet(here(
  "data",
  "prolific_testing",
  "choice_data_battery.parquet"
)) %>%
  mutate(data_source = "prolific")

data_dynata_vehicle <- read_parquet(here(
  "data",
  "dynata_testing",
  "choice_data_vehicle.parquet"
)) %>%
  mutate(data_source = "dynata")

data_dynata_battery <- read_parquet(here(
  "data",
  "dynata_testing",
  "choice_data_battery.parquet"
)) %>%
  mutate(data_source = "dynata")

data_prolific_vehicle_round2 <- read_parquet(here(
  "data",
  "prolific_testing",
  "choice_data_vehicle_round2_feb_26.parquet"
)) %>%
  mutate(data_source = "prolific_round2")

data_prolific_battery_round2 <- read_parquet(here(
  "data",
  "prolific_testing",
  "choice_data_battery_round2_feb_26.parquet"
)) %>%
  mutate(data_source = "prolific_round2")

data_dynata_vehicle$respID <- data_dynata_vehicle$respID +
  nrow(data_prolific_vehicle) / 24
data_dynata_vehicle$obsID <- data_dynata_vehicle$obsID +
  nrow(data_prolific_vehicle) / 4
data_dynata_battery$respID <- data_dynata_battery$respID +
  nrow(data_prolific_battery) / 24
data_dynata_battery$obsID <- data_dynata_battery$obsID +
  nrow(data_prolific_battery) / 4

complete_first_run_vehicle <- nrow(data_prolific_vehicle) +nrow(data_dynata_vehicle)
complete_first_run_battery <- nrow(data_prolific_battery) +nrow(data_dynata_battery)

data_prolific_vehicle_round2$respID <- data_prolific_vehicle_round2$respID +
  complete_first_run_vehicle / 24
data_prolific_vehicle_round2$obsID <- data_prolific_vehicle_round2$obsID +
  complete_first_run_vehicle / 4
data_prolific_battery_round2$respID <- data_prolific_battery_round2$respID +
  complete_first_run_battery / 24
data_prolific_battery_round2$obsID <- data_prolific_battery_round2$obsID +
  complete_first_run_battery / 4

data_joint_vehicle <- rbind(rbind(data_prolific_vehicle, data_dynata_vehicle), data_prolific_vehicle_round2)
data_joint_battery <- rbind(rbind(data_prolific_battery, data_dynata_battery), data_prolific_battery_round2) 

write_parquet(
  data_joint_vehicle,
  here(
    "data",
    "dynata_prolific_joint",
    "data_joint_vehicle.parquet"
  )
)

write_parquet(
  data_joint_battery,
  here(
    "data",
    "dynata_prolific_joint",
    "data_joint_battery.parquet"
  )
)


# --- DCE data set----
psid_prolific <- unique(c(
  data_joint_battery$psid[data_joint_battery$data_source %in% c( 'prolific', 'prolific_round2')],
  data_joint_vehicle$psid[data_joint_vehicle$data_source %in% c( 'prolific', 'prolific_round2')]
))

psid_dynata <- unique(c(
  data_joint_battery$psid[data_joint_battery$data_source == "dynata"],
  data_joint_vehicle$psid[data_joint_vehicle$data_source == "dynata"]
))


#---- FULL DATASET (filtered using DCE data)----

data_prolific <- read_parquet(here(
  "data",
  "prolific_testing",
  "data.parquet"
))  %>%
  mutate(data_source = "prolific") 

  
data_prolific_round2 <- read_parquet(here(
  "data",
  "prolific_testing",
  "data_round2_feb26.parquet"
)) %>%
  mutate(data_source = "prolific_round2") 


#####
#Duplicate check 
test<- inner_join(data_prolific, data_prolific_round2, by= c('prolific_pid', 'respID'))
nrow(test)

#######

data_prolific_both <- rbind(data_prolific, data_prolific_round2) %>% 
  mutate(
    psid = prolific_pid,
    birth_year = as.numeric(birth_year)
  ) %>%
  #select(-prolific_pid) %>% 
  filter(psid %in% psid_prolific)

  


data_dynata <- read_parquet(here(
  "data",
  "dynata_testing",
  "data.parquet"
)) %>%
  mutate(data_source = "dynata") %>%
  filter(psid %in% psid_dynata)

# setdiff(names(data_prolific), names(data_dynata))
# setdiff(names(data_dynata), names(data_prolific))
data_joint <- bind_rows(data_prolific_both, data_dynata) %>%
  select(
    !c(
      attention_check_toyota,
      attitudes_1_a,
      attitudes_1_b,
      attitudes_2_a,
      attitudes_2_b,
      battery_attribute,
      next_veh_fuel
    )
  )

write_parquet(
  data_joint,
  here(
    "data",
    "dynata_prolific_joint",
    "data_joint.parquet"
  )
)

# write_csv(
#   data_joint,
#   here(
#     "data",
#     "dynata_prolific_joint",
#     "data_joint.csv"
#   )
# )
