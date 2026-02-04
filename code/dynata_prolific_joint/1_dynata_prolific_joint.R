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

data_dynata_vehicle$respID <- data_dynata_vehicle$respID +
  nrow(data_prolific_vehicle) / 24
data_dynata_vehicle$obsID <- data_dynata_vehicle$obsID +
  nrow(data_prolific_vehicle) / 4
data_dynata_battery$respID <- data_dynata_battery$respID +
  nrow(data_prolific_battery) / 24
data_dynata_battery$obsID <- data_dynata_battery$obsID +
  nrow(data_prolific_battery) / 4


data_joint_vehicle <- rbind(data_prolific_vehicle, data_dynata_vehicle)
data_joint_battery <- rbind(data_prolific_battery, data_dynata_battery)

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
  data_joint_battery$psid[data_joint_battery$data_source == "prolific"],
  data_joint_vehicle$psid[data_joint_vehicle$data_source == "prolific"]
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
)) %>%
  mutate(
    psid = prolific_pid,
    data_source = "prolific",
    birth_year = as.numeric(birth_year)
  ) %>%
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
data_joint <- bind_rows(data_prolific, data_dynata) %>%
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

write_csv(
  data_joint,
  here(
    "data",
    "dynata_prolific_joint",
    "data_joint.csv"
  )
)
