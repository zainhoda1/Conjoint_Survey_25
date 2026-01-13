# After running both Dynata and Prolific
source(here::here('code', 'setup.R'))

# --------------------------------------------------------------------------
# Load the data set:

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
