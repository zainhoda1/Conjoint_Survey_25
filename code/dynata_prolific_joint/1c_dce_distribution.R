source(here::here('code', 'setup.R'))

# ----Data----
## ----Load dataset----

data_dce <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_vehicle.parquet"
)) %>%
  mutate(respID_qID = paste0(respID, "_", qID))

## Check distribution
### total count by powertrain
powertrain_only <- data_dce %>%
  filter(!is.na(price)) %>%
  group_by(powertrain) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = n / sum(n))

### individual level by powertrain

powertrain_individual <- data_dce %>%
  filter(!is.na(price)) %>%
  group_by(respID, powertrain) %>%
  count() %>%
  pivot_wider(names_from = "powertrain", values_from = n)

powertrain_count <- powertrain_individual %>%
  group_by(bev) %>%
  count()

### chosen by powertrain

powertrain_chosen <- data_dce %>%
  filter(!is.na(price)) %>%
  filter(
    # powertrain=="gas"
    # powertrain=="hev"
    # powertrain=="phev"
    powertrain == "bev" & choice == 1
  )
