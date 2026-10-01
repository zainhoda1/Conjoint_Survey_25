# This script reads the CV + BEV/Hybrid/PHEV nameplate pairs in
# data/vehicle_pairs_2016_2024.csv, matches each one against the listings
# dataset (model years 2016-2024), and summarizes counts and mean price by
# year for the vehicles with enough listings to be meaningful.

source(here::here('code', 'setup.R'))

listings <- open_dataset(
  'D:/Time Periods/Spring 2025/ev-affordability-2025/data/listings.parquet'
)

# to verify the correct names of vehicles in the listings dataset

all_make_models <- listings |>
  select(make, model, powertrain, inventory_type ) |>
  mutate(make = str_trim(str_to_upper(make)),
         model = str_trim(str_to_upper(model))) |>
  count(make, model, powertrain, inventory_type,   name = "n_vehicles") |>
  collect() |>
  arrange(desc(n_vehicles))


vehicle_pairs_raw <- read_csv(
  here('data', 'vehicle_pairs_2016_2024.csv'),
  show_col_types = FALSE
) |>
  clean_names()
