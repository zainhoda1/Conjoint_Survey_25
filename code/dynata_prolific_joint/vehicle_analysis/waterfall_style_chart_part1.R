
# This script gets the selected vehicles listing information

source(here::here('code', 'setup.R'))

listings <- open_dataset(
  'D:/Time Periods/Spring 2025/ev-affordability-2025/data/listings.parquet'
)


vehicle_list <- data.frame(read_csv(
  here('data', 'vehicle_pairs_2016_2024.csv')
) ) |> 
  select (make, model, powertrain) %>% 
  mutate(across(where(is.character), tolower))


vehicles_data <- data.frame(
  make = character(),
  powertrain = character(),
  mean_price = numeric(),
  count = integer()
)

for (i in 1:nrow(vehicle_list))
  {
  current_vehicle = vehicle_list[i,]
  print(current_vehicle)

  current_vehicle_details <- open_dataset(
  'D:/Time Periods/Spring 2025/ev-affordability-2025/data/listings.parquet'
) |>
  filter (year > 2016  & year < 2025) |>
  filter(model == current_vehicle$model,
    make == current_vehicle$make ,
    powertrain == current_vehicle$powertrain,
    inventory_type == 'used',
    age_years < 3.5,
    age_years > 2.5
  ) |>
  group_by(make, model, powertrain) |>
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    mean_mileage = mean(miles, na.rm = TRUE),
    count = n(),
    .groups = 'drop'
  ) |>
  collect()
  
  vehicles_data <- rbind(vehicles_data, current_vehicle_details)
  
}

write_parquet( vehicles_data,here('data', 'vehicle_listing_prices.parquet'))


