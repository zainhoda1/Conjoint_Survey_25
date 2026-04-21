source(here::here('code', 'setup.R'))

listings <- open_dataset(
  'D:/Spring 2025/ev-affordability-2025/data/listings.parquet'
)


vehicle_list <- data.frame(
  model = c(
    "versa sedan", "leaf",
    "cooper", "hardtop 2 door",
    "4 series", "i4",
    "ioniq", "ioniq",
    "kona", "kona ev",
    "niro", "niro", "niro",
    "fusion", "fusion energi"
  ),
  make = c(
    "nissan", "nissan",
    "mini", "mini",
    "bmw", "bmw",
    "hyundai", "hyundai",
    "hyundai", "hyundai",
    "kia", "kia", "kia",
    "ford", "ford"
  ),
  powertrain = c(
    "cv", "bev",
    "cv", "bev",
    "cv", "bev",
    "bev", "hev",
    "cv", "bev",
    "bev", "phev", "hev",
    "cv", "phev"
  ),
  stringsAsFactors = FALSE
)

vehicles_data <- data.frame(
  listing_year = integer(),
  model = character(),
  make = character(),
  inventory_type = character(),
  powertrain = character(),
  vehicle_type = character(),
  price = numeric(),
  range = numeric(),
  age_years = numeric(),
  miles = numeric()
)

for (i in 1:nrow(vehicle_list))
  {
  current_vehicle = vehicle_list[i,]
  print(current_vehicle)

  current_vehicle_details <- open_dataset(
  'D:/Spring 2025/ev-affordability-2025/data/listings.parquet'
) |> 
  filter (year > 2019  & year < 2025) |> 
  filter(model == current_vehicle$model, 
    make == current_vehicle$make , 
    powertrain == current_vehicle$powertrain,
    inventory_type == 'used') |> 
  select( 
     listing_year,
     model,
     make, 
     inventory_type, 
     powertrain, 
     vehicle_type, 
     price, 
     range, 
     age_years, 
     miles) |> 
  collect()
  
  vehicles_data <- rbind(vehicles_data, current_vehicle_details)
  
}

write_parquet( vehicles_data,here('data', 'vehicle_listing_prices.parquet'))


schema(listings)

cars <- listings |> 
  filter (year > 2019  & year < 2024) |> 
  select(model, make, powertrain) |>
  group_by(model, make, powertrain) |> 
  summarise(counts =n()) |> 
  distinct() |>
  collect()

cars


vehicle_list <- left_join(vehicle_list , cars, by = c('model', 'make', 'powertrain') )


vehicles_data |> 
  ggplot(aes(x = price)) +
  geom_histogram() +
  facet_wrap(~model)
  facet_grid(model~powertrain)

