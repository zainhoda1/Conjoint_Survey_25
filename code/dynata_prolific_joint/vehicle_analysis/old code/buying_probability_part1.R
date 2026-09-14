# This script gets the selected vehicles listing information

source(here::here('code', 'setup.R'))

listings <- open_dataset(
  'D:/Time Periods/Spring 2025/ev-affordability-2025/data/listings.parquet'
)

########### Insert new code here to look for vehicles

toyota_listings <- listings |>
  filter(make == 'toyota') |>
  select(model, make, powertrain) |>
  collect()

# RAV4 and Camry, matched loosely on model spelling (e.g. "rav4" vs "rav 4")
toyota_rav_camry <- toyota_listings |>
  filter(str_detect(model, regex('rav|camry', ignore_case = TRUE)))

# Distinct model strings actually present, to confirm how each is spelled
toyota_rav_camry |>
  distinct(model) |>
  as.data.frame() |>
  print()

# Counts by model, make, and powertrain (CV / Hybrid / BEV / PHEV variants)
toyota_rav_camry_counts <- toyota_rav_camry |>
  group_by(model, make, powertrain) |>
  summarise(counts = n(), .groups = 'drop') |>
  arrange(model, powertrain) |>
  as.data.frame()

print(toyota_rav_camry_counts)


#############


vehicle_list <- data.frame(
  model = c(
    "versa sedan", "leaf",
    "cooper", "hardtop 2 door",
    "kona", "kona ev",
    "niro", "niro", "niro",
    "fusion", "fusion energi",
    "camry", "camry",
    "rav4", "rav4", "rav4"
  ),
  make = c(
    "nissan", "nissan",
    "mini", "mini",
    "hyundai", "hyundai",
    "kia", "kia", "kia",
    "ford", "ford",
    "toyota", "toyota",
    "toyota", "toyota", "toyota"
  ),
  powertrain = c(
    "cv", "bev",
    "cv", "bev",
    "cv", "bev",
    "bev", "phev", "hev",
    "cv", "phev",
    "cv", "hev",
    "cv", "hev", "bev"
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
  'D:/Time Periods/Spring 2025/ev-affordability-2025/data/listings.parquet'
) |> 
  filter (year > 2016  & year < 2025) |> 
  filter(model == current_vehicle$model, 
    make == current_vehicle$make , 
    powertrain == current_vehicle$powertrain,
    inventory_type == 'used') |> 
  select( 
     year,
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
  #facet_grid(model~powertrain)

