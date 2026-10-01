source(here::here('code', 'setup.R'))

library(ggrepel)
library(cowplot)

# Load the estimated model



# Load the estimated model

load(here("models", "mixed_model_1_car_low_panel.RData"))
load(here("models", "mixed_model_1_car_high_panel.RData"))
load(here("models", "mixed_model_1_suv_low_panel.RData"))
load(here("models", "mixed_model_1_suv_high_panel.RData"))



depreciation_rate = 0.05  # Annual percentage depreciation for EVs

# Load Data :


vehicles_comparsion_list <- read_csv(here('data', 'vehicle_pairs_2016_2024.csv')) |>
  mutate(across(where(is.character), tolower))

vehicles_comparsion_list$id <- paste0(vehicles_comparsion_list$make,'_',
  vehicles_comparsion_list$model,'_',
  vehicles_comparsion_list$powertrain, '_',
  vehicles_comparsion_list$vehicle_type
)

vehicles_comparsion_list <- vehicles_comparsion_list |>
  select (id, Pair_id, bev_range)


predicted_car_prices <- read_parquet(here('data', 'predicted_prices.parquet' )) |>
  inner_join(vehicles_comparsion_list, by = c('id'))


####################  Add code here

predicted_car_prices_pairs <- predicted_car_prices |>
  inner_join(
    predicted_car_prices,
    by = c("Pair_id", "age_years"),
    suffix = c("_1", "_2"),
    relationship = "many-to-many"
  ) |>
  filter(str_detect(id_1, "cv"), !str_detect(id_2, "cv")) |>
  filter(age_years >1) |>
  mutate(
    vehicle_type = if_else(str_detect(id_1, "car"), "car", "suv")
  ) |>
  group_by(Pair_id) |>
  mutate(
    price_at_2 = if (any(age_years == 2)) predicted_price_1[age_years == 2] else NA_real_,
    budget = case_when(
      is.na(price_at_2) ~ NA_character_,
      vehicle_type == "car" & price_at_2 < 20000 ~ "low",
      vehicle_type == "car" & price_at_2 > 20000 ~ "high",
      vehicle_type == "suv" & price_at_2 < 25000 ~ "low",
      vehicle_type == "suv" & price_at_2 > 25000 ~ "high"
    )
  ) |>
  ungroup() |>
  select(-price_at_2) |>
  mutate(
    powertrain_2 = case_when(
      str_detect(id_2, "bev") ~ "bev",
      str_detect(id_2, "hev") ~ "hev"
    )
  ) |>
  filter(!Pair_id %in% c(8, 9, 7, 5))



##################

# Data for the depreciation curves plot (part4)

depreciation_curves_data <- predicted_car_prices_pairs |>
  pivot_longer(
    cols = c(id_1, id_2, predicted_price_1, predicted_price_2),
    names_to = c(".value", "side"),
    names_pattern = "(.*)_(\\d)"
  ) |>
  mutate(
    powertrain_label = case_when(
      str_detect(id, "bev") ~ "BEV",
      str_detect(id, "hev") ~ "HEV",
      str_detect(id, "cv")  ~ "Conventional (CV)"
    ),
    powertrain_label = factor(powertrain_label, levels = c("BEV", "HEV", "Conventional (CV)")),
    budget_label = recode(budget, "low" = "Low Budget", "high" = "High Budget"),
    budget_label = factor(budget_label, levels = c("Low Budget", "High Budget")),
    vehicle_category_label = str_to_title(vehicle_type),
    linetype_label = if_else(powertrain_2 == "hev", "dashed", "solid"),
    vehicle_label = id |>
      str_remove("_(bev|hev|cv|phev)_(car|suv)$") |>
      str_replace_all("_", " ") |>
      str_to_title()
  )

write_parquet( depreciation_curves_data , here('data', 'depreciation_curves_data.parquet'))

vehicle_label_points <- depreciation_curves_data |>
  group_by(id) |>
  filter(age_years == max(age_years)) |>
  ungroup()

write_parquet( vehicle_label_points , here('data', 'vehicle_label_points.parquet'))

# BEV/HEV choice probability data, built from predicted_car_prices_pairs

bev_probability_inputs <- predicted_car_prices_pairs |>
  mutate(
    obsID = row_number(),
    model_name = case_when(
      vehicle_type == "car" & budget == "low"  ~ "mixed_model_1_car_low_panel",
      vehicle_type == "car" & budget == "high" ~ "mixed_model_1_car_high_panel",
      vehicle_type == "suv" & budget == "low"  ~ "mixed_model_1_suv_low_panel",
      vehicle_type == "suv" & budget == "high" ~ "mixed_model_1_suv_high_panel"
    )
  ) |>
  filter(!is.na(model_name)) |>
  pivot_longer(
    cols = c(id_1, id_2, predicted_price_1, predicted_price_2,
             miles_1, miles_2, bev_range_1, bev_range_2),
    names_to = c(".value", "side"),
    names_pattern = "(.*)_(\\d)"
  ) |>
  mutate(
    altID = as.integer(side),
    powertrainbev = if_else(str_detect(id, "bev"), 1, 0),
    powertrainhev = if_else(str_detect(id, "hev"), 1, 0),
    operating_cost = case_when(
      str_detect(id, "bev") ~ 0.3,
      str_detect(id, "hev") ~ 0.6,
      .default = 1.2
    ),
    range_bev = (bev_range * (1 - depreciation_rate) ^ age_years) / 100,
    mileage = miles / 10000,
    price = predicted_price / 10000,
    age = age_years,
    no_choice = 0
  )

bev_probabilities <- bev_probability_inputs |>
  group_by(model_name) |>
  group_split() |>
  purrr::map_dfr(function(model_data) {
    predict(
      get(unique(model_data$model_name)),
      newdata = model_data,
      obsID = "obsID",
      interval = "confidence",
      level = 0.95,
      numDrawsCI = 10000,
      returnData = TRUE
    )
  })

bev_probability_data <- bev_probabilities |>
  filter(altID == 2) |>
  mutate(
    powertrain_label = if_else(powertrainbev == 1, "BEV", "HEV"),
    vehicle_category_label = str_to_title(vehicle_type),
    vehicle_label = id |>
      str_remove("_(bev|hev|cv|phev)_(car|suv)$") |>
      str_replace_all("_", " ") |>
      str_to_title(),
    comparisons = paste0(vehicle_label, " ", powertrain_label, " // CV")
  ) |>
  mutate(
    comparisons = factor(comparisons, levels = unique(comparisons[order(Pair_id)]))
  )


write_parquet( bev_probability_data , here('data', 'bev_probability_data.parquet'))
