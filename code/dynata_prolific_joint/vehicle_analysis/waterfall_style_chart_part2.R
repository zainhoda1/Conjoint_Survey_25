source(here::here('code', 'setup.R'))

library(fixest)

# Load the estimated model
load(here("models", "mixed_model_1_car_low_panel.RData"))
load(here("models", "mixed_model_1_car_high_panel.RData"))
load(here("models", "mixed_model_1_suv_low_panel.RData"))
load(here("models", "mixed_model_1_suv_high_panel.RData"))


all_vehicles <- read_parquet(here("data", "vehicle_listing_prices.parquet")) |> 
   mutate(across(where(is.character), toupper)) 

vehicle_list <- data.frame(read_csv(here('data', 'vehicle_pairs_2016_2024.csv'))) |>
 mutate(across(where(is.character), toupper)) |> 
  filter(Pair_id != 7) |> 
  select(-model_years.US.) 

#######
# Add code for self join here

vehicle_list1 <- vehicle_list |>
  filter(powertrain == 'CV') |> 
  select(-bev_range, -vehicle_type, -make) |> 
  left_join(
    vehicle_list |> 
      filter(powertrain != 'CV'),
    by = "Pair_id"
  ) |> 
  mutate (
    name = paste0(make , ' ', model.y, ' (', 
      powertrain.y , ') vs ', model.x, ' (', powertrain.x, ')'),
    powertrain = powertrain.y,
    model = model.y
    ) |> 
  select(make, model, powertrain, bev_range, name, vehicle_type)

######


all_vehicles1 <- right_join(
  all_vehicles,
  vehicle_list1,
  by = c('make', 'model', 'powertrain')
) |>
  filter( count > 500)





# --- Willingness to pay for each vehicle in all_vehicles1 ---

depreciation_rate <- 0.05  # Annual percentage depreciation for EVs
fixed_age <- 3             # all_vehicles1 has no age_years, so a fixed age is used

# NOTE: coefficients come out of the model in $1,000 WTP-space units, so the
# base conversion to dollars is x1000. That is confirmed against the paper
# text for powertrainbev/powertrainhev/range_bev/age. mileage and
# operating_cost may need an *additional* scale factor on top of that (see
# the open scaling question in willingness_to_pay_attributes.R) -- adjust
# these two multipliers once that's confirmed.


wtp_coef_lookup <- list(
  mixed_model_1_car_low_panel  = coef(mixed_model_1_car_low_panel),
  mixed_model_1_car_high_panel = coef(mixed_model_1_car_high_panel),
  mixed_model_1_suv_low_panel  = coef(mixed_model_1_suv_low_panel),
  mixed_model_1_suv_high_panel = coef(mixed_model_1_suv_high_panel)
)

wtp_vcov_lookup <- list(
  mixed_model_1_car_low_panel  = vcov(mixed_model_1_car_low_panel),
  mixed_model_1_car_high_panel = vcov(mixed_model_1_car_high_panel),
  mixed_model_1_suv_low_panel  = vcov(mixed_model_1_suv_low_panel),
  mixed_model_1_suv_high_panel = vcov(mixed_model_1_suv_high_panel)
)

vehicle_wtp <- all_vehicles1 |>
  mutate(
    budget = case_when(
      vehicle_type == "CAR" & mean_price < 20000 ~ "LOW",
      vehicle_type == "CAR" & mean_price > 20000 ~ "HIGH",
      vehicle_type == "SUV" & mean_price < 25000 ~ "LOW",
      vehicle_type == "SUV" & mean_price > 25000 ~ "HIGH"
    ),
    model_name = case_when(
      vehicle_type == "CAR" & budget == "LOW"  ~ "mixed_model_1_car_low_panel",
      vehicle_type == "CAR" & budget == "HIGH" ~ "mixed_model_1_car_high_panel",
      vehicle_type == "SUV" & budget == "LOW"  ~ "mixed_model_1_suv_low_panel",
      vehicle_type == "SUV" & budget == "HIGH" ~ "mixed_model_1_suv_high_panel"
    )
  ) |>
  filter(!is.na(model_name)) |>
  rowwise() |>
  mutate(
    coef_powertrainbev  = wtp_coef_lookup[[model_name]][["powertrainbev"]],
    coef_powertrainhev  = wtp_coef_lookup[[model_name]][["powertrainhev"]],
    coef_range_bev      = wtp_coef_lookup[[model_name]][["range_bev"]],
    coef_mileage        = wtp_coef_lookup[[model_name]][["mileage"]],
    coef_operating_cost = wtp_coef_lookup[[model_name]][["operating_cost"]]
  ) |>
  ungroup() |>
  mutate(
    powertrainbev_dummy = if_else(powertrain == "BEV", 1, 0),
    powertrainhev_dummy = if_else(powertrain == "HEV", 1, 0),
    range_bev_scaled     = (bev_range * (1 - depreciation_rate) ^ fixed_age),
    mileage_scaled       = mean_mileage,
    operating_cost_scaled = case_when(
      powertrain == "BEV" ~ -0.9,  # 0.3 - 1.2
      powertrain == "HEV" ~ -0.6,  # 0.6 - 1.2
      .default = 1.2
    ),

    # Per-attribute dollar WTP, broken out separately
    wtp_powertrain = 1000 * (
      coef_powertrainbev * powertrainbev_dummy +
      coef_powertrainhev * powertrainhev_dummy
    ),
    wtp_range           = 1000 * coef_range_bev * range_bev_scaled,
    wtp_operating_cost  = 1000 * coef_operating_cost * operating_cost_scaled,

    wtp_dollars = wtp_powertrain + wtp_range + wtp_operating_cost
  )

# --- Simulate "Net" WTP (wtp_dollars) via coefficient draws ---
# Draws come from a multivariate normal centered on the model's point
# estimates with its estimated covariance, so the resulting Net dot/error
# bar reflects sampling uncertainty in the underlying coefficients.

n_draws <- 10000
set.seed(123)

simulate_net_wtp <- function(model_name, bev_dummy, hev_dummy,
                              range_scaled, opcost_scaled) {
  mu    <- wtp_coef_lookup[[model_name]]
  sigma <- wtp_vcov_lookup[[model_name]]
  draws <- MASS::mvrnorm(n_draws, mu = mu, Sigma = sigma)

  net_draws <- 1000 * (
    draws[, "powertrainbev"]  * bev_dummy +
    draws[, "powertrainhev"]  * hev_dummy +
    draws[, "range_bev"]      * range_scaled +
    draws[, "operating_cost"] * opcost_scaled
  )

  tibble(
    net_mean  = mean(net_draws),
    net_lower = unname(quantile(net_draws, 0.025)),
    net_upper = unname(quantile(net_draws, 0.975))
  )
}

vehicle_wtp <- vehicle_wtp |>
  rowwise() |>
  mutate(
    net_draws = list(simulate_net_wtp(
      model_name, powertrainbev_dummy, powertrainhev_dummy,
      range_bev_scaled, operating_cost_scaled
    ))
  ) |>
  unnest(net_draws) |>
  ungroup()

vehicle_wtp |>
  select(
    make, model, powertrain, vehicle_type, budget, mean_price,
    wtp_powertrain, wtp_range, wtp_operating_cost, wtp_dollars,
    net_mean, net_lower, net_upper
  )

write_parquet( vehicle_wtp, here('data', 'vehicle_wtp.parquet'))

