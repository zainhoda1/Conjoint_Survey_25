source(here::here('code', 'setup.R'))

library(fixest)

# Load the estimated model
load(here("models", "mixed_model_1_car_low_panel.RData"))
load(here("models", "mixed_model_1_car_high_panel.RData"))
load(here("models", "mixed_model_1_suv_low_panel.RData"))
load(here("models", "mixed_model_1_suv_high_panel.RData"))


all_vehicles <- read_parquet(here("data", "vehicle_listing_prices.parquet"))

vehicle_list <- data.frame(read_csv(here('data', 'vehicle_pairs_2016_2024.csv'))) |>
  mutate(across(where(is.character), tolower))

all_vehicles1 <- full_join(
  all_vehicles,
  vehicle_list,
  by = c('make', 'model', 'powertrain')
) |>
  group_by(Pair_id) |>
  filter(!any(is.na(count) | count < 200)
         ,powertrain != 'cv'
        ) |>
  ungroup()



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

vehicle_wtp <- all_vehicles1 |>
  mutate(
    budget = case_when(
      vehicle_type == "car" & mean_price < 20000 ~ "low",
      vehicle_type == "car" & mean_price > 20000 ~ "high",
      vehicle_type == "suv" & mean_price < 25000 ~ "low",
      vehicle_type == "suv" & mean_price > 25000 ~ "high"
    ),
    model_name = case_when(
      vehicle_type == "car" & budget == "low"  ~ "mixed_model_1_car_low_panel",
      vehicle_type == "car" & budget == "high" ~ "mixed_model_1_car_high_panel",
      vehicle_type == "suv" & budget == "low"  ~ "mixed_model_1_suv_low_panel",
      vehicle_type == "suv" & budget == "high" ~ "mixed_model_1_suv_high_panel"
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
    powertrainbev_dummy = if_else(powertrain == "bev", 1, 0),
    powertrainhev_dummy = if_else(powertrain == "hev", 1, 0),
    range_bev_scaled     = (bev_range * (1 - depreciation_rate) ^ fixed_age),
    mileage_scaled       = mean_mileage,
    operating_cost_scaled = case_when(
      powertrain == "bev" ~ 0.3,
      powertrain == "hev" ~ 0.6,
      .default = 1.2
    ),

    # Per-attribute dollar WTP, broken out separately
    wtp_powertrain = 1000 * (
      coef_powertrainbev * powertrainbev_dummy +
      coef_powertrainhev * powertrainhev_dummy
    ),
    wtp_range           = 1000 * coef_range_bev * range_bev_scaled,
    wtp_mileage         = 1000 * coef_mileage * mileage_scaled,
    wtp_operating_cost  = 1000 * coef_operating_cost * operating_cost_scaled,

    wtp_dollars = wtp_powertrain + wtp_range + wtp_mileage + wtp_operating_cost
  )

vehicle_wtp |>
  select(
    make, model, powertrain, vehicle_type, budget, mean_price,
    wtp_powertrain, wtp_range, wtp_mileage, wtp_operating_cost, wtp_dollars
  )


#############################

# Waterfall chart for all vehicles (faceted), using their per-attribute WTP columns
# (wtp_powertrain, wtp_range, wtp_mileage, wtp_operating_cost -- wtp_dollars excluded)

ink_primary   <- "#0b0b0b"
ink_secondary <- "#52514e"
ink_muted     <- "#898781"
grid_hairline <- "#e1e0d9"
baseline_ink  <- "#c3c2b7"
chart_surface <- "#fcfcfb"
strip_surface <- "#f2f1ee"

waterfall_all <- vehicle_wtp |>
  mutate(vehicle_label = paste0(str_to_title(make), " ", str_to_title(model))) |>
  select(vehicle_label, wtp_powertrain, wtp_range, wtp_mileage, wtp_operating_cost) |>
  pivot_longer(
    cols = c(wtp_powertrain, wtp_range, wtp_mileage, wtp_operating_cost),
    names_to = "attribute", values_to = "wtp"
  ) |>
  mutate(
    attribute = recode(attribute,
      wtp_powertrain     = "Powertrain",
      wtp_range          = "Range",
      wtp_mileage        = "Mileage",
      wtp_operating_cost = "Operating cost"
    ),
    attribute = factor(attribute, levels = c("Powertrain", "Range", "Mileage", "Operating cost"))
  ) |>
  arrange(vehicle_label, attribute) |>
  group_by(vehicle_label) |>
  mutate(
    cum_end    = cumsum(wtp),
    cum_start  = cum_end - wtp,
    bar_bottom = pmin(cum_start, cum_end),
    bar_top    = pmax(cum_start, cum_end),
    bar_type   = if_else(wtp >= 0, "Positive", "Negative")
  ) |>
  ungroup()

bar_type_colors <- c(
  "Positive" = "#009E73",  # bright green
  "Negative" = "#d1495b"   # red
)

waterfall_all_plot <- waterfall_all |>
  ggplot(aes(x = attribute)) +

  geom_hline(yintercept = 0, linetype = "dashed", color = baseline_ink, linewidth = 0.4) +

  geom_rect(
    aes(xmin = as.numeric(attribute) - 0.35,
        xmax = as.numeric(attribute) + 0.35,
        ymin = bar_bottom, ymax = bar_top,
        fill = bar_type)
  ) +

  scale_fill_manual(values = bar_type_colors, name = NULL) +

  scale_y_continuous(
    labels = scales::dollar_format(scale = 1/1000, suffix = "K"),
    breaks = scales::breaks_pretty(n = 5)
  ) +

  facet_wrap(~ vehicle_label, scales = "free_y", ncol = 3) +

  labs(
    title    = "Willingness to Pay Breakdown by Attribute",
    subtitle = "Dollar WTP contribution by attribute, relative to a conventional vehicle",
    x = NULL,
    y = "WTP ($ thousands)"
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = chart_surface, color = NA),
    panel.background  = element_rect(fill = chart_surface, color = NA),
    legend.background = element_rect(fill = chart_surface, color = NA),

    plot.title    = element_text(face = "bold", size = 14, color = ink_primary,
                                  margin = margin(b = 3)),
    plot.subtitle = element_text(size = 10.5, color = ink_secondary,
                                  margin = margin(b = 10)),
    plot.margin   = margin(12, 14, 10, 12),

    strip.text       = element_text(face = "bold", size = 10.5, color = ink_primary),
    strip.background = element_rect(fill = strip_surface, color = NA),

    axis.title  = element_text(size = 10.5, color = ink_secondary),
    axis.text.y = element_text(size = 9, color = ink_muted),
    axis.text.x = element_text(size = 9, color = ink_muted, angle = 30, hjust = 1),
    axis.ticks  = element_line(color = baseline_ink, linewidth = 0.3),
    axis.line   = element_blank(),

    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = grid_hairline, linewidth = 0.35),
    panel.grid.minor    = element_blank(),
    panel.spacing        = unit(1.2, "lines"),
    panel.border         = element_blank(),

    legend.position  = "bottom",
    legend.text      = element_text(size = 9.5, color = ink_secondary),
    legend.key       = element_rect(fill = chart_surface, color = NA)
  )

waterfall_all_plot

ggsave(
  filename = here::here(
    'code', 'output', 'images', 'vehicle_analysis', 'waterfall_wtp_all_vehicles.png'
  ),
  plot = waterfall_all_plot,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)

