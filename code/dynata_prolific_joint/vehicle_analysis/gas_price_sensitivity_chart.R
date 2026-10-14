source(here::here('code', 'setup.R'))

# Sensitivity of Net WTP (alternative powertrain vs. its conventional
# counterpart) to the price of gasoline, faceted the same way as
# waterfall_style_chart_part3.R (budget x vehicle type).
#
# Everything except the operating-cost term is held fixed at the values used
# in waterfall_style_chart_part2.R; only the operating-cost differential is
# recomputed at each gas price:
#   CV:  cents/mile = 100 * price_gas / comb_mpg
#   HEV: cents/mile = 100 * price_gas / comb_mpg
#   BEV: cents/mile = kwh_per_100mi * price_elec   (no gas-price dependence)
# so for BEVs only the conventional side moves, while for HEVs both sides do.

load(here("models", "mixed_model_1_car_low_panel.RData"))
load(here("models", "mixed_model_1_car_high_panel.RData"))
load(here("models", "mixed_model_1_suv_low_panel.RData"))
load(here("models", "mixed_model_1_suv_high_panel.RData"))

price_elec <- 0.17 # dollars / kWh
price_gas_base <- 3.18 # dollars / gallon (baseline used in part2 / part3)
gas_prices <- seq(2.50, 5.00, by = 0.05)

vehicle_wtp <- read_parquet(here("data", "vehicle_wtp.parquet"))

wtp_coef_lookup <- list(
  mixed_model_1_car_low_panel = coef(mixed_model_1_car_low_panel),
  mixed_model_1_car_high_panel = coef(mixed_model_1_car_high_panel),
  mixed_model_1_suv_low_panel = coef(mixed_model_1_suv_low_panel),
  mixed_model_1_suv_high_panel = coef(mixed_model_1_suv_high_panel)
)

wtp_vcov_lookup <- list(
  mixed_model_1_car_low_panel = vcov(mixed_model_1_car_low_panel),
  mixed_model_1_car_high_panel = vcov(mixed_model_1_car_high_panel),
  mixed_model_1_suv_low_panel = vcov(mixed_model_1_suv_low_panel),
  mixed_model_1_suv_high_panel = vcov(mixed_model_1_suv_high_panel)
)

# --- Coefficient draws for the Net WTP interval ---
# One set of draws per model, reused across gas prices, so the ribbon is a
# smooth band rather than a jittery one re-simulated at every x value.

n_draws <- 10000
set.seed(123)

coef_draws <- lapply(names(wtp_coef_lookup), function(m) {
  MASS::mvrnorm(
    n_draws,
    mu = wtp_coef_lookup[[m]],
    Sigma = wtp_vcov_lookup[[m]]
  )
})
names(coef_draws) <- names(wtp_coef_lookup)

simulate_net_wtp <- function(
  model_name,
  bev_dummy,
  hev_dummy,
  range_scaled,
  opcost_scaled
) {
  draws <- coef_draws[[model_name]]

  net_draws <- 1000 *
    (draws[, "powertrainbev"] *
      bev_dummy +
      draws[, "powertrainhev"] * hev_dummy +
      draws[, "range_bev"] * range_scaled +
      draws[, "operating_cost"] * opcost_scaled)

  tibble(
    net_mean = mean(net_draws),
    net_lower = unname(quantile(net_draws, 0.025)),
    net_upper = unname(quantile(net_draws, 0.975))
  )
}

# --- Net WTP across the gas-price grid ---
# ".x" columns are the conventional vehicle, ".y" the alternative powertrain

sensitivity_wtp <- vehicle_wtp |>
  select(
    make,
    model,
    powertrain,
    vehicle_type,
    budget,
    model_name,
    comb_mpg_or_mpge.x,
    comb_mpg_or_mpge.y,
    kwh_per_100mi.y,
    powertrainbev_dummy,
    powertrainhev_dummy,
    range_bev_scaled,
    mean_price_diff
  ) |>
  expand_grid(price_gas = gas_prices) |>
  mutate(
    operating_cost_cpm_cv = 100 * price_gas / comb_mpg_or_mpge.x,
    operating_cost_cpm_alt = if_else(
      powertrain == "BEV",
      kwh_per_100mi.y * price_elec,
      100 * price_gas / comb_mpg_or_mpge.y
    ),
    operating_cost_scaled = operating_cost_cpm_alt - operating_cost_cpm_cv,
    vehicle_type_label = if_else(vehicle_type == "CAR", "Car", "SUV"),
    budget_label = paste0(str_to_title(budget), " Budget")
  ) |>
  rowwise() |>
  mutate(
    net_draws = list(simulate_net_wtp(
      model_name,
      powertrainbev_dummy,
      powertrainhev_dummy,
      range_bev_scaled,
      operating_cost_scaled
    ))
  ) |>
  unnest(net_draws) |>
  ungroup()

#############################

ink_primary <- "#0b0b0b"
ink_secondary <- "#52514e"
grid_hairline <- "#e1e0d9"
baseline_ink <- "#c3c2b7"
chart_surface <- "#fcfcfb"
strip_surface <- "#f2f1ee"

net_color <- "#1f5fa8" # blue
price_diff_color <- "#CC79A7" # reddish purple (colour-blind-safe)

# One mean price premium per facet, plus label placement that keeps the text
# inside the panel and off the zero line
make_premium_ref <- function(sens_data) {
  sens_data |>
    group_by(vehicle_type_label, budget_label) |>
    summarize(
      mean_price_diff = first(mean_price_diff),
      y_min = min(c(net_lower, 0)),
      y_max = max(c(net_upper, 0)),
      .groups = "drop"
    ) |>
    mutate(
      price_diff_label = paste0(
        "Mean price premium: ",
        scales::dollar(mean_price_diff, accuracy = 1)
      ),
      # below the line when it would otherwise run off the top of the panel,
      # or when the premium is ~0 and the label would sit on the zero line
      lab_v = if_else(
        mean_price_diff > y_max - 0.1 * (y_max - y_min) |
          abs(mean_price_diff) < 1000,
        1.3,
        -0.4
      )
    )
}

build_sensitivity_plot <- function(sens_data) {
  premium_ref <- make_premium_ref(sens_data)

  ggplot(sens_data, aes(x = price_gas, y = net_mean)) +

    geom_hline(
      yintercept = 0,
      linetype = "solid",
      color = ink_primary,
      linewidth = 0.6
    ) +

    geom_hline(
      data = premium_ref,
      aes(yintercept = mean_price_diff),
      color = price_diff_color,
      linewidth = 0.6,
      linetype = "dashed"
    ) +

    geom_ribbon(
      aes(ymin = net_lower, ymax = net_upper),
      fill = net_color,
      alpha = 0.18
    ) +

    geom_line(color = net_color, linewidth = 0.8) +

    geom_label(
      data = premium_ref,
      aes(
        x = -Inf,
        y = mean_price_diff,
        label = price_diff_label,
        vjust = lab_v
      ),
      inherit.aes = FALSE,
      hjust = -0.02,
      color = ink_primary,
      fill = chart_surface,
      size = 3.4,
      label.size = 0,
      alpha = 0.9
    ) +

    scale_x_continuous(
      labels = scales::dollar_format(accuracy = 0.01),
      breaks = seq(2.5, 5, 0.5),
      expand = expansion(mult = c(0.02, 0.02))
    ) +

    scale_y_continuous(
      labels = scales::dollar_format(scale = 1 / 1000, suffix = "K"),
      breaks = scales::breaks_pretty(n = 5)
    ) +

    facet_grid(budget_label ~ vehicle_type_label) +

    labs(
      title = NULL, # title/description belong in the manuscript caption (TRD)
      subtitle = NULL,
      x = "Gasoline price ($/gallon)",
      y = "Net WTP ($)"
    ) +

    theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = chart_surface, color = NA),
      panel.background = element_rect(fill = chart_surface, color = NA),
      legend.background = element_rect(fill = chart_surface, color = NA),

      plot.margin = margin(12, 14, 10, 12),

      strip.text = element_text(face = "bold", size = 13, color = ink_primary),
      strip.background = element_rect(fill = strip_surface, color = NA),

      axis.title = element_text(size = 14, color = ink_secondary),
      axis.text.y = element_text(size = 13, color = ink_primary),
      # smaller + extra panel spacing so the $2.50 / $5.00 labels on either
      # side of the facet seam do not collide
      axis.text.x = element_text(size = 11, color = ink_primary),
      axis.ticks = element_line(color = baseline_ink, linewidth = 0.3),
      axis.line = element_blank(),

      panel.grid.major = element_line(color = grid_hairline, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      panel.spacing.y = unit(1.2, "lines"),
      panel.spacing.x = unit(1.8, "lines"),
      panel.border = element_rect(
        color = baseline_ink,
        fill = NA,
        linewidth = 0.5
      ),

      legend.position = "none"
    )
}

save_sensitivity_plot <- function(plot, file_name) {
  ggsave(
    filename = here::here(
      'code',
      'output',
      'images',
      'vehicle_analysis',
      file_name
    ),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  ggsave(
    filename = here::here(
      'paper_writing',
      'vehicle_paper',
      'images',
      'vehicle_analysis',
      file_name
    ),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}

# --- BEV plot ---

sensitivity_bev_plot <- build_sensitivity_plot(
  sensitivity_wtp |> filter(powertrain == "BEV")
)

sensitivity_bev_plot

save_sensitivity_plot(sensitivity_bev_plot, "gas_price_sensitivity_bev.png")

# --- HEV plot ---

sensitivity_hev_plot <- build_sensitivity_plot(
  sensitivity_wtp |> filter(powertrain == "HEV")
)

sensitivity_hev_plot

save_sensitivity_plot(sensitivity_hev_plot, "gas_price_sensitivity_hev.png")
