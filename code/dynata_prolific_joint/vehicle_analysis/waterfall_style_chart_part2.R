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
      powertrain == "BEV" ~ 0.3,
      powertrain == "HEV" ~ 0.6,
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


#############################

# Waterfall chart for all vehicles (faceted), using their per-attribute WTP columns
# (wtp_powertrain, wtp_range, wtp_operating_cost -- wtp_dollars excluded)

ink_primary   <- "#0b0b0b"
ink_secondary <- "#52514e"
ink_muted     <- "#898781"
grid_hairline <- "#e1e0d9"
baseline_ink  <- "#c3c2b7"
chart_surface <- "#fcfcfb"
strip_surface <- "#f2f1ee"

bar_type_colors <- c(
  "Positive" = "#009E73",  # bright green
  "Negative" = "#d1495b"   # red
)

net_color <- "#1f5fa8"  # blue

# attribute_levels includes "Net" even though it is plotted as a point/error
# bar (not a rect), so the bar data and the Net data share one discrete x scale
make_waterfall_bars <- function(pt, attribute_levels) {
  vehicle_wtp |>
    filter(powertrain == pt) |>
    mutate(
      vehicle_label = name,
      pair_label    = paste0(
        if_else(vehicle_type == "CAR", "Car", "SUV"), " ",
        str_to_title(budget), " Budget"
      )
    ) |>
    select(vehicle_label, pair_label,
           wtp_powertrain, wtp_range, wtp_operating_cost) |>
    pivot_longer(
      cols = c(wtp_powertrain, wtp_range, wtp_operating_cost),
      names_to = "attribute", values_to = "wtp"
    ) |>
    mutate(
      attribute = recode(attribute,
        wtp_powertrain     = "Powertrain",
        wtp_range          = "Range",
        wtp_operating_cost = "Operating cost"
      )
    ) |>
    filter(attribute %in% attribute_levels) |>
    mutate(attribute = factor(attribute, levels = attribute_levels)) |>
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
}

make_waterfall_net <- function(pt, attribute_levels) {
  vehicle_wtp |>
    filter(powertrain == pt) |>
    mutate(
      pair_label = paste0(
        if_else(vehicle_type == "CAR", "Car", "SUV"), " ",
        str_to_title(budget), " Budget"
      ),
      attribute = factor("Net", levels = attribute_levels)
    ) |>
    select(pair_label, attribute, net_mean, net_lower, net_upper)
}

# Dotted horizontal connectors linking each bar's running total to the next
# bar, and from the final bar's running total to the Net point
make_waterfall_connectors <- function(bars) {
  net_x <- nlevels(bars$attribute)

  between_bars <- bars |>
    arrange(vehicle_label, attribute) |>
    group_by(vehicle_label) |>
    mutate(
      x    = as.numeric(attribute) + 0.35,
      xend = lead(as.numeric(attribute)) - 0.35,
      y    = cum_end,
      yend = cum_end
    ) |>
    ungroup() |>
    filter(!is.na(xend)) |>
    select(pair_label, x, xend, y, yend)

  to_net <- bars |>
    arrange(vehicle_label, attribute) |>
    group_by(vehicle_label, pair_label) |>
    slice_tail(n = 1) |>
    ungroup() |>
    transmute(
      pair_label,
      x    = as.numeric(attribute) + 0.35,
      xend = net_x,
      y    = cum_end,
      yend = cum_end
    )

  bind_rows(between_bars, to_net)
}

build_waterfall_plot <- function(bars, net_data, title, subtitle) {
  connectors <- make_waterfall_connectors(bars)

  ggplot(bars, aes(x = attribute)) +

    geom_hline(yintercept = 0, linetype = "solid", color = ink_primary, linewidth = 0.6) +

    geom_rect(
      aes(xmin = as.numeric(attribute) - 0.35,
          xmax = as.numeric(attribute) + 0.35,
          ymin = bar_bottom, ymax = bar_top,
          fill = bar_type)
    ) +

    geom_segment(
      data = connectors,
      aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE, color = ink_primary, linewidth = 0.5,
      linetype = "dotted"
    ) +

    geom_errorbar(
      data = net_data,
      aes(x = attribute, ymin = net_lower, ymax = net_upper),
      inherit.aes = FALSE, width = 0.15, color = net_color, linewidth = 0.6
    ) +

    geom_point(
      data = net_data,
      aes(x = attribute, y = net_mean),
      inherit.aes = FALSE, color = net_color, size = 2.8
    ) +

    scale_fill_manual(values = bar_type_colors, name = NULL) +

    scale_y_continuous(
      labels = scales::dollar_format(scale = 1/1000, suffix = "K"),
      breaks = scales::breaks_pretty(n = 5)
    ) +

    facet_wrap(~ pair_label, nrow = 1) +

    labs(
      title    = stringr::str_wrap(title, width = 55),
      subtitle = subtitle,
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
}

save_waterfall_plot <- function(plot, file_name) {
  ggsave(
    filename = here::here('code', 'output', 'images', 'vehicle_analysis', file_name),
    plot = plot, width = 9, height = 5.5, dpi = 300, bg = "white"
  )
  ggsave(
    filename = here::here('paper_writing', 'vehicle_paper', 'images', 'vehicle_analysis', file_name),
    plot = plot, width = 9, height = 5.5, dpi = 300, bg = "white"
  )
}

# --- BEV plot: Powertrain, Range, Operating cost, Net ---

bev_attribute_levels <- c("Powertrain", "Range", "Operating cost", "Net")
waterfall_bev_bars <- make_waterfall_bars("BEV", bev_attribute_levels)
waterfall_bev_net  <- make_waterfall_net("BEV", bev_attribute_levels)

waterfall_bev_plot <- build_waterfall_plot(
  waterfall_bev_bars, waterfall_bev_net,
  title = "Head-to-head charts showing WTP for attributes of BEV's against their Conventional Counterparts",
  subtitle = "BEV vs. conventional -- dollar WTP contribution by attribute, with simulated Net WTP (mean, 95% interval)"
)

waterfall_bev_plot

save_waterfall_plot(waterfall_bev_plot, "waterfall_wtp_bev.png")

# --- HEV plot: Powertrain, Operating cost, Net (no Range) ---

hev_attribute_levels <- c("Powertrain", "Operating cost", "Net")
waterfall_hev_bars <- make_waterfall_bars("HEV", hev_attribute_levels)
waterfall_hev_net  <- make_waterfall_net("HEV", hev_attribute_levels)

waterfall_hev_plot <- build_waterfall_plot(
  waterfall_hev_bars, waterfall_hev_net,
  title = "Head-to-head charts showing WTP for attributes of HEV's against their Conventional Counterparts", 
  subtitle = "HEV vs. conventional -- dollar WTP contribution by attribute, with simulated Net WTP (mean, 95% interval)"
)

waterfall_hev_plot

save_waterfall_plot(waterfall_hev_plot, "waterfall_wtp_hev.png")


