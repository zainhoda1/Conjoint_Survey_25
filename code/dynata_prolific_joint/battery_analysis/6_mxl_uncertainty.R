source(here::here('code', 'setup.R'))

#Load data----
load(here(
  "code",
  "output",
  "model_output",
  "logitr",
  "battery",
  "mxl_wtp_model_car.RData"
))

load(here(
  "code",
  "output",
  "model_output",
  "logitr",
  "battery",
  "mxl_wtp_model_suv.RData"
))

# range, degradation----
coefs_car <- coef(wtp_model_car)
covariance_car <- vcov(wtp_model_car)
coef_draws_car <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))

coefs_suv <- coef(wtp_model_suv)
covariance_suv <- vcov(wtp_model_suv)
coef_draws_suv <- as.data.frame(MASS::mvrnorm(10^4, coefs_suv, covariance_suv))

# # For each coefficient, get the mean and 95% confidence interval:
# coef_ci <- ci(coef_draws, level = 0.95)
# coef_ci

# # Separate coefficient CIs by attribute
# coef_ci$par <- row.names(coef_ci)
# coef_range <- coef_ci %>% filter(par == 'battery_range_year0')

range_seq <- seq(0, 350, by = 10)
deg_vals <- c(1, 2, 3, 4)
grid <- expand.grid(range = range_seq, degradation = deg_vals)

# range, degradation----
wtp_summary_car <- map_dfr(seq_len(nrow(grid)), function(i) {
  r <- grid$range[i] / 100 # convert to model units
  d <- grid$degradation[i]
  vals <- ((coef_draws_car$battery_range_year0 *
    r +
    coef_draws_car$battery_degradation * d)) *
    10
  tibble(
    range = grid$range[i], # show actual range in plot
    degradation = d,
    mean = mean(vals, na.rm = TRUE),
    lower = quantile(vals, 0.025, na.rm = TRUE),
    upper = quantile(vals, 0.975, na.rm = TRUE)
  )
})

wtp_summary_suv <- map_dfr(seq_len(nrow(grid)), function(i) {
  r <- grid$range[i] / 100 # convert to model units
  d <- grid$degradation[i]
  vals <- ((coef_draws_suv$battery_range_year0 *
    r +
    coef_draws_suv$battery_degradation * d)) *
    10
  tibble(
    range = grid$range[i], # show actual range in plot
    degradation = d,
    mean = mean(vals, na.rm = TRUE),
    lower = quantile(vals, 0.025, na.rm = TRUE),
    upper = quantile(vals, 0.975, na.rm = TRUE)
  )
})

wtp_summary_degradation <- bind_rows(
  wtp_summary_car %>% mutate(vehicle_type = "Car"),
  wtp_summary_suv %>% mutate(vehicle_type = "SUV")
)
# uncertainty_combined_plot <- ggplot(
#   wtp_summary,
#   aes(
#     x = range,
#     y = mean,
#     color = factor(degradation),
#     fill = factor(degradation)
#   )
# ) +
#   xlim(100, 350) +
#   geom_line(linewidth = 1) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, color = NA) +
#   labs(
#     x = "Vehicle range at year 0 (miles)",
#     y = "WTP ($1000)",
#     color = "Degradation\n(% per year)",
#     fill = "Degradation\n(% per year)",
#     title = "Estimated WTP for Vehicle Range and Battery Degradation Rate\n(95% Uncertainty Intervals)"
#   ) +
#   theme_minimal()

# uncertainty_combined_plot

# ggsave(
#   plot = uncertainty_combined_plot,
#   filename = here::here(
#     'code',
#     'output',
#     "images",
#     "uncertainty_combined_plot.png"
#   ),
#   width = 6,
#   height = 4,
#   dpi = 300
# )

uncertainty_degradation_plot <- ggplot(
  wtp_summary_degradation,
  aes(
    x = range,
    y = mean,
    color = factor(degradation),
    fill = factor(degradation)
  )
) +
  facet_grid(vehicle_type ~ factor(degradation)) +
  xlim(100, 350) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, color = NA) +
  labs(
    x = "Vehicle range at year 0 (miles)",
    y = "WTP ($1000)",
    color = "Degradation\n(% per year)",
    fill = "Degradation\n(% per year)",
    title = "Estimated WTP for Vehicle Range and Battery Degradation Rate by Vehicle Type\n(95% Uncertainty Intervals)"
  ) +
  theme_minimal()

uncertainty_degradation_plot

ggsave(
  plot = uncertainty_degradation_plot,
  filename = here::here(
    'code',
    'output',
    "images",
    "uncertainty_degradation_plot.png"
  ),
  width = 9,
  height = 6,
  dpi = 300
)

# range, refurbishment----

grid <- expand.grid(
  range = range_seq,
  refurbishment = c("No", "Cell-replace", "Pack-replace")
)

wtp_summary_car <- map_dfr(seq_len(nrow(grid)), function(i) {
  r <- grid$range[i] / 100
  refurb <- grid$refurbishment[i]
  refurb_coef <- case_when(
    refurb == "Cell-replace" ~ coef_draws_car$battery_refurbishcellreplace,
    refurb == "Pack-replace" ~ coef_draws_car$battery_refurbishpackreplace,
    TRUE ~ 0
  )
  vals <- (coef_draws_car$battery_range_year0 * r + refurb_coef) * 10
  tibble(
    range = grid$range[i],
    refurbishment = refurb,
    mean = mean(vals, na.rm = TRUE),
    lower = quantile(vals, 0.025, na.rm = TRUE),
    upper = quantile(vals, 0.975, na.rm = TRUE)
  )
})

wtp_summary_suv <- map_dfr(seq_len(nrow(grid)), function(i) {
  r <- grid$range[i] / 100
  refurb <- grid$refurbishment[i]
  refurb_coef <- case_when(
    refurb == "Cell-replace" ~ coef_draws_car$battery_refurbishcellreplace,
    refurb == "Pack-replace" ~ coef_draws_car$battery_refurbishpackreplace,
    TRUE ~ 0
  )
  vals <- (coef_draws_car$battery_range_year0 * r + refurb_coef) * 10
  tibble(
    range = grid$range[i],
    refurbishment = refurb,
    mean = mean(vals, na.rm = TRUE),
    lower = quantile(vals, 0.025, na.rm = TRUE),
    upper = quantile(vals, 0.975, na.rm = TRUE)
  )
})

wtp_summary_refurb <- bind_rows(
  wtp_summary_car %>% mutate(vehicle_type = "Car"),
  wtp_summary_suv %>% mutate(vehicle_type = "SUV")
)

uncertainty_refurb_plot <- ggplot(
  wtp_summary_refurb,
  aes(
    x = range,
    y = mean,
    color = refurbishment,
    fill = refurbishment
  )
) +
  facet_grid(vehicle_type ~ refurbishment) +
  xlim(100, 350) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, color = NA) +
  labs(
    x = "Vehicle range at year 0 (miles)",
    y = "WTP ($1000)",
    color = "Refurbishment",
    fill = "Refurbishment",
    title = "Estimated WTP for Vehicle Range, Battery Degradation, and Refurbishment\n(95% Uncertainty Intervals)"
  ) +
  theme_minimal()

uncertainty_refurb_plot

ggsave(
  plot = uncertainty_refurb_plot,
  filename = here::here(
    'code',
    'output',
    "images",
    "uncertainty_refurbishment_plot.png"
  ),
  width = 8,
  height = 6,
  dpi = 300
)
