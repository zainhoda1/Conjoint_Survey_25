source(here::here('code', 'setup.R'))

#Load data----

model_lc <- apollo_loadModel(
  paste0(
    "code/",
    "output/",
    "model_output/",
    "battery_analysis/",
    "apollo/",
    "nonlinear_rangeloss_car_suv_lc_3c_1"
  )
)

# covariance <- apollo_varcov(model_lc)

# load(here(
#   "code",
#   "output",
#   "model_output",
#   "battery_analysis",
#   "logitr",
#   "mxl_wtp_model_car.RData"
# ))

coefs <- model_lc$estimate
# covariance <- model_lc$BHHHvarcov
covariance <- model_lc$BHHHvarcov
# Ensure correct types
coefs <- as.numeric(coefs)
names(coefs) <- names(model_lc$estimate)
covariance <- as.matrix(covariance)

# Align using names
common <- intersect(names(coefs), rownames(covariance))

coefs <- coefs[common]
covariance <- covariance[common, common]

# Draws
coef_draws <- as.data.frame(
  MASS::mvrnorm(10^4, mu = coefs, Sigma = covariance)
)

# Fixed-at-zero parameters are excluded from the covariance matrix and
# therefore absent from coef_draws. Add them back as columns of zeros so
# the WTP calculation handles them correctly.
fixed_zero_params <- c(
  "b_range_year0_quadratic_b",
  "b_range_year0_quadratic_c",
  "b_degradation_quadratic_b"
)
for (p in fixed_zero_params) {
  if (!p %in% names(coef_draws)) coef_draws[[p]] <- 0
}

# range, degradation----
range_seq <- seq(50, 350, by = 10)
deg_vals <- c(10, 20, 30)
grid <- expand.grid(range = range_seq, degradation = deg_vals)

# class suffix -> display label (preserving original ordering)
class_labels <- c(a = "Class2", b = "Class1", c = "Class3")

# Helper: WTP for range + degradation with quadratic terms, for one class suffix
# suffix <- "a"  # example for Class2
wtp_degradation <- function(suffix) {
  b_r <- coef_draws[[paste0("b_range_year0_", suffix)]]
  b_rq <- coef_draws[[paste0("b_range_year0_quadratic_", suffix)]]
  b_d <- coef_draws[[paste0("b_degradation_", suffix)]]
  b_dq <- coef_draws[[paste0("b_degradation_quadratic_", suffix)]]
  b_p <- coef_draws[[paste0("b_price_", suffix)]]

  map_dfr(seq_len(nrow(grid)), function(i) {
    r <- grid$range[i] / 100 # convert to model units
    d <- grid$degradation[i]

    mu1 <- (b_r * r + b_rq * r^2) / (-b_p)
    mu2 <- (b_d * d + b_dq * d^2) / (-b_p)
    vals <- (mu1 + mu2) * 10
    vals <- vals[is.finite(vals)]
    vals <- vals[
      vals > quantile(vals, 0.01, na.rm = TRUE) &
        vals < quantile(vals, 0.99, na.rm = TRUE)
    ]
    tibble(
      range = grid$range[i],
      degradation = d,
      mean = mean(vals, na.rm = TRUE),
      lower = quantile(vals, 0.025, na.rm = TRUE),
      upper = quantile(vals, 0.975, na.rm = TRUE)
    )
  })
}

wtp_summary_degradation <- map_dfr(
  names(class_labels),
  ~ wtp_degradation(.x) |> mutate(vehicle_type = class_labels[.x])
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
# "battery_analysis",
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
  # facet_grid(factor(degradation) ~ vehicle_type) +
  facet_wrap(~vehicle_type, strip.position = "bottom") +
  xlim(50, 300) +
  # ylim(-50, 50) +
  geom_line(linewidth = 1) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, color = NA) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.3
  ) +
  labs(
    x = "Vehicle range at year 3 (miles)",
    y = "WTP ($1000)",
    color = "Range loss rate\n(% over the next 5 years)",
    fill = "Range loss rate\n(% over the next 5 years)",
    title = "Estimated WTP for Vehicle Range and Range Loss Rate"
    # title = "Estimated WTP for Vehicle Range and Range Loss Rate\n by Latent Classes(95% Confidence Intervals)"
  ) +
  theme_minimal_grid(
    font_family = "Roboto Condensed",
    font_size = 12
  ) +
  theme(
    legend.position = "bottom",
    strip.placement = "outside", # ensures facet labels are truly at bottom
    plot.title = element_text(face = "bold")
  )

uncertainty_degradation_plot

ggsave(
  plot = uncertainty_degradation_plot,
  filename = here::here(
    'code',
    'output',
    "images",
    "battery_analysis",
    "latent_class",
    "nonlinear_uncertainty_degradation_plot.jpg"
  ),
  width = 6,
  height = 3,
  dpi = 300
)

# range, refurbishment----

grid <- expand.grid(
  range = range_seq,
  refurbishment = c("No", "Cell-replace", "Pack-replace")
)

# Helper: WTP for range (with quadratic) + refurbishment, for one class suffix
wtp_refurbishment <- function(suffix) {
  b_r <- coef_draws[[paste0("b_range_year0_", suffix)]]
  b_rq <- coef_draws[[paste0("b_range_year0_quadratic_", suffix)]]
  b_cell <- coef_draws[[paste0("b_cellreplace_", suffix)]]
  b_pack <- coef_draws[[paste0("b_packreplace_", suffix)]]
  b_p <- coef_draws[[paste0("b_price_", suffix)]]
  valid <- b_p < 0

  map_dfr(seq_len(nrow(grid)), function(i) {
    r <- grid$range[i] / 100
    refurb <- grid$refurbishment[i]

    refurb_coef <- case_when(
      refurb == "Cell-replace" ~ b_cell,
      refurb == "Pack-replace" ~ b_pack,
      TRUE ~ 0
    )

    mu1 <- (b_r[valid] * r + b_rq[valid] * r^2) / (-b_p[valid])
    mu2 <- refurb_coef[valid] / (-b_p[valid])
    vals <- (mu1 + mu2) * 10
    vals <- vals[is.finite(vals)]
    vals <- vals[
      vals > quantile(vals, 0.01, na.rm = TRUE) &
        vals < quantile(vals, 0.99, na.rm = TRUE)
    ]
    tibble(
      range = grid$range[i],
      refurbishment = refurb,
      mean = mean(vals, na.rm = TRUE),
      lower = quantile(vals, 0.025, na.rm = TRUE),
      upper = quantile(vals, 0.975, na.rm = TRUE)
    )
  })
}

wtp_summary_refurbishment <- map_dfr(
  names(class_labels),
  ~ wtp_refurbishment(.x) |> mutate(vehicle_type = class_labels[.x])
)

uncertainty_refurbishment_plot <- ggplot(
  wtp_summary_refurbishment,
  aes(
    x = range,
    y = mean,
    color = factor(refurbishment),
    fill = factor(refurbishment)
  )
) +
  facet_grid(factor(refurbishment) ~ vehicle_type) +
  # xlim(50, 300) +
  # ylim(-50, 50) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, color = NA) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.3
  ) +
  labs(
    x = "Vehicle range at year 3 (miles)",
    y = "WTP ($1000)",
    color = "Refurbishment history",
    fill = "Refurbishment history",
    title = "Estimated WTP for Range Loss Rate and Refurbishment History by Latent Classes\n(95% Confidence Intervals)"
  ) +
  # Add a secondary axis with a name but no labels/ticks
  theme_minimal_grid(
    font_family = "Roboto Condensed",
    font_size = 12
  ) +
  theme(
    legend.position = "bottom",
    strip.placement = "outside", # ensures facet labels are truly at bottom
    plot.title = element_text(face = "bold")
  )
uncertainty_refurbishment_plot

ggsave(
  plot = uncertainty_refurbishment_plot,
  filename = here::here(
    'code',
    'output',
    "images",
    "battery_analysis",
    "latent_class",
    "uncertainty_refurbishment_plot.jpg"
  ),
  width = 6,
  height = 5,
  dpi = 300
)
