source(here::here('code', 'setup.R'))

#Load data----

model_lc <- apollo_loadModel(
  paste0(
    "code/",
    "output/",
    "model_output/",
    "battery_analysis/",
    "apollo/",
    "rangeloss_car_suv_lc_3c_1"
    # "degradation_car_suv_lc_3c_1"
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

# range, degradation----
# # For each coefficient, get the mean and 95% confidence interval:
# coef_ci <- ci(coef_draws, level = 0.95)
# coef_ci

# # Separate coefficient CIs by attribute
# coef_ci$par <- row.names(coef_ci)
# coef_range <- coef_ci %>% filter(par == 'battery_range_year0')

range_seq <- seq(50, 350, by = 10)
# deg_vals <- c(25, 50, 100, 150)
deg_vals <- c(10, 20, 30)
# deg_vals <- c(1, 2, 3, 4)
grid <- expand.grid(range = range_seq, degradation = deg_vals)


# range, degradation----
wtp_summary_class1 <- map_dfr(seq_len(nrow(grid)), function(i) {
  r <- grid$range[i] / 100 # convert to model units
  d <- grid$degradation[i] # convert to model units

  valid <- coef_draws$b_price_b < 0

  mu1 <- coef_draws$b_range_year0_a / (-coef_draws$b_price_a) * r
  mu2 <- coef_draws$b_degradation_a / (-coef_draws$b_price_a) * d
  vals <- (mu1 + mu2) * 10
  vals <- vals[is.finite(vals)]

  # Trim top/bottom 1%
  vals <- vals[
    vals > quantile(vals, 0.01, na.rm = TRUE) &
      vals < quantile(vals, 0.99, na.rm = TRUE)
  ]
  tibble(
    range = grid$range[i], # show actual range in plot
    degradation = d,
    mean = mean(vals, na.rm = TRUE),
    lower = quantile(vals, 0.025, na.rm = TRUE),
    upper = quantile(vals, 0.975, na.rm = TRUE)
  )
})

wtp_summary_class2 <- map_dfr(seq_len(nrow(grid)), function(i) {
  r <- grid$range[i] / 100 # convert to model units
  d <- grid$degradation[i] # convert to model units
  valid <- coef_draws$b_price_b < 0

  mu1 <- coef_draws$b_range_year0_b / (-coef_draws$b_price_b) * r
  mu2 <- coef_draws$b_degradation_b / (-coef_draws$b_price_b) * d
  vals <- (mu1 + mu2) * 10
  vals <- vals[is.finite(vals)]

  # Trim top/bottom 1%
  vals <- vals[
    vals > quantile(vals, 0.01, na.rm = TRUE) &
      vals < quantile(vals, 0.99, na.rm = TRUE)
  ]
  tibble(
    range = grid$range[i], # show actual range in plot
    degradation = d,
    mean = mean(vals, na.rm = TRUE),
    lower = quantile(vals, 0.025, na.rm = TRUE),
    upper = quantile(vals, 0.975, na.rm = TRUE)
  )
})

wtp_summary_class3 <- map_dfr(seq_len(nrow(grid)), function(i) {
  r <- grid$range[i] / 100 # convert to model units
  d <- grid$degradation[i] # convert to model units
  valid <- coef_draws$b_price_b < 0

  mu1 <- coef_draws$b_range_year0_c / (-coef_draws$b_price_c) * r
  mu2 <- coef_draws$b_degradation_c / (-coef_draws$b_price_c) * d
  vals <- (mu1 + mu2) * 10
  vals <- vals[is.finite(vals)]

  # Trim top/bottom 1%
  vals <- vals[
    vals > quantile(vals, 0.01, na.rm = TRUE) &
      vals < quantile(vals, 0.99, na.rm = TRUE)
  ]
  tibble(
    range = grid$range[i], # show actual range in plot
    degradation = d,
    mean = mean(vals, na.rm = TRUE),
    lower = quantile(vals, 0.025, na.rm = TRUE),
    upper = quantile(vals, 0.975, na.rm = TRUE)
  )
})


wtp_summary_degradation <- bind_rows(
  wtp_summary_class1 %>% mutate(vehicle_type = "Class2"),
  wtp_summary_class2 %>% mutate(vehicle_type = "Class1"),
  wtp_summary_class3 %>% mutate(vehicle_type = "Class3")
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
    "uncertainty_degradation_plot.png"
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

wtp_summary_class1 <- map_dfr(seq_len(nrow(grid)), function(i) {
  r <- grid$range[i] / 100
  refurb <- grid$refurbishment[i]
  refurb_coef <- case_when(
    refurb == "Cell-replace" ~ coef_draws$b_cellreplace_a,
    refurb == "Pack-replace" ~ coef_draws$b_packreplace_a,
    TRUE ~ 0
  )
  valid <- coef_draws$b_price_a < 0

  mu1 <- coef_draws$b_range_year0_a[valid] / (-coef_draws$b_price_a[valid]) * r
  mu2 <- refurb_coef[valid] / (-coef_draws$b_price_a[valid])
  vals <- (mu1 + mu2) * 10
  vals <- (mu1 + mu2) * 10
  vals <- vals[is.finite(vals)]

  # Trim top/bottom 1%
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

wtp_summary_class2 <- map_dfr(seq_len(nrow(grid)), function(i) {
  r <- grid$range[i] / 100
  refurb <- grid$refurbishment[i]
  refurb_coef <- case_when(
    refurb == "Cell-replace" ~ coef_draws$b_cellreplace_b,
    refurb == "Pack-replace" ~ coef_draws$b_packreplace_b,
    TRUE ~ 0
  )
  valid <- coef_draws$b_price_b < 0

  mu1 <- coef_draws$b_range_year0_b[valid] / (-coef_draws$b_price_b[valid]) * r
  mu2 <- refurb_coef[valid] / (-coef_draws$b_price_b[valid])
  vals <- (mu1 + mu2) * 10
  vals <- vals[is.finite(vals)]

  # Trim top/bottom 1%
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

wtp_summary_class3 <- map_dfr(seq_len(nrow(grid)), function(i) {
  r <- grid$range[i] / 100
  refurb <- grid$refurbishment[i]
  refurb_coef <- case_when(
    refurb == "Cell-replace" ~ coef_draws$b_cellreplace_c,
    refurb == "Pack-replace" ~ coef_draws$b_packreplace_c,
    TRUE ~ 0
  )
  valid <- coef_draws$b_price_c < 0

  mu1 <- coef_draws$b_range_year0_c[valid] / (-coef_draws$b_price_c[valid]) * r
  mu2 <- refurb_coef[valid] / (-coef_draws$b_price_c[valid])
  vals <- (mu1 + mu2) * 10
  vals <- vals[is.finite(vals)]

  # Trim top/bottom 1%
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


wtp_summary_refurbishment <- bind_rows(
  wtp_summary_class1 %>% mutate(vehicle_type = "Class2"),
  # wtp_summary_class2 %>% mutate(vehicle_type = "Class1"),
  wtp_summary_class3 %>% mutate(vehicle_type = "Class3")
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
    x = "Range loss rate (% over the next 5 years)",
    y = "WTP ($1000)",
    # color = "Range loss rate\n(% over the next 5 years)",
    # fill = "Range loss rate\n(% over the next 5 years)",
    title = "Estimated WTP for Range Loss Rate and Refurbishment History by Latent Classes\n(95% Confidence Intervals)"
  ) +
  # Add a secondary axis with a name but no labels/ticks
  scale_y_continuous(
    sec.axis = sec_axis(~., name = "Refurbishment History")
  ) +
  theme(
    # Rotate and style the right-side title
    axis.title.y.right = element_text(angle = -90, vjust = 0.5),
    # Optional: Hide the ticks and labels of the secondary axis
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    legend.position = "none",
    panel.background = element_blank()
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
    "uncertainty_refurbishment_plot.png"
  ),
  width = 5,
  height = 4,
  dpi = 300
)
