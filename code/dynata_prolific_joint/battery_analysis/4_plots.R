source(here::here('code', 'setup.R'))

# Model Upload----
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

summary(wtp_model_car)
summary(wtp_model_suv)

# Table & Plot----
## WTP table----

# Extract WTP estimates from both models
wtp_table <- data.frame(
  Parameter = c(
    "Scale parameter",
    "Mileage",
    "Battery range (year 0)",
    "Battery annual degradation rate",
    "Battery refurbishment (cell level)",
    "Battery refurbishment (pack level)",
    "No Choice",
    "sd_Battery range (year 0)",
    "sd_Battery annual degradation rate",
    "sd_Battery refurbishment (cell level)",
    "sd_Battery refurbishment (pack level)"
  ),
  `WTP Unit` = c(
    "-",
    "10,000 miles",
    "100 miles",
    "% per year",
    "-",
    "-",
    "-",
    "-",
    "-",
    "-",
    "-"
  ),
  Estimate_car = round(coef(wtp_model_car), 3),
  StdError_car = round(se(wtp_model_car), 3),
  Estimate_suv = round(coef(wtp_model_suv), 3),
  StdError_suv = round(se(wtp_model_suv), 3)
)

wtp_table_long <- wtp_table %>%
  pivot_longer(
    cols = c(Estimate_car, Estimate_suv, StdError_car, StdError_suv),
    names_to = c(".value", "segment"),
    names_pattern = "(.+)_(car|suv)"
  ) %>%
  mutate(
    segment = factor(
      segment,
      levels = c("car", "suv"),
      labels = c("Car", "SUV")
    )
  ) %>%
  mutate(
    LowerBound = Estimate - 1.96 * StdError,
    UpperBound = Estimate + 1.96 * StdError
  )

## WTP plot----
## Mean WTP ----
wtp_plot <- wtp_table_long %>%
  slice(3:(nrow(wtp_table_long) - 8)) %>%
  ggplot(aes(x = Estimate, y = Parameter, color = segment)) +
  geom_point(position = position_dodge(0.5), size = 0.5) +
  geom_errorbar(
    aes(xmin = LowerBound, xmax = UpperBound),
    position = position_dodge(0.5),
    height = 0.2
  ) +
  scale_color_manual(values = c("Car" = "#1F4E79", "SUV" = "#C2A56F")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Mean WTP by Vehicle Segment",
    x = "WTP (unit : $10k)",
    y = NULL,
    color = "Segment"
  ) +

  theme_minimal() +
  guides(color = guide_legend(reverse = TRUE))

wtp_plot

ggsave(
  plot = wtp_plot,
  filename = here::here(
    'code',
    'output',
    "images",
    "battery_mean_wtp_plot.png"
  ),
  width = 6,
  height = 3,
  dpi = 300
)

## Heterogeneity WTP ----
# ── 0. Extract coefficients from both WTP models ─────────────────────────────
extract_wtp_heterogeneity <- function(model, seg_label) {
  cf <- coef(model)
  se <- sqrt(diag(solve(-model$hessian)))

  # Random pars: mean + SD (sd params are prefixed "sd_")
  rand_pars <- c(
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace"
  )

  par_labels <- c(
    battery_range_year0 = "Battery range (year 0)",
    battery_degradation = "Battery annual degradation rate",
    battery_refurbishpackreplace = "Battery refurbishment (pack level)",
    battery_refurbishcellreplace = "Battery refurbishment (cell level)"
  )

  tibble(
    attribute = par_labels[rand_pars],
    mean_wtp = cf[rand_pars],
    sd_wtp = abs(cf[paste0("sd_", rand_pars)]),
    se_mean = se[rand_pars],
    segment = seg_label
  )
}

het_car <- extract_wtp_heterogeneity(wtp_model_car, "Car")
het_suv <- extract_wtp_heterogeneity(wtp_model_suv, "SUV")
het <- bind_rows(het_car, het_suv)

# ── 1. Mean WTP ± 1 SD (population spread) ───────────────────────────────────
hetegoneity_plot <- het |>
  ggplot(aes(x = mean_wtp, y = attribute, color = segment)) +
  geom_pointrange(
    aes(xmin = mean_wtp - sd_wtp, xmax = mean_wtp + sd_wtp),
    position = position_dodge(0.5),
    linewidth = 0.8,
    size = 0.5
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Car" = "#1F4E79", "SUV" = "#C2A56F")) +
  labs(
    title = "Heterogeneity in WTP: Mean ± 1 SD",
    x = "WTP (unit: $10k)",
    y = NULL,
    color = "Segment"
  ) +
  theme_minimal(base_size = 11) +
  guides(color = guide_legend(reverse = TRUE))

hetegoneity_plot

ggsave(
  plot = hetegoneity_plot,
  filename = here::here(
    'code',
    'output',
    "images",
    "battery_hetegoneity_plot.png"
  ),
  width = 6,
  height = 3,
  dpi = 300
)
