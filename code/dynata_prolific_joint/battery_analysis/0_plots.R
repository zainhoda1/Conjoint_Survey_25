source(here::here('code', 'setup.R'))

# Data & Model Upload----
data_dce <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_logitr_dce_only_battery.parquet"
))

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

# WTP Table & Plot----
## WTP table----

# Extract WTP estimates from both models
wtp_table <- data.frame(
  Parameter = c(
    "Scale parameter",
    "Mileage (unit: 10,000 miles)",
    "Battery range (year 0, unit: 100 miles)",
    "Battery annual degradation rate (unit: % per year)",
    "Battery refurbishment (pack level)",
    "Battery refurbishment (cell level)",
    "No Choice",
    "sd_Mileage",
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
  slice(3:(nrow(wtp_table_long) - 10)) %>%
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
    "mileage",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace"
  )

  par_labels <- c(
    mileage = "Mileage (unit: 10,000 miles)",
    battery_range_year0 = "Battery range (year 0, unit: 100 miles)",
    battery_degradation = "Battery annual degradation rate (unit: % per year)",
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

## Simulated individual WTP distributions (ridgeline) ----
set.seed(42)
n_sim <- 5000

sim_wtp <- het |>
  rowwise() |>
  reframe(
    sim_val = rnorm(n_sim, mean = mean_wtp, sd = sd_wtp),
    attribute = attribute,
    segment = segment
  )

simulated_wtp_plot <- sim_wtp |>
  ggplot(aes(x = sim_val, y = attribute, fill = segment, color = segment)) +
  geom_density_ridges(
    alpha = 0.45,
    scale = 0.9,
    quantile_lines = TRUE,
    quantiles = 2 # median line
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~segment) +
  scale_fill_manual(values = c("Car" = "#1F4E79", "SUV" = "#C2A56F")) +
  scale_color_manual(values = c("Car" = "#1F4E79", "SUV" = "#C2A56F")) +
  labs(
    title = "WTP Distributions of Simulated Population (n=5,000)",
    subtitle = "Drawn from N(mean, SD) — vertical line at median",
    x = "WTP (unit: $10k)",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

simulated_wtp_plot

ggsave(
  plot = simulated_wtp_plot,
  filename = here::here(
    'code',
    'output',
    "images",
    "battery_simulated_wtp_plot.png"
  ),
  width = 7.5,
  height = 3,
  dpi = 300
)

# Heatmap of WTP correlation (if model with correlation is estimated) ----
# helper to pull coeffs safely (replace missing with 0)
pull_coef <- function(coefs, name) {
  val <- coefs[name]
  if (is.null(val) || length(val) == 0 || is.na(val)) 0 else as.numeric(val)
}

# extract mean & sd parameters from a wtp model
get_model_pars <- function(model, segment_label) {
  coefs <- coef(model)
  tibble(
    segment = segment_label,
    mean_range = pull_coef(coefs, "battery_range_year0"),
    mean_degrad = pull_coef(coefs, "battery_degradation"),
    mean_pack = pull_coef(coefs, "battery_refurbishpackreplace"),
    mean_cell = pull_coef(coefs, "battery_refurbishcellreplace"),
    sd_range = abs(pull_coef(coefs, "sd_battery_range_year0")),
    sd_degrad = abs(pull_coef(coefs, "sd_battery_degradation")),
    sd_pack = abs(pull_coef(coefs, "sd_battery_refurbishpackreplace")),
    sd_cell = abs(pull_coef(coefs, "sd_battery_refurbishcellreplace"))
  )
}

pars_car <- get_model_pars(wtp_model_car, "Car")
pars_suv <- get_model_pars(wtp_model_suv, "SUV")
pars_all <- bind_rows(pars_car, pars_suv)

# choose sensible ranges from your DCE data (use 1st/99th pct to avoid outliers)
rng_degrad <- quantile(
  data_dce$battery_degradation,
  probs = c(0.01, 0.99),
  na.rm = TRUE
)
rng_range <- quantile(
  data_dce$battery_range_year0,
  probs = c(0.01, 0.99),
  na.rm = TRUE
)

degrad_seq <- seq(rng_degrad[1], rng_degrad[2], length.out = 80)
range_seq <- seq(rng_range[1], rng_range[2], length.out = 80)

# refurbishment levels: none / cell / pack
refurb_levels <- tribble(
  ~refurb , ~cell , ~pack ,
  "none"  ,     0 ,     0 ,
  "cell"  ,     1 ,     0 ,
  "pack"  ,     0 ,     1
)

# build grid and compute combined mean WTP and population SD
grid <- expand_grid(
  battery_degradation = degrad_seq,
  battery_range_year0 = range_seq
)

calc_df <- expand_grid(pars_all, refurb_levels) %>%
  crossing(grid) %>%
  mutate(
    mean_wtp = mean_range *
      battery_range_year0 +
      mean_degrad * battery_degradation +
      mean_pack * pack +
      mean_cell * cell,
    # assume independent random coeffs (diagonal mixing) -> var(sum) = sum((sd_i * x_i)^2)
    pop_sd_wtp = sqrt(
      (sd_range * battery_range_year0)^2 +
        (sd_degrad * battery_degradation)^2 +
        (sd_pack * pack)^2 +
        (sd_cell * cell)^2
    ),
    wtp_lo = mean_wtp - 1.96 * pop_sd_wtp,
    wtp_hi = mean_wtp + 1.96 * pop_sd_wtp
  ) %>%
  mutate(
    refurb = case_when(
      refurb == "none" ~ "No\nrefurbishment",
      refurb == "cell" ~ "Cell-level\nrefurbishment",
      refurb == "pack" ~ "Pack-level\nrefurbishment"
    ),
    refurb = factor(
      refurb,
      levels = c(
        "No\nrefurbishment",
        "Cell-level\nrefurbishment",
        "Pack-level\nrefurbishment"
      )
    ),
    segment = factor(segment, levels = c("Car", "SUV"))
  )

# plot: 3 rows (refurb) x 2 cols (segment) -> six heatmaps

heatmap_plot <- calc_df %>%
  ggplot(aes(
    x = battery_degradation,
    y = battery_range_year0 * 100,
    fill = mean_wtp * 10
  )) +
  geom_raster(interpolate = TRUE) +
  geom_contour(
    data = calc_df,
    aes(
      x = battery_degradation,
      y = battery_range_year0 * 100,
      z = mean_wtp * 10
    ),
    breaks = 0,
    color = "black",
    linetype = "dashed",
    size = 0.4,
    inherit.aes = FALSE,
    na.rm = TRUE
  ) +
  facet_grid(refurb ~ segment) +
  scale_fill_gradient2(
    name = "Mean WTP\n(unit: $1,000)",
    low = "#da5f07ff",
    mid = "white",
    high = "#0170d8ff",
    midpoint = 0,
    na.value = "transparent"
  ) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50)) +
  labs(
    x = "Battery degradation rate (unit: percentage per year)",
    y = "Battery range (year 0, unit: miles)",
    title = "Combined mean WTP for battery range and degradation across refurbishment levels and vehicle segments",
  ) +
  theme_minimal(base_size = 11) +
  # coord_fixed(
  #   ratio = (range(degrad_seq) %>% diff()) / (range(range_seq) %>% diff())
  # ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "grey95", colour = NA)
  )

# return the ggplot object
heatmap_plot
