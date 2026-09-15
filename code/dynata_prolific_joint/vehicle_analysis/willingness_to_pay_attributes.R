source(here::here('code', 'setup.R'))


# Load the estimated model
load(here("models", "mixed_model_1_car_low_panel.RData"))
load(here("models", "mixed_model_1_car_high_panel.RData"))
load(here("models", "mixed_model_1_suv_low_panel.RData"))
load(here("models", "mixed_model_1_suv_high_panel.RData"))


get_wtp_draws <- function(model) {
  coefs <- coef(model)
  # Get the model coefficients and covariance matrix
  covariance <- vcov(model)
  
  # Take 10,000 draws of the coefficients
  coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
  
  return(coef_draws) 
  
}

get_cis <- function(df, current_age=2) {
  
  # Adding dollar values
  wtp_draws <- df %>%
    mutate(
      BEV = powertrainbev * 10^3,
      HEV = powertrainhev * 10^3,
      CV = 0,
      age_year = age * 10^3 * -1,
      mileage_10k = mileage * 10^3 * -1 * 10^4,
      range_bev = range_bev * 10^3,
      operating_cost_mile = operating_cost * 10^3 * -1,
      BEV_100 = BEV + range_bev*100 ,
      BEV_200 = BEV + range_bev*200 ,
      BEV_300 = BEV + range_bev*300,
      range_bev = range_bev * -1
    )%>% 
    select (starts_with('HEV'), starts_with('BEV_'), 
  age_year, mileage_10k, operating_cost_mile)
  
  
  # For each coefficient, get the mean and 95% confidence interval of WTP
  wtp_ci <- ci(wtp_draws, level = 0.95)%>%
    mutate(across(everything(), ~ round(.x, 2)))
  
  return(wtp_ci)
}


conf_mixed_model_1_car_low <- get_cis(get_wtp_draws(mixed_model_1_car_low_panel))
conf_mixed_model_1_car_high <- get_cis(get_wtp_draws(mixed_model_1_car_high_panel))
conf_mixed_model_1_suv_low <- get_cis(get_wtp_draws(mixed_model_1_suv_low_panel))
conf_mixed_model_1_suv_high <- get_cis(get_wtp_draws(mixed_model_1_suv_high_panel))


# ---- WTP plot: CV / HEV / BEV100 / BEV200 / BEV300 with 95% CIs ----

combine_wtp <- function(df, vehicle_type, budget) {
  df %>%
    tibble::rownames_to_column("attribute") %>%
    mutate(vehicle_type = vehicle_type, budget = budget)
}



  
  
# For each vehicle_type x budget combo, mark which BEV range levels are
# within the segment's typical budget (solid) vs. extrapolated (dashed).
bev_keep <- tribble(
  ~vehicle_type, ~budget,       ~attribute, ~line_type,
  "CAR", "Low Budget",  "BEV_100", "solid",
  "CAR", "Low Budget",  "BEV_200", "dashed",
  "CAR", "Low Budget",  "BEV_300", "dashed",
  "CAR", "High Budget", "BEV_100", "solid",
  "CAR", "High Budget", "BEV_200", "solid",
  "CAR", "High Budget", "BEV_300", "dashed",
  "SUV", "Low Budget",  "BEV_200", "solid",
  "SUV", "Low Budget",  "BEV_100", "dashed",
  "SUV", "Low Budget",  "BEV_300", "dashed",
  "SUV", "High Budget", "BEV_200", "solid",
  "SUV", "High Budget", "BEV_300", "solid",
  "SUV", "High Budget", "BEV_100", "dashed"
)

wtp_plot_df <- bind_rows(
  combine_wtp(conf_mixed_model_1_car_low,  "CAR", "Low Budget"),
  combine_wtp(conf_mixed_model_1_car_high, "CAR", "High Budget"),
  combine_wtp(conf_mixed_model_1_suv_low,  "SUV", "Low Budget"),
  combine_wtp(conf_mixed_model_1_suv_high, "SUV", "High Budget")
) %>%
  left_join(bev_keep, by = c("vehicle_type", "budget", "attribute")) %>%
  mutate(line_type = if_else(is.na(line_type), "solid", line_type)) %>%
  mutate(
    attribute = recode(attribute,
      "BEV_100" = "BEV 100",
      "BEV_200" = "BEV 200",
      "BEV_300" = "BEV 300",
      "age_year" = "Reduce age by 1 yr",
      "mileage_10k" = "Reduce mileage by 10k mi",
      "operating_cost_mile" = "Reduce Op. Cost 1 cent / 1 mi"

    ),
    vehicle_type = factor(vehicle_type, levels = c("CAR", "SUV")),
    budget = factor(budget, levels = c("Low Budget", "High Budget"))
  ) 

wtp_plot_df <- wtp_plot_df %>%
  mutate(attribute = factor(attribute, levels = c(
    "Reduce Op. Cost 1 cent / 1 mi",
    "Reduce mileage by 10k mi",
    "Reduce age by 1 yr",
    "BEV 100",
    "BEV 200",
    "BEV 300",
    "HEV"
  )))

ink_primary   <- "#0b0b0b"
ink_secondary <- "#52514e"
ink_muted     <- "#898781"
grid_hairline <- "#e1e0d9"
baseline_ink  <- "#c3c2b7"
chart_surface <- "#fcfcfb"
strip_surface <- "#f2f1ee"

budget_colors <- c(
  "Low Budget"  = "#00798c",  # teal
  "High Budget" = "#d1495b"   # red
)

wtp_plot <- ggplot(wtp_plot_df, aes(x = mean, y = attribute, color = budget)) +

  # 0-line reference -- WTP parity with the conventional (CV) baseline
  geom_vline(xintercept = 0, linetype = "dashed", color = baseline_ink, linewidth = 0.4) +

  geom_pointrange(
    aes(xmin = lower, xmax = upper, linetype = line_type),
    position = position_dodge(width = 0.5),
    linewidth = 0.9,
    fatten = 3.2
  ) +

  annotate(
    "text", x = 0, y = Inf, label = "Baseline: CV",
    vjust = 1.6, hjust = 1.08, size = 3.1, color = ink_muted
  ) +

  scale_color_manual(values = budget_colors, name = NULL) +
  scale_linetype_manual(
    values = c(solid = "solid", dashed ="dotted"),
    labels = c(solid = "In-range BEV estimate", dashed = "Extrapolated beyond budget range"),
    name = NULL
  ) +
  guides(
    color = guide_legend(order = 1),
    linetype = "none"
  ) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1/1000, suffix = "K", accuracy = 1)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1))) +

  facet_wrap(~ vehicle_type, ncol = 1) +

  labs(
    title    = "Willingness to Pay for Vehicle Attributes",
    subtitle = "By vehicle segment and budget tier; error bars show 95% confidence intervals",
    x = "Willingness to Pay ($ thousands)",
    y = NULL
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = chart_surface, color = NA),
    panel.background  = element_rect(fill = chart_surface, color = NA),
    legend.background = element_rect(fill = chart_surface, color = NA),

    plot.title    = element_text(face = "bold", size = 14.5, color = ink_primary,
                                  margin = margin(b = 3)),
    plot.subtitle = element_text(size = 10.5, color = ink_secondary,
                                  margin = margin(b = 10)),
    plot.margin   = margin(12, 14, 10, 12),

    # Grey band behind the "CAR" / "SUV" facet labels
    strip.text       = element_text(face = "bold", size = 11, color = ink_primary),
    strip.background = element_rect(fill = strip_surface, color = NA),

    axis.title = element_text(size = 10.5, color = ink_secondary),
    axis.text.x = element_text(size = 9.5, color = ink_muted),
    axis.text.y = element_text(size = 10, color = ink_primary, face = "bold"),
    axis.ticks = element_line(color = baseline_ink, linewidth = 0.3),

    panel.grid.major = element_line(color = grid_hairline, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.2, "lines"),
    panel.border     = element_blank(),

    legend.position = "bottom",
    legend.box      = "vertical",
    legend.text     = element_text(size = 9.5, color = ink_secondary),
    legend.key      = element_rect(fill = chart_surface, color = NA)
  )

wtp_plot

ggsave(
  filename = here::here(
    'code',
    'output',
    "images",
    "vehicle_analysis",
    "wtp_plot_vehicle_attributes.png"
  ),
  plot = wtp_plot,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = here::here(
    'paper_writing',
    'vehicle_paper',
    "images",
    "vehicle_analysis",
    "wtp_plot_vehicle_attributes.png"
  ),
  plot = wtp_plot,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

##########################################
