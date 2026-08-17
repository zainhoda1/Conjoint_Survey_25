# wtp_plots_facet_budget.R
#
# Faceted dumbbell plot of BEV WTP by budget tier (low and high).
# Range fixed at 300 miles; ages 2, 5, 8 shown as colors with Y-axis dodging.
# facet_wrap by vehicle type (car / SUV).
#
# Mirrors Figure 2 (dumbbell_wtp_age_fixed_range_300_budgets.png) but includes
# both low- and high-budget subgroups and uses the facet layout.

source(here::here('code', 'setup.R'))

library(showtext)
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 300)

# ---- Load WTP-space models ---------------------------------------------
load(here("models", "mixed_model_1_car_low_panel.RData"))
load(here("models", "mixed_model_1_car_high_panel.RData"))
load(here("models", "mixed_model_1_suv_low_panel.RData"))
load(here("models", "mixed_model_1_suv_high_panel.RData"))

all_models <- c(
  "conf_car_low", "conf_car_high",
  "conf_suv_low", "conf_suv_high"
)

# ---- WTP draw helpers --------------------------------------------------
# WTP-space logitr: coefficients are already in WTP units (price / 10,000).
# Draw from parameter distribution; scale to dollars.
get_wtp_draws <- function(model) {
  as.data.frame(MASS::mvrnorm(10^4, coef(model), vcov(model)))
}

get_cis <- function(df) {
  wtp_draws <- df %>%
    mutate(
      BEV         = powertrainbev * 10^4,
      HEV         = powertrainhev * 10^4,
      CV          = 0,
      age_year    = age          * 10^4,
      mileage_10k = mileage      * 10^4,
      range_bev   = range_bev   * 10^4
    )
  ci(wtp_draws, level = 0.95) %>%
    mutate(across(everything(), ~ round(.x, 2)))
}

# ---- Compute CIs -------------------------------------------------------
conf_car_low  <- get_cis(get_wtp_draws(mixed_model_1_car_low_panel))
conf_car_high <- get_cis(get_wtp_draws(mixed_model_1_car_high_panel))
conf_suv_low  <- get_cis(get_wtp_draws(mixed_model_1_suv_low_panel))
conf_suv_high <- get_cis(get_wtp_draws(mixed_model_1_suv_high_panel))

# ---- Build long-form plot data -----------------------------------------
plot_data <- data.frame(
  model_name     = character(),
  vehicle_type   = character(),
  age_year       = numeric(),
  mileage_10k    = numeric(),
  range_bev      = numeric(),
  wtp            = numeric(),
  wtp_value_type = character(),
  id             = numeric(),
  stringsAsFactors = FALSE
)

for (current in all_models) {
  df  <- get(current)
  tdf <- as.data.frame(t(df %>% select(lower, mean, upper)))
  tdf$wtp_value_type <- rownames(tdf)

  tdf <- tdf %>%
    select(BEV, HEV, CV, age_year, mileage_10k, range_bev, wtp_value_type) %>%
    pivot_longer(cols = c(BEV, HEV, CV), names_to = "vehicle_type", values_to = "wtp") %>%
    mutate(id = 1, model_name = current)

  plot_data <- rbind(plot_data, tdf)
}

# ---- Cross with age × range grid ---------------------------------------
age_range_grid <- expand.grid(
  current_range = seq(0.5, 3.5, 0.5),
  current_age   = seq(2, 8)
) %>% mutate(id = 1)

plot_long <- full_join(plot_data, age_range_grid, by = "id") %>%
  mutate(wtp_total = wtp + age_year * current_age + range_bev * current_range)

# ---- Label groups and filter -------------------------------------------
plot_wide <- plot_long %>%
  mutate(
    model_type = case_when(
      model_name %in% c("conf_car_low", "conf_car_high") ~ "Car",
      .default = "SUV"
    ),
    segment = case_when(
      model_name %in% c("conf_car_low", "conf_suv_low") ~ "Low budget",
      .default = "High budget"
    )
  ) %>%
  filter(
    vehicle_type  == "BEV",
    current_range == 3,
    current_age   %in% c(2, 5, 8)
  ) %>%
  select(model_type, segment, current_age, wtp_total, wtp_value_type) %>%
  pivot_wider(names_from = wtp_value_type, values_from = wtp_total) %>%
  mutate(
    current_age = factor(current_age, levels = c(2, 5, 8),
                         labels = c("2 years", "5 years", "8 years")),
    model_type  = factor(model_type, levels = c("Car", "SUV")),
    segment     = factor(segment, levels = c("Low budget", "High budget"))
  ) %>%
  mutate(
    seg_num    = as.numeric(segment),
    age_offset = case_when(
      current_age == "2 years" ~ -0.22,
      current_age == "5 years" ~  0.00,
      current_age == "8 years" ~  0.22
    ),
    y_pos = seg_num + age_offset
  )

# ---- Plot --------------------------------------------------------------
ggplot(plot_wide, aes(y = y_pos, colour = current_age)) +
  geom_segment(
    aes(x = lower, xend = upper, yend = y_pos),
    linewidth = 0.8
  ) +
  geom_point(aes(x = mean), shape = 18, size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  facet_wrap(~ model_type, nrow = 1) +
  scale_colour_manual(
    values = c(
      "2 years" = "#E69F00",
      "5 years" = "#009E73",
      "8 years" = "#4A90E2"
    ),
    name = "Vehicle age"
  ) +
  scale_y_continuous(
    breaks = c(1, 2),
    labels = c("Low\nbudget", "High\nbudget"),
    limits = c(0.6, 2.4)
  ) +
  scale_x_continuous(
    labels = scales::label_dollar(scale = 1 / 1000, suffix = "K")
  ) +
  labs(
    title    = "Modelled WTP for Used BEVs by Budget Tier and Vehicle Type",
    subtitle = "Driving range fixed at 300 miles. Points: mean estimate; bars: 95% CI.",
    x        = "Willingness to pay",
    y        = NULL
  ) +
  theme_minimal(base_size = 13, base_family = "Roboto Condensed") +
  theme(
    plot.background     = element_rect(fill = "white", color = NA),
    panel.background    = element_rect(fill = "white", color = NA),
    plot.title          = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    plot.title.position = "plot",
    plot.subtitle       = element_text(size = 11, colour = "grey40", margin = margin(b = 8)),
    strip.text          = element_text(face = "bold", size = 11),
    strip.background    = element_rect(fill = "grey92", color = NA),
    panel.grid.minor    = element_blank(),
    panel.grid.major.y  = element_blank(),
    panel.spacing       = unit(1.2, "lines"),
    legend.position     = "bottom",
    legend.direction    = "horizontal"
  )

ggsave(
  filename = here::here(
    "code", "output", "images", "vehicle_analysis",
    "facet_wtp_budget_range300.png"
  ),
  width = 5, height = 3, dpi = 300
)

ggsave(
  filename = here::here(
    "paper_writing", "vehicle_paper", "images", "dumbbell_plots",
    "facet_wtp_budget_range300.png"
  ),
  width = 5, height = 3, dpi = 300
)

cat("Saved: facet_wtp_budget_range300.png\n")
