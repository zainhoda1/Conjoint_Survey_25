#source(here::here('code', 'dynata_prolific_joint', 'vehicle_analysis', 'buying_probability_part3.R'))

depreciation_curves_data <- read_parquet(here('data', 'depreciation_curves_data.parquet' )) 

vehicle_label_points <- read_parquet(here('data', 'vehicle_label_points.parquet' )) 

bev_probability_data <- read_parquet(here('data', 'bev_probability_data.parquet' )) 

# Chart chrome tokens, matching the visual system used across this file's
# other figures so the paper's plots read as one consistent set.
ink_primary   <- "#0b0b0b"
ink_secondary <- "#52514e"
ink_muted     <- "#898781"
grid_hairline <- "#e1e0d9"
baseline_ink  <- "#c3c2b7"
chart_surface <- "#fcfcfb"
strip_surface <- "#f2f1ee"

powertrain_colors <- c(
  "BEV"                 = "#009E73",  # bright green
  "HEV"                 = "#D55E00",  # bright vermillion/orange
  "Conventional (CV)"   = "#4D4D4D"   # dark grey
)

depreciation_curves_plot <- depreciation_curves_data |>
  ggplot(aes(x = age_years, y = predicted_price,
             group = id, color = powertrain_label, linetype = linetype_label)) +

  facet_grid(budget_label ~ vehicle_category_label ) +

  # Line: 2px, series color carries identity
  geom_line(linewidth = 0.9, lineend = "round") +

  # Point: white surface ring beneath a series-colored marker (>=8px)
  geom_point(size = 3.6, color = chart_surface) +
  geom_point(size = 2.5) +

  # ggrepel::geom_text_repel(
  #   data = vehicle_label_points,
  #   aes(label = vehicle_label),
  #   size = 3,
  #   fontface = "bold",
  #   direction = "y",
  #   hjust = 0,
  #   nudge_x = 0.3,
  #   segment.color = NA,
  #   show.legend = FALSE,
  #   seed = 42
  # ) +

  scale_color_manual(values = powertrain_colors, name = NULL) +
  scale_linetype_identity() +

  scale_y_continuous(
    labels = scales::dollar_format(scale = 1/1000, suffix = "K"),
    breaks = scales::breaks_pretty(n = 5),
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(1),
    expand = expansion(mult = c(0.03, 0.18))
  ) +

  labs(
    title    = "Predicted Vehicle Depreciation by Powertrain",
    subtitle = "Predicted resale price by vehicle age (dashed lines = HEV comparison, solid = BEV comparison)",
    x = "Vehicle age (years)",
    y = "Predicted price ($ thousands)"
  ) +

  guides(color = guide_legend(override.aes = list(linewidth = 1.6, size = 3))) +

  theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = chart_surface, color = NA),
    panel.background  = element_rect(fill = chart_surface, color = NA),
    legend.background = element_rect(fill = chart_surface, color = NA),

    plot.title    = element_text(face = "bold", size = 14.5, color = ink_primary,
                                  margin = margin(b = 3)),
    plot.subtitle = element_text(size = 11, color = ink_secondary,
                                  margin = margin(b = 10)),
    plot.margin   = margin(12, 14, 10, 12),

    strip.text       = element_text(face = "bold", size = 11, color = ink_primary),
    strip.background = element_rect(fill = strip_surface, color = NA),

    axis.title = element_text(size = 10.5, color = ink_secondary),
    axis.text  = element_text(size = 9.5, color = ink_muted),
    axis.ticks = element_line(color = baseline_ink, linewidth = 0.3),
    axis.line  = element_blank(),

    panel.grid.major = element_line(color = grid_hairline, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.2, "lines"),
    panel.border     = element_blank(),

    legend.position  = "bottom",
    legend.text      = element_text(size = 9.5, color = ink_secondary),
    legend.key       = element_rect(fill = chart_surface, color = NA)
  )

depreciation_curves_plot

ggsave(
  filename = here::here(
    'code',
    'output',
    "images",
    "vehicle_analysis",
    "depreciation_curves_plot.png"
  ),
  plot = depreciation_curves_plot,
  width = 10,
  height = 6.5,
  dpi = 300
)

ggsave(
  filename = here::here(
    'paper_writing',
    'vehicle_paper',
    "images",
    "vehicle_analysis",
    "depreciation_curves_plot.png"
  ),
  plot = depreciation_curves_plot,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)


####################################

# Fixed-order categorical palette: "minou" from the ltc color-palette
# library (https://loukesio.github.io/ltc-color-palettes/). Assigned by
# position to whichever comparisons are present in the data.
comparison_colors <- c(
  "#00798c",  # teal
  "#d1495b",  # red
  "#edae49",  # amber
  "#66a182",  # sage green
  "#2e4057"   # dark navy
)

bev_probability_data <- bev_probability_data |>
  mutate(
    budget_label      = recode(budget, "low" = "Low Budget", "high" = "High Budget"),
    comparison_label  = paste0(budget_label, " - ", powertrain_label, " // CV")
)

bev_probability_plot <- bev_probability_data |>
  ggplot(aes(x = age_years, y = predicted_prob,
             group = comparisons, color = comparison_label)) +

  facet_wrap(~vehicle_category_label) +

  # 50% reference line -- choice parity with the conventional counterpart
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = baseline_ink,
    linewidth = 0.4
  ) +

  # Confidence band from 10k-draw simulation (predicted_prob_lower/upper)
  geom_ribbon(
    aes(ymin = predicted_prob_lower, ymax = predicted_prob_upper, fill = comparison_label),
    color = NA,
    alpha = 0.15
  ) +

  # Line: 2px, series color carries identity
  geom_line(linewidth = 0.9, lineend = "round") +

  # Point: white surface ring beneath a series-colored marker (>=8px)
  geom_point(size = 3.6, color = chart_surface) +
  geom_point(size = 2.5) +

  scale_color_manual(values = comparison_colors, name = NULL) +
  scale_fill_manual(values = comparison_colors, guide = "none") +

  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1L),
    breaks = scales::breaks_pretty(n = 6),
    expand = expansion(mult = c(0.03, 0.06))
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(1),
    expand = expansion(mult = c(0.03, 0.06))
  ) +

  labs(
    title = "Probability of Choosing a BEV or HEV Over Its Conventional Counterpart",
    subtitle = "By vehicle age, for matched car and SUV model pairs",
    x = "Vehicle age (years)",
    y = "Probability of alternative-powertrain choice"
  ) +

  guides(color = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(linewidth = 1.6, size = 3))) +

  theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = chart_surface, color = NA),
    panel.background  = element_rect(fill = chart_surface, color = NA),
    legend.background = element_rect(fill = chart_surface, color = NA),

    plot.title    = element_text(face = "bold", size = 14.5, color = ink_primary,
                                  margin = margin(b = 3)),
    plot.subtitle = element_text(size = 11, color = ink_secondary,
                                  margin = margin(b = 10)),
    plot.caption  = element_text(size = 8.5, color = ink_muted, hjust = 0,
                                  margin = margin(t = 8)),
    plot.margin   = margin(12, 14, 10, 12),

    strip.text       = element_text(face = "bold", size = 11, color = ink_primary),
    strip.background = element_rect(fill = strip_surface, color = NA),

    axis.title = element_text(size = 10.5, color = ink_secondary),
    axis.text  = element_text(size = 9.5, color = ink_muted),
    axis.ticks = element_line(color = baseline_ink, linewidth = 0.3),
    axis.line  = element_blank(),

    panel.grid.major = element_line(color = grid_hairline, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.4, "lines"),

    legend.position  = "bottom",
    legend.text      = element_text(size = 9, color = ink_secondary),
    legend.key       = element_rect(fill = chart_surface, color = NA),
    legend.spacing.x = unit(6, "pt")
  )

bev_probability_plot

ggsave(
  filename = here::here(
    'code',
    'output',
    "images",
    "vehicle_analysis",
    "BEV_probability_age_with_depreciation.png"
  ),
  plot = bev_probability_plot,
  width = 10,
  height = 6.5,
  dpi = 300
)

ggsave(
  filename = here::here(
    'paper_writing',
    'vehicle_paper',
    "images",
    "vehicle_analysis",
    "BEV_probability_age_with_depreciation.png"
  ),
  plot = bev_probability_plot,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

