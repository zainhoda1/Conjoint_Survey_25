source(here::here('code', 'setup.R'))
# Load data
data <- read_parquet(here::here(
  'data',
  "dynata_prolific_joint",
  'data_clean_variables.parquet'
))


# Load LC class probabilities
class_probs <- read_parquet(here::here(
  'code',
  'output',
  'model_output',
  'battery_analysis',
  'apollo',
  '0_Combined_5c_class_probabilities.parquet'
))

# Join class probabilities to main data
data <- data |>
  left_join(
    class_probs |>
      select(
        respID,
        prob_class1,
        prob_class2,
        prob_class3,
        prob_class4,
        prob_class5,
        prob_class_assign
      ),
    by = "respID"
  ) %>%
  filter(
    !is.na(prob_class1) &
      !is.na(prob_class2) &
      !is.na(prob_class3) &
      !is.na(prob_class4) &
      !is.na(prob_class5)
  )

# ── Battery Attribute Importance: Cross-tab Table & Bar Chart ──────────────

## Ordered factor levels for importance scale
importance_levels <- c(
  "unimportant",
  "little_important",
  "moderately_important",
  "important",
  "very_important"
)

importance_labels <- c(
  "Unimportant or little importance",
  "Unimportant or little importance",
  "Moderately important",
  "Important",
  "Very important"
)

# Attribute name lookup
attr_labels <- c(
  battery_attribute_veh_mile = "Vehicle Mileage",
  battery_attribute_purchase_price = "Purchase Price",
  battery_attribute_battery_refurbish = "Battery Refurbishment",
  battery_attribute_veh_range = "Vehicle Range",
  battery_attribute_battery_health = "Battery Health"
)

# Pivot to long, include class probabilities for weighting
battery_long <- data |>
  select(
    respID,
    prob_class1,
    prob_class2,
    prob_class3,
    prob_class4,
    prob_class5,
    starts_with("battery_attribute_")
  ) |>
  pivot_longer(
    cols = starts_with("battery_attribute_"),
    names_to = "attribute",
    values_to = "response"
  ) |>
  filter(response %in% importance_levels) |>
  mutate(
    attribute = factor(attr_labels[attribute], levels = attr_labels),
    response = factor(
      response,
      levels = importance_levels,
      labels = importance_labels
    )
  )

# ── Weighted long format: each respondent weighted by posterior class probability ──
battery_long_class <- battery_long |>
  pivot_longer(
    cols = c(prob_class1, prob_class2, prob_class3, prob_class4, prob_class5),
    names_to = "class",
    values_to = "weight"
  ) |>
  mutate(
    class = case_when(
      class == "prob_class1" ~ "Class 1",
      class == "prob_class2" ~ "Class 2",
      class == "prob_class3" ~ "Class 3",
      class == "prob_class4" ~ "Class 4",
      class == "prob_class5" ~ "Class 5",
      TRUE ~ NA_character_
    )
  )

# Effective weighted n per class for facet labels
class_n_labels <- class_probs |>
  summarise(
    `Class 1` = round(sum(prob_class1)),
    `Class 2` = round(sum(prob_class2)),
    `Class 3` = round(sum(prob_class3)),
    `Class 4` = round(sum(prob_class4)),
    `Class 5` = round(sum(prob_class5))
  ) |>
  pivot_longer(everything(), names_to = "class", values_to = "n") |>
  mutate(class_label = paste0(class, " (n=", n, ")")) |>
  select(class, class_label) |>
  deframe()

# Weighted summary by class × attribute × response
battery_long_class_summary <- battery_long_class |>
  group_by(class, attribute, response) |>
  summarise(weighted_n = sum(weight), .groups = "drop") |>
  mutate(
    pct = weighted_n / sum(weighted_n),
    pct_text = round(pct * 100),
    .by = c(class, attribute)
  ) |>
  mutate(class = class_n_labels[class])

### ── Bar Chart by Latent Class (weighted, 100% bars) ───────────────────────

barplot_bev_attribute_rank <- battery_long_class_summary |>
  ggplot(aes(x = attribute, y = pct, fill = response)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(pct_text)),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Unimportant or little importance" = "#A6761D",
      "Moderately important" = "#D3D3D3",
      "Important" = "#92B6D5",
      "Very important" = "#4682B4"
    )
  ) +
  facet_wrap(~class, nrow = 1) +
  labs(
    title = "Importance of Used BEV Attributes by Latent Class",
    x = "BEV Attributes",
    y = "% of Respondents",
    fill = "Importance"
  ) +
  coord_flip() +
  scale_y_continuous(labels = label_percent()) +
  theme_minimal_grid(font_family = "Roboto Condensed") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = .5),
    axis.text.x = element_text(colour = "black", size = 9),
    axis.text.y = element_text(colour = "black", size = 9),
    axis.title = element_text(colour = "black", size = 12),
    legend.key = element_rect(fill = "white", colour = "white"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(colour = "black", size = 12),
    legend.text = element_text(colour = "black", size = 9),
    # panel.grid.major = element_line(colour = "#d3d3d3"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 12, face = "bold", )
  ) +
  guides(fill = guide_legend(reverse = T))

barplot_bev_attribute_rank

ggsave(
  plot = barplot_bev_attribute_rank,
  filename = here::here(
    'code',
    'output',
    "images",
    "battery_analysis",
    "latent_class",
    "barplot_bev_attribute_rank_5c.jpg"
  ),
  width = 10,
  height = 4,
  dpi = 300
)


### ── Bar Chart By class (probability-weighted, alternative view) ─────────────
# Reuses battery_long_class_summary computed above.
# Each respondent contributes to all five classes weighted by their
# posterior class membership probabilities; no hard assignment.

barplot_bev_attribute_rank_weighted <- battery_long_class_summary |>
  ggplot(aes(x = attribute, y = pct, fill = response)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(pct_text)),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Unimportant or little importance" = "#A6761D",
      "Moderately important" = "#D3D3D3",
      "Important" = "#92B6D5",
      "Very important" = "#4682B4"
    )
  ) +
  scale_y_continuous(labels = label_percent()) +
  facet_wrap(~class, nrow = 1) +
  labs(
    title = "Importance of Used BEV Attributes by Latent Class (Probability-Weighted)",
    x = "BEV Attributes",
    y = "% of Respondents (weighted)",
    fill = "Importance"
  ) +
  coord_flip() +
  theme_minimal_grid(font_family = "Roboto Condensed") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = .5),
    axis.text.x = element_text(colour = "black", size = 9),
    axis.text.y = element_text(colour = "black", size = 9),
    axis.title = element_text(colour = "black", size = 12),
    legend.key = element_rect(fill = "white", colour = "white"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(colour = "black", size = 12),
    legend.text = element_text(colour = "black", size = 9),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  guides(fill = guide_legend(reverse = TRUE))

barplot_bev_attribute_rank_weighted

ggsave(
  plot = barplot_bev_attribute_rank_weighted,
  filename = here::here(
    'code',
    'output',
    'images',
    'battery_analysis',
    'latent_class',
    'barplot_bev_attribute_rank_5c_weighted.jpg'
  ),
  width = 10,
  height = 4,
  dpi = 300
)
