source(here::here('code', 'setup.R'))
# Load data
data <- read_parquet(here::here(
  'data',
  "dynata_prolific_joint",
  'data_clean_variables.parquet'
))


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
  "Unimportant",
  "Of little importance",
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

# Pivot to long, filter out NA/"" responses, apply factor ordering
battery_long <- data |>
  select(starts_with("battery_attribute_")) |>
  pivot_longer(
    cols = everything(),
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

# ── Cross-tab: attributes (rows) × importance levels (columns) ──────────────
crosstab <- battery_long |>
  count(attribute, response) |>
  pivot_wider(names_from = response, values_from = n, values_fill = 0)

crosstab |>
  knitr::kable(caption = "Battery Attribute Importance Ratings (n respondents)")

### ── Bar Chart --

barplot_bev_attribute_rank <- battery_long |>
  count(attribute, response) |>
  mutate(pct = round(n / sum(n) * 100), .by = attribute) |>
  ggplot(aes(x = attribute, y = n, fill = response)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(pct, "%")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Unimportant" = "#A6761D",
      "Of little importance" = "#C2A56F",
      "Moderately important" = "#D3D3D3",
      "Important" = "#92B6D5",
      "Very important" = "#4682B4"
    )
  ) +
  labs(
    title = "Importance of Used BEV Attributes",
    x = "BEV Attributes",
    y = "Number of Respondents",
    fill = "Importance"
  ) +
  coord_flip() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = .5),
    axis.text.x = element_text(colour = "black", size = 9),
    axis.text.y = element_text(colour = "black", size = 9),
    axis.title = element_text(colour = "black", size = 12),
    legend.key = element_rect(fill = "white", colour = "white"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(colour = "black", size = 9),
    legend.text = element_text(colour = "black", size = 9),
    # panel.grid.major = element_line(colour = "#d3d3d3"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  guides(fill = guide_legend(reverse = T))

barplot_bev_attribute_rank

ggsave(
  plot = barplot_bev_attribute_rank,
  filename = here::here(
    'code',
    'output',
    "images",
    "barplot_bev_attribute_rank.png"
  ),
  width = 10,
  height = 4,
  dpi = 300
)

### ── Bar Chart By Socio-demographics --
data <- data |>
  mutate(
    hh_income_num = as.numeric(hh_income), # NAs for "prefer_not_answer"
    income_cat = case_when(
      hh_income_num < 25000 ~ "less than $25,000",
      hh_income_num >= 25000 & hh_income_num < 50000 ~ "$25,000–$49,999",
      hh_income_num >= 50000 & hh_income_num < 100000 ~ "$50,000–$99,999",
      hh_income_num >= 100000 & hh_income_num < 150000 ~ "$100,000-$149,999",

      hh_income_num >= 200000 ~ "$150,000 or more",

      TRUE ~ NA_character_
    ) |>
      factor(
        levels = c(
          "less than $25,000",
          "$25,000–$49,999",
          "$50,000–$99,999",
          "$100,000-$149,999",
          "$150,000 or more"
        )
      )
  )

# Check distribution
table(data$income_cat, useNA = "ifany")

# Rebuild battery_long with income_cat, drop NA income
battery_long_income <- data |>
  select(starts_with("battery_attribute_"), income_cat) |>
  filter(!is.na(income_cat)) |>
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

# Build chart
barplot_bev_attribute_rank_income <- battery_long_income |>
  count(income_cat, attribute, response) |>
  mutate(pct = round(n / sum(n) * 100), .by = c(income_cat, attribute)) |>
  ggplot(aes(x = attribute, y = pct, fill = response)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = paste0(pct, "%")),
    position = position_fill(vjust = 0.5),
    size = 2.5,
    color = "black",
    fontface = "bold"
  ) +
  facet_grid(. ~ income_cat, switch = "y") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Unimportant" = "#A6761D",
      "Of little importance" = "#C2A56F",
      "Moderately important" = "#D3D3D3",
      "Important" = "#92B6D5",
      "Very important" = "#4682B4"
    )
  ) +
  coord_flip() +
  labs(
    title = "Importance of Used BEV Attributes by Household Income",
    x = "BEV Attributes",
    y = "Percentage of Respondents",
    fill = "Importance"
  ) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = .5),
    strip.background = element_rect(fill = "#f0f0f0", colour = "black"),
    strip.text = element_text(colour = "black", size = 8, face = "bold"),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.text.y = element_text(colour = "black", size = 8),
    axis.title = element_text(colour = "black", size = 11),
    legend.key = element_rect(fill = "white", colour = "white"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(colour = "black", size = 9),
    legend.text = element_text(colour = "black", size = 9),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 11, face = "bold")
  ) +
  guides(fill = guide_legend(reverse = TRUE))

barplot_bev_attribute_rank_income

ggsave(
  plot = barplot_bev_attribute_rank_income,
  filename = here::here(
    'code',
    'output',
    "images",
    "barplot_bev_attribute_rank_income.png"
  ),
  width = 20,
  height = 4,
  dpi = 300
)


# Rebuild battery_long with veh_type
battery_long_veh_type <- data |>
  select(starts_with("battery_attribute_"), next_veh_style) |>
  filter(!is.na(next_veh_style)) |>
  pivot_longer(
    cols = starts_with("battery_attribute_"),
    names_to = "attribute",
    values_to = "response"
  ) |>
  mutate(
    attribute = factor(attr_labels[attribute], levels = attr_labels),
    response = factor(
      response,
      levels = importance_levels,
      labels = importance_labels
    )
  )

# Build chart
barplot_bev_attribute_rank_veh_type <- battery_long_veh_type |>
  filter(!is.na(response)) |>
  count(next_veh_style, attribute, response) |>
  mutate(pct = round(n / sum(n) * 100), .by = c(next_veh_style, attribute)) |>
  ggplot(aes(x = attribute, y = pct, fill = response)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = paste0(pct, "%")),
    position = position_fill(vjust = 0.5),
    size = 2.5,
    color = "black",
    fontface = "bold"
  ) +
  facet_grid(. ~ next_veh_style, switch = "y") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Unimportant" = "#A6761D",
      "Of little importance" = "#C2A56F",
      "Moderately important" = "#D3D3D3",
      "Important" = "#92B6D5",
      "Very important" = "#4682B4"
    )
  ) +
  coord_flip() +
  labs(
    title = "Importance of Used BEV Attributes by Next Vehicle Style",
    x = "BEV Attributes",
    y = "Percentage of Respondents",
    fill = "Importance"
  ) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = .5),
    strip.background = element_rect(fill = "#f0f0f0", colour = "black"),
    strip.text = element_text(colour = "black", size = 8, face = "bold"),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.text.y = element_text(colour = "black", size = 8),
    axis.title = element_text(colour = "black", size = 11),
    legend.key = element_rect(fill = "white", colour = "white"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(colour = "black", size = 9),
    legend.text = element_text(colour = "black", size = 9),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 11, face = "bold")
  ) +
  guides(fill = guide_legend(reverse = TRUE))

barplot_bev_attribute_rank_veh_type

ggsave(
  plot = barplot_bev_attribute_rank_veh_type,
  filename = here::here(
    'code',
    'output',
    "images",
    "barplot_bev_attribute_rank_veh_type.png"
  ),
  width = 12,
  height = 4,
  dpi = 300
)
