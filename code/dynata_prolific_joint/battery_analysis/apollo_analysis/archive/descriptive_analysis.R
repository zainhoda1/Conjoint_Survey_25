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
    cols = c(prob_class1, prob_class2, prob_class3),
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
      "Unimportant" = "#A6761D",
      "Of little importance" = "#C2A56F",
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
    "barplot_bev_attribute_rank_3c.jpg"
  ),
  width = 10,
  height = 4,
  dpi = 300
)


### ── Bar Chart By class assigned (hard assignment) ─────────────────────────

# n per assigned class for facet labels
class_assigned_n <- data |>
  count(prob_class_assign) |>
  mutate(
    class_label = paste0(
      str_to_title(str_replace(prob_class_assign, "class", "Class ")),
      " (n=",
      n,
      ")"
    )
  ) |>
  select(prob_class_assign, class_label) |>
  deframe()

# Long format using hard-assigned class
battery_long_assigned <- data |>
  select(respID, prob_class_assign, starts_with("battery_attribute_")) |>
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
    ),
    class = class_assigned_n[prob_class_assign]
  )

# Summary: pct within each class × attribute
battery_assigned_summary <- battery_long_assigned |>
  count(class, attribute, response) |>
  mutate(
    pct = n / sum(n),
    pct_text = round(pct * 100),
    .by = c(class, attribute)
  )

barplot_bev_attribute_rank_assigned <- battery_assigned_summary |>
  ggplot(aes(x = attribute, y = pct, fill = response)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(pct_text, "%")),
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
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~class, nrow = 1) +
  labs(
    title = "Importance of Used BEV Attributes by Latent Class (Assigned)",
    x = "BEV Attributes",
    y = "% of Respondents",
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

barplot_bev_attribute_rank_assigned

ggsave(
  plot = barplot_bev_attribute_rank_assigned,
  filename = here::here(
    'code',
    'output',
    'images',
    'battery_analysis',
    'latent_class',
    'barplot_bev_attribute_rank_3c_assigned.jpg'
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
    "battery_analysis",
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
    "battery_analysis",
    "barplot_bev_attribute_rank_veh_type.png"
  ),
  width = 12,
  height = 4,
  dpi = 300
)
