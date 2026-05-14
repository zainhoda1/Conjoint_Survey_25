source(here::here('code', 'setup.R'))

data_full <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint.parquet"
))

data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery.parquet"
))

data_model <- data_model %>%
  filter(
    # !is.na(FA_EV_benefit) &
    # !is.na(FA_EV_anxiety) &
    !is.na(ATT_range_anxiety) &
      !is.na(ATT_risktaker) &
      !is.na(hhincome_num_10k) &
      !is.na(EV_charger) &
      # !is.na(EV_neighbor) &
      # !is.na(knowledge_ev) &
      # !is.na(knowledge_subsidy) &
      # !is.na(Veh_hh_count) &
      !is.na(Veh_hh_fuel) &
      # !is.na(Veh_primary_refuel_monthly) &
      !is.na(Veh_primary_range) &
      !is.na(ATT_EVB_environment) &
      !is.na(ATT_EVB_function) &
      !is.na(vehicle_typesuv)
  )

data_nobev <- data_full %>%
  filter(psid %in% data_model$psid) %>%
  filter(!is.na(no_bev_selected0)) %>%
  select(psid, prime_group_label, no_bev_selected0) %>%
  mutate(
    treatment = case_when(
      prime_group_label == "prime_long" ~ "Extended Info",
      prime_group_label == "prime_short" ~ "Basic Info",
      TRUE ~ NA_character_
    )
  )


cat("N in data_nobev:", nrow(data_nobev), "\n")
cat("Treatment distribution:\n")
print(table(data_nobev$treatment, useNA = "always"))


# ── Step 1: Load LLM Theme Classifications ────────────────────────────────────
# Themes coded by Claude (claude-sonnet-4-6) directly from 146 responses.
# Categories: price_cost, range_anxiety, charging_infrastructure,
#             battery_concern, ev_skepticism, used_vehicle_trust,
#             environmental_doubt, other (multi-label, not mutually exclusive)

themes <- c(
  "conventional_gas_vehicle_enthusiasm",
  "simply_not_interested",
  "price_cost",
  "range_anxiety",
  "charging_infrastructure",
  "battery_concern",
  "ev_skepticism",
  "ev_distrust",
  "used_vehicle_distrust",
  "environmental_doubt",
  "other"
)

coded <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "0_nobev_themes_coded.parquet"
)) %>%
  mutate(n_themes = rowSums(select(., all_of(themes)), na.rm = TRUE))
cat("Loaded", nrow(coded), "coded responses.\n")
cat("Theme counts:\n")
print(colSums(coded[, themes]))


# ── Step 2: Crosstab and Chi-Square by Treatment ───────────────────────────────

theme_labels <- c(
  conventional_gas_vehicle_enthusiasm = "Conventional Gas Vehicle Enthusiasm",
  simply_not_interested = "Simply Not Interested in BEVs",
  price_cost = "Price / Cost",
  range_anxiety = "Range Anxiety",
  charging_infrastructure = "Charging Infrastructure",
  battery_concern = "Battery Concerns",
  ev_skepticism = "General EV Skepticism",
  ev_distrust = "EV Distrust",
  used_vehicle_distrust = "Used Vehicle Distrust",
  environmental_doubt = "Environmental Doubt",
  other = "Other"
)

# Prevalence by treatment with chi-square p-value
crosstab <- purrr::map_dfr(themes, function(th) {
  tab <- coded %>%
    filter(!is.na(.data[[th]])) %>%
    group_by(treatment) %>%
    summarise(
      n_total = n(),
      n_theme = sum(.data[[th]], na.rm = TRUE),
      pct = mean(.data[[th]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(theme = th, theme_label = theme_labels[th])

  ct <- table(coded$treatment, coded[[th]])
  if (all(dim(ct) == c(2, 2))) {
    chi <- chisq.test(ct, correct = FALSE)
    tab$p_value <- chi$p.value
  } else {
    tab$p_value <- NA_real_
  }
  tab
})

# Wide format for printing
crosstab_wide <- crosstab %>%
  mutate(
    cell = paste0(n_theme, " (", scales::percent(pct, accuracy = 0.1), ")")
  ) %>%
  select(theme_label, treatment, cell, p_value) %>%
  pivot_wider(names_from = treatment, values_from = cell) %>%
  mutate(
    p_sig = case_when(
      p_value < 0.001 ~ "<0.001 ***",
      p_value < 0.01 ~ paste0(round(p_value, 3), " **"),
      p_value < 0.05 ~ paste0(round(p_value, 3), " *"),
      TRUE ~ as.character(round(p_value, 3))
    )
  ) %>%
  select(-p_value) %>%
  rename(Theme = theme_label, `p-value` = p_sig)

cat("\n── Theme Prevalence by Treatment ──────────────────────────────\n")
print(crosstab_wide)

# GT table
n_basic <- sum(coded$treatment == "Basic Info", na.rm = TRUE)
n_extended <- sum(coded$treatment == "Extended Info", na.rm = TRUE)

gt_crosstab <- crosstab_wide %>%
  gt(rowname_col = "Theme") %>%
  cols_label(
    `Basic Info` = paste0("Basic Info (n=", n_basic, ")"),
    `Extended Info` = paste0("Extended Info (n=", n_extended, ")"),
    `p-value` = "Chi-sq p"
  ) %>%
  tab_header(
    title = md("**Self-Reported Opt-Out Reasons by Information Treatment**"),
    subtitle = md(
      "Respondents who did not select any BEV among six choice tasks"
    )
  ) %>%
  tab_footnote(
    footnote = "Each response may be assigned to multiple themes. Chi-square tests are unadjusted. * p<0.05, ** p<0.01, *** p<0.001.",
    locations = cells_title(groups = "subtitle")
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Basic Info`, `Extended Info`, `p-value`)
  ) %>%
  cols_align(align = "left", columns = Theme) %>%
  tab_options(
    table.font.size = px(13),
    table.font.names = "Roboto Condensed",
    heading.align = "left",
    column_labels.font.weight = "bold"
  ) %>%
  opt_stylize(style = 1, color = "blue")

gt_crosstab

gtsave(
  gt_crosstab,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_nobev_themes_by_treatment.html"
  )
)


# ── Bar chart: theme prevalence by treatment ───────────────────────────────────

plot_data <- crosstab %>%
  mutate(
    theme_label = factor(theme_label, levels = rev(theme_labels)),
    treatment = factor(treatment, levels = c("Basic Info", "Extended Info"))
  )

bar_optout_themes <- plot_data %>%
  ggplot(aes(x = theme_label, y = pct, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_dodge(width = 0.7),
    hjust = -0.15,
    size = 3,
    color = "black"
  ) +
  scale_y_continuous(
    labels = label_percent(),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(
    values = c("Basic Info" = "#92B6D5", "Extended Info" = "#4682B4")
  ) +
  coord_flip() +
  labs(
    title = "Self-Reported Opt-Out Reasons by Information Treatment",
    x = NULL,
    y = "% of Respondents",
    fill = "Treatment"
  ) +
  theme_minimal_grid(font_family = "Roboto Condensed") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = .5),
    axis.text = element_text(colour = "black", size = 10),
    axis.title = element_text(colour = "black", size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.background = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  )

bar_optout_themes

ggsave(
  plot = bar_optout_themes,
  filename = here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "latent_class",
    "barplot_optout_themes_by_treatment.jpg"
  ),
  width = 9,
  height = 5,
  dpi = 300
)
