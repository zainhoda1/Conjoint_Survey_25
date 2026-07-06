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
    !is.na(ATT_range_anxiety) &
      !is.na(ATT_risktaker) &
      !is.na(hhincome_num_10k) &
      !is.na(EV_charger) &
      !is.na(Veh_hh_fuel) &
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


cat("N in data_nobev (model sample):", nrow(data_nobev), "\n")
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


# write coded to as a exc for manual review if needed
write_csv(
  coded,
  here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_nobev_themes_coded.csv"
  )
)

# ── Check for new uncoded responses ──────────────────────────────────────────
new_uncoded <- data_nobev %>%
  filter(!psid %in% coded$psid)

if (nrow(new_uncoded) > 0) {
  cat(
    "\nWARNING:",
    nrow(new_uncoded),
    "new no-BEV responses not yet LLM-coded.\n"
  )
  cat("Saving to: 0_nobev_themes_to_code.parquet\n")

  write_parquet(
    new_uncoded,
    here(
      "code",
      "output",
      "model_output",
      "battery_analysis",
      "apollo",
      "0_nobev_themes_to_code.parquet"
    )
  )
  cat(
    "Re-run LLM coding on this file and append to 0_nobev_themes_coded.parquet.\n\n"
  )
  # Restrict analysis to already-coded responses
  coded <- coded %>% filter(psid %in% data_nobev$psid)
} else {
  cat("All no-BEV responses are coded. Proceeding with full dataset.\n")
}

cat("Using", nrow(coded), "coded responses for analysis.\n")
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

coded <- coded %>%
  filter(psid %in% data_nobev$psid)

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

# Register Roboto Condensed via showtext (not installed as a system font)
if (!requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext")
}
library(showtext)
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 300)

# Order themes by total frequency across both treatment groups (highest at top)
theme_order <- crosstab %>%
  group_by(theme_label) %>%
  summarise(total_n = sum(n_theme), .groups = "drop") %>%
  arrange(total_n) %>%
  pull(theme_label)

plot_data <- crosstab %>%
  mutate(
    theme_label = factor(theme_label, levels = theme_order),
    treatment = factor(treatment, levels = c("Basic Info", "Extended Info"))
  )

bar_optout_themes <- plot_data %>%
  ggplot(aes(x = theme_label, y = pct, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(
      label = paste0(
        "(n=",
        n_theme,
        " | ",
        scales::percent(pct, accuracy = 0.1),
        ")"
      )
    ),
    position = position_dodge(width = 0.7),
    hjust = -0.05,
    size = 2.5,
    color = "black"
  ) +
  scale_y_continuous(
    labels = label_percent(),
    expand = expansion(mult = c(0, 0.28))
  ) +
  scale_fill_manual(
    values = c("Basic Info" = "#92B6D5", "Extended Info" = "#4682B4"),
    labels = c(
      "Basic Info" = paste0("Basic Info (n=", n_basic, ")"),
      "Extended Info" = paste0("Extended Info (n=", n_extended, ")")
    )
  ) +
  coord_flip() +
  labs(
    title = NULL,
    x = NULL,
    y = "% of Respondents",
    fill = "Treatment"
  ) +
  theme_minimal_grid(font_family = "Roboto Condensed") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = .5),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
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
  width = 7,
  height = 4,
  dpi = 300
)


# ── Word cloud of battery-related concerns ────────────────────────────────────
if (!requireNamespace("tidytext", quietly = TRUE)) {
  install.packages("tidytext", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("ggwordcloud", quietly = TRUE)) {
  install.packages("ggwordcloud", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("textstem", quietly = TRUE)) {
  install.packages("textstem", repos = "https://cloud.r-project.org")
}
library(tidytext)
library(ggwordcloud)
library(textstem)

# Battery-related = any theme flag for battery/range/charging OR keyword hit in raw text
battery_kw <- "\\b(batter(y|ies)|charg|kwh|range|degrad|replace|lifespan|fire)"
battery_responses <- coded %>%
  mutate(
    kw_hit = grepl(battery_kw, no_bev_selected0, ignore.case = TRUE),
    battery_related = battery_concern == 1 |
      range_anxiety == 1 |
      charging_infrastructure == 1 |
      kw_hit
  ) %>%
  filter(battery_related)

cat("\n── Battery-related responses ──\n")
cat(
  "N =",
  nrow(battery_responses),
  "of",
  nrow(coded),
  "(",
  scales::percent(nrow(battery_responses) / nrow(coded), accuracy = 0.1),
  ")\n"
)

# Drop generic EV/vehicle vocabulary so the cloud surfaces *concerns*, not the topic
custom_stop <- tibble(
  word = c(
    "ev",
    "evs",
    "electric",
    "vehicle",
    "vehicles",
    "car",
    "cars",
    "bev",
    "bevs",
    "buy",
    "get",
    "would",
    "much",
    "really",
    "ive",
    "im",
    "dont",
    "doesnt",
    "isnt",
    "just",
    "thing",
    "things",
    "lot",
    "people",
    "want",
    "like"
  )
)

word_counts <- battery_responses %>%
  select(psid, no_bev_selected0) %>%
  unnest_tokens(word, no_bev_selected0) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop, by = "word") %>%
  filter(!grepl("^[0-9]+$", word), nchar(word) > 2) %>%
  mutate(word = lemmatize_words(word)) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop, by = "word") %>%
  count(word, sort = TRUE)

cat("Top 20 words:\n")
print(head(word_counts, 20))

theme_map <- tribble(
  ~word            , ~theme            ,
  "battery"        , "Battery"         , "cell"         , "Battery"         ,
  "lithium"        , "Battery"         , "capacity"     , "Battery"         ,
  "batt"           , "Battery"         , "tesla"        , "Battery"         ,
  # Charging
  "charge"         , "Charging"        , "charger"      , "Charging"        ,
  "recharge"       , "Charging"        , "station"      , "Charging"        ,
  "outlet"         , "Charging"        , "plug"         , "Charging"        ,
  "infrastructure" , "Charging"        , "wall"         , "Charging"        ,
  "access"         , "Charging"        , "rural"        , "Charging"        ,
  "home"           , "Charging"        , "electrical"   , "Charging"        ,
  "location"       , "Charging"        , "availability" , "Charging"        ,
  "accessibility"  , "Charging"        , "ample"        , "Charging"        ,
  # Range / distance
  "range"          , "Range"           , "distance"     , "Range"           ,
  "mile"           , "Range"           , "mileage"      , "Range"           ,
  "travel"         , "Range"           , "drive"        , "Range"           ,
  "trip"           , "Range"           , "far"          , "Range"           ,
  "highway"        , "Range"           , "road"         , "Range"           ,
  "limit"          , "Range"           , "low"          , "Range"           ,
  "run"            , "Range"           , "stop"         , "Range"           ,
  "strand"         , "Range"           , "short"        , "Range"           ,
  "destination"    , "Range"           , "route"        , "Range"           ,
  "terrain"        , "Range"           ,
  # Cost
  "expensive"      , "Cost"            , "price"        , "Cost"            ,
  "cost"           , "Cost"            , "purchase"     , "Cost"            ,
  "money"          , "Cost"            , "afford"       , "Cost"            ,
  "pay"            , "Cost"            , "cheap"        , "Cost"            ,
  "spend"          , "Cost"            ,
  # Lifespan / reliability / safety
  "life"           , "Lifespan/Safety" , "lifespan"     , "Lifespan/Safety" ,
  "degradation"    , "Lifespan/Safety" , "replace"      , "Lifespan/Safety" ,
  "replacement"    , "Lifespan/Safety" , "wear"         , "Lifespan/Safety" ,
  "last"           , "Lifespan/Safety" , "reliable"     , "Lifespan/Safety" ,
  "reliability"    , "Lifespan/Safety" , "fire"         , "Lifespan/Safety" ,
  "safety"         , "Lifespan/Safety" , "safe"         , "Lifespan/Safety" ,
  "issue"          , "Lifespan/Safety" , "degrade"      , "Lifespan/Safety" ,
  "degradation"    , "Lifespan/Safety" , "refurbish"    , "Lifespan/Safety" ,
  "risk"           , "Lifespan/Safety" , "concern"      , "Lifespan/Safety" ,
  "worry"          , "Lifespan/Safety" , "trust"        , "Lifespan/Safety" ,
  "distrust"       , "Lifespan/Safety" , "health"       , "Lifespan/Safety" ,
  "maintain"       , "Lifespan/Safety" , "dispose"      , "Lifespan/Safety" ,
  "waste"          , "Lifespan/Safety" , "dangerous"    , "Lifespan/Safety" ,
  "poor"           , "Lifespan/Safety" , "uncertainty"  , "Lifespan/Safety" ,
  "coverage"       , "Lifespan/Safety" , "negative"     , "Lifespan/Safety" ,
  "rely"           , "Lifespan/Safety" , "afraid"       , "Lifespan/Safety" ,
  "warranty"       , "Lifespan/Safety" , "cold"         , "Lifespan/Safety" ,
  "weather"        , "Lifespan/Safety" , "winter"       , "Lifespan/Safety" ,
  "temperature"    , "Lifespan/Safety" ,
  # Alt fuels
  "gas"            , "Alt Fuels"       , "gasoline"     , "Alt Fuels"       ,
  "hybrid"         , "Alt Fuels"       , "fuel"         , "Alt Fuels"       ,
  "diesel"         , "Alt Fuels"       , "engine"       , "Alt Fuels"       ,
  "tank"           , "Alt Fuels"       , "gallon"       , "Alt Fuels"       ,
  "conventional"   , "Alt Fuels"       ,
  # Time / inconvenience
  "time"           , "Time"            , "slow"         , "Time"            ,
  "wait"           , "Time"            , "hour"         , "Time"            ,
  "fast"           , "Time"            , "quick"        , "Time"            ,
  "inconvenient"   , "Time"            , "minute"       , "Time"            ,
  "month"          , "Time"            ,
  # Power / performance
  "power"          , "Power"           , "performance"  , "Power"           ,
  "electricity"    , "Power"           , "energy"       , "Power"
)

theme_palette <- c(
  "Battery" = "#1F3D5C",
  "Charging" = "#4682B4",
  "Range" = "#2E8B57",
  "Cost" = "#B22222",
  "Lifespan/Safety" = "#8B008B",
  "Alt Fuels" = "#8B4513",
  "Time" = "#DAA520",
  "Power" = "#008080",
  "Other" = "#808080"
)

wc_data <- word_counts %>%
  inner_join(theme_map, by = "word") %>%
  slice_max(n, n = 80)

set.seed(42)
wc_battery <- wc_data %>%
  ggplot(aes(label = word, size = n, color = theme)) +
  geom_text_wordcloud_area(
    family = "Roboto Condensed",
    shape = "circle",
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 22) +
  scale_color_manual(values = theme_palette) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

# Theme → color reference (printed since no in-plot legend)
cat("\n── Theme color code ──\n")
purrr::iwalk(theme_palette, ~ cat(sprintf("  %-18s %s\n", .y, .x)))

wc_battery

ggsave(
  plot = wc_battery,
  filename = here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "latent_class",
    "wordcloud_battery_concerns.jpg"
  ),
  width = 3,
  height = 2,
  dpi = 350
)


# ── Standalone theme color legend ─────────────────────────────────────────────
themes_used <- names(theme_palette)[names(theme_palette) != "Other"]
legend_data <- tibble(
  theme = factor(themes_used, levels = themes_used),
  idx = seq_along(themes_used)
)

legend_fig <- legend_data %>%
  ggplot(aes(x = idx, y = 1)) +
  geom_point(aes(color = theme), size = 10, shape = 15) +
  geom_text(
    aes(label = theme),
    y = 0.55,
    vjust = 1,
    family = "Roboto Condensed",
    fontface = "bold",
    size = 4
  ) +
  scale_color_manual(values = theme_palette) +
  scale_x_continuous(expand = expansion(add = 0.5)) +
  scale_y_continuous(limits = c(0.2, 1.3)) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  )

legend_fig

ggsave(
  plot = legend_fig,
  filename = here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "thematic_analysis",
    "wordcloud_battery_concerns_legend.jpg"
  ),
  width = 12,
  height = 1.4,
  dpi = 300
)
