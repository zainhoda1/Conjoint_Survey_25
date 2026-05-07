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
      prime_group_label == "prime_long"  ~ "Extended Info",
      prime_group_label == "prime_short" ~ "Basic Info",
      TRUE ~ NA_character_
    )
  )

cat("N in data_nobev:", nrow(data_nobev), "\n")
cat("Treatment distribution:\n")
print(table(data_nobev$treatment, useNA = "always"))


# ── Step 1: LLM-Assisted Theme Coding ─────────────────────────────────────────
# Calls Anthropic API to classify each open-ended response into themes.
# Results are cached to avoid repeat API calls.

cache_path <- here(
  "code", "output", "model_output", "battery_analysis", "apollo",
  "0_nobev_themes_coded.parquet"
)

themes <- c(
  "price_cost",
  "range_anxiety",
  "charging_infrastructure",
  "battery_concern",
  "ev_skepticism",
  "used_vehicle_trust",
  "environmental_doubt",
  "other"
)

classify_response <- function(text, api_key) {
  prompt <- paste0(
    "You are classifying open-ended survey responses from a study on used battery electric vehicles (BEVs).\n\n",
    "A respondent was shown used BEV choice tasks and did not select any BEV. ",
    "They explained their reason in their own words.\n\n",
    "Classify the following response into ONE OR MORE of these themes. ",
    "A response can have multiple themes.\n\n",
    "Themes:\n",
    "- price_cost: Concerns about purchase price or running costs being too high\n",
    "- range_anxiety: Concerns about driving range or mileage being insufficient\n",
    "- charging_infrastructure: Lack of charging access, inconvenience of charging, or long charging times\n",
    "- battery_concern: Specific concerns about battery degradation, replacement costs, or battery reliability\n",
    "- ev_skepticism: General dislike or distrust of EVs, preference for gas/ICE vehicles, not interested in EVs at all\n",
    "- used_vehicle_trust: Distrust or hesitation specific to buying a USED vehicle (not EV-specific)\n",
    "- environmental_doubt: Doubts that EVs are actually better for the environment\n",
    "- other: Response does not fit any of the above\n\n",
    "Response: \"", text, "\"\n\n",
    "Return ONLY a JSON object with each theme as a key and 1 (applies) or 0 (does not apply) as value. ",
    "Example: {\"price_cost\":0,\"range_anxiety\":1,\"charging_infrastructure\":0,",
    "\"battery_concern\":0,\"ev_skepticism\":1,\"used_vehicle_trust\":0,",
    "\"environmental_doubt\":0,\"other\":0}"
  )

  response <- httr::POST(
    url = "https://api.anthropic.com/v1/messages",
    httr::add_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ),
    body = jsonlite::toJSON(list(
      model      = "claude-haiku-4-5-20251001",
      max_tokens = 200,
      messages   = list(list(role = "user", content = prompt))
    ), auto_unbox = TRUE),
    encode = "raw"
  )

  if (httr::status_code(response) != 200) {
    warning("API error for response: ", text)
    return(rep(NA_integer_, length(themes)) |> setNames(themes))
  }

  content <- httr::content(response, as = "parsed")
  json_str <- content$content[[1]]$text
  parsed   <- jsonlite::fromJSON(json_str)
  as.integer(parsed[themes])
}

# Load from cache if available, otherwise call API
if (file.exists(cache_path)) {
  cat("Loading cached theme classifications...\n")
  coded <- read_parquet(cache_path)
} else {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nchar(api_key) == 0) stop("ANTHROPIC_API_KEY not set in environment.")

  cat("Classifying", nrow(data_nobev), "responses via API...\n")

  theme_matrix <- map(
    seq_len(nrow(data_nobev)),
    function(i) {
      if (i %% 20 == 0) cat("  Processed", i, "/", nrow(data_nobev), "\n")
      res <- classify_response(data_nobev$no_bev_selected0[i], api_key)
      as.data.frame(t(res))
    }
  ) |> bind_rows()

  coded <- bind_cols(data_nobev, theme_matrix)
  write_parquet(coded, cache_path)
  cat("Saved to cache:", cache_path, "\n")
}


# ── Step 2: Crosstab and Chi-Square by Treatment ───────────────────────────────

theme_labels <- c(
  price_cost              = "Price / Cost",
  range_anxiety           = "Range Anxiety",
  charging_infrastructure = "Charging Infrastructure",
  battery_concern         = "Battery Concerns",
  ev_skepticism           = "General EV Skepticism",
  used_vehicle_trust      = "Used Vehicle Distrust",
  environmental_doubt     = "Environmental Doubt",
  other                   = "Other"
)

# Prevalence by treatment with chi-square p-value
crosstab <- map_dfr(themes, function(th) {
  tab <- coded %>%
    filter(!is.na(.data[[th]])) %>%
    group_by(treatment) %>%
    summarise(
      n_total = n(),
      n_theme = sum(.data[[th]], na.rm = TRUE),
      pct     = mean(.data[[th]], na.rm = TRUE),
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
      p_value < 0.01  ~ paste0(round(p_value, 3), " **"),
      p_value < 0.05  ~ paste0(round(p_value, 3), " *"),
      TRUE            ~ as.character(round(p_value, 3))
    )
  ) %>%
  select(-p_value) %>%
  rename(Theme = theme_label, `p-value` = p_sig)

cat("\n── Theme Prevalence by Treatment ──────────────────────────────\n")
print(crosstab_wide)

# GT table
n_basic    <- sum(coded$treatment == "Basic Info",    na.rm = TRUE)
n_extended <- sum(coded$treatment == "Extended Info", na.rm = TRUE)

gt_crosstab <- crosstab_wide %>%
  gt(rowname_col = "Theme") %>%
  cols_label(
    `Basic Info`    = paste0("Basic Info (n=", n_basic, ")"),
    `Extended Info` = paste0("Extended Info (n=", n_extended, ")"),
    `p-value`       = "Chi-sq p"
  ) %>%
  tab_header(
    title    = md("**Self-Reported Opt-Out Reasons by Information Treatment**"),
    subtitle = md(
      "Respondents who did not select any BEV — open-ended reason coded by LLM"
    )
  ) %>%
  tab_footnote(
    footnote  = "Each response may be assigned to multiple themes. Chi-square tests are unadjusted. * p<0.05, ** p<0.01, *** p<0.001.",
    locations = cells_title(groups = "subtitle")
  ) %>%
  cols_align(align = "center", columns = c(`Basic Info`, `Extended Info`, `p-value`)) %>%
  cols_align(align = "left",   columns = Theme) %>%
  tab_options(
    table.font.size           = px(13),
    table.font.names          = "Roboto Condensed",
    heading.align             = "left",
    column_labels.font.weight = "bold"
  ) %>%
  opt_stylize(style = 1, color = "blue")

gt_crosstab

gtsave(
  gt_crosstab,
  file = here(
    "code", "output", "model_output", "battery_analysis", "apollo",
    "0_nobev_themes_by_treatment.html"
  )
)


# ── Bar chart: theme prevalence by treatment ───────────────────────────────────

plot_data <- crosstab %>%
  mutate(
    theme_label = factor(theme_label, levels = rev(theme_labels)),
    treatment   = factor(treatment, levels = c("Basic Info", "Extended Info"))
  )

bar_optout_themes <- plot_data %>%
  ggplot(aes(x = theme_label, y = pct, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_dodge(width = 0.7),
    hjust    = -0.15,
    size     = 3,
    color    = "black"
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
    x     = NULL,
    y     = "% of Respondents",
    fill  = "Treatment"
  ) +
  theme_minimal_grid(font_family = "Roboto Condensed") +
  theme(
    panel.border     = element_rect(colour = "black", fill = NA, size = .5),
    axis.text        = element_text(colour = "black", size = 10),
    axis.title       = element_text(colour = "black", size = 11),
    legend.position  = "bottom",
    legend.title     = element_text(size = 11),
    legend.text      = element_text(size = 10),
    panel.background = element_blank(),
    plot.title       = element_text(size = 12, face = "bold")
  )

bar_optout_themes

ggsave(
  plot     = bar_optout_themes,
  filename = here(
    "code", "output", "images", "battery_analysis", "latent_class",
    "barplot_optout_themes_by_treatment.jpg"
  ),
  width  = 9,
  height = 5,
  dpi    = 300
)
