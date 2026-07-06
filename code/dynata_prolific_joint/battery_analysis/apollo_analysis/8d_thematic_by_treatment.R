source(here::here('code', 'setup.R'))

# Treatment comparison of detailed thematic codings (25 sub-themes, 10 active
# parents). Uses the full N = 251 thematic-analysis sample produced by
# 8a_recode_themes_detailed.R, not the smaller modeling subset — see the
# methods discussion: thematic analysis includes every respondent who gave a
# valid open-ended answer, not just those with complete covariates for the
# choice model.

detailed <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "0_nobev_themes_coded_detailed.parquet"
))

cat("Loaded", nrow(detailed), "coded responses.\n")
cat("Treatment distribution:\n")
print(table(detailed$treatment, useNA = "always"))

n_basic <- sum(detailed$treatment == "Basic Info", na.rm = TRUE)
n_extended <- sum(detailed$treatment == "Extended Info", na.rm = TRUE)

# ── Codebook labels ──────────────────────────────────────────────────────────
subtheme_meta <- tribble(
  ~subtheme                    , ~parent_id                 , ~subtheme_label                         ,
  "charging_home_access"       , "parent_charging"          , "Lack of Home Chargers"                 ,
  "charging_public_lack"       , "parent_charging"          , "Lack of Public Chargers"               ,
  "charging_time"              , "parent_charging"          , "Charging Time"                         ,
  "range_daily_insufficient"   , "parent_range"             , "Daily Range Insufficient"              ,
  "range_long_trip"            , "parent_range"             , "Long-Trip Range Insufficient"          ,
  "cost_upfront_purchase"      , "parent_cost"              , "Upfront Purchase"                      ,
  "cost_battery_replacement"   , "parent_cost"              , "Battery Replacement Cost"              ,
  "cost_electricity_operating" , "parent_cost"              , "Operation Cost"                        ,
  "cost_maintenance_insurance" , "parent_cost"              , "Maintenance / Repair Cost"             ,
  "cost_general_value"         , "parent_cost"              , "Generic 'Not Worth It'"                ,
  "battery_degradation"        , "parent_battery_concern"   , "Degradation / Lifespan"                ,
  "battery_safety"             , "parent_battery_concern"   , "Safety (Fire, Crash)"                  ,
  "battery_weather_cold"       , "parent_battery_concern"   , "Cold-Weather Low Performance"          ,
  "ev_tech_immature"           , "parent_ev_distrust"       , "Technology Immature"                   ,
  "ev_general_distrust"        , "parent_ev_distrust"       , "General Distrust"                      ,
  "env_grid_source"            , "parent_environmental"     , "Grid Source"                           ,
  "env_mining_manufacturing"   , "parent_environmental"     , "Mining / Manufacturing"                ,
  "env_disposal_recycle"       , "parent_environmental"     , "Battery Disposal / Recycle"            ,
  "env_overall_skepticism"     , "parent_environmental"     , "Overall 'Green' Skepticism"            ,
  "used_vehicle_distrust"      , "parent_used_distrust"     , "Used Vehicle Distrust"                 ,
  "simply_not_interested"      , "parent_simply_not_int"    , "Simply Not Interested"                 ,
  "gas_engine_love"            , "parent_gas_enthusiasm"    , "Love of Gas Engines"                   ,
  "missing_ICE_features"       , "parent_gas_enthusiasm"    , "BEVs Missing Certain Vehicle Features" ,
  "knowledge_limited"          , "parent_limited_knowledge" , "Limited Knowledge on BEVs"             ,
  "other"                      , "parent_other"             , "Other"
)

parent_meta <- tribble(
  ~parent_id                 , ~parent_label               ,
  "parent_charging"          , "Charging Inconvenience"    ,
  "parent_range"             , "Range Anxiety"             ,
  "parent_cost"              , "Economic Barriers"         ,
  "parent_battery_concern"   , "Battery Concerns"          ,
  "parent_ev_distrust"       , "EV Distrust"               ,
  "parent_environmental"     , "Environmental Concerns"    ,
  "parent_used_distrust"     , "Used Vehicle Distrust"     ,
  "parent_simply_not_int"    , "Simply Not Interested"     ,
  "parent_gas_enthusiasm"    , "Gas Vehicle Enthusiasm"    ,
  "parent_limited_knowledge" , "Limited Knowledge on BEVs" ,
  "parent_other"             , "Other"
)

# Drop zero-count items (so empty rows don't pollute the tables)
sub_counts <- sapply(subtheme_meta$subtheme, function(s) sum(detailed[[s]]))
subtheme_meta <- subtheme_meta %>%
  mutate(total_n = sub_counts[subtheme]) %>%
  filter(total_n > 0)

parent_counts <- sapply(parent_meta$parent_id, function(p) sum(detailed[[p]]))
parent_meta <- parent_meta %>%
  mutate(total_n = parent_counts[parent_id]) %>%
  filter(total_n > 0)

# ── Helper: build prevalence × treatment crosstab with chi-square p-value ────
crosstab_by_treatment <- function(theme_cols, label_lookup, label_col_name) {
  purrr::map_dfr(theme_cols, function(th) {
    tab <- detailed %>%
      filter(!is.na(treatment), !is.na(.data[[th]])) %>%
      group_by(treatment) %>%
      summarise(
        n_total = n(),
        n_theme = sum(.data[[th]], na.rm = TRUE),
        pct = mean(.data[[th]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(theme = th, theme_label = label_lookup[[th]])

    ct <- table(detailed$treatment, detailed[[th]])
    if (all(dim(ct) == c(2, 2))) {
      # Use Fisher's exact when any expected cell < 5 (rare-theme guard);
      # otherwise plain chi-square without continuity correction.
      expected <- chisq.test(ct, correct = FALSE)$expected
      tab$p_value <- if (any(expected < 5)) {
        fisher.test(ct)$p.value
      } else {
        chisq.test(ct, correct = FALSE)$p.value
      }
    } else {
      tab$p_value <- NA_real_
    }
    tab
  })
}

# ── Sub-theme level ──────────────────────────────────────────────────────────
sub_lookup <- setNames(subtheme_meta$subtheme_label, subtheme_meta$subtheme)
crosstab_sub <- crosstab_by_treatment(
  subtheme_meta$subtheme,
  sub_lookup,
  "subtheme_label"
) %>%
  left_join(
    subtheme_meta %>% select(theme = subtheme, parent_id),
    by = "theme"
  ) %>%
  left_join(parent_meta %>% select(parent_id, parent_label), by = "parent_id")

# ── Parent level ─────────────────────────────────────────────────────────────
par_lookup <- setNames(parent_meta$parent_label, parent_meta$parent_id)
crosstab_par <- crosstab_by_treatment(
  parent_meta$parent_id,
  par_lookup,
  "parent_label"
)

# ── Wide-format printable tables ─────────────────────────────────────────────
fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "<0.001 ***",
    p < 0.01 ~ paste0(sprintf("%.3f", p), " **"),
    p < 0.05 ~ paste0(sprintf("%.3f", p), " *"),
    p < 0.10 ~ paste0(sprintf("%.3f", p), " ."),
    TRUE ~ sprintf("%.3f", p)
  )
}

widen <- function(ct, theme_col, extra_cols = NULL) {
  out <- ct %>%
    mutate(
      cell = paste0(n_theme, " (", scales::percent(pct, accuracy = 0.1), ")")
    ) %>%
    select(all_of(c(extra_cols, theme_col, "treatment", "cell", "p_value"))) %>%
    pivot_wider(names_from = treatment, values_from = cell) %>%
    mutate(p_sig = fmt_p(p_value)) %>%
    select(-p_value) %>%
    arrange(desc(`Extended Info`)) # arrange visual descending by Extended pct
  out
}

table_sub <- widen(crosstab_sub, "theme_label", c("parent_label")) %>%
  rename(Theme = theme_label, Parent = parent_label, `p-value` = p_sig) %>%
  select(Parent, Theme, `Basic Info`, `Extended Info`, `p-value`)

table_par <- widen(crosstab_par, "theme_label") %>%
  rename(Theme = theme_label, `p-value` = p_sig)

cat("\n── Parent-level prevalence by treatment ───────────────────────\n")
print(table_par)
cat("\n── Sub-theme-level prevalence by treatment ────────────────────\n")
print(table_sub, n = Inf)

# ── GT tables ────────────────────────────────────────────────────────────────
title_cell <- md(
  "**Opt-Out Reasons by Information Treatment (Detailed Codebook)**"
)
subtitle_cell <- md(
  paste0(
    "N = ",
    nrow(detailed),
    " (Basic n=",
    n_basic,
    ", Extended n=",
    n_extended,
    "). ",
    "Each response may carry multiple sub-themes."
  )
)

footnote_text <- paste0(
  "Chi-square tests are unadjusted (Fisher's exact when any expected cell <5). ",
  "* p<0.05, ** p<0.01, *** p<0.001, . p<0.10."
)

# Parent-level table
gt_par <- table_par %>%
  gt(rowname_col = "Theme") %>%
  cols_label(
    `Basic Info` = paste0("Basic Info (n=", n_basic, ")"),
    `Extended Info` = paste0("Extended Info (n=", n_extended, ")"),
    `p-value` = "Chi-sq / Fisher p"
  ) %>%
  tab_header(title = title_cell, subtitle = md("Parent themes")) %>%
  tab_footnote(
    footnote = footnote_text,
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

# Sub-theme-level table, grouped by parent
gt_sub <- table_sub %>%
  group_by(Parent) %>%
  gt(rowname_col = "Theme") %>%
  cols_label(
    `Basic Info` = paste0("Basic Info (n=", n_basic, ")"),
    `Extended Info` = paste0("Extended Info (n=", n_extended, ")"),
    `p-value` = "Chi-sq / Fisher p"
  ) %>%
  tab_header(
    title = title_cell,
    subtitle = md("Sub-themes (grouped by parent)")
  ) %>%
  tab_footnote(
    footnote = footnote_text,
    locations = cells_title(groups = "subtitle")
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Basic Info`, `Extended Info`, `p-value`)
  ) %>%
  cols_align(align = "left", columns = Theme) %>%
  tab_options(
    table.font.size = px(12),
    table.font.names = "Roboto Condensed",
    heading.align = "left",
    row_group.font.weight = "bold",
    column_labels.font.weight = "bold"
  ) %>%
  opt_stylize(style = 1, color = "blue")

gtsave(
  gt_par,
  file = here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "thematic_analysis",
    "0_nobev_detailed_parent_themes_by_treatment.html"
  )
)
gtsave(
  gt_sub,
  file = here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "thematic_analysis",
    "0_nobev_detailed_subthemes_by_treatment.html"
  )
)

# ── Bar charts (one per level) ───────────────────────────────────────────────
if (!requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext")
}
if (!requireNamespace("ggtext", quietly = TRUE)) {
  install.packages("ggtext", repos = "https://cloud.r-project.org")
}
library(showtext)
library(ggtext)
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 300)

# Parent color palette — same hex codes as 8b_thematic_network.R, so a viewer
# moving between the network figure and the bar chart sees the same theme color.
parent_color_tbl <- tribble(
  ~parent_id                 , ~color    ,
  "parent_range"             , "#2E8B57" ,
  "parent_charging"          , "#4682B4" ,
  "parent_cost"              , "#B22222" ,
  "parent_battery_concern"   , "#1F3D5C" ,
  "parent_environmental"     , "#556B2F" ,
  "parent_ev_distrust"       , "#8B008B" ,
  "parent_used_distrust"     , "#A0522D" ,
  "parent_simply_not_int"    , "#708090" ,
  "parent_gas_enthusiasm"    , "#8B4513" ,
  "parent_limited_knowledge" , "#DAA520" ,
  "parent_other"             , "#000000"
)

# Color lookup keyed by display label.
# Parent chart: each parent's own color.
parent_color_lookup <- parent_meta %>%
  left_join(parent_color_tbl, by = "parent_id") %>%
  select(label = parent_label, color) %>%
  tibble::deframe()
# Sub-theme chart: each sub-theme inherits its parent's color.
sub_color_lookup <- subtheme_meta %>%
  left_join(parent_color_tbl, by = "parent_id") %>%
  select(label = subtheme_label, color) %>%
  tibble::deframe()

make_bar <- function(
  crosstab_long,
  level_name,
  color_lookup,
  order_levels = NULL
) {
  # `order_levels` lets the caller pin the theme order (e.g. to group sub-themes
  # by parent so same-color labels sit together). When NULL, fall back to the
  # default: ascending total frequency, so the highest-frequency theme appears
  # at the top of the horizontal bars after coord_flip().
  if (is.null(order_levels)) {
    ord <- crosstab_long %>%
      group_by(theme_label) %>%
      summarise(total_n = sum(n_theme), .groups = "drop") %>%
      arrange(total_n) %>%
      pull(theme_label)
  } else {
    ord <- order_levels
  }

  # Star annotations for significant differences
  sig_lookup <- crosstab_long %>%
    distinct(theme_label, p_value) %>%
    mutate(
      sig = case_when(
        is.na(p_value) ~ "",
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        p_value < 0.10 ~ ".",
        TRUE ~ ""
      )
    )

  pd <- crosstab_long %>%
    left_join(sig_lookup %>% select(theme_label, sig), by = "theme_label") %>%
    mutate(
      theme_label_disp = paste0(
        theme_label,
        ifelse(sig != "", paste0(" ", sig), "")
      ),
      treatment = factor(treatment, levels = c("Basic Info", "Extended Info")),
      # Look up parent color by the original (un-starred) theme label
      color = unname(color_lookup[as.character(theme_label)]),
      color = ifelse(is.na(color), "#000000", color),
      # Markdown label: colored bullet + bold colored theme name.
      # ggtext::element_markdown() renders the HTML on the axis.
      label_md = paste0(
        "<span style='color:",
        color,
        ";'>● <b>",
        theme_label_disp,
        "</b></span>"
      )
    )

  # Order the markdown labels in the same sequence as `ord` (lowest total at top
  # of the horizontal bars after coord_flip).
  disp_to_md <- pd %>% distinct(theme_label, label_md) %>% tibble::deframe()
  pd$label_md <- factor(pd$label_md, levels = unname(disp_to_md[ord]))

  ggplot(pd, aes(x = label_md, y = pct, fill = treatment)) +
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
      expand = expansion(mult = c(0, 0.32))
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
      x = NULL,
      y = "% of Respondents",
      fill = "Treatment",
      caption = "Significance markers next to theme name: . p<0.10  * p<0.05  ** p<0.01  *** p<0.001 (chi-sq / Fisher's exact)."
    ) +
    theme_minimal_grid(font_family = "Roboto Condensed") +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = .5),
      axis.text.x = element_text(colour = "black", size = 11),
      # After coord_flip, axis.text.y holds the theme labels — render markdown
      # so the per-label color spans take effect.
      axis.text.y = element_markdown(size = 11, family = "Roboto Condensed"),
      axis.title = element_text(colour = "black", size = 12),
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      panel.background = element_blank(),
      plot.caption = element_text(size = 9, color = "grey30")
    )
}

bar_par <- make_bar(crosstab_par, "parent", color_lookup = parent_color_lookup)

# Sub-theme order: group by parent (so same-color labels stay together) and
# rank parents by their unique-respondent rollup (parent_meta$total_n) — NOT by
# summing child mentions, because multi-label respondents make that ambiguous
# and two parents can tie on summed mentions even when their unique-respondent
# totals differ (e.g. Charging 11+20+33=64 vs Range 21+43=64 both sum to 64,
# which would interleave their sub-themes). Using parent_rank as a strict
# primary key guarantees contiguous blocks regardless of ties; theme_n is the
# secondary key. Ascending so that the largest parent / largest sub-theme ends
# at the top after coord_flip().
parent_rank_tbl <- parent_meta %>%
  arrange(total_n) %>%
  mutate(parent_rank = row_number()) %>%
  select(parent_id, parent_rank)

sub_order <- crosstab_sub %>%
  filter(!is.na(theme_label)) %>%
  group_by(parent_id, theme_label) %>%
  summarise(theme_n = sum(n_theme), .groups = "drop") %>%
  left_join(parent_rank_tbl, by = "parent_id") %>%
  arrange(parent_rank, theme_n) %>%
  pull(theme_label)

bar_sub <- make_bar(
  crosstab_sub %>% filter(!is.na(theme_label)),
  "subtheme",
  color_lookup = sub_color_lookup,
  order_levels = sub_order
)

ggsave(
  plot = bar_par,
  filename = here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "thematic_analysis",
    "barplot_detailed_parent_themes_by_treatment.jpg"
  ),
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  plot = bar_sub,
  filename = here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "thematic_analysis",
    "barplot_detailed_subthemes_by_treatment.jpg"
  ),
  width = 8.5,
  height = 9,
  dpi = 300
)

cat("\nDone. Outputs:\n")
cat("  parent table:  0_nobev_detailed_parent_themes_by_treatment.html\n")
cat("  subtheme tbl:  0_nobev_detailed_subthemes_by_treatment.html\n")
cat("  parent bar:    barplot_detailed_parent_themes_by_treatment.jpg\n")
cat("  subtheme bar:  barplot_detailed_subthemes_by_treatment.jpg\n")
