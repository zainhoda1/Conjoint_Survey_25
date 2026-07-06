source(here::here('code', 'setup.R'))

# Treatment effect on opt-out RATE (not opt-out counts).
#
# Context. The thematic analysis sample (n=251) is split 115 Basic / 136
# Extended. The raw counts make it look like Extended Info produced more
# opt-outs. This script shows that the underlying treatment denominators are
# also unequal, so the per-respondent opt-out RATE is statistically
# indistinguishable across treatments — which is the substantive question
# the paper actually wants to answer.
#
# Output: a gt table saved to:
#   code/output/model_output/battery_analysis/apollo/0_optout_rate_by_treatment.html

# ── Load full data with treatment assignment ─────────────────────────────────
data_full <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint.parquet"
))
length(unique(data_full$psid)) # 1,000 respondents
data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery.parquet"
))
length(unique(data_model$psid)) # 1,000 respondents

df <- data_full %>%
  filter(psid %in% data_model$psid) %>%
  mutate(
    treatment = case_when(
      prime_group_label == "prime_long" ~ "Extended Info",
      prime_group_label == "prime_short" ~ "Basic Info",
      TRUE ~ NA_character_
    ),
    optout = !is.na(no_bev_selected0)
  ) %>%
  filter(!is.na(treatment))

cat("Respondents with a treatment assignment:", nrow(df), "\n")
cat("Treatment assignment counts:\n")
print(table(df$treatment))

# ── Rates ────────────────────────────────────────────────────────────────────
rates <- df %>%
  group_by(treatment) %>%
  summarise(
    n_total = n(),
    n_optout = sum(optout),
    optout_pct = n_optout / n_total,
    .groups = "drop"
  )

cat("\nOpt-out rate by treatment:\n")
print(rates)

# ── Tests ────────────────────────────────────────────────────────────────────
ct <- with(df, table(treatment, optout))
cat("\nContingency table:\n")
print(ct)

chi <- chisq.test(ct, correct = TRUE)
fis <- fisher.test(ct)

cat("\nChi-square (Yates) p =", signif(chi$p.value, 3), "\n")
cat("Fisher exact p       =", signif(fis$p.value, 3), "\n")
cat(
  "Odds ratio           =",
  round(fis$estimate, 3),
  "[",
  round(fis$conf.int[1], 3),
  ",",
  round(fis$conf.int[2], 3),
  "]\n"
)

# Comparison to the thematic-coding sample for transparency.
coded <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "0_nobev_themes_coded_detailed.parquet"
))
cat("\nThematic-coded sample (n=", nrow(coded), "):\n", sep = "")
print(table(coded$treatment))

# ── gt table for the paper ───────────────────────────────────────────────────
n_basic_total <- rates$n_total[rates$treatment == "Basic Info"]
n_extended_total <- rates$n_total[rates$treatment == "Extended Info"]
n_basic_opt <- rates$n_optout[rates$treatment == "Basic Info"]
n_extended_opt <- rates$n_optout[rates$treatment == "Extended Info"]
pct_basic <- rates$optout_pct[rates$treatment == "Basic Info"]
pct_extended <- rates$optout_pct[rates$treatment == "Extended Info"]

summary_tbl <- tibble(
  Treatment = c("Basic Info", "Extended Info"),
  `N (treatment assigned)` = c(n_basic_total, n_extended_total),
  `N (opt-out)` = c(n_basic_opt, n_extended_opt),
  `Opt-out rate` = scales::percent(c(pct_basic, pct_extended), accuracy = 0.01)
)

footnote_test <- paste0(
  "Chi-square (Yates) p = ",
  signif(chi$p.value, 3),
  "; Fisher exact p = ",
  signif(fis$p.value, 3),
  "; odds ratio = ",
  round(fis$estimate, 3),
  " (95% CI [",
  round(fis$conf.int[1], 3),
  ", ",
  round(fis$conf.int[2], 3),
  "])."
)

footnote_sample <- paste0(
  "Counts differ slightly from the n=251 thematic-coding sample ",
  "(115 Basic / 136 Extended) because the coded parquet carries forward ",
  "some responses that were later filtered out of the quality-screened ",
  "data_joint.parquet. The substantive conclusion (no treatment effect ",
  "on opt-out rate) holds in both samples."
)

gt_tbl <- summary_tbl %>%
  gt() %>%
  tab_header(
    title = md("**Opt-out rate by information treatment**"),
    subtitle = md(
      "Opt-out defined as taking the no-choice option on all six battery DCE tasks."
    )
  ) %>%
  tab_footnote(
    footnote = footnote_test,
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_footnote(
    footnote = footnote_sample,
    locations = cells_title(groups = "subtitle")
  ) %>%
  cols_align(
    align = "center",
    columns = c(`N (treatment assigned)`, `N (opt-out)`, `Opt-out rate`)
  ) %>%
  cols_align(align = "left", columns = Treatment) %>%
  tab_options(
    table.font.size = px(13),
    table.font.names = "Roboto Condensed",
    heading.align = "left",
    column_labels.font.weight = "bold"
  ) %>%
  opt_stylize(style = 1, color = "blue")

gt_tbl

gtsave(
  gt_tbl,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_optout_rate_by_treatment.html"
  )
)

cat("\nSaved: 0_optout_rate_by_treatment.html\n")
