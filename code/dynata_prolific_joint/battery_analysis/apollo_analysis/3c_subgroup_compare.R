# ============================================================
# CLASS 2 SUBGROUP ANALYSIS
# Hypothesis: C2 contains two distinct groups:
#   (a) genuine pre-committed adopters (slow, occasionally opt out)
#   (b) satisficers avoiding opt-out from survey fear (fast, never opt out)
# Strategy: median split on mean task duration; compare all characteristics
# ============================================================

library(glue)
library(lubridate)

# --- 1. Class probabilities ---
class_probs_2c <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "0_Combined_2c_class_probabilities.parquet"
)) %>%
  left_join(
    database %>% distinct(respID, psid),
    by = "respID"
  )

# --- 2. Per-task decision time ---
data_full_raw <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint.parquet"
))

timing_c2 <- data_full_raw %>%
  mutate(
    across(
      c(
        time_p_battery_pageQ1_button,
        time_p_battery_pageQ2_button,
        time_p_battery_pageQ3_button,
        time_p_battery_pageQ4_button,
        time_p_battery_pageQ5_button,
        time_p_battery_pageQ6_button,
        time_q_battery_cbc_q1_button,
        time_q_battery_cbc_q2_button,
        time_q_battery_cbc_q3_button,
        time_q_battery_cbc_q4_button,
        time_q_battery_cbc_q5_button,
        time_q_battery_cbc_q6_button
      ),
      ~ ymd_hms(.x, tz = "UTC")
    ),
    cbc_q1_sec = -as.numeric(
      time_p_battery_pageQ1_button - time_q_battery_cbc_q1_button,
      units = "secs"
    ),
    cbc_q2_sec = -as.numeric(
      time_p_battery_pageQ2_button - time_q_battery_cbc_q2_button,
      units = "secs"
    ),
    cbc_q3_sec = -as.numeric(
      time_p_battery_pageQ3_button - time_q_battery_cbc_q3_button,
      units = "secs"
    ),
    cbc_q4_sec = -as.numeric(
      time_p_battery_pageQ4_button - time_q_battery_cbc_q4_button,
      units = "secs"
    ),
    cbc_q5_sec = -as.numeric(
      time_p_battery_pageQ5_button - time_q_battery_cbc_q5_button,
      units = "secs"
    ),
    cbc_q6_sec = -as.numeric(
      time_p_battery_pageQ6_button - time_q_battery_cbc_q6_button,
      units = "secs"
    )
  ) %>%
  select(psid, starts_with("cbc_q")) %>%
  mutate(
    mean_cbc_sec = rowMeans(across(starts_with("cbc_q")), na.rm = TRUE),
    valid_timing = if_all(starts_with("cbc_q"), ~ is.na(.x) | .x >= 0)
  ) %>%
  filter(valid_timing) %>%
  select(-valid_timing)

# --- 3. Opt-out behavior per respondent ---
optout_per_resp <- database %>%
  select(psid, qID, choice) %>%
  distinct() %>%
  group_by(psid) %>%
  summarise(
    optout_rate_resp = mean(choice == 4, na.rm = TRUE),
    optout_never = as.integer(sum(choice == 4, na.rm = TRUE) == 0),
    .groups = "drop"
  )

# --- 4. Respondent-level covariates ---
resp_covariates <- database %>%
  distinct(respID, .keep_all = TRUE) %>%
  select(psid, respID, all_of(num_vars), all_of(cate_vars))

# --- 5. Class 2 respondent-level dataset ---
c2_resp <- class_probs_2c %>%
  filter(prob_class_assign == "class2") %>%
  left_join(timing_c2, by = "psid") %>%
  left_join(optout_per_resp, by = "psid") %>%
  left_join(resp_covariates, by = "psid") %>%
  filter(!is.na(mean_cbc_sec))

# --- 6. Median split on mean task duration ---
median_sec <- median(c2_resp$mean_cbc_sec, na.rm = TRUE)

c2_resp <- c2_resp %>%
  mutate(
    speed_group = factor(
      if_else(mean_cbc_sec <= median_sec, "Fast", "Slow"),
      levels = c("Fast", "Slow")
    )
  )

# --- 7. Compute group summaries ---

# Behavioral diagnostics
diag_summary <- c2_resp %>%
  group_by(speed_group) %>%
  summarise(
    n = n(),
    mean_duration_sec = weighted.mean(mean_cbc_sec, prob_class2, na.rm = TRUE),
    optout_rate = weighted.mean(optout_rate_resp, prob_class2, na.rm = TRUE),
    optout_never_pct = weighted.mean(optout_never, prob_class2, na.rm = TRUE),
    .groups = "drop"
  )

# Numeric variable comparison
compare_num <- map_dfr(num_vars, function(v) {
  map_dfr(c("Fast", "Slow"), function(grp) {
    sub <- c2_resp %>% filter(speed_group == grp)
    tibble(
      variable = v,
      speed_group = grp,
      value = weighted.mean(sub[[v]], sub$prob_class2, na.rm = TRUE)
    )
  })
}) %>%
  pivot_wider(names_from = speed_group, values_from = value)

# Binary categorical variable comparison
binary_cate_vars <- c(
  "ATT_range_anxiety",
  "ATT_risktaker",
  "ATT_EVB_environment",
  "ATT_EVB_function",
  "EV_charger",
  "EV_neighbor",
  "knowledge_ev",
  "knowledge_subsidy"
)

compare_cate <- map_dfr(binary_cate_vars, function(v) {
  map_dfr(c("Fast", "Slow"), function(grp) {
    sub <- c2_resp %>% filter(speed_group == grp)
    tibble(
      variable = v,
      speed_group = grp,
      value = weighted.mean(
        as.numeric(sub[[v]] == 1),
        sub$prob_class2,
        na.rm = TRUE
      )
    )
  })
}) %>%
  pivot_wider(names_from = speed_group, values_from = value)

# Combine and format
compare_all <- bind_rows(compare_num, compare_cate) %>%
  left_join(
    var_meta %>% select(variable, label, section, fmt),
    by = "variable"
  ) %>%
  mutate(
    label = coalesce(label, variable),
    fmt = coalesce(fmt, "number"),
    Diff = Slow - Fast,
    across(
      c(Fast, Slow, Diff),
      list(
        fmt = ~ case_when(
          fmt == "dollar" ~ dollar(.x, accuracy = 0.1),
          fmt == "pct" ~ percent(.x, accuracy = 0.1),
          TRUE ~ number(.x, accuracy = 0.1)
        )
      ),
      .names = "{col}_fmt"
    )
  ) %>%
  filter(!is.na(section))

# --- 8. Behavioral header rows ---
n_fast <- diag_summary$n[diag_summary$speed_group == "Fast"]
n_slow <- diag_summary$n[diag_summary$speed_group == "Slow"]
dur_fast <- diag_summary$mean_duration_sec[diag_summary$speed_group == "Fast"]
dur_slow <- diag_summary$mean_duration_sec[diag_summary$speed_group == "Slow"]
oo_fast <- diag_summary$optout_rate[diag_summary$speed_group == "Fast"]
oo_slow <- diag_summary$optout_rate[diag_summary$speed_group == "Slow"]
oo_never_fast <- diag_summary$optout_never_pct[
  diag_summary$speed_group == "Fast"
]
oo_never_slow <- diag_summary$optout_never_pct[
  diag_summary$speed_group == "Slow"
]

diag_rows <- tribble(
  ~label                     , ~section            , ~Fast_fmt                              , ~Slow_fmt                              , ~Diff_fmt                                              ,
  "Mean task duration (sec)" , "Behavioral Checks" , number(dur_fast, accuracy = 0.1)       , number(dur_slow, accuracy = 0.1)       , number(dur_slow - dur_fast, accuracy = 0.1)            ,
  "Opt-out rate"             , "Behavioral Checks" , percent(oo_fast, accuracy = 0.1)       , percent(oo_slow, accuracy = 0.1)       , percent(oo_slow - oo_fast, accuracy = 0.1)             ,
  "Never opted out (%)"      , "Behavioral Checks" , percent(oo_never_fast, accuracy = 0.1) , percent(oo_never_slow, accuracy = 0.1) , percent(oo_never_slow - oo_never_fast, accuracy = 0.1)
)

display_tbl <- bind_rows(
  diag_rows,
  compare_all %>% select(label, section, Fast_fmt, Slow_fmt, Diff_fmt)
) %>%
  mutate(
    section = factor(
      section,
      levels = c(
        "Behavioral Checks",
        "WTP (*1000 USD) for Vehicle Attributes",
        "Active Indicators",
        "Inactive Indicators: Socioeconomics",
        "Inactive Indicators: Attitudes"
      )
    )
  )

# --- 9. GT table ---
gt_c2_subgroup <- display_tbl %>%
  group_by(section) %>%
  gt(rowname_col = "label") %>%
  tab_header(
    title = md(
      "**Class 2 Internal Subgroup Comparison: Fast vs. Slow Responders**"
    ),
    subtitle = md(glue(
      "Median split on mean task duration ({round(median_sec, 1)} sec). ",
      "Fast: N={n_fast}, avg {round(dur_fast, 1)}s. ",
      "Slow: N={n_slow}, avg {round(dur_slow, 1)}s."
    ))
  ) %>%
  tab_spanner(
    label = "Speed Group (median split on task duration)",
    columns = c(Fast_fmt, Slow_fmt, Diff_fmt)
  ) %>%
  cols_label(
    Fast_fmt = md(glue("**Fast** (<=>{round(median_sec,1)}s)  \nN={n_fast}")),
    Slow_fmt = md(glue("**Slow** (>{round(median_sec,1)}s)  \nN={n_slow}")),
    Diff_fmt = "Slow - Fast"
  ) %>%
  tab_footnote(
    footnote = "All means probability-weighted by each respondent's posterior P(class 2). Behavioral rows use raw means.",
    locations = cells_title("subtitle")
  ) %>%
  tab_style(
    style = cell_fill(color = "#FFF9C4"),
    locations = cells_row_groups(groups = "Behavioral Checks")
  ) %>%
  cols_align(align = "center", columns = c(Fast_fmt, Slow_fmt, Diff_fmt)) %>%
  cols_align(align = "left", columns = label) %>%
  tab_options(
    table.font.size = px(13),
    table.font.names = "Roboto Condensed",
    heading.align = "left",
    row_group.font.weight = "bold",
    column_labels.font.weight = "bold"
  ) %>%
  opt_stylize(style = 1, color = "blue")

gt_c2_subgroup

gtsave(
  gt_c2_subgroup,
  file = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_c2_subgroup_fast_vs_slow.html"
  )
)

# --- 10. Scatter: duration vs opt-out rate per C2 respondent ---
p_c2_scatter <- c2_resp %>%
  mutate(
    mean_cbc_cap = pmin(
      mean_cbc_sec,
      quantile(mean_cbc_sec, 0.97, na.rm = TRUE)
    )
  ) %>%
  ggplot(aes(
    x = mean_cbc_cap,
    y = optout_rate_resp,
    color = prob_class2,
    size = prob_class2
  )) +
  geom_point(alpha = 0.7) +
  geom_vline(
    xintercept = median_sec,
    linetype = "dashed",
    color = "firebrick",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = median_sec + 1,
    y = 0.95,
    label = glue("median = {round(median_sec, 1)}s"),
    hjust = 0,
    color = "firebrick",
    size = 3.2,
    family = "Roboto Condensed"
  ) +
  scale_color_gradient(low = "#FFCDD2", high = "#B71C1C", name = "P(class 2)") +
  scale_size_continuous(range = c(1, 4), guide = "none") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.2)) +
  labs(
    title = "C2 Respondents: Task Duration vs. Opt-Out Rate",
    subtitle = "Each point = one respondent. If two groups exist, expect a cluster bottom-left (fast, never opt-out).",
    x = "Mean seconds per task (capped at 97th pct)",
    y = "Opt-out rate across 6 tasks"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

p_c2_scatter

ggsave(
  here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "c2_duration_vs_optout_scatter.png"
  ),
  p_c2_scatter,
  width = 9,
  height = 5.5,
  dpi = 300
)
