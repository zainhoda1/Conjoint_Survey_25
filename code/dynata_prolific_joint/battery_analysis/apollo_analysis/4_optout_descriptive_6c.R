source(here::here('code', 'setup.R'))

# Data ----

data_full <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint.parquet"
))

data_time <- data_full %>%
  mutate(
    # Compute time through whole survey
    time_start = ymd_hms(time_start, tz = "UTC"),
    time_end = ymd_hms(time_end, tz = "UTC"),
    calc_time_total = as.numeric(time_end - time_start, units = "secs"),
    ## Battery
    time_p_battery_cbc_demo = ymd_hms(time_p_battery_cbc_demo, tz = "UTC"),
    time_p_battery_pageQ1_button = ymd_hms(
      time_p_battery_pageQ1_button,
      tz = "UTC"
    ),
    time_p_battery_pageQ2_button = ymd_hms(
      time_p_battery_pageQ2_button,
      tz = "UTC"
    ),
    time_p_battery_pageQ3_button = ymd_hms(
      time_p_battery_pageQ3_button,
      tz = "UTC"
    ),
    time_p_battery_pageQ4_button = ymd_hms(
      time_p_battery_pageQ4_button,
      tz = "UTC"
    ),
    time_p_battery_pageQ5_button = ymd_hms(
      time_p_battery_pageQ5_button,
      tz = "UTC"
    ),
    time_p_battery_pageQ6_button = ymd_hms(
      time_p_battery_pageQ6_button,
      tz = "UTC"
    ),
    ###########################
    time_q_battery_cbc_q1_button = ymd_hms(
      time_q_battery_cbc_q1_button,
      tz = "UTC"
    ),
    time_q_battery_cbc_q2_button = ymd_hms(
      time_q_battery_cbc_q2_button,
      tz = "UTC"
    ),
    time_q_battery_cbc_q3_button = ymd_hms(
      time_q_battery_cbc_q3_button,
      tz = "UTC"
    ),
    time_q_battery_cbc_q4_button = ymd_hms(
      time_q_battery_cbc_q4_button,
      tz = "UTC"
    ),
    time_q_battery_cbc_q5_button = ymd_hms(
      time_q_battery_cbc_q5_button,
      tz = "UTC"
    ),
    time_q_battery_cbc_q6_button = ymd_hms(
      time_q_battery_cbc_q6_button,
      tz = "UTC"
    ),
    calc_battery_demo_p_pageQ1 = as.numeric(
      time_p_battery_pageQ1_button - time_p_battery_cbc_demo,
      units = "secs"
    ),
    calc_battery_demo_p_cbc_q1 = as.numeric(
      time_q_battery_cbc_q1_button - time_p_battery_cbc_demo,
      units = "secs"
    ),
    calc_battery_cbc_q1_pageQ1 = -as.numeric(
      time_p_battery_pageQ1_button - time_q_battery_cbc_q1_button,
      units = "secs"
    ),

    calc_battery_pageQ1_pageQ2 = as.numeric(
      time_p_battery_pageQ2_button - time_p_battery_pageQ1_button,
      units = "secs"
    ),
    calc_battery_pageQ1_cbc_q2 = as.numeric(
      time_q_battery_cbc_q2_button - time_p_battery_pageQ1_button,
      units = "secs"
    ),
    calc_battery_cbc_q2_pageQ2 = -as.numeric(
      time_p_battery_pageQ2_button - time_q_battery_cbc_q2_button,
      units = "secs"
    ),

    calc_battery_pageQ2_pageQ3 = as.numeric(
      time_p_battery_pageQ3_button - time_p_battery_pageQ2_button,
      units = "secs"
    ),
    calc_battery_pageQ2_cbc_q3 = as.numeric(
      time_q_battery_cbc_q3_button - time_p_battery_pageQ2_button,
      units = "secs"
    ),
    calc_battery_cbc_q3_pageQ3 = -as.numeric(
      time_p_battery_pageQ3_button - time_q_battery_cbc_q3_button,
      units = "secs"
    ),

    calc_battery_pageQ3_pageQ4 = as.numeric(
      time_p_battery_pageQ4_button - time_p_battery_pageQ3_button,
      units = "secs"
    ),
    calc_battery_pageQ3_cbc_q4 = as.numeric(
      time_q_battery_cbc_q4_button - time_p_battery_pageQ3_button,
      units = "secs"
    ),
    calc_battery_cbc_q4_pageQ4 = -as.numeric(
      time_p_battery_pageQ4_button - time_q_battery_cbc_q4_button,
      units = "secs"
    ),

    calc_battery_pageQ4_pageQ5 = as.numeric(
      time_p_battery_pageQ5_button - time_p_battery_pageQ4_button,
      units = "secs"
    ),
    calc_battery_pageQ4_cbc_q5 = as.numeric(
      time_q_battery_cbc_q5_button - time_p_battery_pageQ4_button,
      units = "secs"
    ),
    calc_battery_cbc_q5_pageQ5 = -as.numeric(
      time_p_battery_pageQ5_button - time_q_battery_cbc_q5_button,
      units = "secs"
    ),

    calc_battery_pageQ5_pageQ6 = as.numeric(
      time_p_battery_pageQ6_button - time_p_battery_pageQ5_button,
      units = "secs"
    ),
    calc_battery_pageQ5_cbc_q6 = as.numeric(
      time_q_battery_cbc_q6_button - time_p_battery_pageQ5_button,
      units = "secs"
    ),
    calc_battery_cbc_q6_pageQ6 = -as.numeric(
      time_p_battery_pageQ6_button - time_q_battery_cbc_q6_button,
      units = "secs"
    ),
    ## Battery
    calc_battery_cbc_total = as.numeric(
      time_p_battery_pageQ6_button - time_p_battery_pageQ1_button,
      units = "secs"
    ),
    calc_min_battery_cbc = calc_battery_cbc_total / 60
  ) %>%
  select(
    psid,
    starts_with("calc")
  )

class_prop <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "0_Combined_6c_class_probabilities.parquet"
))

data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery.parquet"
))

class_prop <- class_prop %>%
  left_join(
    data_model %>% select(respID, psid),
    by = "respID"
  ) %>%
  distinct(across(-respID), .keep_all = TRUE) %>%
  left_join(
    data_full,
    by = "psid"
  )

# ---- Class labels ----

class_labels <- c(
  # class1 = "C1: Disengaged / Range Anxious",
  # class2 = "C2: Range-Aware / Refurbishment-Averse",
  # class3 = "C3: High-Value Range Enthusiasts",
  # class4 = "C4: Budget-Constrained",
  # class5 = "C5: Loss-Averse / Battery-Sensitive",
  # class6 = "C6: Opt-Out Resistant"
  class1 = "Class 1",
  class2 = "Class 2",
  class3 = "Class 3",
  class4 = "Class 4",
  class5 = "Class 5",
  class6 = "Class 6"
)

# Posterior class probabilities per respondent (model sample only)
class_probs <- class_prop %>%
  select(
    psid,
    prob_class1,
    prob_class2,
    prob_class3,
    prob_class4,
    prob_class5,
    prob_class6
  )

# ---- Probability-weighted opt-out rates ----

# Inner join: restricts to model sample, eliminating NA rows
optout_data <- data_model %>%
  select(psid, qID, choice) %>%
  inner_join(class_probs, by = "psid") %>%
  mutate(optout = as.integer(choice == 4))

# For each class: weighted mean opt-out rate per question
optout_wide <- map_dfr(
  paste0("prob_class", 1:6),
  function(prob_col) {
    class_key <- str_replace(prob_col, "prob_", "")
    optout_data %>%
      group_by(qID) %>%
      summarise(
        rate = weighted.mean(optout, .data[[prob_col]]),
        .groups = "drop"
      ) %>%
      pivot_wider(names_from = qID, values_from = rate, names_prefix = "Q") %>%
      mutate(
        class_label = class_labels[class_key],
        Mean = rowMeans(across(starts_with("Q")))
      )
  }
)

# Overall row: unweighted mean across model sample
optout_overall <- optout_data %>%
  group_by(qID) %>%
  summarise(rate = mean(optout), .groups = "drop") %>%
  pivot_wider(names_from = qID, values_from = rate, names_prefix = "Q") %>%
  mutate(class_label = "Overall", Mean = rowMeans(across(starts_with("Q"))))

optout_table <- bind_rows(optout_wide, optout_overall) %>%
  mutate(across(where(is.numeric), ~ scales::percent(., accuracy = 0.1)))

# ---- Probability-weighted survey duration ----

# Inner join: respondent-level time restricted to model sample
time_data <- data_time %>%
  inner_join(class_probs, by = "psid")

# Weighted median: sort by x, find value where cumulative weight >= 0.5
weighted_median <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]
    w <- w[keep]
  }
  ord <- order(x)
  x <- x[ord]
  cumw <- cumsum(w[ord]) / sum(w[ord])
  x[which(cumw >= 0.5)[1]]
}

# For each class: probability-weighted median duration per metric
time_by_class <- map_dfr(
  paste0("prob_class", 1:6),
  function(prob_col) {
    class_key <- str_replace(prob_col, "prob_", "")
    w <- time_data[[prob_col]]
    tibble(
      class_label = class_labels[class_key],
      Q1 = weighted_median(time_data$calc_battery_cbc_q1_pageQ1, w),
      Q2 = weighted_median(time_data$calc_battery_cbc_q2_pageQ2, w),
      Q3 = weighted_median(time_data$calc_battery_cbc_q3_pageQ3, w),
      Q4 = weighted_median(time_data$calc_battery_cbc_q4_pageQ4, w),
      Q5 = weighted_median(time_data$calc_battery_cbc_q5_pageQ5, w),
      Q6 = weighted_median(time_data$calc_battery_cbc_q6_pageQ6, w),
      CBC_Total = weighted_median(time_data$calc_battery_cbc_total, w),
      Survey_Total = weighted_median(time_data$calc_time_total, w)
    )
  }
)

# Overall row: unweighted median across model sample
time_overall <- time_data %>%
  summarise(
    Q1 = median(calc_battery_cbc_q1_pageQ1, na.rm = TRUE),
    Q2 = median(calc_battery_cbc_q2_pageQ2, na.rm = TRUE),
    Q3 = median(calc_battery_cbc_q3_pageQ3, na.rm = TRUE),
    Q4 = median(calc_battery_cbc_q4_pageQ4, na.rm = TRUE),
    Q5 = median(calc_battery_cbc_q5_pageQ5, na.rm = TRUE),
    Q6 = median(calc_battery_cbc_q6_pageQ6, na.rm = TRUE),
    CBC_Total = median(calc_battery_cbc_total, na.rm = TRUE),
    Survey_Total = median(calc_time_total, na.rm = TRUE)
  ) %>%
  mutate(class_label = "Overall")

time_table <- bind_rows(time_by_class, time_overall) %>%
  mutate(across(where(is.numeric), ~ round(., 1)))

# Save class-level durations for use in 3b_summarize_class_profile_6c.R
write_parquet(
  time_table %>%
    filter(class_label != "Overall") %>%
    mutate(class = paste0("class", str_extract(class_label, "\\d+"))) %>%
    select(
      class,
      duration_q1          = Q1,
      duration_q2          = Q2,
      duration_q3          = Q3,
      duration_q4          = Q4,
      duration_q5          = Q5,
      duration_q6          = Q6,
      duration_battery_dce = CBC_Total,
      duration_full_survey = Survey_Total
    ),
  here(
    "code", "output", "model_output", "battery_analysis", "apollo",
    "0_survey_duration_by_class_6c.parquet"
  )
)

# ---- GT: Opt-out rate table ----

gt_optout <- optout_table %>%
  gt(rowname_col = "class_label") %>%
  tab_spanner(
    label = "Opt-Out Rate by Choice Task",
    columns = starts_with("Q")
  ) %>%
  cols_label(
    Q1 = "Q1",
    Q2 = "Q2",
    Q3 = "Q3",
    Q4 = "Q4",
    Q5 = "Q5",
    Q6 = "Q6",
    Mean = "Mean"
  ) %>%
  tab_header(
    title = md("**Opt-Out Rate by Latent Class and Choice Task**"),
    subtitle = "Share choosing no-choice option per task"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = "Overall")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = class_label) %>%
  tab_options(
    table.font.size = px(13),
    table.font.names = "Roboto Condensed",
    heading.align = "left",
    column_labels.font.weight = "bold"
  ) %>%
  opt_stylize(style = 1, color = "blue")

gt_optout

gtsave(
  gt_optout,
  file = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_optout_rate_by_class_6c.html"
  )
)

# ---- GT: Survey duration table ----

gt_time <- time_table %>%
  gt(rowname_col = "class_label") %>%
  fmt_number(
    columns = c(Q1, Q2, Q3, Q4, Q5, Q6, CBC_Total, Survey_Total),
    decimals = 1
  ) %>%
  tab_spanner(
    label = "Median of Duration\n(Seconds per Choice Task)",
    columns = c(Q1, Q2, Q3, Q4, Q5, Q6)
  ) %>%
  tab_spanner(
    label = "Median of Total Duration (seconds)",
    columns = c(CBC_Total, Survey_Total)
  ) %>%
  cols_label(
    Q1 = "Q1",
    Q2 = "Q2",
    Q3 = "Q3",
    Q4 = "Q4",
    Q5 = "Q5",
    Q6 = "Q6",
    CBC_Total = "Battery DCE Section",
    Survey_Total = "Full Survey"
  ) %>%
  tab_header(
    title = md("**Survey Duration by Latent Class**"),
    subtitle = "Median time (seconds) per choice task and overall"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = "Overall")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = class_label) %>%
  tab_options(
    table.font.size = px(13),
    table.font.names = "Roboto Condensed",
    heading.align = "left",
    column_labels.font.weight = "bold"
  ) %>%
  opt_stylize(style = 1, color = "blue")

gt_time

gtsave(
  gt_time,
  file = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_survey_duration_by_class_6c.html"
  )
)

# ---- Class 6: Duration Distribution (Bimodality Check) ----
# C6 has massive opt-out aversion but insignificant attribute WTPs.
# Check whether this reflects a fast-clicker / satisficing subgroup.

# Hard-assigned class 6 respondents
c6_time <- data_time %>%
  inner_join(
    class_prop %>% select(psid, prob_class6, prob_class_assign),
    by = "psid"
  ) %>%
  filter(prob_class_assign == "class6")

# Reference: hard-assigned class 3 (engaged, high-value range class)
c3_time <- data_time %>%
  inner_join(
    class_prop %>% select(psid, prob_class3, prob_class_assign),
    by = "psid"
  ) %>%
  filter(prob_class_assign == "class3")

# Reshape both to long format
c6_long <- c6_time %>%
  select(
    psid,
    prob_class6,
    Q1 = calc_battery_cbc_q1_pageQ1,
    Q2 = calc_battery_cbc_q2_pageQ2,
    Q3 = calc_battery_cbc_q3_pageQ3,
    Q4 = calc_battery_cbc_q4_pageQ4,
    Q5 = calc_battery_cbc_q5_pageQ5,
    Q6 = calc_battery_cbc_q6_pageQ6
  ) %>%
  pivot_longer(Q1:Q6, names_to = "Question", values_to = "seconds") %>%
  filter(!is.na(seconds), seconds > 0) %>%
  mutate(
    Class = "C6: Opt-Out Resistant",
    # Cap at 90th percentile to suppress extreme outliers in display
    seconds_cap = pmin(seconds, quantile(seconds, 0.90, na.rm = TRUE))
  )

c3_long <- c3_time %>%
  select(
    psid,
    Q1 = calc_battery_cbc_q1_pageQ1,
    Q2 = calc_battery_cbc_q2_pageQ2,
    Q3 = calc_battery_cbc_q3_pageQ3,
    Q4 = calc_battery_cbc_q4_pageQ4,
    Q5 = calc_battery_cbc_q5_pageQ5,
    Q6 = calc_battery_cbc_q6_pageQ6
  ) %>%
  pivot_longer(Q1:Q6, names_to = "Question", values_to = "seconds") %>%
  filter(!is.na(seconds), seconds > 0) %>%
  mutate(
    Class = "C3: High-Value Range Enthusiasts",
    seconds_cap = pmin(seconds, quantile(seconds, 0.90, na.rm = TRUE))
  )

# --- Plot 1: C6 density per question, with rug to expose fast clickers ---
p_c6_dist <- c6_long %>%
  ggplot(aes(x = seconds_cap)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 4,
    fill = "#E53935",
    alpha = 0.5
  ) +
  geom_density(color = "#B71C1C", linewidth = 0.9) +
  geom_rug(alpha = 0.25, color = "#B71C1C") +
  facet_wrap(~Question, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 120, 20)) +
  labs(
    title = "C6 (Opt-Out Resistant): Decision Time Distribution per Choice Task",
    subtitle = "Hard-assigned class 6 respondents. Rug marks individual observations. X capped at 90th pct.",
    x = "Seconds on choice task",
    y = "Density"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p_c6_dist

ggsave(
  here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "c6_duration_distribution.png"
  ),
  p_c6_dist,
  width = 10,
  height = 6,
  dpi = 300
)

# --- Plot 2: C3 vs C6 overlaid density per question ---
p_c3_c6 <- bind_rows(c3_long, c6_long) %>%
  ggplot(aes(x = seconds_cap, fill = Class, color = Class)) +
  geom_density(alpha = 0.35, linewidth = 0.8) +
  facet_wrap(~Question, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 120, 20)) +
  scale_fill_manual(
    values = c(
      "C3: High-Value Range Enthusiasts" = "#1565C0",
      "C6: Opt-Out Resistant" = "#E53935"
    )
  ) +
  scale_color_manual(
    values = c(
      "C3: High-Value Range Enthusiasts" = "#1565C0",
      "C6: Opt-Out Resistant" = "#E53935"
    )
  ) +
  labs(
    title = "Decision Time Distribution: C3 vs C6 by Choice Task",
    subtitle = "Hard-assigned respondents. X capped at 90th pct. Faster C6 peak would indicate satisficing.",
    x = "Seconds on choice task",
    y = "Density",
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

p_c3_c6

ggsave(
  here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "c3_c6_duration_comparison.png"
  ),
  p_c3_c6,
  width = 10,
  height = 6,
  dpi = 300
)
