source(here::here('code', 'setup.R'))

# Distribution of choice-task decision times by latent class.
# Diagnostic for whether Class 4 (low price sensitivity, ratio-inflated WTPs)
# is disproportionately composed of fast / inattentive responders.

# ---- Data: respondent-level decision times from main model data ----

data_time <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery_removebadcase.parquet"
)) %>%
  distinct(
    psid,
    duration_q1,
    duration_q2,
    duration_q3,
    duration_q4,
    duration_q5,
    duration_q6,
    duration_battery_dce
  ) %>%
  rename(
    Q1 = duration_q1,
    Q2 = duration_q2,
    Q3 = duration_q3,
    Q4 = duration_q4,
    Q5 = duration_q5,
    Q6 = duration_q6,
    CBC_Total = duration_battery_dce
  )

# ---- Hard class assignment ----

class_prop <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "0_Combined_6c_class_probabilities_removebadcase.parquet"
))

data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery_removebadcase.parquet"
))

class_assign <- class_prop %>%
  left_join(data_model %>% select(respID, psid), by = "respID") %>%
  distinct(psid, prob_class_assign)

time_class <- data_time %>%
  inner_join(class_assign, by = "psid") %>%
  rename(class = prob_class_assign) %>%
  mutate(
    class = factor(
      class,
      levels = paste0("class", 1:6),
      labels = paste0("Class ", 1:6)
    )
  )

time_long <- time_class %>%
  pivot_longer(Q1:Q6, names_to = "Question", values_to = "seconds") %>%
  filter(!is.na(seconds), seconds > 0)

# Consistent 6-class palette (Class 4 highlighted in red as the diagnostic class)
class_colors <- c(
  "Class 1" = "#757575",
  "Class 2" = "#1565C0",
  "Class 3" = "#2E7D32",
  "Class 4" = "#E53935",
  "Class 5" = "#6A1B9A",
  "Class 6" = "#EF6C00"
)

# ---- Plot 1: Per-question density, all 6 classes overlaid ----

cap_q <- quantile(time_long$seconds, 0.90, na.rm = TRUE)

p_q_density <- time_long %>%
  mutate(seconds_cap = pmin(seconds, cap_q)) %>%
  ggplot(aes(x = seconds_cap, color = class, fill = class)) +
  geom_density(alpha = 0.12, linewidth = 0.7) +
  facet_wrap(~Question, ncol = 3) +
  scale_color_manual(values = class_colors) +
  scale_fill_manual(values = class_colors) +
  scale_x_continuous(breaks = seq(0, ceiling(cap_q / 10) * 10, by = 20)) +
  labs(
    title = "Decision time distribution per choice task, by latent class",
    subtitle = "Hard-assigned class membership. X-axis capped at 90th percentile of pooled times.",
    x = "Seconds on choice task",
    y = "Density",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "duration_per_question_by_class_6c_removebadcase.png"
  ),
  p_q_density,
  width = 11,
  height = 7,
  dpi = 300
)

# ---- Plot 2: Per-question boxplots (easier visual comparison) ----

p_q_box <- time_long %>%
  mutate(seconds_cap = pmin(seconds, cap_q)) %>%
  ggplot(aes(x = class, y = seconds_cap, fill = class)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.4, outlier.alpha = 0.35) +
  facet_wrap(~Question, ncol = 3) +
  scale_fill_manual(values = class_colors) +
  labs(
    title = "Decision time per choice task by latent class",
    subtitle = "Hard-assigned class membership. Y-axis capped at 90th percentile of pooled times.",
    x = NULL,
    y = "Seconds on choice task",
    fill = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave(
  here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "duration_per_question_boxplot_by_class_6c_removebadcase.png"
  ),
  p_q_box,
  width = 11,
  height = 7,
  dpi = 300
)

# ---- Plot 3: Total CBC-section time distribution by class ----

cap_total <- quantile(time_class$CBC_Total, 0.95, na.rm = TRUE)

p_total <- time_class %>%
  filter(!is.na(CBC_Total), CBC_Total > 0) %>%
  mutate(CBC_Total_cap = pmin(CBC_Total, cap_total)) %>%
  ggplot(aes(x = CBC_Total_cap, color = class, fill = class)) +
  geom_density(alpha = 0.12, linewidth = 0.9) +
  scale_color_manual(values = class_colors) +
  scale_fill_manual(values = class_colors) +
  labs(
    title = "Total time on choice experiment section, by latent class",
    subtitle = "Time from Q1 page to Q6 page. X-axis capped at 95th percentile.",
    x = "Seconds across six choice tasks",
    y = "Density",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "duration_total_cbc_by_class_6c_removebadcase.png"
  ),
  p_total,
  width = 9,
  height = 5.5,
  dpi = 300
)

# ---- Per-class summary statistics ----

# summary_q <- time_long %>%
#   group_by(class, Question) %>%
#   summarise(
#     n = n(),
#     mean = mean(seconds, na.rm = TRUE),
#     median = median(seconds, na.rm = TRUE),
#     p25 = quantile(seconds, 0.25, na.rm = TRUE),
#     p75 = quantile(seconds, 0.75, na.rm = TRUE),
#     pct_under_3s = mean(seconds < 3, na.rm = TRUE),
#     pct_under_5s = mean(seconds < 5, na.rm = TRUE),
#     .groups = "drop"
#   )

# write_parquet(
#   summary_q,
#   here(
#     "code",
#     "output",
#     "model_output",
#     "battery_analysis",
#     "apollo",
#     "0_duration_summary_by_class_6c.parquet"
#   )
# )

# ---- Per-class fast-responder share (key Class 4 diagnostic) ----

# fast_share <- time_class %>%
#   pivot_longer(Q1:Q6, names_to = "Question", values_to = "seconds") %>%
#   filter(!is.na(seconds), seconds > 0) %>%
#   group_by(class, psid) %>%
#   summarise(mean_sec = mean(seconds, na.rm = TRUE), .groups = "drop") %>%
#   group_by(class) %>%
#   summarise(
#     n_resp = n(),
#     median_of_means = median(mean_sec, na.rm = TRUE),
#     pct_mean_under_5s = mean(mean_sec < 5, na.rm = TRUE),
#     pct_mean_under_10s = mean(mean_sec < 10, na.rm = TRUE),
#     pct_mean_under_15s = mean(mean_sec < 15, na.rm = TRUE),
#     .groups = "drop"
#   )

# write_parquet(
#   fast_share,
#   here("code", "output", "model_output", "battery_analysis", "apollo",
#        "0_fast_clicker_share_by_class_6c.parquet")
# )
