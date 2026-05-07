source(here::here('code', 'setup.R'))

# Data ----
data_dce <- read_parquet(here(
  "data", "dynata_prolific_joint",
  "data_logitr_dce_covariate_battery.parquet"
))

# Keep only the opt-out row per choice occasion (altID == 4 / no_choice == 1)
# and filter to respondents with non-missing treatment assignment
opt <- data_dce %>%
  filter(no_choice == 1, !is.na(battery_info_treat))

# Respondent-level summary (6 questions each) ----
per_resp <- opt %>%
  group_by(respID, battery_info_treat, vehicle_typesuv) %>%
  summarise(
    n_optout  = sum(choice),
    total_q   = n(),
    .groups   = "drop"
  ) %>%
  mutate(
    optout_type = case_when(
      n_optout == 0             ~ "Never",
      n_optout == total_q       ~ "Always",
      TRUE                      ~ "Partial"
    ),
    optout_type = factor(optout_type, levels = c("Never", "Partial", "Always")),
    segment     = case_when(
      vehicle_typesuv == 0 ~ "Car",
      vehicle_typesuv == 1 ~ "SUV"
    ),
    treat_label = case_when(
      battery_info_treat == 1 ~ "With treatment",
      battery_info_treat == 0 ~ "Without treatment"
    ),
    treat_label = factor(treat_label, levels = c("Without treatment", "With treatment"))
  )

# Occasion-level rates by subgroup ----
subgroup_rates <- opt %>%
  mutate(
    segment     = if_else(vehicle_typesuv == 0, "Car", "SUV"),
    treat_label = if_else(
      battery_info_treat == 1, "With treatment", "Without treatment"
    ),
    treat_label = factor(treat_label, levels = c("Without treatment", "With treatment"))
  ) %>%
  group_by(segment, treat_label) %>%
  summarise(
    n_occasions = n(),
    n_optout    = sum(choice),
    rate        = mean(choice) * 100,
    .groups     = "drop"
  )

# Question-sequence opt-out rate (fatigue / learning) ----
seq_rates <- opt %>%
  group_by(respID) %>%
  mutate(q_seq = row_number()) %>%
  ungroup() %>%
  group_by(q_seq) %>%
  summarise(rate = mean(choice) * 100, se = sd(choice) / sqrt(n()) * 100, .groups = "drop")

# Covariate correlates ----
covar_rates <- bind_rows(
  opt %>%
    filter(!is.na(EV_charger)) %>%
    mutate(label = paste0("Charger: ", EV_charger)) %>%
    group_by(label) %>%
    summarise(rate = mean(choice) * 100, n = n(), .groups = "drop") %>%
    mutate(group = "Home EV Charger"),

  opt %>%
    filter(!is.na(knowledge_ev)) %>%
    mutate(label = if_else(knowledge_ev == 1, "Knowledgeable: yes", "Knowledgeable: no")) %>%
    group_by(label) %>%
    summarise(rate = mean(choice) * 100, n = n(), .groups = "drop") %>%
    mutate(group = "EV Knowledge"),

  opt %>%
    filter(!is.na(EV_neighbor)) %>%
    mutate(label = paste0("Neighbor EV: ", EV_neighbor)) %>%
    group_by(label) %>%
    summarise(rate = mean(choice) * 100, n = n(), .groups = "drop") %>%
    mutate(group = "Neighbor Owns EV")
) %>%
  mutate(label = str_remove(label, "^[^:]+: "))

# Shared theme ----
theme_base <- theme_minimal_grid(font_family = "Roboto Condensed", font_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, colour = "grey40"),
    legend.position = "bottom"
  )

clr_treat  <- c("Without treatment" = "#C2A56F", "With treatment" = "#1F4E79")
clr_type   <- c("Never" = "#2E86AB", "Partial" = "#A8DADC", "Always" = "#E63946")
clr_seg    <- c("Car" = "#1F4E79", "SUV" = "#C2A56F")

# Plot 1: Histogram of opt-out count per respondent ----
p1 <- per_resp %>%
  count(n_optout) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(n_optout), y = pct)) +
  geom_col(fill = "#1F4E79", width = 0.65) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.4, family = "Roboto Condensed", size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Opt-Out Frequency per Respondent",
    subtitle = "Number of times (out of 6 questions) respondent chose the opt-out option",
    x        = "Number of opt-outs (out of 6)",
    y        = "Share of respondents (%)"
  ) +
  theme_base

# Plot 2: Opt-out rate by segment x treatment ----
p2 <- subgroup_rates %>%
  ggplot(aes(x = segment, y = rate, fill = treat_label)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(
    aes(label = paste0(round(rate, 1), "%")),
    position = position_dodge(0.7), vjust = -0.4,
    family = "Roboto Condensed", size = 3.5
  ) +
  scale_fill_manual(values = clr_treat, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)), limits = c(0, 32)) +
  labs(
    title    = "Opt-Out Rate by Segment and Information Treatment",
    subtitle = "Share of choice occasions where the opt-out was selected",
    x        = NULL,
    y        = "Opt-out rate (%)"
  ) +
  theme_base

# Plot 3: Never / Partial / Always by segment ----
p3_data <- per_resp %>%
  group_by(segment, optout_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(segment) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

p3 <- p3_data %>%
  ggplot(aes(x = segment, y = pct, fill = optout_type)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    position = position_stack(vjust = 0.5),
    family = "Roboto Condensed", size = 3.5, colour = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = clr_type, name = "Opt-out pattern") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = "Opt-Out Behaviour Pattern by Segment",
    subtitle = "Never = 0 of 6 | Partial = 1–5 of 6 | Always = all 6",
    x        = NULL,
    y        = "Share of respondents (%)"
  ) +
  theme_base

# Plot 4: Opt-out rate by question sequence (fatigue/learning) ----
p4 <- seq_rates %>%
  ggplot(aes(x = q_seq, y = rate)) +
  geom_line(colour = "#1F4E79", linewidth = 0.9) +
  geom_point(colour = "#1F4E79", size = 2.5) +
  geom_ribbon(
    aes(ymin = rate - 1.96 * se, ymax = rate + 1.96 * se),
    fill = "#1F4E79", alpha = 0.15
  ) +
  geom_text(
    aes(label = paste0(round(rate, 1), "%")),
    vjust = -0.8, family = "Roboto Condensed", size = 3.3
  ) +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(limits = c(18, 30), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Opt-Out Rate by Question Sequence",
    subtitle = "Shaded band = 95% CI  |  Each respondent answered 6 questions",
    x        = "Question number (1 = first shown)",
    y        = "Opt-out rate (%)"
  ) +
  theme_base

# Plot 5: Opt-out rate by covariate subgroups ----
p5 <- covar_rates %>%
  mutate(group = factor(group, levels = c("Home EV Charger", "EV Knowledge", "Neighbor Owns EV"))) %>%
  ggplot(aes(x = label, y = rate, fill = group)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(round(rate, 1), "%")),
    vjust = -0.4, family = "Roboto Condensed", size = 3.5
  ) +
  facet_wrap(~group, scales = "free_x") +
  scale_fill_manual(values = c(
    "Home EV Charger"  = "#2E86AB",
    "EV Knowledge"     = "#1F4E79",
    "Neighbor Owns EV" = "#C2A56F"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, 35)) +
  labs(
    title    = "Opt-Out Rate by EV Familiarity Indicators",
    subtitle = "Respondents with EV access or knowledge opt out less frequently",
    x        = NULL,
    y        = "Opt-out rate (%)"
  ) +
  theme_base +
  theme(strip.text = element_text(face = "bold"))

# Plot 6: Always opt-out by treatment group ----
always_data <- per_resp %>%
  group_by(treat_label, segment) %>%
  summarise(
    always_pct = mean(optout_type == "Always") * 100,
    .groups    = "drop"
  )

p6 <- always_data %>%
  ggplot(aes(x = segment, y = always_pct, fill = treat_label)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(
    aes(label = paste0(round(always_pct, 1), "%")),
    position = position_dodge(0.7), vjust = -0.4,
    family = "Roboto Condensed", size = 3.5
  ) +
  scale_fill_manual(values = clr_treat, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Share of Respondents Who Always Opted Out",
    subtitle = "Chose opt-out in all 6 questions — potential non-traders",
    x        = NULL,
    y        = "Share of respondents (%)"
  ) +
  theme_base

# Save individual plots ----
out_dir <- here("code", "output", "images", "battery_analysis", "optout")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(out_dir, "optout_1_frequency_histogram.png"),
       p1, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(out_dir, "optout_2_rate_by_subgroup.png"),
       p2, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(out_dir, "optout_3_pattern_type.png"),
       p3, width = 6, height = 4.5, dpi = 300)
ggsave(file.path(out_dir, "optout_4_question_sequence.png"),
       p4, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(out_dir, "optout_5_covariate_rates.png"),
       p5, width = 9, height = 4.5, dpi = 300)
ggsave(file.path(out_dir, "optout_6_always_optout.png"),
       p6, width = 7, height = 4.5, dpi = 300)

# Combined panel (top 4 plots) ----
panel <- plot_grid(p1, p2, p4, p3,
                   ncol = 2, labels = c("A", "B", "C", "D"),
                   label_fontfamily = "Roboto Condensed",
                   label_size = 13)

ggsave(file.path(out_dir, "optout_panel.png"),
       panel, width = 14, height = 9, dpi = 300)

cat("Saved to:", out_dir, "\n")

# Summary table ----
cat("\n=== Summary Statistics ===\n")
cat("Total choice occasions (excl. NA treat):", nrow(opt), "\n")
cat("Overall opt-out rate:", round(mean(opt$choice) * 100, 1), "%\n")
cat("Total respondents:", nrow(per_resp), "\n")
cat("Always opted out:", sum(per_resp$optout_type == "Always"),
    "(", round(mean(per_resp$optout_type == "Always") * 100, 1), "%)\n")
cat("Never opted out:", sum(per_resp$optout_type == "Never"),
    "(", round(mean(per_resp$optout_type == "Never") * 100, 1), "%)\n")
cat("Partial opt-out:", sum(per_resp$optout_type == "Partial"),
    "(", round(mean(per_resp$optout_type == "Partial") * 100, 1), "%)\n")
