source(here::here('code', 'setup.R'))

# ---- Data ----

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
# ---- Identify all-optout respondents ----
# A respondent is "all-optout" if they chose the no-choice option (choice == 4)
# in every one of the 6 battery choice tasks.

optout_flag <- data_model %>%
  group_by(psid) %>%
  summarise(
    n_tasks = n(),
    n_optout = sum(choice == 4),
    all_optout = (n_optout == 6),
    .groups = "drop"
  )

# Join flags to full respondent data (treatment + platform + demographics)
data_analysis <- data_full %>%
  select(
    psid,
    data_source,
    prime_group_label,
    gender,
    hh_income,
    education,
    housing_tenure,
    charger_access,
    attitudes_1_a_ev_environment,
    attitudes_2_a_ev_battery_environment,
    attitudes_2_b_price_sensitive,
    attitudes_2_b_risk_taker,
    next_veh_fuel_new_bev,
    next_veh_fuel_used_bev
  ) %>%
  inner_join(optout_flag, by = "psid") %>%
  mutate(
    platform = case_when(
      data_source == "dynata" ~ "Dynata",
      data_source %in% c("prolific", "prolific_round2") ~ "Prolific",
      TRUE ~ NA_character_
    ),
    treatment = case_when(
      prime_group_label == "prime_long" ~ "Extended Info",
      prime_group_label == "prime_short" ~ "Basic Info",
      TRUE ~ NA_character_
    )
  )

cat("Total respondents in model sample:", nrow(data_analysis), "\n")
cat("All-optout respondents:", sum(data_analysis$all_optout), "\n")

# ---- Table 1: All-optout rate by treatment and platform ----

rate_by_treat_platform <- data_analysis %>%
  group_by(treatment, platform) %>%
  summarise(
    n_total = n(),
    n_allout = sum(all_optout),
    rate = mean(all_optout),
    .groups = "drop"
  )

rate_by_treat <- data_analysis %>%
  group_by(treatment) %>%
  summarise(
    n_total = n(),
    n_allout = sum(all_optout),
    rate = mean(all_optout),
    .groups = "drop"
  ) %>%
  mutate(platform = "All Platforms")

rate_by_platform <- data_analysis %>%
  group_by(platform) %>%
  summarise(
    n_total = n(),
    n_allout = sum(all_optout),
    rate = mean(all_optout),
    .groups = "drop"
  ) %>%
  mutate(treatment = "All Treatments")

rate_overall <- data_analysis %>%
  summarise(
    n_total = n(),
    n_allout = sum(all_optout),
    rate = mean(all_optout)
  ) %>%
  mutate(treatment = "All Treatments", platform = "All Platforms")

rate_summary <- bind_rows(
  rate_by_treat_platform,
  rate_by_treat,
  rate_by_platform,
  rate_overall
) %>%
  arrange(treatment, platform)

print(rate_summary)

# ---- Statistical test: chi-square, treatment vs all-optout ----

cat("\n--- Chi-square: All-optout ~ Treatment ---\n")
chisq_treat <- chisq.test(
  table(data_analysis$treatment, data_analysis$all_optout)
)
print(chisq_treat)

cat("\n--- Chi-square: All-optout ~ Platform ---\n")
chisq_platform <- chisq.test(
  table(data_analysis$platform, data_analysis$all_optout)
)
print(chisq_platform)

cat("\n--- Chi-square: All-optout ~ Treatment x Platform ---\n")
chisq_interact <- chisq.test(
  table(
    paste(data_analysis$treatment, data_analysis$platform, sep = " | "),
    data_analysis$all_optout
  )
)
print(chisq_interact)

# ---- Table 2: Demographic profile, all-optout vs non-all-optout by treatment ----

profile_vars <- c(
  "gender",
  "hh_income",
  "education",
  "housing_tenure",
  "charger_access",
  "attitudes_1_a_ev_environment",
  "attitudes_2_a_ev_battery_environment",
  "attitudes_2_b_price_sensitive",
  "attitudes_2_b_risk_taker",
  "next_veh_fuel_new_bev",
  "next_veh_fuel_used_bev"
)

profile_summary <- data_analysis %>%
  group_by(treatment, all_optout) %>%
  summarise(
    n = n(),
    pct_female = mean(gender == "female", na.rm = TRUE),
    pct_high_income = mean(
      hh_income %in%
        c(
          "100k_150k",
          "150k_200k",
          "200k_plus"
        ),
      na.rm = TRUE
    ),
    pct_college_plus = mean(
      education %in%
        c(
          "bachelors",
          "graduate"
        ),
      na.rm = TRUE
    ),
    pct_homeowner = mean(housing_tenure == "own", na.rm = TRUE),
    pct_charger_access = mean(
      charger_access == TRUE |
        charger_access == "yes",
      na.rm = TRUE
    ),
    mean_ev_environ = mean(attitudes_1_a_ev_environment, na.rm = TRUE),
    mean_batt_environ = mean(
      attitudes_2_a_ev_battery_environment,
      na.rm = TRUE
    ),
    mean_price_sensitive = mean(attitudes_2_b_price_sensitive, na.rm = TRUE),
    mean_risk_taker = mean(attitudes_2_b_risk_taker, na.rm = TRUE),
    pct_open_new_bev = mean(
      next_veh_fuel_new_bev == TRUE |
        next_veh_fuel_new_bev == 1,
      na.rm = TRUE
    ),
    pct_open_used_bev = mean(
      next_veh_fuel_used_bev == TRUE |
        next_veh_fuel_used_bev == 1,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(group = ifelse(all_optout, "All-Optout", "Not All-Optout")) %>%
  select(-all_optout)

print(profile_summary)

# ---- GT Table: All-optout rate by treatment and platform ----

gt_allout <- rate_summary %>%
  filter(platform != "All Platforms", treatment != "All Treatments") %>%
  bind_rows(
    rate_by_treat %>%
      select(-platform) %>%
      mutate(platform = "Overall") %>%
      bind_rows(
        rate_overall %>% mutate(platform = "Overall", treatment = "Overall")
      )
  ) %>%
  select(treatment, platform, n_total, n_allout, rate) %>%
  mutate(rate = scales::percent(rate, accuracy = 0.1)) %>%
  pivot_wider(
    names_from = platform,
    values_from = c(n_total, n_allout, rate)
  ) %>%
  gt(rowname_col = "treatment") %>%
  tab_spanner(label = "Dynata", columns = ends_with("_Dynata")) %>%
  tab_spanner(label = "Prolific", columns = ends_with("_Prolific")) %>%
  tab_spanner(label = "Overall", columns = ends_with("_Overall")) %>%
  cols_label(
    n_total_Dynata = "N",
    n_allout_Dynata = "All-Optout",
    rate_Dynata = "%",
    n_total_Prolific = "N",
    n_allout_Prolific = "All-Optout",
    rate_Prolific = "%",
    n_total_Overall = "N",
    n_allout_Overall = "All-Optout",
    rate_Overall = "%"
  ) %>%
  tab_header(
    title = md("**All-Optout Rate by Information Treatment and Platform**"),
    subtitle = md(paste0(
      "Respondents choosing no-choice in all 6 tasks  |  ",
      "Chi-sq (treatment): p = ",
      round(chisq_treat$p.value, 3),
      "  |  ",
      "Chi-sq (platform): p = ",
      round(chisq_platform$p.value, 3)
    ))
  ) %>%
  tab_footnote(
    footnote = "'All-optout' = chose the no-choice option in every one of the 6 battery choice tasks.",
    locations = cells_title(groups = "subtitle")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = treatment) %>%
  tab_options(
    table.font.size = px(13),
    table.font.names = "Roboto Condensed",
    heading.align = "left",
    column_labels.font.weight = "bold"
  ) %>%
  opt_stylize(style = 1, color = "blue")

gt_allout

gtsave(
  gt_allout,
  file = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_alloptout_by_treatment_platform.html"
  )
)

# ---- GT Table: Demographic profile by treatment x optout status ----

gt_profile <- profile_summary %>%
  mutate(
    across(starts_with("pct_"), ~ scales::percent(., accuracy = 0.1)),
    across(starts_with("mean_"), ~ round(., 2))
  ) %>%
  gt(rowname_col = "group", groupname_col = "treatment") %>%
  cols_label(
    n = "N",
    pct_female = "Female (%)",
    pct_high_income = "High Income (%)",
    pct_college_plus = "College+ (%)",
    pct_homeowner = "Homeowner (%)",
    pct_charger_access = "Charger Access (%)",
    mean_ev_environ = "EV-Enviro Attitude",
    mean_batt_environ = "Battery-Enviro Attitude",
    mean_price_sensitive = "Price Sensitivity",
    mean_risk_taker = "Risk Tolerance",
    pct_open_new_bev = "Open to New BEV (%)",
    pct_open_used_bev = "Open to Used BEV (%)"
  ) %>%
  # tab_header(
  #   title    = md("**Demographic Profile: All-Optout vs Non-All-Optout by Treatment**"),
  #   subtitle = "Model sample only; grouped by information treatment condition"
  # ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = group) %>%
  tab_options(
    table.font.size = px(13),
    table.font.names = "Roboto Condensed",
    heading.align = "left",
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold"
  ) %>%
  opt_stylize(style = 1, color = "blue")

gt_profile

gtsave(
  gt_profile,
  file = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_alloptout_profile_by_treatment.html"
  )
)
