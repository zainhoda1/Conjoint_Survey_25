# rm(list = ls())

source(here::here('code', 'setup.R'))

# UPLOAD MODELS ----
car_suv_lc_6c <- readRDS(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "piecewise_rangeloss_car_suv_lc_6c_1_model.rds"
))

car_suv_lc_6c_apollo_inputs <- readRDS(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "car_suv_lc_6c_apollo_inputs_piecewise_rangeloss.rds"
))

car_suv_lc_6c_apollo_probabilities <- readRDS(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "car_suv_lc_6c_apollo_probabilities_piecewise_rangeloss.rds"
))

data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery.parquet"
)) %>%
  filter(
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
      !is.na(ATT_EVB_function)
  )
database <- data_model %>%
  mutate(
    hhincome_num_k = hhincome_num / 1000,
    ATT_range_anxiety = case_when(
      ATT_range_anxiety > 3 ~ 1,
      TRUE ~ 0
    ),
    ATT_risktaker = case_when(
      ATT_risktaker > 3 ~ 1,
      TRUE ~ 0
    ),
    ATT_price_sensitive = case_when(
      ATT_price_sensitive > 3 ~ 1,
      TRUE ~ 0
    ),
    ATT_climate = case_when(
      ATT_climate > 3 ~ 1,
      TRUE ~ 0
    ),
    ATT_EVB_environment = case_when(
      ATT_EVB_environment > 3 ~ 1,
      TRUE ~ 0
    ),
    ATT_EVB_function = case_when(
      ATT_EVB_function < 3 ~ 1,
      TRUE ~ 0
    ),
    next_veh_fuel_used_bev = case_when(
      next_veh_fuel_used_bev == 1 ~ "strongly_disagree",
      next_veh_fuel_used_bev == 2 ~ "somewhat_disagree",
      next_veh_fuel_used_bev == 3 ~ "neutral",
      next_veh_fuel_used_bev == 4 ~ "somewhat_agree",
      next_veh_fuel_used_bev == 5 ~ "strongly_agree"
    )
  )

# ---- WTP significance: bounded by the less significant of (attribute, price) ----
# Significance rule: WTP_pval = max(p_attr, p_price); stars reflect the weaker
# of the two parameters. If price is insignificant, WTP inherits that.

parse_class_suffix <- function(x) {
  case_when(
    x == "a" ~ "class1",
    x == "b" ~ "class2",
    x == "c" ~ "class3",
    x == "d" ~ "class4",
    x == "e" ~ "class5",
    TRUE ~ "class6"
  )
}

estimates_6c <- read_csv(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "piecewise_rangeloss_car_suv_lc_6c_1_estimates.csv"
))
colnames(estimates_6c)[1] <- "variable"

estimates_long <- estimates_6c %>%
  as.data.frame() %>%
  select(1:2, 6, 8) %>%
  setNames(c("Variables", "Est.", "Std.Err.", "P.Value")) %>%
  filter(!is.na(Std.Err.)) %>%
  mutate(
    Variables = str_replace_all(Variables, "b_|gamma_", ""),
    Variables = str_replace_all(Variables, "delta", "ASC")
  ) %>%
  separate(
    col = Variables,
    into = c("Variables", "Class"),
    sep = "_(?=[^_]+$)",
    extra = "merge",
    fill = "right"
  ) %>%
  mutate(Class = parse_class_suffix(Class))

# Price p-values per class
price_pval_by_class <- estimates_long %>%
  filter(Variables == "price") %>%
  select(Class, price_pval = P.Value)

# WTP significance for utility attributes (bounded by min significance)
wtp_sig <- estimates_long %>%
  filter(
    Variables %in%
      c(
        "no_choice",
        "mileage",
        "range_pw1",
        "range_pw2",
        "range_pw3",
        "loss_pw1",
        "loss_pw2",
        "loss_pw3",
        "packreplace",
        "cellreplace"
      )
  ) %>%
  left_join(price_pval_by_class, by = "Class") %>%
  mutate(
    WTP_pval = pmax(P.Value, price_pval, na.rm = TRUE),
    WTP_sig = case_when(
      WTP_pval < 0.001 ~ "***",
      WTP_pval < 0.01 ~ "**",
      WTP_pval < 0.05 ~ "*",
      WTP_pval < 0.1 ~ ".",
      TRUE ~ " "
    )
  ) %>%
  select(Variables, Class, WTP_sig) %>%
  pivot_wider(
    id_cols = Variables,
    names_from = Class,
    values_from = WTP_sig
  )

# Compute probability-weighted summaries for a model and return tidy long table
summarize_lc_model <- function(
  model,
  apollo_probabilities,
  apollo_inputs,
  db,
  model_tag,
  attributes,
  num_vars,
  cate_vars
) {
  uncond <- apollo_unconditionals(model, apollo_probabilities, apollo_inputs)
  conditionals <- apollo_conditionals(
    model,
    apollo_probabilities,
    apollo_inputs
  ) %>%
    rename_with(~ c("ID", paste0("prob_class", seq_len(length(.) - 1))))
  db_indiv <- db %>%
    left_join(conditionals, by = c("respID" = "ID")) %>%
    distinct(respID, .keep_all = TRUE) %>%
    relocate(
      prob_class1,
      prob_class2,
      prob_class3,
      prob_class4,
      prob_class5,
      prob_class6,
      .after = last_col()
    ) %>%
    mutate(
      prob_class_max = pmax(
        prob_class1,
        prob_class2,
        prob_class3,
        prob_class4,
        prob_class5,
        prob_class6
      ),
      prob_class_assign = case_when(
        prob_class1 == prob_class_max ~ "class1",
        prob_class2 == prob_class_max ~ "class2",
        prob_class3 == prob_class_max ~ "class3",
        prob_class4 == prob_class_max ~ "class4",
        prob_class5 == prob_class_max ~ "class5",
        prob_class6 == prob_class_max ~ "class6",
        TRUE ~ NA_character_
      )
    )

  n_classes <- length(uncond[["pi_values"]])
  write_parquet(
    db_indiv %>% select(respID, starts_with("prob_class")),
    here(
      "code",
      "output",
      "model_output",
      "battery_analysis",
      "apollo",
      paste0("0_", model_tag, "_", n_classes, "c_class_probabilities.parquet")
    )
  )

  # WTP
  wtp_df <- map_dfr(attributes, function(attr) {
    vals <- map_dbl(1:n_classes, function(k) {
      beta_attr <- uncond[[paste0("b_", attr)]][[k]]
      beta_price <- uncond[["b_price"]][[k]]
      beta_attr / (-beta_price)
    })
    tibble(
      variable = attr,
      !!!set_names(as.list(vals), paste0("class", 1:n_classes))
    )
  })

  # Numeric summaries
  num_summary <- map_dfr(num_vars, function(v) {
    map_dbl(1:n_classes, function(k) {
      w <- db_indiv[[paste0("prob_class", k)]]
      weighted.mean(db_indiv[[v]], w, na.rm = TRUE)
    }) %>%
      {
        tibble(
          variable = v,
          !!!set_names(as.list(.), paste0("class", 1:n_classes))
        )
      }
  })

  # Categorical summaries: produce one variable row per level (share)
  cat_summary <- map_dfr(cate_vars, function(v) {
    levs <- sort(na.omit(unique(db_indiv[[v]])))
    map_dfr(levs, function(lv) {
      vals <- map_dbl(1:n_classes, function(k) {
        w <- db_indiv[[paste0("prob_class", k)]]
        weighted.mean(as.numeric(db_indiv[[v]] == lv), w, na.rm = TRUE)
      })
      tibble(
        variable = paste0(v, "_", make.names(lv), "_share"),
        label_raw = paste(v, lv, sep = " : "),
        !!!set_names(as.list(vals), paste0("class", 1:n_classes))
      )
    })
  })
  # Combine
  combined <- bind_rows(wtp_df, num_summary, cat_summary) %>%
    replace_na(list(label_raw = NA_character_))
  # pivot to wide with prefixed class columns for this model
  combined_renamed <- combined %>%
    rename_with(~ paste0(model_tag, "_", .), starts_with("class"))
  combined_renamed
}

# --- Parameters ---
attributes <- c(
  "no_choice",
  "mileage",
  "range_pw1",
  "range_pw2",
  "range_pw3",
  "loss_pw1",
  "loss_pw2",
  "loss_pw3",
  "packreplace",
  "cellreplace"
)
attributes_long <- attributes

num_vars <- c(
  "hhincome_num_k",
  "Veh_primary_refuel_monthly",
  "Veh_primary_range",
  "age_num",
  "hhsize_num",
  "Veh_hh_count",
  "next_veh_budget_k"
  # "next_veh_fuel_new_phev",
  # "next_veh_fuel_used_phev",
  # "next_veh_fuel_new_bev",
)
cate_vars <- c(
  "ATT_range_anxiety",
  "ATT_risktaker",
  # "ATT_price_sensitive",
  "ATT_climate",
  "ATT_EVB_environment",
  "ATT_EVB_function",
  "EV_charger",
  "EV_neighbor",
  "next_veh_fuel_used_bev",
  "knowledge_ev",
  "knowledge_subsidy",
  "gender_cate",
  "ethnicity_cate",
  "race_cate",
  "education_cate",
  "student_cate",
  "employment_cate",
  "hhtenure_cate",
  "hhtype_cate",
  "Veh_hh_fuel",
  "Veh_primary_fuel",
  # "ATT_political",
  # "ATT_voting",
  "vehicle_typesuv",
  "battery_info_treat"
)

# --- Run summaries ---
combined_res <- summarize_lc_model(
  car_suv_lc_6c,
  car_suv_lc_6c_apollo_probabilities,
  car_suv_lc_6c_apollo_inputs,
  database,
  "Combined",
  attributes,
  num_vars,
  cate_vars
)

# --- Merge by variable ---
combined_all <- combined_res %>%
  mutate(label = coalesce(label_raw, variable)) %>%
  select(
    variable,
    label,
    starts_with("Combined_class")
  ) %>%
  filter(!is.na(Combined_class1))

# --- Convert WTP to $1,000 units (price is in 10k USD) ---
combined_all <- combined_all %>%
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ case_when(
        variable %in% attributes_long ~ .x * 10,
        TRUE ~ .x
      )
    )
  )

# change labels
var_meta <- tribble(
  ~variable                    , ~label                                          , ~section                                 , ~fmt     ,
  # WTP
  "no_choice"                  , "No-Choice Option (opt-out)"                    , "WTP (*1000 USD) for Vehicle Attributes" , "dollar" ,
  "mileage"                    , "Mileage (10,000 miles)"                        , "WTP (*1000 USD) for Vehicle Attributes" , "dollar" ,
  "range_pw1"                  , "BEV Range: 40-130 mi segment (per 100 miles)"  , "WTP (*1000 USD) for Vehicle Attributes" , "dollar" ,
  "range_pw2"                  , "BEV Range: 130-200 mi segment (per 100 miles)" , "WTP (*1000 USD) for Vehicle Attributes" , "dollar" ,
  "range_pw3"                  , "BEV Range: 200+ mi segment (per 100 miles)"    , "WTP (*1000 USD) for Vehicle Attributes" , "dollar" ,
  "loss_pw1"                   , "Range Loss Rate: 5-12% (per %)"                , "WTP (*1000 USD) for Vehicle Attributes" , "dollar" ,
  "loss_pw2"                   , "Range Loss Rate: 12-24% (per %)"               , "WTP (*1000 USD) for Vehicle Attributes" , "dollar" ,
  "loss_pw3"                   , "Range Loss Rate: 24%+ (per %)"                 , "WTP (*1000 USD) for Vehicle Attributes" , "dollar" ,
  "packreplace"                , "Battery Refurbishment: Pack Replace"           , "WTP (*1000 USD) for Vehicle Attributes" , "dollar" ,
  "cellreplace"                , "Battery Refurbishment: Cell Replace"           , "WTP (*1000 USD) for Vehicle Attributes" , "dollar" ,
  # Active variables
  "ATT_risktaker"              , "Risk-taking Propensity: Agree"                 , "Active Indicators"                      , "pct"    ,
  "ATT_range_anxiety"          , "Perceived EV Range Anxiety: Agree"             , "Active Indicators"                      , "pct"    ,
  "ATT_EVB_environment"        , "EV Battery Environmentally Positive: Agree"    , "Active Indicators"                      , "pct"    ,
  "ATT_EVB_function"           , "EV Battery Functionally Negative: Disagree"    , "Active Indicators"                      , "pct"    ,
  "hhincome_num_k"             , "Household Income (1000 USD)"                   , "Active Indicators"                      , "number" ,
  "knowledge_ev"               , "EV Knowledge"                                  , "Active Indicators"                      , "pct"    ,
  "knowledge_subsidy"          , "EV Subsidy Knowledge"                          , "Inactive Indicators"                    , "pct"    ,
  "EV_charger"                 , "Electrical Outlet Access"                      , "Active Indicators"                      , "pct"    ,
  "EV_neighbor"                , "Neighbor Owns/Leases a BEV/PHEV"               , "Inactive Indicators"                    , "pct"    ,
  "Veh_primary_refuel_monthly" , "Primary Vehicle Refuel Frequency (monthly)"    , "Inactive Indicators"                    , "number" ,
  "Veh_primary_range"          , "Primary Vehicle Typical Range (miles)"         , "Active Indicators"                      , "number" ,
  "Veh_hh_fuel"                , "Household Vehicle Fuel Composition"            , "Active Indicators"                      , "pct"    ,
  "vehicle_typesuv"            , "Next Vehicle Type: SUV"                        , "Active Indicators"                      , "pct"    ,
  # Inactive / Socioeconomic & other variables
  "age_num"                    , "Age"                                           , "Inactive Indicators"                    , "number" ,
  "gender_cate"                , "Gender"                                        , "Inactive Indicators"                    , "pct"    ,
  "ethnicity_cate"             , "Ethnicity"                                     , "Inactive Indicators"                    , "pct"    ,
  "race_cate"                  , "Race"                                          , "Inactive Indicators"                    , "pct"    ,
  "education_cate"             , "Education Level"                               , "Inactive Indicators"                    , "pct"    ,
  "student_cate"               , "Student Status"                                , "Inactive Indicators"                    , "pct"    ,
  "employment_cate"            , "Employment Status"                             , "Inactive Indicators"                    , "pct"    ,
  "hhsize_num"                 , "Household Size"                                , "Inactive Indicators"                    , "number" ,
  "hhtenure_cate"              , "Household Tenure"                              , "Inactive Indicators"                    , "pct"    ,
  "hhtype_cate"                , "Household Type"                                , "Inactive Indicators"                    , "pct"    ,
  "Veh_hh_count"               , "Household Vehicle Count"                       , "Inactive Indicators"                    , "number" ,
  "Veh_primary_fuel"           , "Primary Vehicle Fuel Type"                     , "Inactive Indicators"                    , "pct"    ,
  "next_veh_budget_k"          , "Next Vehicle Budget (1000 USD)"                , "Inactive Indicators"                    , "dollar" ,
  # "next_veh_fuel_new_phev"     , "Likelihood of buying new PHEV (1-5)"               , "Inactive Indicators"        , "number" ,
  # "next_veh_fuel_used_phev"    , "Likelihood of buying used PHEV (1-5)"              , "Inactive Indicators"        , "number" ,
  # "next_veh_fuel_new_bev"      , "Likelihood of buying new BEV (1-5)"                , "Inactive Indicators"        , "number" ,
  "next_veh_fuel_used_bev"     , "Likelihood of buying used BEV"                 , "Inactive Indicators"                    , "number" ,
  # Attitudes / psychological
  # "ATT_price_sensitive"        , "Price Sensitivity"                                 , "Inactive Indicators"             , "pct"    ,
  "ATT_climate"                , "Climate Concern"                               , "Inactive Indicators"                    , "pct"    ,
  # "ATT_political"              , "Political Spectrum"                                , "Inactive Indicators"             , "pct"    ,
  # "ATT_voting"                 , "Voting Behavior"                                   , "Inactive Indicators"             , "pct"    ,
  "battery_info_treat"         , "Received Battery Information Treatment"        , "Metadata: Information Treatment"        , "pct"
)

combined_all <- combined_all %>%
  separate(
    col = label,
    into = c("variable_origin", "category"),
    sep = " : "
  ) %>%
  mutate(
    category = case_when(
      category == "0" ~ "no",
      category == "1" ~ "yes",
      TRUE ~ category
    )
  ) %>%
  left_join(var_meta, by = c("variable_origin" = "variable")) %>%
  mutate(
    label = case_when(
      is.na(category) ~ label,
      TRUE ~ paste(label, category, sep = ": ")
    )
  )

# ── Share of respondents by number of opt-outs (0–6) ─────────────────────────
{
  n_k <- sum(grepl("^Combined_class", names(combined_all)))
  cp_optout <- read_parquet(here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    paste0("0_Combined_", n_k, "c_class_probabilities.parquet")
  )) %>%
    select(respID, paste0("prob_class", 1:n_k))

  # Total opt-outs per respondent across all 6 tasks
  resp_optouts <- database %>%
    select(respID, qID, choice) %>%
    mutate(optout = as.integer(choice == 4)) %>%
    group_by(respID) %>%
    summarise(n_optout = sum(optout), .groups = "drop") %>%
    inner_join(cp_optout, by = "respID")

  # Weighted share of respondents with exactly n opt-outs, per class
  optout_mat <- map_dfc(1:n_k, function(k) {
    pcol <- paste0("prob_class", k)
    shares <- map_dbl(0:6, function(n) {
      weighted.mean(resp_optouts$n_optout == n, resp_optouts[[pcol]])
    })
    tibble(!!paste0("Combined_class", k) := shares)
  })

  optout_rows <- bind_cols(
    tibble(
      variable = paste0("optout_n", 0:6),
      variable_origin = paste0("optout_n", 0:6),
      category = NA_character_,
      label = paste0("Opt-out: ", 0:6, " time", ifelse(0:6 == 1, "", "s")),
      section = "Metadata: Opt-out Count",
      fmt = "pct"
    ),
    optout_mat
  )

  combined_all <- bind_rows(combined_all, optout_rows)
}

# ── Survey duration by class (median seconds) ──────────────────────────────────
# Source: 0_survey_duration_by_class_6c.parquet (generated by 3b_survey_duration_6c.R)
# Re-run 3b_survey_duration_6c.R first whenever duration results are updated.
{
  duration_raw <- read_parquet(here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_survey_duration_by_class_6c.parquet"
  ))

  duration_rows <- duration_raw %>%
    pivot_longer(-class, names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = class, values_from = value) %>%
    rename_with(~ paste0("Combined_", .), starts_with("class")) %>%
    mutate(
      variable_origin = variable,
      category = NA_character_,
      label = case_when(
        variable == "duration_q1" ~ "Battery DCE Section: Q1",
        variable == "duration_q2" ~ "Battery DCE Section: Q2",
        variable == "duration_q3" ~ "Battery DCE Section: Q3",
        variable == "duration_q4" ~ "Battery DCE Section: Q4",
        variable == "duration_q5" ~ "Battery DCE Section: Q5",
        variable == "duration_q6" ~ "Battery DCE Section: Q6",
        variable == "duration_battery_dce" ~ "Battery DCE Section: Total",
        variable == "duration_full_survey" ~ "Full Survey"
      ),
      section = "Metadata: Survey Duration",
      fmt = "number"
    )

  combined_all <- bind_rows(combined_all, duration_rows)
}

# --- Format numeric values per-row ---
fmt_row <- function(x, fmt) {
  if (fmt == "dollar") {
    dollar(x, accuracy = 0.1)
  } else if (fmt == "pct") {
    percent(x, accuracy = 0.1)
  } else {
    number(x, accuracy = 0.1)
  }
}

formatted <- combined_all %>%
  rowwise() %>%
  mutate(
    fmt = case_when(
      variable %in% attributes_long ~ "dollar",
      grepl("_share$", variable) ~ "pct",
      grepl("^optout_", variable) ~ "pct",
      TRUE ~ "number"
    ),
    across(
      starts_with("Combined_class"),
      ~ fmt_row(.x, fmt),
      .names = "fmt_{col}"
    )
  ) %>%
  ungroup()

# pick formatted columns
combined_cols <- sort(names(formatted)[grepl(
  "^fmt_Combined_class",
  names(formatted)
)])

### Add WTP significance stars ----
# WTP significance is bounded above by the less significant of (attribute, price).
# wtp_sig columns (class1..class6) were computed from pmax(p_attr, p_price) above.
gt_formatted <- formatted %>%
  left_join(
    wtp_sig %>% filter(Variables %in% attributes),
    by = c("variable" = "Variables")
  ) %>%
  mutate(
    fmt_Combined_class1 = paste0(
      fmt_Combined_class1,
      "\n",
      coalesce(class1, "")
    ),
    fmt_Combined_class2 = paste0(
      fmt_Combined_class2,
      "\n",
      coalesce(class2, "")
    ),
    fmt_Combined_class3 = paste0(
      fmt_Combined_class3,
      "\n",
      coalesce(class3, "")
    ),
    fmt_Combined_class4 = paste0(
      fmt_Combined_class4,
      "\n",
      coalesce(class4, "")
    ),
    fmt_Combined_class5 = paste0(
      fmt_Combined_class5,
      "\n",
      coalesce(class5, "")
    ),
    fmt_Combined_class6 = paste0(
      fmt_Combined_class6,
      "\n",
      coalesce(class6, "")
    )
  )

### Summarize class size ----
summarize_class_size <- function(
  model,
  apollo_probabilities,
  apollo_inputs
) {
  conditionals <- apollo_conditionals(
    model,
    apollo_probabilities,
    apollo_inputs
  ) %>%
    rename_with(~ c("ID", paste0("prob_class", seq_len(length(.) - 1))))
  conditionals_long <- conditionals %>%
    pivot_longer(
      cols = starts_with("prob_class"),
      names_prefix = "prob_class",
      names_to = "class",
      values_to = "probability"
    ) %>%
    group_by(class) %>%
    summarise(
      class_size = n(),
      mean_probability = mean(probability, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      class_size = round(class_size * mean_probability, 0),
      mean_probability = round(mean_probability, 3)
    ) %>%
    ungroup() %>%
    arrange(class) %>%
    mutate(
      class_name = c("", "", "", "", "", "")
    ) %>%
    mutate(
      class_label = paste0(
        "**Class ",
        class,
        "**  \n",
        "\n",
        class_name,
        "\n",
        "(",
        format(class_size, big.mark = ","),
        "|",
        mean_probability * 100,
        "%",
        ")",
        "\n"
      )
    )
  conditionals_long
}

combined_size <- summarize_class_size(
  car_suv_lc_6c,
  car_suv_lc_6c_apollo_probabilities,
  car_suv_lc_6c_apollo_inputs
)

# --- Build gt table ---
gt_car_suv_lc_6c <- gt_formatted %>%
  select(
    section,
    label,
    all_of(combined_cols)
  ) %>%
  group_by(section) %>%
  gt(rowname_col = "label") %>%
  tab_header(
    title = md(
      "**BEV Battery Information Valuation: Latent Class Profile Summary**"
    ),
    subtitle = md("Probability-weighted means by class · Cars & SUVs")
  ) %>%
  tab_spanner(label = md("**Combined**"), columns = all_of(combined_cols)) %>%
  cols_label(
    !!!set_names(lapply(combined_size$class_label, md), combined_cols)
  ) %>%
  cols_align(align = "right", columns = everything()) %>%
  cols_align(align = "left", columns = label) %>%
  tab_footnote(
    footnote = "WTP computed as beta_attr / (-beta_price) from unconditional class-level parameters.",
    locations = cells_title("subtitle")
  ) %>%
  tab_footnote(
    footnote = "WTP significance bounded by the less significant of (attribute, price): stars reflect max(p_attr, p_price).",
    locations = cells_title("subtitle")
  ) %>%
  tab_footnote(
    footnote = "All summaries are probability-weighted using posterior class membership probabilities.",
    locations = cells_title("subtitle")
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.align = "left",
    row_group.font.weight = "bold",
    column_labels.font.weight = "bold"
  ) %>%
  opt_stylize(style = 1, color = "blue")

# ── Bold the maximum value in each row ────────────────────────────────────────
{
  numeric_cols <- gsub("^fmt_", "", combined_cols) # Combined_class1..6

  max_col_idx <- gt_formatted %>%
    select(all_of(numeric_cols)) %>%
    apply(1, function(x) {
      idx <- which.max(x)
      if (length(idx) == 0L) NA_integer_ else as.integer(idx)
    })

  for (k in seq_along(combined_cols)) {
    bold_rows <- which(max_col_idx == k)
    if (length(bold_rows) > 0) {
      gt_car_suv_lc_6c <- gt_car_suv_lc_6c %>%
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(
            columns = all_of(combined_cols[k]),
            rows = bold_rows
          )
        )
    }
  }
}

gt_car_suv_lc_6c

gtsave(
  gt_car_suv_lc_6c,
  file = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_class_profile_summary_combined_lc_6c_rangeloss.html"
  )
)

# ── LaTeX Export ──────────────────────────────────────────────────────────────
{
  library(kableExtra)
  latex_dir <- here("paper_writing", "battery_paper", "attachments")

  # Strip newlines separating value from significance stars;
  # kbl(escape = TRUE, the default) handles $ and % automatically
  latex_clean <- function(x) trimws(gsub("\n", "", x, fixed = TRUE))

  latex_vals <- gt_formatted %>%
    select(section, label, all_of(combined_cols)) %>%
    mutate(across(all_of(combined_cols), latex_clean)) %>%
    filter(!is.na(section))

  # Plain-text column headers with n and share
  latex_col_hdrs <- combined_size %>%
    transmute(
      hdr = paste0(
        "Class ",
        class,
        " (n=",
        format(class_size, big.mark = ","),
        ", ",
        round(mean_probability * 100, 1),
        "%)"
      )
    ) %>%
    pull(hdr)

  # Helper: build a longtable kbl from a filtered latex_vals subset
  make_kbl <- function(dat, caption_text, label_text) {
    grps <- dat %>%
      mutate(row_id = row_number()) %>%
      group_by(section) %>%
      summarise(start = min(row_id), end = max(row_id), .groups = "drop") %>%
      arrange(start)

    tbl <- dat %>%
      select(label, all_of(combined_cols)) %>%
      kbl(
        format = "latex",
        booktabs = TRUE,
        longtable = TRUE,
        col.names = c("", latex_col_hdrs),
        align = c("l", rep("r", length(combined_cols))),
        caption = caption_text,
        label = label_text
      ) %>%
      kable_styling(
        latex_options = "repeat_header",
        font_size = 8
      ) %>%
      column_spec(1, width = "6cm") %>%
      column_spec(2:7, width = "1.7cm")

    for (i in seq_len(nrow(grps))) {
      tbl <- tbl %>%
        pack_rows(
          group_label = grps$section[i],
          start_row = grps$start[i],
          end_row = grps$end[i],
          bold = TRUE,
          latex_gap_space = "0.3em"
        )
    }
    tbl
  }

  # kableExtra puts \label inside \caption{}; move it outside (before \\) so
  # the caption text and label are visually separated: \caption{text}\label{...}\\
  fix_caption_label <- function(s) {
    gsub(
      "\\\\caption\\{\\\\label\\{([^}]+)\\}([^}]*)\\}\\\\\\\\",
      "\\\\caption{\\2}\\\\label{\\1}\\\\\\\\",
      s,
      perl = TRUE
    )
  }

  # ── Table 1: WTP + Active Indicators + Inactive Indicators ────────────────
  latex_t1 <- latex_vals %>%
    filter(!grepl("^Metadata", section))

  kbl1 <- make_kbl(latex_t1, "WTP and Characteristics by Class", "lc6_profile")

  writeLines(
    fix_caption_label(as.character(kbl1)),
    file.path(latex_dir, "table_lc6_profile_main.tex")
  )

  # ── Table 2: Metadata sections ────────────────────────────────────────────
  latex_t2 <- latex_vals %>%
    filter(grepl("^Metadata", section))

  kbl2 <- make_kbl(
    latex_t2,
    "Metadata Summary by Class",
    "lc6_metadata"
  )

  writeLines(
    fix_caption_label(as.character(kbl2)),
    file.path(latex_dir, "table_lc6_metadata.tex")
  )

  cat("Saved:\n")
  cat(" -", file.path(latex_dir, "table_lc6_profile_main.tex"), "\n")
  cat(" -", file.path(latex_dir, "table_lc6_metadata.tex"), "\n")
}
