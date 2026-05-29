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
    next_veh_fuel_used_bev=case_when(next_veh_fuel_used_bev==1 ~"strongly_disagree",
    next_veh_fuel_used_bev==2 ~"somewhat_disagree",
    next_veh_fuel_used_bev==3 ~"neutral",
    next_veh_fuel_used_bev==4 ~"somewhat_agree",
    next_veh_fuel_used_bev==5 ~"strongly_agree")

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
  "ATT_political",
  "ATT_voting",
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
  ~variable                    , ~label                                              , ~section                                     , ~fmt     ,
  # WTP
  "no_choice"                  , "No-Choice Option (opt-out)"                        , "WTP (*1000 USD) for Vehicle Attributes"     , "dollar" ,
  "mileage"                    , "Mileage (10,000 miles)"                            , "WTP (*1000 USD) for Vehicle Attributes"     , "dollar" ,
  "range_pw1"                  , "BEV Range WTP: 40-130 mi segment (per 100 miles)"  , "WTP (*1000 USD) for Vehicle Attributes"     , "dollar" ,
  "range_pw2"                  , "BEV Range WTP: 130-200 mi segment (per 100 miles)" , "WTP (*1000 USD) for Vehicle Attributes"     , "dollar" ,
  "range_pw3"                  , "BEV Range WTP: 200+ mi segment (per 100 miles)"    , "WTP (*1000 USD) for Vehicle Attributes"     , "dollar" ,
  "loss_pw1"                   , "Range Loss WTP: 5-12% segment (per %)"             , "WTP (*1000 USD) for Vehicle Attributes"     , "dollar" ,
  "loss_pw2"                   , "Range Loss WTP: 12-24% segment (per %)"            , "WTP (*1000 USD) for Vehicle Attributes"     , "dollar" ,
  "loss_pw3"                   , "Range Loss WTP: 24%+ segment (per %)"              , "WTP (*1000 USD) for Vehicle Attributes"     , "dollar" ,
  "packreplace"                , "Battery Refurbishment: Pack Replace"               , "WTP (*1000 USD) for Vehicle Attributes"     , "dollar" ,
  "cellreplace"                , "Battery Refurbishment: Cell Replace"               , "WTP (*1000 USD) for Vehicle Attributes"     , "dollar" ,
  # Active variables
  "ATT_risktaker"              , "Risk-taking Propensity: Agree"                     , "Inactive Indicators: Attitudes"             , "pct"    ,
  "ATT_range_anxiety"          , "Perceived EV Range Anxiety: Agree"                 , "Active Indicators"                          , "pct"    ,
  "ATT_EVB_environment"        , "EV Battery Environmentally Positive: Agree"        , "Active Indicators"                          , "pct"    ,
  "ATT_EVB_function"           , "EV Battery Functionally Negative: Disagree"        , "Active Indicators"                          , "pct"    ,
  "hhincome_num_k"             , "Household Income (1000 USD)"                       , "Active Indicators"                          , "number" ,
  "knowledge_ev"               , "EV Knowledge"                                      , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "knowledge_subsidy"          , "EV Subsidy Knowledge"                              , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "EV_charger"                 , "Electrical Outlet Access"                          , "Active Indicators"                          , "pct"    ,
  "EV_neighbor"                , "Neighbor Owns/Leases a BEV/PHEV"                   , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "Veh_primary_refuel_monthly" , "Primary Vehicle Refuel Frequency (monthly)"        , "Inactive Indicators: Socioeconomics"        , "number" ,
  "Veh_primary_range"          , "Primary Vehicle Typical Range (miles)"             , "Active Indicators"                          , "number" ,
  "Veh_hh_fuel"                , "Household Vehicle Fuel Composition"                , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "vehicle_typesuv"            , "Next Vehicle Type: SUV"                            , "Active Indicators"                          , "pct"    ,
  # Inactive / Socioeconomic & other variables
  "age_num"                    , "Age"                                               , "Inactive Indicators: Socioeconomics"        , "number" ,
  "gender_cate"                , "Gender"                                            , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "ethnicity_cate"             , "Ethnicity"                                         , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "race_cate"                  , "Race"                                              , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "education_cate"             , "Education Level"                                   , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "student_cate"               , "Student Status"                                    , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "employment_cate"            , "Employment Status"                                 , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "hhsize_num"                 , "Household Size"                                    , "Inactive Indicators: Socioeconomics"        , "number" ,
  "hhtenure_cate"              , "Household Tenure"                                  , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "hhtype_cate"                , "Household Type"                                    , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "Veh_hh_count"               , "Household Vehicle Count"                           , "Inactive Indicators: Socioeconomics"        , "number" ,
  "Veh_primary_fuel"           , "Primary Vehicle Fuel Type"                         , "Inactive Indicators: Socioeconomics"        , "pct"    ,
  "next_veh_budget_k"          , "Next Vehicle Budget (1000 USD)"                    , "Inactive Indicators: Socioeconomics"        , "dollar" ,
  # "next_veh_fuel_new_phev"     , "Likelihood of buying new PHEV (1-5)"               , "Inactive Indicators: Socioeconomics"        , "number" ,
  # "next_veh_fuel_used_phev"    , "Likelihood of buying used PHEV (1-5)"              , "Inactive Indicators: Socioeconomics"        , "number" ,
  # "next_veh_fuel_new_bev"      , "Likelihood of buying new BEV (1-5)"                , "Inactive Indicators: Socioeconomics"        , "number" ,
  "next_veh_fuel_used_bev"     , "Likelihood of buying used BEV"               , "Inactive Indicators: Socioeconomics"        , "number" ,
  # Attitudes / psychological
  # "ATT_price_sensitive"        , "Price Sensitivity"                                 , "Inactive Indicators: Attitudes"             , "pct"    ,
  "ATT_climate"                , "Climate Concern"                                   , "Inactive Indicators: Attitudes"             , "pct"    ,
  "ATT_political"              , "Political Spectrum"                                , "Inactive Indicators: Attitudes"             , "pct"    ,
  "ATT_voting"                 , "Voting Behavior"                                   , "Inactive Indicators: Attitudes"             , "pct"    ,
  "battery_info_treat"         , "Received Battery Information Treatment"            , "Inactive Indicators: Information Treatment" , "pct"
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
      mean_probability = round(mean_probability, 2)
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

# ============================================================
# CLASS 6 SUBGROUP ANALYSIS
# Hypothesis: C6 contains two distinct groups:
#   (a) genuine pre-committed adopters (slow, occasionally opt out)
#   (b) satisficers avoiding opt-out from survey fear (fast, never opt out)
# Strategy: median split on mean task duration; compare all characteristics
# ============================================================

library(glue)
library(lubridate)

# --- 1. Class probabilities ---
class_probs_6c <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "0_Combined_6c_class_probabilities.parquet"
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

timing_c6 <- data_full_raw %>%
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

# --- 5. Class 6 respondent-level dataset ---
c6_resp <- class_probs_6c %>%
  filter(prob_class_assign == "class6") %>%
  left_join(timing_c6, by = "psid") %>%
  left_join(optout_per_resp, by = "psid") %>%
  left_join(resp_covariates, by = "psid") %>%
  filter(!is.na(mean_cbc_sec))

# --- 6. Median split on mean task duration ---
median_sec <- median(c6_resp$mean_cbc_sec, na.rm = TRUE)

c6_resp <- c6_resp %>%
  mutate(
    speed_group = factor(
      if_else(mean_cbc_sec <= median_sec, "Fast", "Slow"),
      levels = c("Fast", "Slow")
    )
  )

# --- 7. Compute group summaries ---

# Behavioral diagnostics
diag_summary <- c6_resp %>%
  group_by(speed_group) %>%
  summarise(
    n = n(),
    mean_duration_sec = weighted.mean(mean_cbc_sec, prob_class6, na.rm = TRUE),
    optout_rate = weighted.mean(optout_rate_resp, prob_class6, na.rm = TRUE),
    optout_never_pct = weighted.mean(optout_never, prob_class6, na.rm = TRUE),
    .groups = "drop"
  )

# Numeric variable comparison
compare_num <- map_dfr(num_vars, function(v) {
  map_dfr(c("Fast", "Slow"), function(grp) {
    sub <- c6_resp %>% filter(speed_group == grp)
    tibble(
      variable = v,
      speed_group = grp,
      value = weighted.mean(sub[[v]], sub$prob_class6, na.rm = TRUE)
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
    sub <- c6_resp %>% filter(speed_group == grp)
    tibble(
      variable = v,
      speed_group = grp,
      value = weighted.mean(
        as.numeric(sub[[v]] == 1),
        sub$prob_class6,
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
gt_c6_subgroup <- display_tbl %>%
  group_by(section) %>%
  gt(rowname_col = "label") %>%
  tab_header(
    title = md(
      "**Class 6 Internal Subgroup Comparison: Fast vs. Slow Responders**"
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
    footnote = "All means probability-weighted by each respondent's posterior P(class 6). Behavioral rows use raw means.",
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

gt_c6_subgroup

gtsave(
  gt_c6_subgroup,
  file = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_c6_subgroup_fast_vs_slow.html"
  )
)

# --- 10. Scatter: duration vs opt-out rate per C6 respondent ---
p_c6_scatter <- c6_resp %>%
  mutate(
    mean_cbc_cap = pmin(
      mean_cbc_sec,
      quantile(mean_cbc_sec, 0.97, na.rm = TRUE)
    )
  ) %>%
  ggplot(aes(
    x = mean_cbc_cap,
    y = optout_rate_resp,
    color = prob_class6,
    size = prob_class6
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
  scale_color_gradient(low = "#FFCDD2", high = "#B71C1C", name = "P(class 6)") +
  scale_size_continuous(range = c(1, 4), guide = "none") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.2)) +
  labs(
    title = "C6 Respondents: Task Duration vs. Opt-Out Rate",
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

p_c6_scatter

ggsave(
  here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "c6_duration_vs_optout_scatter.png"
  ),
  p_c6_scatter,
  width = 9,
  height = 5.5,
  dpi = 300
)
