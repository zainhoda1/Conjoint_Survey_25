# rm(list = ls())

source(here::here('code', 'setup.R'))

# UPLOAD MODELS ----
car_suv_lc_4c <- readRDS(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "piecewise_rangeloss_car_suv_lc_4c_1_model.rds"
))
car_suv_lc_4c_apollo_inputs <- readRDS(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "car_suv_lc_4c_apollo_inputs_piecewise_rangeloss.rds"
))
car_suv_lc_4c_apollo_probabilities <- readRDS(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "car_suv_lc_4c_apollo_probabilities_piecewise_rangeloss.rds"
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
      !is.na(Veh_hh_fuel) &
      !is.na(Veh_primary_range) &
      !is.na(ATT_EVB_environment) &
      !is.na(ATT_EVB_function)
  )

database <- data_model %>%
  mutate(
    hhincome_num_k = hhincome_num / 1000,
    ATT_range_anxiety = case_when(ATT_range_anxiety > 3 ~ 1, TRUE ~ 0),
    ATT_risktaker = case_when(ATT_risktaker > 3 ~ 1, TRUE ~ 0),
    ATT_price_sensitive = case_when(ATT_price_sensitive > 3 ~ 1, TRUE ~ 0),
    ATT_climate = case_when(ATT_climate > 3 ~ 1, TRUE ~ 0),
    ATT_EVB_environment = case_when(ATT_EVB_environment > 3 ~ 1, TRUE ~ 0),
    ATT_EVB_function = case_when(ATT_EVB_function < 3 ~ 1, TRUE ~ 0),
    next_veh_fuel_used_bev = case_when(
      next_veh_fuel_used_bev == 1 ~ "strongly_disagree",
      next_veh_fuel_used_bev == 2 ~ "somewhat_disagree",
      next_veh_fuel_used_bev == 3 ~ "neutral",
      next_veh_fuel_used_bev == 4 ~ "somewhat_agree",
      next_veh_fuel_used_bev == 5 ~ "strongly_agree"
    )
  )

parse_class_suffix <- function(x) {
  case_when(
    x == "a" ~ "class1",
    x == "b" ~ "class2",
    x == "c" ~ "class3",
    TRUE ~ "class4"
  )
}

estimates_4c <- read_csv(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "piecewise_rangeloss_car_suv_lc_4c_1_estimates.csv"
))
colnames(estimates_4c)[1] <- "variable"

estimates_long <- estimates_4c %>%
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

price_pval_by_class <- estimates_long %>%
  filter(Variables == "price") %>%
  select(Class, price_pval = P.Value)

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
  pivot_wider(id_cols = Variables, names_from = Class, values_from = WTP_sig)

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
      .after = last_col()
    ) %>%
    mutate(
      prob_class_max = pmax(prob_class1, prob_class2, prob_class3, prob_class4),
      prob_class_assign = case_when(
        prob_class1 == prob_class_max ~ "class1",
        prob_class2 == prob_class_max ~ "class2",
        prob_class3 == prob_class_max ~ "class3",
        prob_class4 == prob_class_max ~ "class4",
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
  wtp_df <- map_dfr(attributes, function(attr) {
    vals <- map_dbl(1:n_classes, function(k) {
      uncond[[paste0("b_", attr)]][[k]] / (-uncond[["b_price"]][[k]])
    })
    tibble(
      variable = attr,
      !!!set_names(as.list(vals), paste0("class", 1:n_classes))
    )
  })
  num_summary <- map_dfr(num_vars, function(v) {
    map_dbl(1:n_classes, function(k) {
      weighted.mean(
        db_indiv[[v]],
        db_indiv[[paste0("prob_class", k)]],
        na.rm = TRUE
      )
    }) %>%
      {
        tibble(
          variable = v,
          !!!set_names(as.list(.), paste0("class", 1:n_classes))
        )
      }
  })
  cat_summary <- map_dfr(cate_vars, function(v) {
    levs <- sort(na.omit(unique(db_indiv[[v]])))
    map_dfr(levs, function(lv) {
      vals <- map_dbl(1:n_classes, function(k) {
        weighted.mean(
          as.numeric(db_indiv[[v]] == lv),
          db_indiv[[paste0("prob_class", k)]],
          na.rm = TRUE
        )
      })
      tibble(
        variable = paste0(v, "_", make.names(lv), "_share"),
        label_raw = paste(v, lv, sep = " : "),
        !!!set_names(as.list(vals), paste0("class", 1:n_classes))
      )
    })
  })
  bind_rows(wtp_df, num_summary, cat_summary) %>%
    replace_na(list(label_raw = NA_character_)) %>%
    rename_with(~ paste0(model_tag, "_", .), starts_with("class"))
}

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
)
cate_vars <- c(
  "ATT_range_anxiety",
  "ATT_risktaker",
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

combined_res <- summarize_lc_model(
  car_suv_lc_4c,
  car_suv_lc_4c_apollo_probabilities,
  car_suv_lc_4c_apollo_inputs,
  database,
  "Combined",
  attributes,
  num_vars,
  cate_vars
)

combined_all <- combined_res %>%
  mutate(label = coalesce(label_raw, variable)) %>%
  select(variable, label, starts_with("Combined_class")) %>%
  filter(!is.na(Combined_class1)) %>%
  mutate(across(
    .cols = where(is.numeric),
    .fns = ~ case_when(variable %in% attributes_long ~ .x * 10, TRUE ~ .x)
  ))

var_meta <- tribble(
  ~variable                    , ~label                                              , ~section                                     , ~fmt     ,
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
  "next_veh_fuel_used_bev"     , "Likelihood of buying used BEV"                     , "Inactive Indicators: Socioeconomics"        , "number" ,
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

combined_cols <- sort(names(formatted)[grepl(
  "^fmt_Combined_class",
  names(formatted)
)])

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
    )
  )

summarize_class_size <- function(model, apollo_probabilities, apollo_inputs) {
  apollo_conditionals(model, apollo_probabilities, apollo_inputs) %>%
    rename_with(~ c("ID", paste0("prob_class", seq_len(length(.) - 1)))) %>%
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
    mutate(class_name = c("", "", "", "")) %>%
    mutate(
      class_label = paste0(
        "**Class ",
        class,
        "**  \n\n",
        class_name,
        "\n(",
        format(class_size, big.mark = ","),
        "|",
        mean_probability * 100,
        "%)\n"
      )
    )
}

combined_size <- summarize_class_size(
  car_suv_lc_4c,
  car_suv_lc_4c_apollo_probabilities,
  car_suv_lc_4c_apollo_inputs
)

gt_car_suv_lc_4c <- gt_formatted %>%
  select(section, label, all_of(combined_cols)) %>%
  group_by(section) %>%
  gt(rowname_col = "label") %>%
  tab_header(
    title = md(
      "**BEV Battery Information Valuation: Latent Class Profile Summary**"
    ),
    subtitle = md("Probability-weighted means by class · Cars & SUVs 1,2,3")
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
    footnote = "WTP significance bounded by the less significant of (attribute, price).",
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

gt_car_suv_lc_4c

gtsave(
  gt_car_suv_lc_4c,
  file = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_class_profile_summary_combined_lc_4c_rangeloss.html"
  )
)
