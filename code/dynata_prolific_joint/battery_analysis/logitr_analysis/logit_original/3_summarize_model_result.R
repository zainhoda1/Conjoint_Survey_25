source(here::here('code', 'setup.R'))

# UPLOAD MODELS ----
car_lc_3c <- readRDS(here(
  "code",
  "output",
  "model_output",
  "vehicle_analysis",
  "apollo",
  "car_lc_3c_1_model.rds"
))

suv_lc_3c <- readRDS(here(
  "code",
  "output",
  "model_output",
  "vehicle_analysis",
  "apollo",
  "suv_lc_3c_1_model.rds"
))

car_lc_3c_apollo_inputs <- readRDS(here(
  "code",
  "output",
  "model_output",
  "vehicle_analysis",
  "apollo",
  "car_lc_3c_apollo_inputs.rds"
))

car_lc_3c_apollo_probabilities <- readRDS(here(
  "code",
  "output",
  "model_output",
  "vehicle_analysis",
  "apollo",
  "car_lc_3c_apollo_probabilities.rds"
))

suv_lc_3c_apollo_inputs <- readRDS(here(
  "code",
  "output",
  "model_output",
  "vehicle_analysis",
  "apollo",
  "suv_lc_3c_apollo_inputs.rds"
))

suv_lc_3c_apollo_probabilities <- readRDS(here(
  "code",
  "output",
  "model_output",
  "vehicle_analysis",
  "apollo",
  "suv_lc_3c_apollo_probabilities.rds"
))

data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_vehicle.parquet"
)) %>%
  filter(
    !is.na(hhincome_num) &
      !is.na(FA_EV_benefit) &
      !is.na(FA_EV_anxiety) &
      !is.na(hhincome_num_10k) &
      !is.na(EV_charger) &
      !is.na(EV_neighbor) &
      !is.na(knowledge_ev) &
      !is.na(knowledge_subsidy) &
      !is.na(Veh_hh_count) &
      !is.na(Veh_hh_fuel) &
      !is.na(Veh_primary_refuel_monthly) &
      !is.na(Veh_primary_range)
  )

database <- data_model %>%
  mutate(hhincome_num_k = hhincome_num / 1000)

# R
# Compute probability-weighted summaries for a model and return tidy long table

model <- suv_lc_3c
db <- database %>%
  filter(vehicle_typesuv == 1)
apollo_probabilities <- suv_lc_3c_apollo_probabilities
apollo_inputs <- suv_lc_3c_apollo_inputs

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
    rename_with(~ c("ID", paste0("prob_class_", seq_len(length(.) - 1))))
  db_indiv <- db %>%
    left_join(conditionals, by = c("respID" = "ID")) %>%
    distinct(respID, .keep_all = TRUE)
  n_classes <- length(uncond[["pi_values"]])

  # WTP
  wtp_df <- map_dfr(attributes, function(attr) {
    vals <- map_dbl(1:n_classes, function(k) {
      beta_attr <- uncond[[paste0("b_", attr)]][[k]]
      beta_price <- uncond[["b_price"]][[k]]
      beta_attr / (-beta_price)
    })
    tibble(
      variable = attr,
      !!!set_names(as.list(vals), paste0("class_", 1:n_classes))
    )
  })

  ## THIS STEP NEEDS TO BE CUSTOMIZED
  row_bev <- wtp_df %>% slice(2) %>% select(-variable) %>% unlist() # extract the row for range_bev (assuming it's the second attribute) as a numeric vector
  row_range <- wtp_df %>% slice(4) %>% select(-variable) %>% unlist() # extract the row for mileage (assuming it's the fourth attribute) as a numeric vector

  new_rows <- map_dfr(
    c(1.5, 2.5, 3.5),
    ~ as_tibble_row(row_bev + row_range * .x) %>%
      mutate(variable = paste0("bev_", .x * 100, "milerange")) %>%
      relocate(variable)
  )
  wtp_df <- bind_rows(wtp_df, new_rows)

  # Numeric summaries
  num_summary <- map_dfr(num_vars, function(v) {
    map_dbl(1:n_classes, function(k) {
      w <- db_indiv[[paste0("prob_class_", k)]]
      weighted.mean(db_indiv[[v]], w, na.rm = TRUE)
    }) %>%
      {
        tibble(
          variable = v,
          !!!set_names(as.list(.), paste0("class_", 1:n_classes))
        )
      }
  })

  # Categorical summaries: produce one variable row per level (share)
  cat_summary <- map_dfr(cate_vars, function(v) {
    levs <- sort(na.omit(unique(db_indiv[[v]])))
    map_dfr(levs, function(lv) {
      vals <- map_dbl(1:n_classes, function(k) {
        w <- db_indiv[[paste0("prob_class_", k)]]
        weighted.mean(as.numeric(db_indiv[[v]] == lv), w, na.rm = TRUE)
      })
      tibble(
        variable = paste0(v, "_", make.names(lv), "_share"),
        label_raw = paste(v, lv, sep = " : "),
        !!!set_names(as.list(vals), paste0("class_", 1:n_classes))
      )
    })
  })
  # Combine
  combined <- bind_rows(wtp_df, num_summary, cat_summary) %>%
    replace_na(list(label_raw = NA_character_))
  # pivot to wide with prefixed class columns for this model
  combined_renamed <- combined %>%
    rename_with(~ paste0(model_tag, "_", .), starts_with("class_"))
  combined_renamed
}

# --- Parameters (adjust lists to include all variables you want summarized) ---
attributes <- c(
  "no_choice",
  "powertrainbev",
  "powertrainhev",
  "range_bev",
  "mileage",
  "age",
  "operating_cost"
)
attributes_long <- c(
  attributes,
  "bev_150milerange",
  "bev_250milerange",
  "bev_350milerange"
)

num_vars <- c(
  "FA_EV_benefit",
  "FA_EV_anxiety",
  "hhincome_num_k",
  "Veh_primary_refuel_monthly",
  "Veh_primary_range",
  "age_num",
  "hhsize_num",
  "Veh_hh_count",
  "ATT_risktaker",
  "ATT_price_sensitive",
  "ATT_climate",
  "ATT_EVB_environment",
  "ATT_EVB_function",
  "next_veh_budget_k",
  "next_veh_fuel_new_phev",
  "next_veh_fuel_used_phev",
  "next_veh_fuel_new_bev",
  "next_veh_fuel_used_bev"
)
cate_vars <- c(
  "EV_charger",
  "EV_neighbor",
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
  "ATT_voting"
)

# --- Run summaries for car and suv models (must have car_lc_3c and suv_lc_3c loaded) ---
car_res <- summarize_lc_model(
  car_lc_3c,
  car_lc_3c_apollo_probabilities,
  car_lc_3c_apollo_inputs,
  database %>%
    filter(vehicle_typesuv == 0),
  "Car",
  attributes,
  num_vars,
  cate_vars
)
suv_res <- summarize_lc_model(
  suv_lc_3c,
  suv_lc_3c_apollo_probabilities,
  suv_lc_3c_apollo_inputs,
  database %>%
    filter(vehicle_typesuv == 1),
  "SUV",
  attributes,
  num_vars,
  cate_vars
)


# --- Merge car and suv by variable (use label if available) ---
combined_all <- full_join(
  car_res,
  suv_res %>% select(-label_raw),
  by = "variable"
) %>%
  mutate(label = coalesce(label_raw, variable)) %>%
  select(
    variable,
    label,
    starts_with("Car_class_"),
    starts_with("SUV_class_")
  ) %>%
  filter(!is.na(Car_class_1)) # keep only variables that appear in at least one model

# change class order
combined_all <- combined_all %>%
  rename(
    Car_class_2 = Car_class_3,
    Car_class_3 = Car_class_2,
    SUV_class_2 = SUV_class_3,
    SUV_class_3 = SUV_class_2
  )


# --- change unit ---
combined_all <- combined_all %>%
  mutate(
    across(
      .cols = where(is.numeric), # Select all numeric columns
      .fns = ~ case_when(
        variable %in% attributes_long ~ .x * 10, # Multiply by 10 if attribute matches
        TRUE ~ .x # Otherwise, keep the original value
      )
    )
  )

# change labels
var_meta <- tribble(
  ~variable                    , ~label                                          , ~section                                       , ~fmt     ,
  # WTP
  "no_choice"                  , "No-Choice Option (opt-out)"                    , "WTP (*1000 USD) for Vehicle Attributes"       , "dollar" ,
  "powertrainbev"              , "Powertrain: BEV"                               , "WTP (*1000 USD) for Vehicle Attributes"       , "dollar" ,
  "powertrainhev"              , "Powertrain: HEV"                               , "WTP (*1000 USD) for Vehicle Attributes"       , "dollar" ,
  "range_bev"                  , "BEV Electric Range (per 100 miles)"            , "WTP (*1000 USD) for Vehicle Attributes"       , "dollar" ,
  "mileage"                    , "Mileage (per 10,000 miles)"                    , "WTP (*1000 USD) for Vehicle Attributes"       , "dollar" ,
  "age"                        , "Vehicle Age (per year)"                        , "WTP (*1000 USD) for Vehicle Attributes"       , "dollar" ,
  "operating_cost"             , "Operating Cost (per cent/mile)"                , "WTP (*1000 USD) for Vehicle Attributes"       , "dollar" ,
  "bev_150milerange"           , "BEV with 150 Mile Range"                       , "WTP (*1000 USD) for BEVs with Specific Range" , "dollar" ,
  "bev_250milerange"           , "BEV with 250 Mile Range"                       , "WTP (*1000 USD) for BEVs with Specific Range" , "dollar" ,
  "bev_350milerange"           , "BEV with 350 Mile Range"                       , "WTP (*1000 USD) for BEVs with Specific Range" , "dollar" ,
  # Active variables
  "FA_EV_benefit"              , "Perceived EV Benefits (factor score)"          , "Active Indicators"                            , "number" ,
  "FA_EV_anxiety"              , "EV Range Anxiety (factor score)"               , "Active Indicators"                            , "number" ,
  "hhincome_num_k"             , "Household Income (*1000 USD)"                  , "Active Indicators"                            , "number" ,
  "knowledge_ev"               , "EV Knowledge"                                  , "Active Indicators"                            , "pct"    ,
  "knowledge_subsidy"          , "EV Subsidy Knowledge"                          , "Active Indicators"                            , "pct"    ,
  "EV_charger"                 , "Home Charger Access"                           , "Active Indicators"                            , "pct"    ,
  "EV_neighbor"                , "Neighbor Owns/Leases a BEV/PHEV"               , "Active Indicators"                            , "pct"    ,
  "Veh_primary_refuel_monthly" , "Primary Vehicle Refuel Frequency (monthly)"    , "Active Indicators"                            , "number" ,
  "Veh_primary_range"          , "Primary Vehicle Typical Range (miles)"         , "Active Indicators"                            , "number" ,

  # Inactive / Socioeconomic & other variables
  "age_num"                    , "Age"                                           , "Inactive Indicators: Socioeconomics"          , "number" ,
  "gender_cate"                , "Gender"                                        , "Inactive Indicators: Socioeconomics"          , "pct"    ,
  "ethnicity_cate"             , "Ethnicity"                                     , "Inactive Indicators: Socioeconomics"          , "pct"    ,
  "race_cate"                  , "Race"                                          , "Inactive Indicators: Socioeconomics"          , "pct"    ,
  "education_cate"             , "Education Level"                               , "Inactive Indicators: Socioeconomics"          , "pct"    ,
  "student_cate"               , "Student Status"                                , "Inactive Indicators: Socioeconomics"          , "pct"    ,
  "employment_cate"            , "Employment Status"                             , "Inactive Indicators: Socioeconomics"          , "pct"    ,
  "hhsize_num"                 , "Household Size"                                , "Inactive Indicators: Socioeconomics"          , "number" ,
  "hhtenure_cate"              , "Household Tenure"                              , "Inactive Indicators: Socioeconomics"          , "pct"    ,
  "hhtype_cate"                , "Household Type"                                , "Inactive Indicators: Socioeconomics"          , "pct"    ,
  "Veh_hh_count"               , "Household Vehicle Count"                       , "Inactive Indicators: Socioeconomics"          , "number" ,
  "Veh_hh_fuel"                , "Household Vehicle Fuel Composition"            , "Inactive Indicators: Socioeconomics"          , "pct"    ,
  "Veh_primary_fuel"           , "Primary Vehicle Fuel Type"                     , "Inactive Indicators: Socioeconomics"          , "pct"    ,
  "next_veh_budget_k"          , "Next Vehicle Budget (*1000 USD)"               , "Inactive Indicators: Socioeconomics"          , "dollar" ,
  "next_veh_fuel_new_phev"     , "Likelihood of buying new PHEV (1–5)"           , "Inactive Indicators: Socioeconomics"          , "number" ,
  "next_veh_fuel_used_phev"    , "Likelihood of buying used PHEV (1–5)"          , "Inactive Indicators: Socioeconomics"          , "number" ,
  "next_veh_fuel_new_bev"      , "Likelihood of buying new BEV (1–5)"            , "Inactive Indicators: Socioeconomics"          , "number" ,
  "next_veh_fuel_used_bev"     , "Likelihood of buying used BEV (1–5)"           , "Inactive Indicators: Socioeconomics"          , "number" ,

  # Attitudes / psychological
  "ATT_risktaker"              , "Risk-taking Propensity (1–5)"                  , "Inactive Indicators: Attitudes"               , "number" ,
  "ATT_price_sensitive"        , "Price Sensitivity (1–5)"                       , "Inactive Indicators: Attitudes"               , "number" ,
  "ATT_climate"                , "Climate Concern (1–5)"                         , "Inactive Indicators: Attitudes"               , "number" ,
  "ATT_political"              , "Political Spectrum"                            , "Inactive Indicators: Attitudes"               , "pct"    ,
  "ATT_voting"                 , "Voting Behavior"                               , "Inactive Indicators: Attitudes"               , "pct"    ,
  "ATT_EVB_environment"        , "EV Battery Environmental Attitude (1–5 scale)" , "Inactive Indicators: Attitudes"               , "number" ,
  "ATT_EVB_function"           , "EV Battery Functional Attitude (1–5 scale)"    , "Inactive Indicators: Attitudes"               , "number"
)

section_order <- c(
  "WTP (*1000 USD) for Vehicle Attributes",
  "WTP (*1000 USD) for BEVs with Specific Range",
  "Active Indicators",
  "Socioeconomics",
  "Attitudes"
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
# is_active_num <- combined_all$variable %in% num_vars
# is_cate_prefix <- map_lgl(
#   combined_all$variable,
#   ~ any(startsWith(.x, cate_vars))
# )
# combined_all <- combined_all %>%
#   arrange(is_wtp, is_active_num, is_cate_prefix, variable)

# Change the value based on unit

# --- Format numeric values per-row (dollar for WTP, pct for *_share, number otherwise) ---
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
    across(starts_with("Car_class_"), ~ fmt_row(.x, fmt), .names = "fmt_{col}"),
    across(starts_with("SUV_class_"), ~ fmt_row(.x, fmt), .names = "fmt_{col}")
  ) %>%
  ungroup()

# pick formatted columns
car_cols <- sort(names(formatted)[grepl("^fmt_Car_class_", names(formatted))])
suv_cols <- sort(names(formatted)[grepl("^fmt_SUV_class_", names(formatted))])

### Summarize class size----

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
    rename_with(~ c("ID", paste0("prob_class_", seq_len(length(.) - 1))))
  conditionals_long <- conditionals %>%
    pivot_longer(
      cols = starts_with("prob_class_"),
      names_prefix = "prob_class_",
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
    mutate(
      class_name = c("BEV-adverse", "BEV-skeptical", "BEV-ready")
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

car_size <- summarize_class_size(
  car_lc_3c,
  car_lc_3c_apollo_probabilities,
  car_lc_3c_apollo_inputs
)
suv_size <- summarize_class_size(
  suv_lc_3c,
  suv_lc_3c_apollo_probabilities,
  suv_lc_3c_apollo_inputs
)

# --- Build gt table with Car and SUV spanners ---

# --- 3. Join meta to long table and arrange by section order ---

# prepare display frame (uses `formatted` and `var_meta` already in session)
# build base gt table

gt_car_suv_lc_3c <- formatted %>%
  select(section, label, all_of(car_cols), all_of(suv_cols)) %>%
  group_by(section) %>%
  gt(rowname_col = "label") %>%
  tab_header(
    title = md("**Latent Class Profile Summary**"),
    subtitle = md("Probability-weighted means by class · Cars & SUVs")
  ) %>%
  tab_spanner(label = md("**Car**"), columns = all_of(car_cols)) %>%
  tab_spanner(label = md("**SUV**"), columns = all_of(suv_cols)) %>%
  cols_label(
    !!!set_names(lapply(car_size$class_label, md), car_cols),
    !!!set_names(lapply(suv_size$class_label, md), suv_cols)
  ) %>%
  tab_footnote(
    footnote = "WTP computed as –β_attr / β_price from unconditional class-level parameters.",
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
  opt_stylize(style = 1, color = "blue") # subtle row striping for readability

# render / save
gt_car_suv_lc_3c

gtsave(
  gt_car_suv_lc_3c,
  file = here::here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "class_profile_summary_cars_suvs_lc_3c.html"
  )
)

gtsave(
  gt_car_suv_lc_3c,
  file = file = here::here(
    "code",
    "output",
    "images",
    "battery_analysis",
    "class_profile_summary_cars_suvs_lc_3c.png"
  ),
  expand = 20
)
