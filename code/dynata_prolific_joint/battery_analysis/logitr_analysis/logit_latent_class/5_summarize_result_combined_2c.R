source(here::here('code', 'setup.R'))

# UPLOAD MODELS ----
wtp_models <- readRDS(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "logitr",
  "latent_class",
  "ml_2c_wtp_combined.Rds"
))

# Coef models----
# Helper to assign significance stars
get_sig <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ ".",
    TRUE ~ ""
  )
}

# mod <- wtp_models[[1]]
# Extract summary for each model
coef_tbl <- map_dfr(
  names(wtp_models),
  function(tag) {
    mod <- wtp_models[[tag]]
    if (inherits(mod, "logitr")) {
      s <- summary(mod)
      tibble(
        tag = tag,
        variable = rownames(s$coefTable),
        estimate = round(s$coefTable[, "Estimate"], 3),
        std_error = round(s$coefTable[, "Std. Error"], 3),
        # pval = s$coefTable[, "Pr(>|z|)"],
        sig = get_sig(s$coefTable[, "Pr(>|z|)"])
      )
    }
  },
  .id = NULL
)

# # Combine estimate and significance
# coef_tbl <- coef_tbl %>%
#   mutate(est_sig = str_c(estimate, sig)) %>%
#   select(tag, coef, est_sig)

# # Pivot to wide format: rows = coef, columns = tag (segment)
# summary_table <- coef_tbl %>%
#   pivot_wider(names_from = tag, values_from = est_sig) %>%
#   as.data.frame() %>%
#   select(coef, starts_with("treat0_car"), starts_with("treat0_suv"), starts_with("treat1_car"), starts_with("treat1_suv"))

# summary_table

# Coef models----

formatted <- coef_tbl %>%
  separate(
    col = tag,
    into = c("Segment", "Class"),
    sep = "_",
    extra = "merge",
    fill = "right"
  ) %>%
  pivot_wider(
    id_cols = c(variable),
    names_from = c(Segment, Class),
    values_from = c(estimate, std_error, sig),
    names_glue = "{.value}_{Segment}_{Class}"
  )


var_meta <- tribble(
  ~variable                         , ~label                                     , ~section                                                     , ~fmt     ,
  # WTP

  "mileage"                         , "Mileage (10,000 miles)"                   , "Vehicle Attributes (Directly interpretable as $10,000 WTP)" , "dollar" ,
  "battery_range_year0"             , "BEV Electric Range at Year 0 (100 miles)" , "Vehicle Attributes (Directly interpretable as $10,000 WTP)" , "dollar" ,
  "battery_degradation"             , "Battery Degradation Rate (% per year)"    , "Vehicle Attributes (Directly interpretable as $10,000 WTP)" , "dollar" ,
  "battery_refurbishpackreplace"    , "Battery Refurbishment: Pack Replace"      , "Vehicle Attributes (Directly interpretable as $10,000 WTP)" , "dollar" ,
  "battery_refurbishcellreplace"    , "Battery Refurbishment: Cell Replace"      , "Vehicle Attributes (Directly interpretable as $10,000 WTP)" , "dollar" ,
  # Active variables
  "scalePar"                        , "Scale parameter"                          , "Other Parameters"                                           , "number" ,
  "no_choice"                       , "No-Choice Option (opt-out)"               , "Other Parameters"                                           , "number" ,
  "sd_mileage"                      , "SD (Mileage)"                             , "Other Parameters"                                           , "number" ,
  "sd_battery_range_year0"          , "SD (Battery Range Year 0)"                , "Other Parameters"                                           , "number" ,
  "sd_battery_degradation"          , "SD (Battery Degradation)"                 , "Other Parameters"                                           , "number" ,
  "sd_battery_refurbishpackreplace" , "SD (Battery Refurbishment: Pack Replace)" , "Other Parameters"                                           , "number" ,
  "sd_battery_refurbishcellreplace" , "SD (Battery Refurbishment: Cell Replace)" , "Other Parameters"                                           , "number"
)

formatted <- formatted %>%
  left_join(
    var_meta %>% select(variable, label, section, fmt),
    by = c("variable")
  )

write.csv(
  formatted,
  here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "latent_class",
    "0_car_suv_lc_3c_formatted_model_estimates.csv"
  ),
  row.names = FALSE
)

library(glue)
# 1. Create display columns for each class in each segment
gt_formatted <- formatted %>%
  as.data.frame() %>%
  mutate(
    car_class1 = glue(
      "{estimate_car_class1}\n({std_error_car_class1}){sig_car_class1}",
      .na = NULL
    ),
    car_class2 = glue(
      "{estimate_car_class2}\n({std_error_car_class2}){sig_car_class2}",
      .na = NULL
    ),
    suv_class1 = glue(
      "{estimate_suv_class1}\n({std_error_suv_class1}){sig_suv_class1}",
      .na = NULL
    ),
    suv_class2 = glue(
      "{estimate_suv_class2}\n({std_error_suv_class2}){sig_suv_class2}",
      .na = NULL
    )
  ) %>%
  select(
    label,
    section,
    starts_with("car"),
    starts_with("suv"),
  ) %>%
  arrange(desc(section))


summarize_class_size <- ftable(
  data_dce$vehicle_typesuv,
  # data_dce$battery_info_treat,
  data_dce$prob_class_assign
) %>%
  as.data.frame() %>%
  rename(
    vehicle_typesuv = Var1,
    # battery_info_treat = Var2,
    class_name = Var2,
    class_size = Freq
  ) %>%
  mutate(class_size = class_size / 24) %>%
  group_by(vehicle_typesuv) %>%
  mutate(mean_probability = round(class_size / sum(class_size), 3)) %>%
  ungroup() %>%
  arrange(vehicle_typesuv, class_name) %>%
  mutate(
    vehicle_typesuv = case_when(
      vehicle_typesuv == 0 ~ "car",
      vehicle_typesuv == 1 ~ "suv",
      TRUE ~ vehicle_typesuv
    ),
    tag = paste0(
      # "treat",
      # battery_info_treat,
      # "_",
      vehicle_typesuv,
      "_",
      class_name
    ),
    class_name = case_when(
      class_name == "class1" ~ "Class1\nBEV-adverse",
      class_name == "class2" ~ "Class2\nBEV-open"
    )
  ) %>%
  mutate(
    class_label = paste0(
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
  ) %>%
  select(tag, class_label) %>%
  deframe()

gt_car_suv_ml_2c <- gt_formatted |>
  group_by(section) |>
  gt(rowname_col = "label") |>
  # tab_spanner(
  #   label = "Car",
  #   id = "treat0_car",
  #   columns = starts_with("treat0") & contains("car")
  # ) |>
  tab_spanner(
    label = "SUV",
    columns = contains("suv")
  ) |>
  tab_spanner(
    label = "Car",
    columns = contains("car")
  ) |>
  cols_label(.list = summarize_class_size) |>
  tab_header(
    title = md(
      "**BEV Battery Information Valuation: Model Results (Car vs SUV)**"
    ),
    subtitle = "Est. (SE)[Sig.] for each group "
  ) |>
  cols_align(align = "left", everything()) |>
  tab_options(
    table.font.size = px(13),
    heading.align = "left",
    row_group.font.weight = "bold",
    column_labels.font.weight = "bold"
  ) |>
  opt_stylize(style = 1, color = "blue")

gt_car_suv_ml_2c

gtsave(
  gt_car_suv_ml_2c,
  file = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "latent_class",
    "0_model_summary_cars_suvs_ml_2c_combined.html"
  )
)
