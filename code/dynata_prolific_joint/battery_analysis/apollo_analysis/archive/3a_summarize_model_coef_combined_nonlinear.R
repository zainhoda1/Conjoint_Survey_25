source(here::here('code', 'setup.R'))

# Suppress scientific notation so small values like 0.0003 print as-is
options(scipen = 999)

# UPLOAD MODELS ----

car_suv_lc_3c <- read_csv(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "nonlinear_rangeloss_car_suv_lc_3c_1_estimates.csv"
))
colnames(car_suv_lc_3c)[1] <- "variable"


# ---- Format combined model coefficients ----
# Columns: 1=variable, 2=Estimate, 6=Rob.std.err., 8=Rob.p-val(0)
# (trailing comma in CSV creates an empty 9th column; excluded here)
combined_all <- car_suv_lc_3c %>%
  mutate(vehicle = "Combined") %>%
  as.data.frame() %>%
  select(1:2, 6, 8) %>%
  mutate(across(
    where(is.numeric),
    ~ if_else(abs(.) < 0.005 & . != 0, round(., 4), round(., 2))
  )) %>%
  setNames(c("Variables", "Est.", "Std.Err.", "P.Value")) %>%
  mutate(
    Sig. = case_when(
      P.Value < 0.001 ~ "***",
      P.Value < 0.01 ~ "**",
      P.Value < 0.05 ~ "*",
      P.Value < 0.1 ~ ".",
      T ~ " "
    )
  ) %>%
  filter(!is.na(Std.Err.)) %>%
  mutate(
    Variables = str_replace_all(
      Variables,
      pattern = "b_|gamma_",
      replacement = ""
    )
  ) %>%
  mutate(
    Variables = str_replace_all(
      Variables,
      pattern = "delta",
      replacement = "ASC"
    )
  ) %>%
  select(-P.Value)


formatted <- combined_all %>%
  separate(
    col = Variables,
    into = c("Variables", "Class"),
    sep = "_(?=[^_]+$)",
    extra = "merge",
    fill = "right"
  ) %>%
  mutate(
    Class = case_when(
      Class == "a" ~ "class1",
      Class == "b" ~ "class2",
      TRUE ~ "class3"
    )
  ) %>%
  pivot_wider(
    id_cols = c(Variables),
    names_from = Class,
    values_from = c(Est., Std.Err., Sig.),
    names_glue = "{.value}_{Class}"
  )

var_meta <- tribble(
  ~variable               , ~label                                       , ~section             , ~fmt     ,
  "no_choice"             , "No-Choice Option (opt-out)"                 , "Vehicle Attributes" , "dollar" ,
  "mileage"               , "Mileage (10,000 miles)"                     , "Vehicle Attributes" , "dollar" ,
  "range_year0"           , "BEV Electric Range (Year 3, 100 miles)"     , "Vehicle Attributes" , "dollar" ,
  "range_year0_quadratic" , "BEV Electric Range\u00b2 (Year 3)"          , "Vehicle Attributes" , "dollar" ,
  "degradation"           , "Range Loss Rate (%)"                        , "Vehicle Attributes" , "dollar" ,
  "rangeloss_quadratic"   , "Range Loss Rate\u00b2 (%\u00b2)"            , "Vehicle Attributes" , "dollar" ,
  "packreplace"           , "Battery Refurbishment: Pack Replace"        , "Vehicle Attributes" , "dollar" ,
  "cellreplace"           , "Battery Refurbishment: Cell Replace"        , "Vehicle Attributes" , "dollar" ,
  "price"                 , "Purchase Price (10,000 USD)"                , "Vehicle Attributes" , "dollar" ,
  # Active variables
  "ASC"                   , "ASC"                                        , "Active Indicators"  , "number" ,
  "EV_rangeanxiety"       , "Perceived EV Range Anxiety: Agree"          , "Active Indicators"  , "number" ,
  "risktaker"             , "Risk Taking Propensity: Agree"              , "Active Indicators"  , "number" ,
  "evenvironment_agree"   , "EV Battery Environmentally Positive: Agree" , "Active Indicators"  , "number" ,
  "evfunction_disagree"   , "EV Battery Functionally Negative: Disagree" , "Active Indicators"  , "number" ,
  "hhincome"              , "Household Income (10,000 USD)"              , "Active Indicators"  , "number" ,
  "ev_charge"             , "Electrical Outlet Access"                   , "Active Indicators"  , "pct"    ,
  "hhveh_fuel"            , "Primary Household Vehicle Fuel Type: ICEV"  , "Active Indicators"  , "pct"    ,
  "veh_range"             , "Primary Vehicle Typical Range (miles)"      , "Active Indicators"  , "number" ,
  "suv"                   , "SUV Segment"                                , "Active Indicators"  , "pct"
)

formatted <- formatted %>%
  left_join(
    var_meta %>% select(variable, label, section, fmt),
    by = c("Variables" = "variable")
  ) %>%
  mutate(label = coalesce(label, Variables))

write.csv(
  formatted,
  here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_car_suv_lc_3c_formatted_model_estimates_nonlinear_rangeloss.csv"
  ),
  row.names = FALSE
)

library(glue)
# Create display columns for each class
gt_formatted <- formatted %>%
  mutate(
    Combined_class1 = glue(
      "{Est._class1}\n({Std.Err._class1}){Sig._class1}",
      .na = NULL
    ),
    Combined_class2 = glue(
      "{Est._class2}\n({Std.Err._class2}){Sig._class2}",
      .na = NULL
    ),
    Combined_class3 = glue(
      "{Est._class3}\n({Std.Err._class3}){Sig._class3}",
      .na = NULL
    )
  ) %>%
  select(label, section, starts_with("Combined_class"))

# gt_car_suv_lc_3c <- gt_formatted |>
#   group_by(section) %>%
#   gt(rowname_col = "label") |>
#   tab_spanner(
#     label = "Car",
#     columns = c(Car_class1, Car_class2, Car_class3)
#   ) |>
#   tab_spanner(
#     label = "SUV",
#     columns = c(SUV_class1, SUV_class2, SUV_class3)
#   ) |>
#   tab_spanner(
#     label = "Combined",
#     columns = c(Combined_class1, Combined_class2, Combined_class3)
#   ) |>
#   cols_align(align = "right", columns = everything()) %>%
#   cols_align(align = "left", columns = label) %>%
#   cols_label(
#     Car_class1 = "Class 1",
#     Car_class2 = "Class 2",
#     Car_class3 = "Class 3",
#     SUV_class1 = "Class 1",
#     SUV_class2 = "Class 2",
#     SUV_class3 = "Class 3",
#     Combined_class1 = "Class 1",
#     Combined_class2 = "Class 2",
#     Combined_class3 = "Class 3"
#   ) |>
#   tab_header(
#     title = md(
#       "**BEV Battery Information Valuation: Model Results (Car vs SUV)**"
#     ),
#     subtitle = "Est. (SE)[Sig.] for each class"
#   ) %>%
#   cols_align(align = "left", everything()) %>%
#   tab_options(
#     table.font.size = px(13),
#     heading.align = "left",
#     row_group.font.weight = "bold",
#     column_labels.font.weight = "bold"
#   ) %>%
#   opt_stylize(style = 1, color = "blue") #

# gt_car_suv_lc_3c

# gtsave(
#   gt_car_suv_lc_3c,
#   file = here::here(
#     "code",
#     "output",
#     "model_output",
#     "battery_analysis",
#     "apollo",
#     "0_model_summary_cars_suvs_lc_3c.html"
#   )
# )

gt_car_suv_lc_3c <- gt_formatted |>
  group_by(section) %>%
  gt(rowname_col = "label") |>
  # tab_spanner(
  #   label = "Car",
  #   columns = c(Car_class1, Car_class2, Car_class3)
  # ) |>
  # tab_spanner(
  #   label = "SUV",
  #   columns = c(SUV_class1, SUV_class2, SUV_class3)
  # ) |>
  tab_spanner(
    label = "Combined",
    columns = c(Combined_class1, Combined_class2, Combined_class3)
  ) |>
  cols_align(align = "right", columns = everything()) %>%
  cols_align(align = "left", columns = label) %>%
  cols_label(
    # Car_class1 = "Class 1",
    # Car_class2 = "Class 2",
    # Car_class3 = "Class 3",
    # SUV_class1 = "Class 1",
    # SUV_class2 = "Class 2",
    # SUV_class3 = "Class 3",
    Combined_class1 = "Class 1",
    Combined_class2 = "Class 2",
    Combined_class3 = "Class 3"
  ) |>
  tab_header(
    title = md(
      "**BEV Battery Information Valuation: Model Results (Car vs SUV)**"
    ),
    subtitle = "Est. (SE)[Sig.] for each class"
  ) %>%
  cols_align(align = "left", everything()) %>%
  tab_options(
    table.font.size = px(13),
    heading.align = "left",
    row_group.font.weight = "bold",
    column_labels.font.weight = "bold"
  ) %>%
  opt_stylize(style = 1, color = "blue") #

gt_car_suv_lc_3c

gtsave(
  gt_car_suv_lc_3c,
  file = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_model_summary_cars_suvs_lc_3c_nonlinear_rangeloss.html"
  )
)
