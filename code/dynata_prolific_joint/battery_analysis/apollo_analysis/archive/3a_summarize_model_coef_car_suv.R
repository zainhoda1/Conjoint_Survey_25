source(here::here('code', 'setup.R'))

# UPLOAD MODELS ----
car_lc_3c <- read_csv(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "rangeloss_car_lc_3c_1_estimates.csv"
))
colnames(car_lc_3c)[1] <- "variable"

suv_lc_3c <- read_csv(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "rangeloss_suv_lc_3c_1_estimates.csv"
))
colnames(suv_lc_3c)[1] <- "variable"

car_suv_lc_3c <- read_csv(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "rangeloss_car_suv_lc_3c_1_estimates.csv"
  # "degradation_car_suv_lc_3c_1_estimates.csv"
))
colnames(car_suv_lc_3c)[1] <- "variable"


# ----Combine model coefficients for car and suv----
combined_all <- bind_rows(
  car_lc_3c %>%
    mutate(vehicle = "Car"),
  suv_lc_3c %>%
    mutate(vehicle = "SUV"),
  car_suv_lc_3c %>%
    mutate(vehicle = "Combined")
) %>%
  as.data.frame() %>%
  select(1:2, 6, 8, 9) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  setNames(c("Variables", "Est.", "Std.Err.", "P.Value", "Segment")) %>%
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
    names_from = c(Class, Segment),
    values_from = c(Est., Std.Err., Sig.),
    names_glue = "{.value}_{Segment}_{Class}"
  )

var_meta <- tribble(
  ~variable             , ~label                                             , ~section             , ~fmt     ,
  # WTP
  "no_choice"           , "No-Choice Option (opt-out)"                       , "Vehicle Attributes" , "dollar" ,
  "mileage"             , "Mileage (10,000 miles)"                           , "Vehicle Attributes" , "dollar" ,
  # "range_year0"         , "BEV Electric Range at Year 0 (100 miles)"   , "Vehicle Attributes" , "dollar" ,
  # "degradation"         , "Battery Degradation Rate (% per year)"      , "Vehicle Attributes" , "dollar" ,
  # "range_year0"         , "BEV Electric Range at Year 3 (per 100 miles)"   , "Vehicle Attributes" , "dollar" ,
  # "degradation"         , "Range loss in the next 5 years (per 100 miles)" , "Vehicle Attributes" , "dollar" ,
  "range_year0"         , "BEV Electric Range at Year 3 (per 100 miles)"     , "Vehicle Attributes" , "dollar" ,
  "degradation"         , "Range loss rate in the next 5 years (percentage)" , "Vehicle Attributes" , "dollar" ,
  # "range_year0"         , "BEV Electric Range at Year 0 (per 100 miles)" , "Vehicle Attributes" , "dollar" ,
  # "degradation"         , "Range loss rate after 8 years (percentage)"   , "Vehicle Attributes" , "dollar" ,

  "packreplace"         , "Battery Refurbishment: Pack Replace"              , "Vehicle Attributes" , "dollar" ,
  "cellreplace"         , "Battery Refurbishment: Cell Replace"              , "Vehicle Attributes" , "dollar" ,
  "price"               , "Purchase Price (10,000 USD)"                      , "Vehicle Attributes" , "dollar" ,
  # Active variables
  "ASC"                 , "ASC"                                              , "Active Indicators"  , "number" ,
  "EV_rangeanxiety"     , "Perceived EV Range Anxiety: Agree"                , "Active Indicators"  , "number" ,
  "risktaker"           , "Risk Taking Propensity: Agree"                    , "Active Indicators"  , "number" ,
  "evenvironment_agree" , "EV Battery Environmentally Positive: Agree"       , "Active Indicators"  , "number" ,
  "evfunction_disagree" , "EV Battery Functionally Negative: Disagree"       , "Active Indicators"  , "number" ,
  "hhincome"            , "Household Income (10,000 USD)"                    , "Active Indicators"  , "number" ,
  # "knowledge_ev"        , "EV Knowledge"                                   , "Active Indicators"  , "pct"    ,
  "ev_charge"           , "Electrical Outlet Access"                         , "Active Indicators"  , "pct"    ,
  # "ev_neighbor"         , "Neighbor Owns/Leases a BEV/PHEV"                , "Active Indicators"  , "pct"    ,
  "hhveh_fuel"          , "Primary Household Vehicle Fuel Type: ICEV"        , "Active Indicators"  , "pct"    ,
  # "veh_refuel"          , "Primary Vehicle Refuel Frequency (monthly)"     , "Active Indicators"  , "number" ,
  "veh_range"           , "Primary Vehicle Typical Range (miles)"            , "Active Indicators"  , "number" ,
  "suv"                 , "SUV Segment"                                      , "Active Indicators"  , "pct"
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
    "0_car_suv_lc_3c_formatted_model_estimates_rangeloss.csv"
    # "0_car_suv_lc_3c_formatted_model_estimates_degradation.csv"
  ),
  row.names = FALSE
)

library(glue)
# 1. Create display columns for each class in each segment
gt_formatted <- formatted %>%
  mutate(
    Car_class1 = glue(
      "{Est._Car_class1}\n({Std.Err._Car_class1}){Sig._Car_class1}",
      .na = NULL
    ),
    Car_class2 = glue(
      "{Est._Car_class2}\n({Std.Err._Car_class2}){Sig._Car_class2}",
      .na = NULL
    ),
    Car_class3 = glue(
      "{Est._Car_class3}\n({Std.Err._Car_class3}){Sig._Car_class3}",
      .na = NULL
    ),
    SUV_class1 = glue(
      "{Est._SUV_class1}\n({Std.Err._SUV_class1}){Sig._SUV_class1}",
      .na = NULL
    ),
    SUV_class2 = glue(
      "{Est._SUV_class2}\n({Std.Err._SUV_class2}){Sig._SUV_class2}",
      .na = NULL
    ),
    SUV_class3 = glue(
      "{Est._SUV_class3}\n({Std.Err._SUV_class3}){Sig._SUV_class3}",
      .na = NULL
    ),
    Combined_class1 = glue(
      "{Est._Combined_class1}\n({Std.Err._Combined_class1}){Sig._Combined_class1}",
      .na = NULL
    ),
    Combined_class2 = glue(
      "{Est._Combined_class2}\n({Std.Err._Combined_class2}){Sig._Combined_class2}",
      .na = NULL
    ),
    Combined_class3 = glue(
      "{Est._Combined_class3}\n({Std.Err._Combined_class3}){Sig._Combined_class3}",
      .na = NULL
    )
  ) %>%
  select(
    label,
    section,
    starts_with("Car_class"),
    starts_with("SUV_class"),
    starts_with("Combined_class")
  )

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
  tab_spanner(
    label = "Car",
    columns = c(Car_class1, Car_class2, Car_class3)
  ) |>
  tab_spanner(
    label = "SUV",
    columns = c(SUV_class1, SUV_class2, SUV_class3)
  ) |>
  tab_spanner(
    label = "Combined",
    columns = c(Combined_class1, Combined_class2, Combined_class3)
  ) |>
  cols_align(align = "right", columns = everything()) %>%
  cols_align(align = "left", columns = label) %>%
  cols_label(
    Car_class1 = "Class 1",
    Car_class2 = "Class 2",
    Car_class3 = "Class 3",
    SUV_class1 = "Class 1",
    SUV_class2 = "Class 2",
    SUV_class3 = "Class 3",
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
    "0_model_summary_cars_suvs_lc_3c_rangeloss.html"
    # "0_model_summary_cars_suvs_lc_3c_degradation.html"
  )
)
