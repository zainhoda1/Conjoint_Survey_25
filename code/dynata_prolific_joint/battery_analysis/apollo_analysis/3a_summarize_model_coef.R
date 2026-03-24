source(here::here('code', 'setup.R'))

# UPLOAD MODELS ----
car_lc_3c <- read_csv(here(
  "code",
  "output",
  "model_output",
  "apollo",
  "battery",
  "car_lc_3c_1_estimates.csv"
))
colnames(car_lc_3c)[1] <- "variable"

suv_lc_3c <- read_csv(here(
  "code",
  "output",
  "model_output",
  "apollo",
  "battery",
  "suv_lc_3c_1_estimates.csv"
))
colnames(suv_lc_3c)[1] <- "variable"


# ----Combine model coefficients for car and suv----
combined_all <- bind_rows(
  car_lc_3c %>%
    mutate(vehicle = "Car"),
  suv_lc_3c %>%
    mutate(vehicle = "SUV")
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
      Class == "a" ~ "class3",
      Class == "b" ~ "class1",
      TRUE ~ "class2"
    )
  ) %>%
  pivot_wider(
    id_cols = c(Variables),
    names_from = c(Class, Segment),
    values_from = c(Est., Std.Err., Sig.),
    names_glue = "{.value}_{Segment}_{Class}"
  )

var_meta <- tribble(
  ~variable                , ~label                                          , ~section             , ~fmt     ,
  # WTP
  "no_choice"              , "No-Choice Option (opt-out)"                    , "Vehicle Attributes" , "dollar" ,
  "mileage"                , "Mileage (10,000 miles)"                        , "Vehicle Attributes" , "dollar" ,
  "range_year0"            , "BEV Electric Range at Year 0 (100 miles)"      , "Vehicle Attributes" , "dollar" ,
  "degradation"            , "Battery Degradation Rate (% per year)"         , "Vehicle Attributes" , "dollar" ,
  "packreplace"            , "Battery Refurbishment: Pack Replace"           , "Vehicle Attributes" , "dollar" ,
  "cellreplace"            , "Battery Refurbishment: Cell Replace"           , "Vehicle Attributes" , "dollar" ,
  "price"                  , "Purchase Price (10,000 USD)"                   , "Vehicle Attributes" , "dollar" ,
  # Active variables
  "ASC"                    , "ASC"                                           , "Active Indicators"  , "number" ,
  "EV_benefit"             , "Perceived EV Benefits (factor score)"          , "Active Indicators"  , "number" ,
  "EV_anxiety"             , "EV Range Anxiety (factor score)"               , "Active Indicators"  , "number" ,
  "evenvironment_disagree" , "EV Battery Environmentally Positive: Disagree" , "Active Indicators"  , "number" ,
  "evenvironment_agree"    , "EV Battery Environmentally Positive: Agree"    , "Active Indicators"  , "number" ,
  "evfunction_disagree"    , "EV Battery Functionally Negative: Disagree"    , "Active Indicators"  , "number" ,
  "evfunction_agree"       , "EV Battery Functionally Negative: Agree"       , "Active Indicators"  , "number" ,
  "hhincome"               , "Household Income (10,000 USD)"                 , "Active Indicators"  , "number" ,
  "knowledge_ev"           , "EV Knowledge"                                  , "Active Indicators"  , "pct"    ,
  "knowledge_subsidy"      , "EV Subsidy Knowledge"                          , "Active Indicators"  , "pct"    ,
  "ev_charge"              , "Home Charger Access"                           , "Active Indicators"  , "pct"    ,
  "ev_neighbor"            , "Neighbor Owns/Leases a BEV/PHEV"               , "Active Indicators"  , "pct"    ,
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
    "apollo",
    "battery",
    "car_suv_lc_3c_formatted_model_estimates.csv"
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
    )
  ) %>%
  select(label, section, starts_with("Car_class"), starts_with("SUV_class"))

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
  cols_label(
    Car_class1 = "Class 1",
    Car_class2 = "Class 2",
    Car_class3 = "Class 3",
    SUV_class1 = "Class 1",
    SUV_class2 = "Class 2",
    SUV_class3 = "Class 3"
  ) |>
  tab_header(
    title = "BEV Battery Information Survey: Model Results (Car vs SUV)",
    subtitle = "Est. (SE)[Sig.] for each class"
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.align = "left",
    cols_align(align = "left", everything()),
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
    "apollo",
    "battery",
    "0_model_summary_cars_suvs_lc_3c.html"
  )
)
