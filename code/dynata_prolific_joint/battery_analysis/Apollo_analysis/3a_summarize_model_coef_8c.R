# rm(list = ls())

source(here::here('code', 'setup.R'))

# UPLOAD MODELS ----

car_suv_lc_8c <- read_csv(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "piecewise_rangeloss_car_suv_lc_8c_1_estimates.csv"
))
colnames(car_suv_lc_8c)[1] <- "variable"


# ---- Format combined model coefficients ----
# P.Value is retained here; dropped before pivot for the coefficient table,
# but used separately for WTP significance computation.
combined_all <- car_suv_lc_8c %>%
  mutate(vehicle = "Combined") %>%
  as.data.frame() %>%
  select(1:2, 6, 8, 9) %>%
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
  mutate(across(where(is.numeric), round, 2)) %>%
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
  )


# Helper: parse class suffix to class label (used in both coef and WTP tables)
parse_class <- function(x) {
  case_when(
    x == "a" ~ "class1",
    x == "b" ~ "class2",
    x == "c" ~ "class3",
    x == "d" ~ "class4",
    x == "e" ~ "class5",
    x == "f" ~ "class6",
    x == "g" ~ "class7",
    TRUE      ~ "class8"
  )
}

# ---- Coefficient table (no P.Value in output) ----
formatted <- combined_all %>%
  select(-P.Value) %>%
  separate(
    col = Variables,
    into = c("Variables", "Class"),
    sep = "_(?=[^_]+$)",
    extra = "merge",
    fill = "right"
  ) %>%
  mutate(Class = parse_class(Class)) %>%
  pivot_wider(
    id_cols = c(Variables),
    names_from = c(Class, Segment),
    values_from = c(Est., Std.Err., Sig.),
    names_glue = "{.value}_{Segment}_{Class}"
  )

var_meta <- tribble(
  ~variable             , ~label                                       , ~section             , ~fmt     ,
  "no_choice"           , "No-Choice Option (opt-out)"                 , "Vehicle Attributes" , "dollar" ,
  "mileage"             , "Mileage (10,000 miles)"                     , "Vehicle Attributes" , "dollar" ,
  # Range (year 3) piecewise segments
  "range_pw1"           , "BEV Electric Range (Year 3): <130 miles"    , "Vehicle Attributes" , "dollar" ,
  "range_pw2"           , "BEV Electric Range (Year 3): 130-200 miles" , "Vehicle Attributes" , "dollar" ,
  "range_pw3"           , "BEV Electric Range (Year 3): 200+ miles"    , "Vehicle Attributes" , "dollar" ,
  # Range loss piecewise segments
  "loss_pw1"            , "Range Loss Rate: <12%"                      , "Vehicle Attributes" , "dollar" ,
  "loss_pw2"            , "Range Loss Rate: 12%-24%"                   , "Vehicle Attributes" , "dollar" ,
  "loss_pw3"            , "Range Loss Rate: 24%+"                      , "Vehicle Attributes" , "dollar" ,
  "packreplace"         , "Battery Refurbishment: Pack Replace"        , "Vehicle Attributes" , "dollar" ,
  "cellreplace"         , "Battery Refurbishment: Cell Replace"        , "Vehicle Attributes" , "dollar" ,
  "price"               , "Purchase Price (10,000 USD)"                , "Vehicle Attributes" , "dollar" ,
  # Active variables
  "ASC"                 , "ASC"                                        , "Active Indicators"  , "number" ,
  "EV_rangeanxiety"     , "Perceived EV Range Anxiety: Agree"          , "Active Indicators"  , "number" ,
  "risktaker"           , "Risk Taking Propensity: Agree"              , "Active Indicators"  , "number" ,
  "evenvironment_agree" , "EV Battery Environmentally Positive: Agree" , "Active Indicators"  , "number" ,
  "evfunction_disagree" , "EV Battery Functionally Negative: Disagree" , "Active Indicators"  , "number" ,
  "hhincome"            , "Household Income (10,000 USD)"              , "Active Indicators"  , "number" ,
  "knowledge_ev"        , "EV Battery Knowledge: Yes"                  , "Active Indicators"  , "number" ,
  "ev_charge"           , "Electrical Outlet Access"                   , "Active Indicators"  , "pct"    ,
  "hhveh_fuel"          , "Primary Household Vehicle Fuel Type: ICEV"  , "Active Indicators"  , "pct"    ,
  "veh_range"           , "Primary Vehicle Typical Range (miles)"      , "Active Indicators"  , "number" ,
  "suv"                 , "SUV Segment"                                , "Active Indicators"  , "pct"
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
    "0_car_suv_lc_8c_formatted_model_estimates_piecewise_rangeloss.csv"
  ),
  row.names = FALSE
)

library(glue)
# Create display columns for each class
gt_formatted <- formatted %>%
  mutate(
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
    ),
    Combined_class4 = glue(
      "{Est._Combined_class4}\n({Std.Err._Combined_class4}){Sig._Combined_class4}",
      .na = NULL
    ),
    Combined_class5 = glue(
      "{Est._Combined_class5}\n({Std.Err._Combined_class5}){Sig._Combined_class5}",
      .na = NULL
    ),
    Combined_class6 = glue(
      "{Est._Combined_class6}\n({Std.Err._Combined_class6}){Sig._Combined_class6}",
      .na = NULL
    ),
    Combined_class7 = glue(
      "{Est._Combined_class7}\n({Std.Err._Combined_class7}){Sig._Combined_class7}",
      .na = NULL
    ),
    Combined_class8 = glue(
      "{Est._Combined_class8}\n({Std.Err._Combined_class8}){Sig._Combined_class8}",
      .na = NULL
    )
  ) %>%
  select(label, section, starts_with("Combined_class"))


gt_car_suv_lc_8c <- gt_formatted |>
  group_by(section) %>%
  gt(rowname_col = "label") |>
  tab_spanner(
    label = "Combined",
    columns = c(
      Combined_class1,
      Combined_class2,
      Combined_class3,
      Combined_class4,
      Combined_class5,
      Combined_class6,
      Combined_class7,
      Combined_class8
    )
  ) |>
  cols_align(align = "right", columns = everything()) %>%
  cols_align(align = "left", columns = label) %>%
  cols_label(
    Combined_class1 = "Class 1",
    Combined_class2 = "Class 2",
    Combined_class3 = "Class 3",
    Combined_class4 = "Class 4",
    Combined_class5 = "Class 5",
    Combined_class6 = "Class 6",
    Combined_class7 = "Class 7",
    Combined_class8 = "Class 8"
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
  opt_stylize(style = 1, color = "blue")

gt_car_suv_lc_8c

gtsave(
  gt_car_suv_lc_8c,
  file = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_model_summary_cars_suvs_lc_8c_piecewise_rangeloss.html"
  )
)

gt_car_suv_lc_8c_latex <- gt_car_suv_lc_8c %>%
  tab_options(
    table.font.size = px(9)
  ) %>%
  cols_width(
    everything() ~ px(80),
    label ~ px(140)
  ) %>%
  as_latex()

writeLines(
  gt_car_suv_lc_8c_latex,
  con = here::here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_class_profile_summary_cars_suvs_lc_8c.tex"
  )
)
