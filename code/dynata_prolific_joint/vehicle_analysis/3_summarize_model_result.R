# POST-PROCESSING ----
## WTP - delta method ----

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
# apollo_sink()

#---- OUT OF SAMPLE TESTING

# apollo_outOfSample(
#   apollo_beta,
#   apollo_fixed,
#   apollo_probabilities,
#   apollo_inputs
# )

#---- BOOTSTRAP ESTIMATION

# apollo_bootstrap(
#   apollo_beta,
#   apollo_fixed,
#   apollo_probabilities,
#   apollo_inputs,
#   bootstrap_settings = list(nRep = 3)
# )

####### POSTERIOR ANALYSIS

### Compute unconditional estimates (averaged over classes) of parameters.
### Unconditional means averaged across classes using the population-level class probabilities
unconditionals = apollo_unconditionals(
  model_final,
  apollo_probabilities,
  apollo_inputs
)

# List of attributes for which we want WTP
attributes <- c(
  "no_choice",
  "powertrainbev",
  "powertrainhev",
  "range_bev",
  "mileage",
  "age",
  "operating_cost"
)

# Number of classes
n_classes <- length(unconditionals[["pi_values"]])

# Create an empty list to store WTPs
wtp_list <- list()

# Loop over attributes
for (attr in attributes) {
  wtp_attr <- numeric(n_classes) # store class-specific WTP
  for (class in 1:n_classes) {
    beta_attr <- unconditionals[[paste0("b_", attr)]][[class]]
    beta_price <- unconditionals[["b_price"]][[class]]
    wtp_attr[class] <- beta_attr / (-beta_price) # WTP = beta_attr / beta_price
  }
  wtp_list[[attr]] <- wtp_attr
}

# Convert to a data frame for easy viewing
wtp_df <- as.data.frame(wtp_list) %>%
  mutate(class_allocation = paste0("class_", 1:n_classes))

## Probabilistic Class Membership Summary Statistics ----
### Compute conditional (posterior) class probabilities per individual
### These conditional probabilities let you assign individuals to classes or at least identify which class they’re more likely to belong to.
conditionals = apollo_conditionals(
  model_final,
  apollo_probabilities,
  apollo_inputs
)

# Rename columns for clarity and keep one row per individual (conditionals is already at indiv level)
conditionals_probs <- conditionals %>%
  rename(prob_class_1 = X1, prob_class_2 = X2, prob_class_3 = X3)

# Join posterior probabilities to database (one row per choice occasion → one row per individual after distinct)
data_output <- database %>%
  left_join(conditionals_probs, by = c("respID" = "ID"))

# One row per individual for summary (database is in long format with repeated rows per respID)
data_indiv <- data_output %>%
  distinct(respID, .keep_all = TRUE)

## Probability-weighted summaries ----
# For each variable, the weighted mean for class k = sum(prob_class_k * x) / sum(prob_class_k)

num_vars_active <- c(
  "FA_EV_benefit",
  "FA_EV_anxiety",
  "hhincome_num_10k",
  "ATT_EVB_environment",
  "ATT_EVB_function",
  "next_veh_budget",
  "next_veh_fuel_used_bev"
)

cate_vars_active <- c(
  "knowledge_ev",
  "knowledge_subsidy",
  "EV_charger",
  "EV_neighbor"
)

# Compute weighted means for numeric variables
num_summary <- map_dfr(1:n_classes, function(k) {
  w <- data_indiv[[paste0("prob_class_", k)]]
  data_indiv %>%
    summarise(
      class_allocation = paste0("class_", k),
      across(
        all_of(num_vars_active),
        ~ weighted.mean(.x, w, na.rm = TRUE)
      )
    )
})

# Compute weighted share of "yes"/1 for categorical variables
cat_summary <- map_dfr(1:n_classes, function(k) {
  w <- data_indiv[[paste0("prob_class_", k)]]
  data_indiv %>%
    mutate(
      knowledge_ev = as.character(knowledge_ev),
      knowledge_subsidy = as.character(knowledge_subsidy)
    ) %>%
    summarise(
      class_allocation = paste0("class_", k),
      across(
        all_of(cate_vars_active),
        ~ weighted.mean(as.numeric(.x %in% c("yes", "1")), w, na.rm = TRUE),
        .names = "{.col}_share_yes"
      )
    )
})

class_character <- num_summary %>%
  left_join(cat_summary, by = "class_allocation")


# combine all results ----
result_summary <- wtp_df %>%
  left_join(class_character, by = "class_allocation") %>%
  select(class_allocation, starts_with("b_"), everything())

## Publication-ready table ----
library(gt)
library(tidyr)

# --- 1. Pivot result_summary to long (variable = rows, class = cols) ---
result_long <- result_summary %>%
  pivot_longer(
    cols = -class_allocation,
    names_to = "variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = class_allocation,
    values_from = value
  )

# --- 2. Add section labels and human-readable variable labels ---
var_meta <- tribble(
  ~variable                     , ~label                                            , ~section         , ~fmt     ,
  # WTP
  "no_choice"                   , "No-Choice Option (opt-out)"                      , "WTP (USD)"      , "dollar" ,
  "powertrainbev"               , "Powertrain: BEV (vs. ICEV)"                      , "WTP (USD)"      , "dollar" ,
  "powertrainhev"               , "Powertrain: HEV (vs. ICEV)"                      , "WTP (USD)"      , "dollar" ,
  "range_bev"                   , "BEV Electric Range (per 100 mile)"               , "WTP (USD)"      , "dollar" ,
  "mileage"                     , "Mileage (per 1,000 miles)"                       , "WTP (USD)"      , "dollar" ,
  "age"                         , "Vehicle Age (per year)"                          , "WTP (USD)"      , "dollar" ,
  "operating_cost"              , "Operating Cost (per $/mile)"                     , "WTP (USD)"      , "dollar" ,
  # Attitudes / psychological
  "FA_EV_benefit"               , "Perceived EV Benefits (factor score)"            , "Attitudes"      , "number" ,
  "FA_EV_anxiety"               , "EV Range Anxiety (factor score)"                 , "Attitudes"      , "number" ,
  "ATT_EVB_environment"         , "EV Battery Environmental Attitude (1–5 scale)"   , "Attitudes"      , "number" ,
  "ATT_EVB_function"            , "EV Battery Functional Attitude (1–5 scale)"      , "Attitudes"      , "number" ,
  # Socioeconomics
  "hhincome_num_10k"            , "Household Income (×$10k)"                        , "Socioeconomics" , "number" ,
  "next_veh_budget"             , "Next Vehicle Budget (USD)"                       , "Socioeconomics" , "dollar" ,
  "next_veh_fuel_used_bev"      , "Likelihood of purchasing a used BEV (1–5 scale)" , "Socioeconomics" , "number" ,
  # EV context
  "knowledge_ev_share_yes"      , "EV Knowledge (% yes)"                            , "EV Context"     , "pct"    ,
  "knowledge_subsidy_share_yes" , "EV Subsidy Knowledge (% yes)"                    , "EV Context"     , "pct"    ,
  "EV_charger_share_yes"        , "Home Charger Access (% yes)"                     , "EV Context"     , "pct"    ,
  "EV_neighbor_share_yes"       , "Neighbor Owns/Leases a BEV/PHEV (% yes)"         , "EV Context"     , "pct"
)

# --- 3. Join meta to long table and arrange by section order ---
section_order <- c("WTP (USD)", "Attitudes", "Socioeconomics", "EV Context")

result_table <- result_long %>%
  left_join(var_meta, by = "variable") %>%
  filter(!is.na(label)) %>%
  mutate(section = factor(section, levels = section_order)) %>%
  arrange(section, match(variable, var_meta$variable)) %>%
  select(section, label, fmt, class_1, class_2, class_3)

# --- 4. Apply per-row number formatting ---
# gt doesn't support mixed per-row formats natively, so we pre-format as character
result_fmt <- result_table %>%
  mutate(
    across(
      c(class_1, class_2, class_3),
      ~ case_when(
        fmt == "dollar" ~ scales::dollar(.x, accuracy = 1),
        fmt == "pct" ~ scales::percent(.x, accuracy = 0.1),
        TRUE ~ scales::number(.x, accuracy = 0.01)
      )
    )
  ) %>%
  select(-fmt)

# --- 5. Build gt table ---
gt_table <- result_fmt %>%
  group_by(section) %>%
  gt(rowname_col = "label") %>%
  tab_header(
    title = md("**Latent Class Profile Summary**"),
    subtitle = md("Probability-weighted means by class · Cars only")
  ) %>%
  tab_spanner(
    label = md("**Class Profiles**"),
    columns = c(class_1, class_2, class_3)
  ) %>%
  cols_label(
    class_1 = md("**Class 1**"),
    class_2 = md("**Class 2**"),
    class_3 = md("**Class 3**")
  ) %>%
  tab_row_group(
    label = "Willingness to Pay (USD)",
    rows = section == "WTP (USD)"
  ) %>%
  tab_row_group(
    label = "Attitudes & Perceptions",
    rows = section == "Attitudes"
  ) %>%
  tab_row_group(
    label = "Socioeconomic Characteristics",
    rows = section == "Socioeconomics"
  ) %>%
  tab_row_group(
    label = "EV Contexts",
    rows = section == "EV Context"
  ) %>%
  tab_footnote(
    footnote = "WTP computed as –β_attr / β_price from unconditional class-level parameters.",
    locations = cells_row_groups("Willingness to Pay (USD)")
  ) %>%
  tab_footnote(
    footnote = "All summaries are probability-weighted using posterior class membership probabilities.",
    locations = cells_title("subtitle")
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.align = "left",
    row_group.font.weight = "bold",
    # stub.font.style = "normal",
    column_labels.font.weight = "bold",
    table.border.top.style = "solid",
    table.border.bottom.style = "solid"
  ) %>%
  opt_stylize(style = 1, color = "blue")

gt_table

# Save
gtsave(gt_table, paste0(path_images, "class_profile_summary.html"))
gtsave(gt_table, paste0(path_images, "class_profile_summary.png"), expand = 20)
