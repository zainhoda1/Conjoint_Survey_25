source(here::here('code', 'setup.R'))

# ----Data----
## ----Load dataset----
data_dce <- read_csv(here(
  "data",
  "main",
  "vehicle_choice_data.csv"
))

data_variable <- read_csv(here(
  "data",
  "main",
  "data_clean_variables.csv"
))

data_variable <- data_variable %>%
  select(
    psid,
    ends_with("_num"),
    ends_with("_cate"),
    starts_with("ATT_"),
    starts_with("FA_"),
    starts_with("knowledge_"),
    Veh_hh_fuel,
    starts_with("EV_")
  )

# head(data)

# glimpse(data)
## ----Processing----
data_dce <- data_dce %>%
  mutate(
    price = price / 10000, # 0.4-6
    range_bev = range_bev / 100, # 0.5 - 2.5
    range_phev = range_phev / 10, # 1 - 4
    mileage = mileage * 10, # 2 - 6
    age = age * 10, # 2 - 8
    operating_cost = operating_cost # 3 - 18,
  ) %>%
  mutate(
    veh_price_cate = case_when(
      is.na(price) ~ NA,
      price < 1.5 ~ "price_1",
      price < 2.5 ~ "price_2",
      price < 3.5 ~ "price_3",
      price < 4.5 ~ "price_4",
      price < 5.5 ~ "price_5",
      T ~ "price_6",
    )
  ) %>%
  select(-range, -operating_cost_text, -session_id, -vehicle_type)

## ----Dummy encode----

data_dce_dummy <- cbc_encode(
  data_dce %>%
    select(!c(psid, price)),
  coding = 'dummy',
  ref_levels = list(powertrain = 'gas', veh_price_cate = 'price_1')
) %>%
  as.data.frame()

data_dce_dummy <- cbind(
  data_dce_dummy,
  data_dce %>%
    select(psid, price) %>%
    mutate(price = case_when(is.na(price) ~ 0, T ~ price))
)

data_covariate <- data_dce_dummy %>%
  left_join(data_variable, by = "psid")
n_distinct(data_covariate$psid) #334

# table variables
# data_covariate %>%
#   select(where(is.character)) %>% # select all character variables
#   map(~ table(.))

data_covariate <- cbc_encode(
  data_covariate %>%
    select(!psid) %>%
    mutate(
      knowledge_gas = as.character(knowledge_gas),
      knowledge_plugin = as.character(knowledge_plugin),
      knowledge_ev = as.character(knowledge_ev),
      knowledge_subsidy = as.character(knowledge_subsidy)
    ),
  coding = 'dummy',
  ref_levels = list(
    gender_cate = 'female',
    ethnicity_cate = 'hispanic',
    race_cate = 'white_only',
    education_cate = 'high_school',
    student_cate = 'student',
    employment_cate = 'full_time',
    hhtenure_cate = 'own',
    hhtype_cate = 'sf_detached',
    ATT_EVB_environment = 'neutral',
    ATT_EVB_function = 'neutral',
    ATT_techsavvy = 'neutral',
    ATT_risktaker = 'neutral',
    ATT_climate = 'not_at_all',
    ATT_political = 'conservative',
    ATT_voting = 'democratic',
    Veh_hh_fuel = 'icev_only',
    EV_charger = 'no',
    EV_neighbor = 'no',
    knowledge_gas = '0',
    knowledge_plugin = '0',
    knowledge_ev = '0',
    knowledge_subsidy = '0'
  )
) %>%
  as.data.frame()

# glimpse(data)

# Estimate MNL model

# ---- ****DCE ONLY**** ----
# ---- Estimate MNL model (range:yr0 + degradation) ----
## --- Preference Space ----
mnl_pref <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "price",
    "mileage",
    "age",
    "operating_cost",
    "range_bev",
    "range_phev",
    "powertrainbev",
    "powertrainphev",
    "powertrainhev",
    "no_choice"
  )
)


# View summary of results
summary(mnl_pref)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_pref$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_pref$hessian)$values

wtp(mnl_pref, scalePar = "price")

### --- Price categories ----
mnl_pref_pricecate <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "veh_price_cateprice_2",
    "veh_price_cateprice_3",
    "veh_price_cateprice_4",
    "veh_price_cateprice_5",
    "veh_price_cateprice_6",
    "mileage",
    "age",
    "operating_cost",
    "range_bev",
    "range_phev",
    "powertrainbev",
    "powertrainphev",
    "powertrainhev",
    "no_choice"
  )
)

summary(mnl_pref_pricecate)
# plot
veh_price_coefs_df <- data.frame(
  coef_name = names(mnl_pref_pricecate$coefficients),
  estimate = mnl_pref_pricecate$coefficients
) %>%
  dplyr::filter(grepl("^veh_price", coef_name)) %>%
  add_row(coef_name = "veh_price_cateprice_1", estimate = 0) %>%
  mutate(
    coef_name = c(
      "price2_15_25k",
      "price3_25_35k",
      "price4_35_45k",
      "price5_45_55k",
      "price6_55_65k",
      "price1_4_15k"
    )
  )

veh_price_coefs_df


ggplot(veh_price_coefs_df, aes(x = coef_name, y = estimate, group = 1)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 3) +
  geom_text(aes(label = round(estimate, 2)), vjust = -0.8, size = 3) +
  labs(
    title = "Coefficients in Preference Space (Vehicle Survey)",
    x = "Vehicle Price Category",
    y = "Coefficient"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )


## --- WTP Space ----

mnl_wtp <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "mileage",
    "age",
    "operating_cost",
    "range_bev",
    "range_phev",
    "powertrainbev",
    "powertrainphev",
    "powertrainhev",
    "no_choice"
  ),
  scalePar = "price",
  numMultiStarts = 50,
  numCores = 1
)

# View summary of results
summary(mnl_wtp)
# Check the 1st order condition: Is the gradient at the solution zero?
mnl_wtp$gradient
# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_wtp$hessian)$values

## ---- WTP comparison ----
wtpCompare(mnl_pref, mnl_wtp, scalePar = "price")

## ---- Detecting outlier ----
#leave-one-out influence analysis (similar to jackknife diagnostics)
# 1. Run full model
full_model <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "mileage",
    "age",
    "operating_cost",
    "range_bev",
    "range_phev",
    "powertrainbev",
    "powertrainphev",
    "powertrainhev",
    "no_choice"
  ),
  scalePar = "price",
  numMultiStarts = 10,
  numCores = 1
)

full_summary <- broom::tidy(full_model)
full_coefs <- full_summary %>% select(term, estimate, std.error)

# 2. Get unique respondent IDs
resp_ids <- unique(data_dce_dummy$respID)

# 3. Loop over respondents, re-run model without each
results_list <- list()

for (i in seq_along(resp_ids)) {
  resp_to_remove <- resp_ids[i]
  # cat("Running model without respondent:", resp_to_remove, "(", i, "of", length(resp_ids), ")\n")

  # Subset data
  data_subset <- data_dce_dummy %>% filter(respID != resp_to_remove)

  model <- tryCatch(
    {
      model <- logitr(
        data = data_dce_dummy,
        outcome = "choice",
        obsID = "obsID",
        pars = c(
          "mileage",
          "age",
          "operating_cost",
          "range_bev",
          "range_phev",
          "powertrainbev",
          "powertrainphev",
          "powertrainhev",
          "no_choice"
        ),
        scalePar = "price",
        numMultiStarts = 10,
        numCores = 1
      )
    },
    error = function(e) NULL
  )

  # Skip failed models
  if (is.null(model)) {
    next
  }

  # Extract coefficients and SEs

  tidy_model <- broom::tidy(model)

  results_list[[i]] <- tidy_model %>%
    mutate(
      resp_removed = resp_to_remove
    ) %>%
    select(resp_removed, term, estimate, std.error)
}

# 4. Combine all results
all_results <- bind_rows(results_list)

# 5. Compare each coefficient to full-sample estimates
comparison <- all_results %>%
  left_join(full_coefs, by = "term", suffix = c("_loo", "_full")) %>%
  mutate(
    diff_estimate = round(estimate_loo - estimate_full, 5),
    diff_std = round(std.error_loo - std.error_full, 5),
    diff_estimate_perc = round(
      ((estimate_loo - estimate_full) / estimate_full) * 100,
      3
    ),
    diff_std_perc = round(
      ((std.error_loo - std.error_full) / std.error_full) * 100,
      3
    )
  )

# 6. Identify respondents who change results substantially
influential <- comparison %>%
  group_by(resp_removed) %>%
  summarise(
    max_abs_coef_change = max(abs(diff_estimate), na.rm = TRUE),
    max_abs_se_change = max(abs(diff_std), na.rm = TRUE)
  ) %>%
  arrange(desc(max_abs_coef_change))

head(influential)

write_csv(
  comparison,
  here(
    "code",
    "main",
    "model_output",
    "logitr",
    "dce_coef_se_comparison_vehicle.csv"
  )
)

write_csv(
  influential,
  here(
    "code",
    "main",
    "model_output",
    "logitr",
    "dce_influential_respID_vehicle.csv"
  )
)

# ---- Estimate MXL model ----
## --- WTP Space ----
### --- full ----
mxl_wtp_full <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "mileage",
    "age",
    "operating_cost",
    "range_bev",
    "range_phev",
    "powertrainbev",
    "powertrainphev",
    "powertrainhev",
    "no_choice"
  ),
  scalePar = "price",
  randPars = c(
    mileage = "n",
    age = "n",
    operating_cost = "n",
    range_bev = "n",
    range_phev = "n",
    powertrainbev = "n",
    powertrainphev = "n",
    powertrainhev = "n"
  ),
  numMultiStarts = 10,
  numCores = 1
)

# View summary of results
summary(mxl_wtp_full)
