source(here::here('code', 'setup.R'))

# ----Data----
## ----Load dataset----

data_dce <- read_csv(here(
  "data",
  "main",
  "battery_choice_data.csv"
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


## ----Data Processing----
data_dce <- data_dce %>%
  mutate(
    veh_mileage = veh_mileage / 10000, #3 - 6
    veh_price = veh_price / 10000, # 0.5 - 6
    battery_range_year0 = battery_range_year0 / 100, # 1-3
    battery_range_year3 = battery_range_year3 / 100, # 1-3
    battery_range_year8 = battery_range_year8 / 100, # 0.5 - 2
    battery_degradation = (battery_degradation * 100) # percentage
  ) %>%
  mutate(
    veh_price_cate = case_when(
      is.na(veh_price) ~ NA,
      veh_price < 1.5 ~ "price_1",
      veh_price < 2.5 ~ "price_2",
      veh_price < 3.5 ~ "price_3",
      veh_price < 4.5 ~ "price_4",
      veh_price < 5.5 ~ "price_5",
      T ~ "price_6"
    )
  ) %>%
  select(
    -session_id,
    -starts_with("battery_health"),
    -starts_with("time"),
    -vehicle_type,
    -battery_condition
  )

## ---- Dummy encode----
data_dce_dummy <- cbc_encode(
  data_dce %>% select(-c(psid, veh_price)),
  coding = 'dummy',
  ref_levels = list(
    battery_refurbish = 'original',
    veh_price_cate = 'price_1'
  )
) %>%
  as.data.frame()

data_dce_dummy <- cbind(
  data_dce_dummy,
  data_dce %>%
    select(psid, veh_price) %>%
    mutate(veh_price = case_when(is.na(veh_price) ~ 0, T ~ veh_price))
)

data_covariate <- data_dce_dummy %>%
  left_join(data_variable, by = "psid")
# n_distinct(data_covariate$psid) #373

data_covariate_dummy <- cbc_encode(
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

# ---- ****DCE ONLY**** ----
# ---- Estimate MNL model (range:yr0 + degradation) ----
## --- Preference Space ----
mnl_pref <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "veh_mileage",
    "veh_price",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice"
  ),
  numMultiStarts = 10,
  numCores = 1
)

# View summary of results
summary(mnl_pref)
# Check the 1st order condition: Is the gradient at the solution zero?
mnl_pref$gradient
# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_pref$hessian)$values

### ---- WTP calculation ----
wtp(mnl_pref, scalePar = "veh_price")

## ---- Price categories----

mnl_pref_pricecate <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "veh_mileage",
    "veh_price_cateprice_2",
    "veh_price_cateprice_3",
    "veh_price_cateprice_4",
    "veh_price_cateprice_5",
    "veh_price_cateprice_6",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice"
  ),
  numMultiStarts = 50,
  numCores = 1
)

# View summary of results
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
    title = "Coefficients in Preference Space (Battery Survey)",
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
### --- Only DCE ----
mnl_wtp <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "veh_mileage",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice"
  ),
  scalePar = "veh_price",
  numMultiStarts = 10,
  numCores = 1
)

# View summary of results
summary(mnl_wtp)
# Check the 1st order condition: Is the gradient at the solution zero?
mnl_wtp$gradient
# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_wtp$hessian)$values
#coefficient
mnl_wtp_coef <- mnl_wtp$coefficients


## ---- WTP comparison ----
wtpCompare(mnl_pref, mnl_wtp, scalePar = "veh_price")

## ---- Detecting outlier ----
#leave-one-out influence analysis (similar to jackknife diagnostics)
# 1. Run full model
full_model <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "veh_mileage",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice"
  ),
  scalePar = "veh_price",
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
          "veh_mileage",
          "battery_range_year0",
          "battery_degradation",
          "battery_refurbishpackreplace",
          "battery_refurbishcellreplace",
          "no_choice"
        ),
        scalePar = "veh_price",
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
    "dce_coef_se_comparison_battery.csv"
  )
)

write_csv(
  influential,
  here(
    "code",
    "main",
    "model_output",
    "logitr",
    "dce_influential_respID_battery.csv"
  )
)


## --- Subgroup comparison ----
### ---- Define subgroups as a named list of filters ----

data_covariate_short <- data_covariate %>% select(-psid)
for (v in names(data_covariate_short)) {
  if (is.character(data_covariate_short[[v]])) {
    cat("\n---", v, "---\n")
    print(table(data_covariate_short[[v]], useNA = "ifany"))
  }
}

# table(data_covariate$ATT_EVB_environment, data_covariate$ATT_EVB_function)
# chisq.test(data_covariate$ATT_EVB_environment, data_covariate$ATT_EVB_function)
#
# a<-data_covariate %>%
#   mutate(ATT_EVB_environment=case_when(ATT_EVB_environment=="strongly_disagree"~1,
#                                        ATT_EVB_environment=="somewhat_disagree"~2,
#                                        ATT_EVB_environment=="neutral"~3,
#                                        ATT_EVB_environment=="somewhat_agree"~4,
#                                        ATT_EVB_environment=="strongly_agree"~5,
#                                        ),
#          ATT_EVB_function =case_when(ATT_EVB_function=="strongly_disagree"~1,
#                                        ATT_EVB_function=="somewhat_disagree"~2,
#                                        ATT_EVB_function=="neutral"~3,
#                                        ATT_EVB_function=="somewhat_agree"~4,
#                                        ATT_EVB_function=="strongly_agree"~5,
#          )
#          )
#
# cor(a$ATT_EVB_environment,
#     a$ATT_EVB_function,
#     # method = "spearman",
#     method = "kendall",
#     use = "complete.obs")

subgroups <- list(
  # current hh veh fuel
  veh_hh_icev_only = quote(Veh_hh_fuel == "icev_only"),
  veh_hh_ev = quote(Veh_hh_fuel %in% c("ev_mix", "ev_only")),
  # EV knowledge
  knowledge_ev_no = quote(
    knowledge_gas == 0 & knowledge_plugin == 0 & knowledge_ev == 0
  ),
  knowledge_ev_full = quote(
    knowledge_gas == 2 & knowledge_plugin == 2 & knowledge_ev == 1
  ),
  knowledge_ev_partial = quote(
    !(knowledge_gas == 0 & knowledge_plugin == 0 & knowledge_ev == 0) &
      !(knowledge_gas == 2 & knowledge_plugin == 2 & knowledge_ev == 1)
  ),

  # att EVB
  refurbish_EVB_function_worse_agree = quote(
    ATT_EVB_function %in% c("somewhat_agree", "strongly_agree")
  ),
  refurbish_EVB_function_worse_disagree = quote(
    ATT_EVB_function %in% c("somewhat_disagree", "strongly_disagree")
  ),
  refurbish_EVB_better_environ_agree = quote(
    ATT_EVB_environment %in% c("somewhat_agree", "strongly_agree")
  ),
  refurbish_EVB_better_environ_disagree = quote(
    ATT_EVB_environment %in% c("somewhat_disagree", "strongly_disagree")
  ),
  # political
  political_conservative = quote(
    ATT_political %in% c("conservative", "very_conservative")
  ),
  political_moderate = quote(ATT_political == "moderate"),
  political_liberal = quote(
    ATT_political %in% c("liberal", "very_liberal")
  )
)

# --- Storage list ---
coef_list <- list()
fit_list <- list()

# --- Run models in loop ---
for (group_name in names(subgroups)) {
  message("Running model for subgroup: ", group_name)

  data_sub <- data_covariate %>%
    filter(eval(subgroups[[group_name]]))

  # Estimate model
  model <- logitr(
    data = data_sub,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "veh_mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",
      "no_choice"
    ),
    scalePar = "veh_price",
    numMultiStarts = 10,
    numCores = 1
  )

  # Extract coefficients
  coefs <- summary(model)$coefficients %>%
    as.data.frame() %>%
    setNames("Estimate") %>%
    rownames_to_column("parameter") %>%
    select(parameter, Estimate) %>%
    rename(!!group_name := Estimate)

  coef_list[[group_name]] <- coefs

  # Extract fit statistics
  fit_list[[group_name]] <- tibble(
    parameter = c("obs", "logLik"),
    !!group_name := c(model$n$obs, model$logLik)
  )
}

# --- Merge all subgroup coefficients side-by-side ---
coef_table_wide <- Reduce(
  function(x, y) full_join(x, y, by = "parameter"),
  coef_list
)

coef_table_wide <- coef_table_wide %>%
  mutate(
    unit = c(
      "",
      "each additional 10,000 miles on the odometer",
      "each additional 100 miles of battery range",
      "each 1% increase in annual battery degradation",
      "refurbished battery with pack replacement (vs new battery)",
      "refurbished battery with cell replacement (vs new battery)",
      "utility of opting out"
    )
  ) %>%
  select(parameter, unit, everything()) %>%
  mutate(across(where(is.numeric), ~ .x * 10000))


# --- Merge model fit stats side-by-side ---
fit_table_wide <- Reduce(
  function(x, y) full_join(x, y, by = "parameter"),
  fit_list
)

# --- Combine coefficients and fit stats (fit below coefs) ---
# final_table <- bind_rows(coef_table, fit_table)

# --- Save results ---
output_dir <- file.path(here(), "code", "main", "model_output", "logitr")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write.xlsx(
  coef_table_wide,
  file = file.path(output_dir, "mnl_wtp_subgroup_coefs_wide.xlsx"),
  rowNames = FALSE
)
write.csv(
  fit_table_wide,
  file.path(output_dir, "mnl_wtp_subgroup_fitstats_wide.csv"),
  row.names = FALSE
)


# wtpCompare(mnl_wtp_EVB_function_agree,mnl_wtp_EVB_function_disagree, scalePar = "veh_price")

# ---- Estimate MXL model ----
## --- WTP Space ----
### --- full ----
mxl_wtp_full <- logitr(
  data = data_dce_dummy,
  outcome = "choice",
  obsID = "obs_id",
  pars = c(
    "veh_mileage",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice"
  ),
  scalePar = "veh_price",
  randPars = c(
    veh_mileage = "n",
    battery_range_year0 = "n",
    battery_degradation = "n",
    battery_refurbishpackreplace = "n",
    battery_refurbishcellreplace = "n"
  ),
  numMultiStarts = 100,
  numCores = 1
)

summary(mxl_wtp_full)

save(
  mxl_wtp_full,
  file = file.path(
    here(),
    "code",
    "main",
    "model_output",
    "logitr",
    "mxl_wtp_full.RData"
  )
)

### --- reduced ----
mxl_wtp_reduced <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obs_id",
  pars = c(
    "veh_mileage",
    "battery_range_year0",
    "battery_degradation",
    "battery_refurbishpackreplace",
    "battery_refurbishcellreplace",
    "no_choice"
  ),
  scalePar = "veh_price",
  randPars = c(
    # veh_mileage = "n",
    battery_range_year0 = "n",
    battery_degradation = "n"
    # battery_refurbishpackreplace = "n",
    # battery_refurbishcellreplace = "n"
  ),
  numMultiStarts = 10,
  numCores = 1
)

summary(mxl_wtp_reduced)
