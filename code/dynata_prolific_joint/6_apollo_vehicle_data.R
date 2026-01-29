source(here::here('code', 'setup.R'))

# ----Data----
## ----Load dataset----

data_dce <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_vehicle.parquet"
)) %>%
  mutate(respID_qID = paste0(respID, "_", qID))

# ### subset
# ###  BEV price >20K
# bev_price <- data_dce %>%
#   filter(!is.na(powertrain) & price >= 20000)
# ### at least see 3 choice sets that include BEV
# bev_count <- bev_price %>%
#   group_by(respID, powertrain) %>%
#   count() %>%
#   filter(powertrain == "bev" & n > 2)
# ### among the 3 choice sets, at least see different price
# veh_price <- data_dce %>%
#   filter(!is.na(powertrain)) %>%
#   filter(
#     respID %in% bev_count$respID & respID_qID %in% bev_price$respID_qID
#   ) %>%
#   filter(powertrain == "bev") %>%
#   group_by(respID, price) %>%
#   count() %>%
#   group_by(respID) %>%
#   count() %>%
#   filter(n >= 2) %>%
#   as.data.frame()
#
# data_dce <- data_dce %>%
#   filter(
#     respID %in% veh_price$respID & respID_qID %in% bev_price$respID_qID
#   ) %>%
#   select(-respID_qID)
#
# n_distinct(data_dce$respID)

data_variable <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_clean_variables.parquet"
))

data_variable <- data_variable %>%
  select(
    psid,
    next_veh_budget,
    ends_with("_num"),
    ends_with("_cate"),
    starts_with("ATT_"),
    starts_with("FA_"),
    starts_with("knowledge_"),
    Veh_hh_fuel,
    starts_with("EV_"),
    starts_with("next_veh_fuel_")
  ) %>%
  mutate(
    hhincome_num_10k = hhincome_num / 10000,
    next_veh_budget_k = next_veh_budget / 1000
  )

# variable correlation
# cor(
#   data_variable$next_veh_budget,
#   data_variable$hhincome_num,
#   use = "complete.obs"
# )

## ----Data Processing----
data_dce <- data_dce %>%
  mutate(
    price = price / 10000, # 0.5-6
    range_bev = range_bev / 100, # 0.5 - 2.5
    mileage = mileage / 10000, # 2 - 6
    age = age, # 2 - 8
    operating_cost = operating_cost / 10 # 0.3 - 2.5,
  )

## ---- Dummy encode----
data_dce_dummy <- cbc_encode(
  data_dce %>%
    select(!c(psid, price, respID_qID)),
  coding = 'dummy',
  ref_levels = list(powertrain = 'gas', vehicle_type = 'car', budget = 'low')
) %>%
  as.data.frame()

data_dce_dummy <- cbind(
  data_dce_dummy,
  data_dce %>%
    select(psid, price, respID_qID) %>%
    mutate(price = case_when(is.na(price) ~ 0, T ~ price))
)

data_dce_dummy_apollo <- data_dce_dummy %>%
  select(
    psid,
    respID,
    qID,
    altID,
    choice,
    powertrainbev,
    powertrainhev,
    range_bev,
    age,
    mileage,
    operating_cost,
    price,
    vehicle_typesuv
  ) %>%
  pivot_wider(
    id_cols = c(psid, respID, qID, vehicle_typesuv),
    names_from = altID,
    values_from = c(
      powertrainbev,
      powertrainhev,
      range_bev,
      age,
      mileage,
      operating_cost,
      price,
      choice
    ),
    names_glue = "{.value}_{altID}"
  ) %>%
  mutate(
    choice = case_when(
      choice_1 == 1 ~ 1,
      choice_2 == 1 ~ 2,
      choice_3 == 1 ~ 3,
      choice_4 == 1 ~ 4
    )
  ) %>%
  select(-c(choice_1, choice_2, choice_3, choice_4))


data_covariate <- data_dce_dummy_apollo %>%
  left_join(data_variable, by = "psid")
# n_distinct(data_covariate$psid) #373

data_covariate_num <- data_covariate %>%
  mutate(
    across(
      any_of(c(
        "ATT_EVB_environment",
        "ATT_EVB_function",
        "ATT_techsavvy",
        "ATT_risktaker"
      )),
      ~ case_when(
        grepl("strongly_disagree", ., ignore.case = TRUE) ~ 1,
        grepl("somewhat_disagree", ., ignore.case = TRUE) ~ 2,
        grepl("neutral", ., ignore.case = TRUE) ~ 3,
        grepl("somewhat_agree", ., ignore.case = TRUE) ~ 4,
        grepl("strongly_agree", ., ignore.case = TRUE) ~ 5,
        TRUE ~ NA
      )
    ),
    across(
      any_of(c(
        "next_veh_fuel_new_bev",
        "next_veh_fuel_used_bev",
        "next_veh_fuel_new_phev",
        "next_veh_fuel_used_phev"
      )),
      ~ case_when(
        grepl("very_unlikely", ., ignore.case = TRUE) ~ 1,
        grepl("somewhat_unlikely", ., ignore.case = TRUE) ~ 2,
        grepl("neutral", ., ignore.case = TRUE) ~ 3,
        grepl("somewhat_likely", ., ignore.case = TRUE) ~ 4,
        grepl("very_likely", ., ignore.case = TRUE) ~ 5,
        TRUE ~ NA
      )
    )
  )

data_covariate_dummy <- cbc_encode(
  data_covariate %>%
    select(!psid) %>%
    mutate(
      obsID = row_number(),
      altID = row_number(),
      profileID = row_number()
    ) %>%
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

### Loading data from package
database_all = data_covariate_num %>%
  filter(
    !is.na(next_veh_fuel_used_bev) &
      !is.na(FA_EV_benefit) &
      !is.na(FA_EV_anxiety) &
      !is.na(hhincome_num)
  ) %>%
  mutate(
    log_veh_price_1 = log(price_1),
    log_veh_price_2 = log(price_2),
    log_veh_price_3 = log(price_3)
  )

write_parquet(
  database_all,
  here(
    "data",
    "dynata_prolific_joint",
    "data_apollo_vehicle.parquet"
  )
)
