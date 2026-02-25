source(here::here('code', 'setup.R'))

# DCE only----
data_dce <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_battery.parquet"
)) %>%
  select(
    -starts_with("battery_health"),
    -starts_with("time")
  ) %>%
  mutate(
    mileage = mileage / 10000, #3 - 6
    price = price / 10000, # 0.5 - 6
    battery_range_year0 = battery_range_year0 / 100, # 1-3
    battery_range_year3 = battery_range_year3 / 100, # 1-3
    battery_range_year8 = battery_range_year8 / 100, # 0.5 - 2
    battery_degradation = (battery_degradation * 100)
  ) %>%
  mutate(
    respID_qID = paste0(respID, "_", qID),
    price = case_when(is.na(price) ~ 0, T ~ price)
  )


# Data Processing----
data_dce_dummy <- cbc_encode(
  data_dce %>%
    select(!c(psid, respID_qID)),
  coding = 'dummy',
  ref_levels = list(
    battery_refurbish = 'original',
    vehicle_type = 'car',
    budget = 'low'
  )
) %>%
  as.data.frame()


data_dce_dummy <- cbind(
  data_dce_dummy,
  data_dce %>%
    select(psid, respID_qID)
)

summary(data_dce_dummy)
# glimpse(data_dce_dummy)

# DCE + covariates----

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


data_dce_covariate <- data_dce_dummy %>%
  left_join(
    data_variable,
    by = "psid"
  )

write_parquet(
  data_dce_dummy,
  here(
    "data",
    "dynata_prolific_joint",
    "data_logitr_dce_only_battery.parquet"
  )
)

write_parquet(
  data_dce_covariate,
  here(
    "data",
    "dynata_prolific_joint",
    "data_logitr_dce_covariate_battery.parquet"
  )
)

# variable correlation
# cor(
#   data_variable$next_veh_budget,
#   data_variable$hhincome_num,
#   use = "complete.obs"
# )
