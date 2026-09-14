source(here::here('code', 'setup.R'))

#######
# Load the Dataset:

data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_vehicle.parquet"
 )) 
# |> 
#   filter(collection_round != 'round_4')


data_raw_dynata <- read_parquet(here(
  "data",
  "dynata_testing",
  "data.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'),
 -respID) %>%
  mutate(data_source = 'dynata')

data_raw_prolific <- read_parquet(here(
  "data",
  "prolific_testing",
  "data.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'), -respID) %>%
  mutate(psid = prolific_pid, data_source = 'prolific') %>%
  select(-study_id, -prolific_session_id, -prolific_pid, -current_page)


data_raw_prolific_round2 <- read_parquet(here(
  "data",
  "prolific_testing",
  "data_round_2+3+4_may_26.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'), -respID) %>%
  mutate(psid = prolific_pid, data_source = 'prolific') %>%
  select(-study_id, -prolific_session_id, -prolific_pid, -current_page
  )


data_joint %>% 
  group_by( data_source) %>% 
  count()

#########

data <- data_joint %>%
  mutate(
    price = price / 10000, # 0.5-6
    range_bev = range_bev / 100, # 0.5 - 2.5
    mileage = mileage / 10000, # 2 - 6
    age = age, # 2 - 8
    operating_cost = operating_cost / 10 # 0.3 - 2.5,
  ) 

data_raw_joined <- rbind(
  rbind(data_raw_dynata, data_raw_prolific),
  data_raw_prolific_round2
)


data_raw_joined <- data_raw_joined |> 
  filter(psid %in% unique(data_joint$psid) )


############################################################
# ---- Table 3: Household vehicle context & used-BEV purchase interest ----
# For paper subsection 3.3, inserted after Table 2 (sample_vs_acs_vehicle).
############################################################

n_total <- nrow(data_raw_joined)
n_primary_fuel <- sum(!is.na(data_raw_joined$primary_veh_fuel))

hh_context_tab <- bind_rows(
data_raw_joined %>%
    filter(!is.na(household_veh_count)) %>%
    mutate(category = if_else(household_veh_count >= 4, "4 or more",
                               as.character(household_veh_count))) %>%
    count(category) %>%
    mutate(
      pct = round(100 * n / sum(n), 1),
      variable = "Household vehicle count",
      order = match(category, c("0", "1", "2", "3", "4 or more"))
    ) %>%
    arrange(order),

data_raw_joined %>%
    filter(!is.na(primary_veh_fuel)) %>%
    mutate(category = recode(primary_veh_fuel,
      "icev" = "Gasoline (ICEV)",
      "hev"  = "Hybrid (HEV)",
      "phev" = "Plug-in hybrid (PHEV)",
      "bev"  = "Battery electric (BEV)",
      "other" = "Other"
    )) %>%
    count(category) %>%
    mutate(
      pct = round(100 * n / sum(n), 1),
      variable = "Current primary vehicle fuel type",
      order = match(category, c("Gasoline (ICEV)", "Hybrid (HEV)",
                                "Plug-in hybrid (PHEV)", "Battery electric (BEV)", "Other"))
    ) %>%
    arrange(order),

data_raw_joined %>%
  group_by(neighbor_ev_info) %>%
  count()

data_raw_joined %>%
  group_by(household_veh_count) %>%
  count()


data_raw_joined %>%
  group_by(next_veh_fuel_new_bev) %>%
  count()

data_raw_joined %>%
  group_by(next_veh_fuel_used_bev) %>%
  count()


data_raw_joined %>%
  group_by(next_veh_fuel_new_bev) %>%
  count()


###########
