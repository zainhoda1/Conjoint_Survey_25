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



#########

# run_model <- function(data) {
#   model <- logitr(
#     data = data,
#     outcome = "choice",
#     obsID = "obsID",
#     pars = c(
#       "powertrainbev",
#       "powertrainhev",
#       "range_bev",
#       "mileage",
#       "age",
#       "operating_cost",
#       "price",
#       "no_choice"
#     )
#   )
#   cat('n =', length(unique(data$respID)))
#   return(model)
# }

# run_mixed_model_1 <- function(data) {
  
#   model <- logitr(
#     data = data,
#     outcome = "choice",
#     obsID = "obsID",
#     panelID = "respID",
#     pars = c(
#       "powertrainbev",
#       "powertrainhev",
#       "range_bev",
#       "mileage",
#       "age",
#       "operating_cost",
#       "no_choice"
#     ),
#     randPars = c(powertrainbev = 'n',
#                  powertrainhev = 'n',
#                  range_bev = 'n',
#                  mileage = 'n',
#                  age = 'n',
#                  operating_cost = 'n',
#                  no_choice = 'n'
#     ),
#     scalePar = 'price',
#     drawType = 'sobol',
#     numDraws = 5000,
#     numMultiStarts = 10
#   )
#   cat('n =', length(unique(data$respID)))
#   return(model)
# }

encoding <- function(data) {
  data <- cbc_encode(
    data,
    coding = 'dummy',
    ref_levels = list(powertrain = 'gas', vehicle_type = 'car', budget = 'low')
  )
  return(data)
}

#########

#data_raw_joined <- left_join(data_joint, data_raw_joined, by = c('psid', 'data_source' , 'budget', 'next_veh_budget'))

data_raw_joined


data_raw_joined %>%
  group_by(primary_veh_fuel) %>%
  count()

data_raw_joined %>%
  group_by(charger_access) %>%
  count()

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


likely_bev_adopter <- data_raw_joined %>%
  filter(
      (next_veh_fuel_used_bev %in%
        c('very_likely', 'somewhat_likely'))  #, 'neutral'
  ) %>%
  select(psid)

nrow(likely_bev_adopter)


likely_bev_adopter_encoded <- encoding(
  inner_join(data, likely_bev_adopter, by = 'psid') |>
    select(-psid, -collection_round, -data_source))
  
data_car <- encoding (data |> 
  select (-psid, -collection_round, -data_source) ) |> 
  filter(vehicle_typesuv == 0)

data_suv <- encoding (data |> 
  select (-psid, -collection_round, -data_source) ) |> 
  filter(vehicle_typesuv == 1)

data_car_low <- data_car |> 
  filter(budgethigh == 0)

data_car_high <- data_car |> 
  filter(budgethigh == 1)

data_suv_low <- data_suv |> 
  filter(budgethigh == 0)

data_suv_high <- data_suv |> 
  filter(budgethigh == 1)



# mixed_model_1_car <- run_mixed_model_1(data_car)
# mixed_model_1_suv <- run_mixed_model_1(data_suv)
# mixed_model_1_car_low <- run_mixed_model_1(data_car_low)
# mixed_model_1_car_high <- run_mixed_model_1(data_car_high)
# mixed_model_1_suv_low <- run_mixed_model_1(data_suv_low)
# mixed_model_1_suv_high <- run_mixed_model_1(data_suv_high)

# mixed_model_1_likely_bev_adopter <- run_mixed_model_1(likely_bev_adopter_encoded)

# mixed_model_1_likely_bev_adopter_car <- run_mixed_model_1(
#   likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 0))

# mixed_model_1_likely_bev_adopter_suv <- run_mixed_model_1(
#   likely_bev_adopter_encoded %>% filter(vehicle_typesuv == 1))


# ######################################

# # Save model object

# save(
#   mixed_model_1_car,
#   file = here("models", "mixed_model_1_car.RData"))

# save(
#   mixed_model_1_suv,
#   file = here("models", "mixed_model_1_suv.RData"))

# save(
#   mixed_model_1_car_low,
#   file = here("models", "mixed_model_1_car_low.RData"))

# save(
#   mixed_model_1_car_high,
#   file = here("models", "mixed_model_1_car_high.RData"))

# save(
#   mixed_model_1_suv_low,
#   file = here("models", "mixed_model_1_suv_low.RData"))

# save(
#   mixed_model_1_suv_high,
#   file = here("models", "mixed_model_1_suv_high.RData"))

# save(
#   mixed_model_1_likely_bev_adopter,
#   file = here("models", "mixed_model_1_likely_bev_adopter.RData"))

# save(
#   mixed_model_1_likely_bev_adopter_car,
#   file = here("models", "mixed_model_1_likely_bev_adopter_car.RData"))

# save(
#   mixed_model_1_likely_bev_adopter_suv,
#   file = here("models", "mixed_model_1_likely_bev_adopter_suv.RData"))



############################################################


############################################################
# ---- Summary statistics: next-vehicle segment & preferences ----
############################################################

# ---- next_veh_style / next_veh_budget ----
summ_next_veh_style <- data_raw_joined %>%
  count(next_veh_style) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# next_veh_budget is a dollar amount -> numeric summary + tabulation
summ_next_veh_budget <- data_raw_joined %>%
  summarise(
    n      = sum(!is.na(next_veh_budget)),
    mean   = round(mean(next_veh_budget, na.rm = TRUE)),
    sd     = round(sd(next_veh_budget, na.rm = TRUE)),
    min    = min(next_veh_budget, na.rm = TRUE),
    p25    = quantile(next_veh_budget, 0.25, na.rm = TRUE),
    median = median(next_veh_budget, na.rm = TRUE),
    p75    = quantile(next_veh_budget, 0.75, na.rm = TRUE),
    max    = max(next_veh_budget, na.rm = TRUE)
  )

summ_next_veh_budget_tab <- data_raw_joined %>%
  count(next_veh_budget) %>%
  mutate(pct = round(100 * n / sum(n), 1))

print(summ_next_veh_style)
print(summ_next_veh_budget)
print(summ_next_veh_budget_tab)

# ---- PHEV / BEV purchase-likelihood preferences, joined into one table ----
pref_levels <- c("very_unlikely", "somewhat_unlikely", "neutral",
                 "somewhat_likely", "very_likely")

pref_vars <- c("next_veh_fuel_new_phev", "next_veh_fuel_used_phev",
               "next_veh_fuel_new_bev",  "next_veh_fuel_used_bev")

pref_joined <- data_raw_joined %>%
  select(all_of(pref_vars)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "response") %>%
  filter(!is.na(response)) %>%
  mutate(response = factor(response, levels = pref_levels)) %>%
  count(item, response, .drop = FALSE) %>%
  group_by(item) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(item = factor(item, levels = pref_vars)) %>%
  arrange(item, response)

pref_joined_n_wide <- pref_joined %>%
  select(item, response, n) %>%
  pivot_wider(names_from = response, values_from = n)

pref_joined_pct_wide <- pref_joined %>%
  select(item, response, pct) %>%
  pivot_wider(names_from = response, values_from = pct)

print(pref_joined_n_wide)
print(pref_joined_pct_wide)

############################################################
# ---- Income and BEV-range distributions vs. the DOE design bands ----
############################################################

# DOE design purchase-price / BEV-range bands by vehicle segment (as designed)
doe_bands <- tribble(
  ~next_veh_style, ~budget, ~price_lo, ~price_hi, ~range_lo, ~range_hi,
  "car", "low",  10000, 20000,  50, 150,
  "car", "high", 20000, 40000, 100, 250,
  "suv", "low",  15000, 25000, 150, 250,
  "suv", "high", 25000, 45000, 200, 350
) %>%
  mutate(
    next_veh_style = factor(next_veh_style, levels = c("car", "suv")),
    budget         = factor(budget, levels = c("low", "high"))
  )

seg_income <- data_raw_joined %>%
  filter(!is.na(next_veh_style), !is.na(budget)) %>%
  mutate(
    hhincome_num   = as.numeric(hh_income),
    next_veh_style = factor(next_veh_style, levels = c("car", "suv")),
    budget         = factor(budget, levels = c("low", "high"))
  )

# Household income summary by segment
seg_income_summary <- seg_income %>%
  group_by(next_veh_style, budget) %>%
  summarise(
    n             = sum(!is.na(hhincome_num)),
    mean_income   = round(mean(hhincome_num, na.rm = TRUE)),
    median_income = median(hhincome_num, na.rm = TRUE),
    p25_income    = quantile(hhincome_num, 0.25, na.rm = TRUE),
    p75_income    = quantile(hhincome_num, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(doe_bands, by = c("next_veh_style", "budget"))

print(seg_income_summary)

# Income distribution by segment, with the DOE design price band shaded
income_dist_plot <- ggplot(seg_income, aes(x = hhincome_num)) +
  geom_rect(
    data = doe_bands,
    aes(xmin = price_lo, xmax = price_hi, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE, fill = "#00798c", alpha = 0.15
  ) +
  geom_histogram(bins = 30, fill = "#52514e", color = "white") +
  facet_grid(next_veh_style ~ budget) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1 / 1000, suffix = "K")) +
  labs(
    title    = "Household income distribution by next-vehicle segment",
    subtitle = "Shaded band = DOE design purchase-price range for the segment",
    x = "Household income", y = "Respondents"
  ) +
  theme_minimal(base_size = 12)

print(income_dist_plot)

# BEV range shown in the DOE design, by segment, with the intended band shaded
design_range <- read_parquet(here(
  "data", "doe", "02-06-26", "design_vehicle.parquet"
)) %>%
  filter(powertrain == "bev", !is.na(range_bev)) %>%
  transmute(
    next_veh_style = factor(vehicle_type, levels = c("car", "suv")),
    budget         = factor(budget, levels = c("low", "high")),
    range_bev
  )

range_dist_plot <- ggplot(design_range, aes(x = range_bev)) +
  geom_rect(
    data = doe_bands,
    aes(xmin = range_lo, xmax = range_hi, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE, fill = "#d1495b", alpha = 0.15
  ) +
  geom_histogram(bins = 20, fill = "#52514e", color = "white") +
  facet_grid(next_veh_style ~ budget) +
  labs(
    title    = "BEV range shown in the DOE design, by next-vehicle segment",
    subtitle = "Shaded band = intended design range for the segment",
    x = "BEV range (miles on a full charge)", y = "Design rows"
  ) +
  theme_minimal(base_size = 12)

print(range_dist_plot)

# ---- Save tables and figures ----
write.xlsx(
  list(
    next_veh_style      = summ_next_veh_style,
    next_veh_budget     = summ_next_veh_budget,
    next_veh_budget_tab = summ_next_veh_budget_tab,
    pref_joined_n       = pref_joined_n_wide,
    pref_joined_pct     = pref_joined_pct_wide,
    seg_income_summary  = seg_income_summary
  ),
  paste0(here(), "/code/output/next_veh_segment_summary.xlsx")
)

ggsave(
  here("code", "output", "images", "vehicle_analysis", "income_dist_by_segment.png"),
  income_dist_plot, width = 8, height = 6, dpi = 300, bg = "white"
)
ggsave(
  here("code", "output", "images", "vehicle_analysis", "doe_range_dist_by_segment.png"),
  range_dist_plot, width = 8, height = 6, dpi = 300, bg = "white"
)

################################################

ggplot(data_raw_joined, aes(x = hh_income, y =  next_veh_budget)) +
  geom_point(alpha = 0.5) +  # helps with overplotting if you have many points
  labs(x = "Next Vehicle Budget", y = "Household Income", 
       title = "Household Income vs. Next Vehicle Budget")
