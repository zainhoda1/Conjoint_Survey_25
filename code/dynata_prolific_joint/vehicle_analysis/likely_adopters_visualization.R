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
  group_by(next_veh_fuel_new_bev) %>%
  count()

data_raw_joined %>%
  group_by(next_veh_fuel_used_bev) %>%
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
  




#########
# Stacked-bar chart of used-BEV purchase likelihood (YouGov-style, net categories)

vehicle_type_lookup <- data_joint |>
  distinct(psid, vehicle_type)

likelihood_levels <- c('Net: Likely', 'Neutral', 'Net: Unlikely')

likelihood_data <- data_raw_joined |>
  distinct(psid, .keep_all = TRUE) |>
  left_join(vehicle_type_lookup, by = 'psid') |>
  mutate(
    age_years = 2026 - as.numeric(birth_year),
    group_age = case_when(
      age_years < 18 ~ NA_character_,
      age_years <= 34 ~ '18-34',
      age_years <= 54 ~ '35-54',
      TRUE ~ '55+'
    ),
    group_vehicle = case_when(
      vehicle_type == 'car' ~ 'Car buyers',
      vehicle_type == 'suv' ~ 'SUV buyers',
      TRUE ~ NA_character_
    ),
    likelihood = case_when(
      next_veh_fuel_used_bev %in% c('very_likely', 'somewhat_likely') ~ 'Net: Likely',
      next_veh_fuel_used_bev == 'neutral' ~ 'Neutral',
      next_veh_fuel_used_bev %in% c('very_unlikely', 'somewhat_unlikely') ~ 'Net: Unlikely'
    )
  ) |>
  filter(!is.na(likelihood))

group_order <- c('All', '18-34', '35-54', '55+', 'Car buyers', 'SUV buyers')

likelihood_summary <- bind_rows(
  likelihood_data |> mutate(group = 'All'),
  likelihood_data |> filter(!is.na(group_age)) |> mutate(group = group_age),
  likelihood_data |> filter(!is.na(group_vehicle)) |> mutate(group = group_vehicle)
) |>
  count(group, likelihood) |>
  group_by(group) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup() |>
  mutate(
    group = factor(group, levels = rev(group_order)),
    likelihood = factor(likelihood, levels = rev(likelihood_levels))
  )

likelihood_colors <- c(
  'Net: Likely'   = '#A033F5',
  'Neutral'       = '#C5C9D6',
  'Net: Unlikely' = '#FF3F2B'
)

likelihood_plot <- ggplot(
  likelihood_summary,
  aes(x = pct, y = group, fill = likelihood)
) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = round(pct), group = likelihood, color = likelihood == 'Neutral'),
    position = position_stack(vjust = 0.5),
    fontface = 'bold',
    size = 4
  ) +
  scale_color_manual(values = c('TRUE' = '#333333', 'FALSE' = 'white'), guide = 'none') +
  scale_fill_manual(values = likelihood_colors, breaks = likelihood_levels, name = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = 'Likelihood of considering a used BEV among used-vehicle buyers',
    subtitle = 'How likely or unlikely are you to consider purchasing a used BEV? (% of respondents)',
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = 'bold', size = 15),
    plot.title.position = 'plot',
    plot.subtitle = element_text(size = 10),
    legend.position = 'top',
    legend.justification = 'left',
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12)
  )

likelihood_plot

ggsave(
  here('code', 'output', 'images', 'vehicle_analysis', 'likely_used_bev_purchase.png'),
  likelihood_plot, width = 8, height = 4, dpi = 300, bg = 'white'
)
