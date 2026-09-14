source(here::here('code', 'setup.R'))

library(ggrepel)
library(cowplot)

# Load the estimated model



# Load the estimated model
load(here("models", "mixed_model_1_car_low.RData"))
load(here("models", "mixed_model_1_car_high.RData"))
load(here("models", "mixed_model_1_suv_low.RData"))
load(here("models", "mixed_model_1_suv_high.RData"))



depreciation_rate = 0.05  # Annual percentage depreciation for EVs
 
# Load Data :

vehicles_comparsion_list <- read_csv(here('data', 'vehicles_comparison_list.csv'))  


vehicles_comparsion_list <- vehicles_comparsion_list |> 
  pivot_longer(
    cols = c(bev_vehicle, other_vehicle, bev_range, other_vehicle_range),
    names_to = c("type", ".value"),
    names_pattern = "(bev|other)_(.*)"
  ) |> mutate(
    range = coalesce(range, vehicle_range)
  ) |> select(vehicle, range, id, comparison, vehicle_category, budget) |> 
  rename(vehicle_grouping = id)

predicted_car_prices <- read_parquet(here('data', 'predicted_prices.parquet' )) |> 
    separate(id , into = c('model', 'make', 'powertrain', 'vehicle_type'), 
   sep = "_", remove = FALSE)  |> 
  left_join(vehicles_comparsion_list, by = c('id' = 'vehicle')) |> 
  mutate(model_type = vehicle_type)


bev_names <- predicted_car_prices  |> 
  #filter(powertrain != 'cv') |> 
  select(model, make, powertrain) |> 
  distinct()

depreciation_curves_plot <- predicted_car_prices |>
  filter(
     vehicle_grouping != 5, 
     age_years >1
     ) |> 
  ggplot(aes(x = age_years, y = predicted_price,
             group = id, color = powertrain)) +
  #facet_wrap(~vehicle_grouping) +
    facet_grid(budget ~ vehicle_category) +
  # Line: 2px, series color carries identity
  geom_line(linewidth = 0.9, lineend = "round")  +
    # Point: white surface ring beneath a series-colored marker (>=8px)
  geom_point(size = 3.6) +
  geom_point(size = 2.5) 

depreciation_curves_plot

vehicle_pairs <- data.frame(
  vehicle_1 = c( 
    'leaf_nissan_bev_car',
    'hardtop 2 door_mini_bev_car',
    'i4_bmw_bev_car',
    'kona ev_hyundai_bev_suv',
    'rav4_toyota_hev_suv',
    'camry_toyota_hev_car'
    ),
  vehicle_2 = c(
    'versa sedan_nissan_cv_car',
    'cooper_mini_cv_car',
    '4 series_bmw_cv_car', 
    'kona_hyundai_cv_suv',
    'rav4_toyota_cv_suv',
    'camry_toyota_cv_car'
),
  vehicle_type = c('car', 'car', 'car', 'suv', 'suv', 'suv'  ),
  model_type = c('low', 'high', 'high', 'low', 'high', 'high'  )
)





df <- data.frame(
  obsID = numeric(),
  altID = numeric(),
  powertrainhev = numeric(),
  powertrainhev = numeric(),
  range_bev = numeric(),
  mileage = numeric(),
  age = numeric(),
  operating_cost = numeric(),
  price = numeric(),
  no_choice = numeric()
)

placeholder_df <- data.frame(
  age_years = numeric(),
  miles = numeric(),
  vehicle1 = character(),
  vehicle2 = character(),
  range_v1= numeric(),
  range_v2= numeric(),
  v1_choice_probability = numeric(),
  model_type = character()
)


#test <- get("model_car_low")

predicted_car_prices <- left_join(predicted_car_prices, 
  bev_names,
   by = c('model', 'make', 'powertrain')) |> 
  replace_na(list(range=0)) |> 
  mutate(
    powertrainbev = case_when(
    powertrain == 'bev' ~ 1,
    .default = 0
  ),
    powertrainhev = case_when(
    powertrain %in% c('phev', 'hev') ~ 1,
    .default = 0
  ),
  operating_cost = case_when(
    powertrain == 'bev' ~ 0.3,
    powertrain %in% c('phev', 'hev') ~ 0.6,
    .default = 1.2
  )
) |> mutate(
    miles = miles /10000,
    predicted_price = predicted_price/ 10000,
    range=  range/100
  )

all_cars <- unique(predicted_car_prices$id)

temp <- predicted_car_prices |> 
  filter(age_years == 2)

for (i in seq(nrow(vehicle_pairs))){

 #i =1 

  print(vehicle_pairs[i,1]) 
  current_model = case_when(
    vehicle_pairs[i,'vehicle_type'] == 'car'  & vehicle_pairs[i,'model_type'] == 'low' ~ 'mixed_model_1_car_low',
    vehicle_pairs[i,'vehicle_type'] == 'car'  & vehicle_pairs[i,'model_type'] == 'high' ~ 'mixed_model_1_car_high',
    vehicle_pairs[i,'vehicle_type'] == 'suv'  & vehicle_pairs[i,'model_type'] == 'low' ~ 'mixed_model_1_suv_low',
    .default = 'mixed_model_1_suv_high'
  )

  vehicle_type1 =  vehicle_pairs[i,'vehicle_type']
  
  vehicle1 <-  vehicle_pairs[i,1]   #  'fusion energi_ford_phev_car'   
  vehicle2 <-  vehicle_pairs[i,2]   # 'fusion_ford_cv_car'

  v1_data <- predicted_car_prices |>
    filter(id == vehicle1) |> 
    mutate (range = range * (1- depreciation_rate) ^ age_years )

  v2_data <- predicted_car_prices |> 
    filter(id == vehicle2)


  for (i in seq(nrow(v1_data)))
    {
      #i = 1
      v1 <- v1_data[i,]
      v2 <- v2_data[i,]
    print(v1)
    model = current_model
    
      df2 <- data.frame(
        obsID = c(1,1),
        altID = c(1,2),
        powertrainbev = c(v1$powertrainbev, v2$powertrainbev),
        powertrainhev = c(v1$powertrainhev, v2$powertrainhev),
        range_bev = c(v1$range, v2$range),
        mileage = c(v1$miles, v2$miles),
        age = c(v1$age_years, v2$age_years),
        operating_cost = c(v1$operating_cost, v2$operating_cost),
          price = c(v1$predicted_price, v2$predicted_price),
          no_choice =c(0,0),
        stringsAsFactors = FALSE
      )

      df2

      probabilities <- predict(
        get(model),  # model_car_low  
        newdata = df2,
        obsID = "obsID",
        returnData = FALSE
      )

      probabilities

      row_data <- data.frame(
        age_years = v1$age_years,
        miles = v1$miles,
        vehicle1 = v1$id,
        vehicle2 = v2$id,
        range_v1= v1$range,
        range_v2= v2$range,
        v1_choice_probability = probabilities[1,2],
        model_type = vehicle_type1 
      )

      placeholder_df <- rbind(placeholder_df, row_data)

  }

}


placeholder_df <- placeholder_df |>
  mutate(
    comparisons = case_when(
      vehicle1 == 'leaf_nissan_bev_car'          ~ 'Nissan Leaf BEV // Nissan Versa CV',
      vehicle1 == 'hardtop 2 door_mini_bev_car'  ~ 'Mini Cooper BEV // Mini Cooper CV',
      vehicle1 == 'i4_bmw_bev_car'               ~ 'BMW i4 BEV // BMW 4 Series CV',
      vehicle1 == 'ioniq_hyundai_bev_car'        ~ 'Hyundai Ioniq BEV // HEV',
      vehicle1 == 'kona ev_hyundai_bev_suv'      ~ 'Hyundai Kona BEV // CV',
      vehicle1 == 'niro_kia_bev_suv'             ~ 'Kia Niro BEV // HEV',
      .default = 'not found'
    ),
    comparisons = factor(
      comparisons,
      levels = c(
        'Nissan Leaf BEV // Nissan Versa CV',
        'Mini Cooper BEV // Mini Cooper CV',
        'BMW i4 BEV // BMW 4 Series CV',
        'Hyundai Ioniq BEV // HEV',
        'Hyundai Kona BEV // CV',
        'Kia Niro BEV // HEV'
      )
    ),
    segment_label = if_else(model_type == 'car', 'Car', 'SUV')
  )

# Fixed-order categorical palette: "minou" from the ltc color-palette
# library (https://loukesio.github.io/ltc-color-palettes/). Assigned by
# identity, never cycled.
comparison_colors <- c(
  'Nissan Leaf BEV // Nissan Versa CV' = "#00798c",  # teal
  'Mini Cooper BEV // Mini Cooper CV'  = "#d1495b",  # red
  'BMW i4 BEV // BMW 4 Series CV'      = "#edae49",  # amber
  'Hyundai Ioniq BEV // HEV'           = "#66a182",  # sage green
  'Hyundai Kona BEV // CV'             = "#2e4057",  # dark navy
  'Kia Niro BEV // HEV'                = "#8d96a3"   # gray
)

ink_primary   <- "#0b0b0b"
ink_secondary <- "#52514e"
ink_muted     <- "#898781"
grid_hairline <- "#e1e0d9"
baseline_ink  <- "#c3c2b7"
chart_surface <- "#fcfcfb"
strip_surface <- "#f2f1ee"

bev_probability_plot <- placeholder_df |>
  ggplot(aes(x = age_years, y = v1_choice_probability,
             group = comparisons, color = comparisons)) +
  facet_wrap(~segment_label) +

  # 50% reference line -- choice parity with the conventional/hybrid counterpart
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = baseline_ink,
    linewidth = 0.4
  ) +

  # Line: 2px, series color carries identity
  geom_line(linewidth = 0.9, lineend = "round") +

  # Point: white surface ring beneath a series-colored marker (>=8px)
  geom_point(size = 3.6, color = chart_surface) +
  geom_point(size = 2.5) +

  scale_color_manual(values = comparison_colors, name = NULL) +

  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1L),
    breaks = scales::breaks_pretty(n = 6),
    expand = expansion(mult = c(0.03, 0.06))
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(1),
    expand = expansion(mult = c(0.03, 0.06))
  ) +

  labs(
    title = "Probability of Choosing a BEV Over Its Conventional or Hybrid Counterpart",
    subtitle = "By vehicle age, for matched car and SUV model pairs",
    x = "Vehicle age (years)",
    y = "Probability of BEV choice"
  ) +

  guides(color = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(linewidth = 1.6, size = 3))) +

  theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = chart_surface, color = NA),
    panel.background  = element_rect(fill = chart_surface, color = NA),
    legend.background = element_rect(fill = chart_surface, color = NA),

    plot.title    = element_text(face = "bold", size = 14.5, color = ink_primary,
                                  margin = margin(b = 3)),
    plot.subtitle = element_text(size = 11, color = ink_secondary,
                                  margin = margin(b = 10)),
    plot.caption  = element_text(size = 8.5, color = ink_muted, hjust = 0,
                                  margin = margin(t = 8)),
    plot.margin   = margin(12, 14, 10, 12),

    strip.text       = element_text(face = "bold", size = 11, color = ink_primary),
    strip.background = element_rect(fill = strip_surface, color = NA),

    axis.title = element_text(size = 10.5, color = ink_secondary),
    axis.text  = element_text(size = 9.5, color = ink_muted),
    axis.ticks = element_line(color = baseline_ink, linewidth = 0.3),
    axis.line  = element_blank(),

    panel.grid.major = element_line(color = grid_hairline, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.4, "lines"),

    legend.position  = "bottom",
    legend.text      = element_text(size = 9, color = ink_secondary),
    legend.key       = element_rect(fill = chart_surface, color = NA),
    legend.spacing.x = unit(6, "pt")
  )

bev_probability_plot

ggsave(
  filename = here::here(
    'code',
    'output',
    "images",
    "vehicle_analysis",
    "BEV_probability_age_with_depreciation.png"
  ),
  plot = bev_probability_plot,
  width = 10,
  height = 6.5,
  dpi = 300
)

ggsave(
  filename = here::here(
    'paper_writing',
    'vehicle_paper',
    "images",
    "vehicle_analysis",
    "BEV_probability_age_with_depreciation.png"
  ),
  plot = bev_probability_plot,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

#############################################################


placeholder_df <- placeholder_df |>
  mutate(
    comparisons = case_when(
      vehicle1 == 'leaf_nissan_bev_car'          ~ 'Low Budget Car - BEV vs CV',   # Nissan Versa cost - 18k
      vehicle1 == 'hardtop 2 door_mini_bev_car'  ~ 'High Budget Car - BEV vs CV',   # Mini Cooper cost - 36k
      vehicle1 == 'i4_bmw_bev_car'               ~ 'High Budget Car - BEV vs CV',  # BMW 4 series cost - 60k
      vehicle1 == 'ioniq_hyundai_bev_car'        ~ 'Mid Budget Car - BEV vs HEV',  # Hyundai Ionic HEV cost - 30k
      vehicle1 == 'kona ev_hyundai_bev_suv'      ~ 'Mid Budget SUV - BEV vs CV',   # Hyundai Kona CV cost - 27k
      vehicle1 == 'niro_kia_bev_suv'             ~ 'High budget SUV - BEV vs HEV', # Kia Nero HEV cost - 36k
      .default = 'not found'
    ),
    comparisons = factor(
      comparisons,
      levels = c(
        'Low Budget Car - BEV vs CV',
        'Mid Budget Car - BEV vs CV',
        'High Budget Car - BEV vs CV',
        'Mid Budget Car - BEV vs HEV',
        'Mid Budget SUV - BEV vs CV',
        'High budget SUV - BEV vs HEV'
      )
    ),
    segment_label = if_else(model_type == 'car', 'Car', 'SUV')
  ) |> 
  filter(vehicle1 != 'hardtop 2 door_mini_bev_car')

# Fixed-order categorical palette: "minou" from the ltc color-palette
# library (https://loukesio.github.io/ltc-color-palettes/). Assigned by
# identity, never cycled.
comparison_colors <- c(
  'Low Budget Car - BEV vs CV' = "#00798c",  # teal
  'Mid Budget Car - BEV vs CV'  = "#d1495b",  # red
  'High Budget Car - BEV vs CV'      = "#edae49",  # amber
  'Mid Budget Car - BEV vs HEV'           = "#66a182",  # sage green
  'Mid Budget SUV - BEV vs CV'             = "#2e4057",  # dark navy
  'High budget SUV - BEV vs HEV'                = "#8d96a3"   # gray
)

ink_primary   <- "#0b0b0b"
ink_secondary <- "#52514e"
ink_muted     <- "#898781"
grid_hairline <- "#e1e0d9"
baseline_ink  <- "#c3c2b7"
chart_surface <- "#fcfcfb"
strip_surface <- "#f2f1ee"

bev_probability_plot <- placeholder_df |>
  ggplot(aes(x = age_years, y = v1_choice_probability,
             group = comparisons, color = comparisons)) +
  facet_wrap(~segment_label) +

  # 50% reference line -- choice parity with the conventional/hybrid counterpart
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = baseline_ink,
    linewidth = 0.4
  ) +

  # Line: 2px, series color carries identity
  geom_line(linewidth = 0.9, lineend = "round") +

  # Point: white surface ring beneath a series-colored marker (>=8px)
  geom_point(size = 3.6, color = chart_surface) +
  geom_point(size = 2.5) +

  scale_color_manual(values = comparison_colors, name = NULL) +

  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1L),
    breaks = scales::breaks_pretty(n = 6),
    expand = expansion(mult = c(0.03, 0.06))
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(1),
    expand = expansion(mult = c(0.03, 0.06))
  ) +

  labs(
    title = "Probability of Choosing a BEV Over Its Conventional or Hybrid Counterpart",
    subtitle = "By vehicle age, for matched car and SUV model pairs",
    x = "Vehicle age (years)",
    y = "Probability of BEV choice"
  ) +

  guides(color = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(linewidth = 1.6, size = 3))) +

  theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = chart_surface, color = NA),
    panel.background  = element_rect(fill = chart_surface, color = NA),
    legend.background = element_rect(fill = chart_surface, color = NA),

    plot.title    = element_text(face = "bold", size = 14.5, color = ink_primary,
                                  margin = margin(b = 3)),
    plot.subtitle = element_text(size = 11, color = ink_secondary,
                                  margin = margin(b = 10)),
    plot.caption  = element_text(size = 8.5, color = ink_muted, hjust = 0,
                                  margin = margin(t = 8)),
    plot.margin   = margin(12, 14, 10, 12),

    strip.text       = element_text(face = "bold", size = 11, color = ink_primary),
    strip.background = element_rect(fill = strip_surface, color = NA),

    axis.title = element_text(size = 10.5, color = ink_secondary),
    axis.text  = element_text(size = 9.5, color = ink_muted),
    axis.ticks = element_line(color = baseline_ink, linewidth = 0.3),
    axis.line  = element_blank(),

    panel.grid.major = element_line(color = grid_hairline, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.4, "lines"),

    legend.position  = "bottom",
    legend.text      = element_text(size = 9, color = ink_secondary),
    legend.key       = element_rect(fill = chart_surface, color = NA),
    legend.spacing.x = unit(6, "pt")
  )

bev_probability_plot

ggsave(
  filename = here::here(
    'code',
    'output',
    "images",
    "vehicle_analysis",
    "BEV_probability_age_with_depreciation_generic.png"
  ),
  plot = bev_probability_plot,
  width = 10,
  height = 6.5,
  dpi = 300
)

ggsave(
  filename = here::here(
    'paper_writing',
    'vehicle_paper',
    "images",
    "vehicle_analysis",
    "BEV_probability_age_with_depreciation_generic.png"
  ),
  plot = bev_probability_plot,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

