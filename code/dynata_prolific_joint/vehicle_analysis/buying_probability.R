source(here::here('code', 'setup.R'))

library(ggrepel)
library(cowplot)

# Load the estimated model

# load(here("models", "model_car_low.RData"))
# load(here("models", "model_car_high.RData"))
# load(here("models", "model_suv_low.RData"))
# load(here("models", "model_suv_high.RData"))


load(here("models", "model_car.RData"))
load(here("models", "model_suv.RData"))

summary(model_car_high)


depreciation_rate = 0.05  # Annual percentage depreciation for EVs
 
# Load Data :

predicted_car_prices <- read_parquet(here('data', 'predicted_prices.parquet' )) |> 
    separate(id , into = c('model', 'make', 'powertrain', 'vehicle_type'),  sep = "_", remove = FALSE) |> 
  mutate (model_type = case_when 
    (
    # (predicted_price <= 25000 & vehicle_type == 'car' ) ~ 'model_car_low',
    # (predicted_price >  25000 & vehicle_type == 'car' ) ~ 'model_car_high',
    # (predicted_price <= 25000 & vehicle_type == 'suv' ) ~ 'model_suv_low',
    # .default = 'model_suv_high'
    vehicle_type == 'car' ~ 'model_car',
    .default = 'model_suv'
  )
)

bev_names <- predicted_car_prices  |> 
  filter(powertrain != 'cv') |> 
  select(model, make, powertrain) |> 
  distinct()

bev_names$range <- c(149, 114, 269, 170, 57, 258, 253, 33, 53, 26) # vehicle range from internet search

vehicle_pairs <- data.frame(
  vehicle_1 = c( 'leaf_nissan_bev_car',
   'hardtop 2 door_mini_bev_car',
    'i4_bmw_bev_car',
     'ioniq_hyundai_bev_car',
     'kona ev_hyundai_bev_suv',
     'niro_kia_bev_suv'
    ),
  vehicle_2 = c('versa sedan_nissan_cv_car',
   'cooper_mini_cv_car',
    '4 series_bmw_cv_car', 
  'ioniq_hyundai_hev_car',
   'kona_hyundai_cv_suv',
  'niro_kia_hev_suv'),
  type = c('car_low', 'car_low', 'car_high', 'car_high', 'suv_low', 'suv_low'  )
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
  v1_choice_probability = numeric()
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


for (i in seq(nrow(vehicle_pairs))){
  print(vehicle_pairs[i,1]) 
  # model = case_when(
  #   vehicle_pairs[i,3] == 'car_low'  ~ 'model_car_low',
  #   vehicle_pairs[i,3] == 'car_high'  ~ 'model_car_high',
  #   vehicle_pairs[i,3] == 'suv_low'  ~ 'model_suv_low',
  #   .default = 'model_suv_low'
  # )
  #i = 1
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
    model = v1$model_type
    
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
        v1_choice_probability = probabilities[1,2]
      )

      placeholder_df <- rbind(placeholder_df, row_data)

  }

}

placeholder_df<- placeholder_df |> 
mutate(
  comparisons =  case_when(
  vehicle1 == 'leaf_nissan_bev_car'  ~ 'Nissan Leaf BEV //  Nissan Versa CV ',
  vehicle1 == 'hardtop 2 door_mini_bev_car'  ~ 'Mini Cooper BEV //  CV',
  vehicle1 == 'i4_bmw_bev_car'  ~ 'BMW I4 BEV // BMW 4 series CV',
  vehicle1 == 'ioniq_hyundai_bev_car' ~ 'Hyundai Ioniq BEV // HEV',
  vehicle1 == 'kona ev_hyundai_bev_suv'  ~ 'Hyundai Kona SUV BEV // CV',
  vehicle1 == 'niro_kia_bev_suv'  ~ 'Niro Kia SUV BEV // HEV',
  .default = 'not found'
)
)




  
  placeholder_df |> 
  ggplot(aes(x = age_years, y = v1_choice_probability, group = vehicle1, color = vehicle1)) +
  # Main line with improved styling
  geom_line(
    linewidth = 1.1,
    alpha = 0.95
  ) +
  # Add points for emphasis
  geom_point(
    color = "#2E86AB",
    size = 2.5,
    alpha = 0.7
  ) +
  # Labels and title
  labs(
    title = "BEV Choice Probability by Age",
    x = "Age (years)",
    y = "Probability of Choice"
  ) +
  # Improved scales
  scale_y_continuous(
    #limits = c(0.15, 0.90),
    labels = scales::percent_format(accuracy = 1L),
    breaks = seq(0.10, 0.90, by = 0.10),
    expand = c(0.02, 0)
  ) +
   theme_minimal(base_size = 14) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    
    strip.text = element_text(face = "bold"),   # facet titles
    strip.background = element_rect(fill = "grey90", color = NA),
    
    legend.position = "none",   # cleaner for papers  #"bottom"
    legend.direction = "horizontal",
    
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines")
  ) +
    geom_text_repel(
  data = placeholder_df |> slice_max(age_years, by = vehicle1),
  aes(label = comparisons),
  nudge_x      = 1,
  direction    = "both",      # only repel vertically
  hjust        = 0.5,
  segment.size = 0.3,
  size         = 4.0
) +
scale_x_continuous(expand = expansion(mult = c(0.02, 0.20)))

ggsave(
  filename = here::here(
    'code',
    'output',
    "images",
    "vehicle_analysis",
    "BEV_probability_age_with_depreciation_0.png"    
  ),
  width = 8,
  height = 6,
  dpi = 300
)





#################################

library(ggplot2)
library(ggrepel)
library(scales)

# Professional color palette (colorblind-friendly)
# bev_colors <- c(
#   "BMW I4 (bev) VS BMW 4 series (cv)"            = "#2166AC",
#   "Mini Hardtop 2 door (bev) VS Mini Cooper (cv)" = "#E07B54",
#   "Nissan Leaf (bev) VS Nissan Versa (cv)"         = "#5AAE61",
#   "Hyundai Ioniq (bev) VS Hyundai Ioniq (hev)"    = "#1B7837",
#   "Hyundai Kona SUV (bev) VS Hyundai Kona SUV (cv)"= "#4393C3",
#   "Niro Kia SUV (bev) VS Niro Kia SUV (hev)"      = "#BF5B8A"
# )

nudge_lookup <- c(
  'Hyundai Kona SUV BEV // CV' = -0.03,  # push down
  'Hyundai Ioniq BEV // HEV'     = -0.05,
  'Nissan Leaf BEV // Nissan Versa CV '           =  0.05
  # all others default to 0
)

label_data <- placeholder_df |>
  slice_min(abs(age_years - median(age_years)), by = vehicle1) |>
  mutate(
    nudge_y_val = coalesce(nudge_lookup[as.character(vehicle1)], 0)
  )

# Colorblind-friendly palette (enough for up to 8 series)
prof_palette <- c("#2166AC", "#E07B54", "#1B7837", "#BF5B8A",
                  "#4393C3", "#5AAE61", "#D6604D", "#762A83")

# Dynamically assign to actual factor levels in your data
vehicle_levels <- unique(placeholder_df$vehicle1)
bev_colors <- setNames(prof_palette[seq_along(vehicle_levels)], vehicle_levels)

placeholder_df |>
  ggplot(aes(x = age_years, y = v1_choice_probability,
             group = vehicle1, color = vehicle1)) +

  # Reference line at 50%
  geom_hline(yintercept = 0.50, linetype = "dashed",
             color = "grey65", linewidth = 0.4) +

  # Main lines
  geom_line(linewidth = 1.0, alpha = 0.9) +

  # Points — match line color instead of overriding to blue
  geom_point(aes(color = vehicle1), size = 2.2, alpha = 0.85) +

  # Labels
geom_text_repel(
  data              = label_data,
  aes(label         = comparisons),
  nudge_x           = 1.2,
  nudge_y           = label_data$nudge_y_val,   # per-label vertical nudge
  direction         = "y",
  hjust             = 0.0,
  #vjust             = 0.5,
  force             = 3,
  force_pull        = 0.5,
  min.segment.length = 0.5,
  segment.size      = 0.35,
  segment.color     = "grey60",
  segment.curvature = -0.1,
  size              = 3.5,
  max.overlaps      = Inf,
  box.padding       = 1.5,
  point.padding     = 0.4
) +

  scale_color_manual(values = bev_colors) +

  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1L),
    breaks = seq(0.10, 0.90, by = 0.10),
    expand = c(0.02, 0)
  ) +

  scale_x_continuous(
    breaks = seq(2, 6, by = 1),
    expand = expansion(mult = c(0.02, 0.22))
  ) +

  labs(
    title    = "Probability of choosing Battery-electric over Conventional \n or Hybrid equivalent",
    x        = "Vehicle age (years)",
    y        = "Probability of BEV choice",
    caption  = "Note: Dashed line indicates 50% choice parity."
  ) +

  theme_minimal(base_size = 13, base_family = "sans") +
  theme_minimal_grid(font_family = "Roboto Condensed", font_size = 14) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),

    plot.title       = element_text(face = "bold", size = 15,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 11, color = "grey40",
                                    margin = margin(b = 12)),
    plot.caption     = element_text(size = 9, color = "grey55",
                                    hjust = 0, margin = margin(t = 8)),
    plot.margin      = margin(12, 12, 10, 12),

    axis.title       = element_text(size = 11, color = "grey30"),
    axis.text        = element_text(size = 10, color = "grey40"),
    axis.ticks       = element_line(color = "grey80", linewidth = 0.3),

    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_blank(),

    legend.position  = "none"
  )


ggsave(
  filename = here::here(
    'code',
    'output',
    "images",
    "vehicle_analysis",
    "BEV_probability_age_with_depreciation.png"    
  ),
  width = 8,
  height = 6,
  dpi = 300
)

##########################################

# placeholder_df |>
#   ggplot(aes(x = age_years, y = v1_choice_probability,
#              group = vehicle1, color = vehicle1)) +

#   geom_hline(yintercept = 0.50, linetype = "dashed",
#              color = "grey65", linewidth = 0.4) +
#   geom_line(linewidth = 1.0, alpha = 0.9) +
#   geom_point(aes(color = vehicle1), size = 2.2, alpha = 0.85) +

#   scale_color_manual(
#     values = bev_colors,
#     labels = setNames(
#       placeholder_df$comparisons[match(names(bev_colors), placeholder_df$vehicle1)],
#       names(bev_colors)
#     )
#   ) +

#   scale_y_continuous(
#     labels = scales::percent_format(accuracy = 1L),
#     breaks = seq(0.10, 0.90, by = 0.10),
#     expand = c(0.02, 0)
#   ) +

#   scale_x_continuous(
#     breaks = seq(2, 6, by = 1),
#     expand = expansion(mult = c(0.02, 0.05))
#   ) +

#   labs(
#     title    = "BEV Choice Probability by Vehicle Age",
#     subtitle = "Probability of choosing Battery-electric over Conventional or Hybrid equivalent",
#     x        = "Vehicle age (years)",
#     y        = "Probability of BEV choice",
#     color    = NULL,          # no legend title
#     caption  = "Note: Dashed line indicates 50% choice parity."
#   ) +
#   guides(
#   color = guide_legend(ncol = 2)
# ) +

#   theme_minimal(base_size = 13) +
#   theme(
#     plot.background  = element_rect(fill = "white", color = NA),
#     panel.background = element_rect(fill = "white", color = NA),
#     plot.title       = element_text(face = "bold", size = 15, margin = margin(b = 4)),
#     plot.subtitle    = element_text(size = 11, color = "grey40", margin = margin(b = 12)),
#     plot.caption     = element_text(size = 9, color = "grey55", hjust = 0, margin = margin(t = 8)),
#     plot.margin      = margin(12, 12, 10, 12),

#     axis.title       = element_text(size = 11, color = "grey30"),
#     axis.text        = element_text(size = 10, color = "grey40"),
#     panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
#     panel.grid.minor = element_blank(),

#     # Legend styling
#     legend.position        = "bottom",
#     legend.direction       = "horizontal",
#     legend.justification   = "center",
#     legend.text            = element_text(size = 8.5, color = "grey20"),
#     legend.key.width       = unit(1.2, "cm"),  # wider color swatch
#     legend.key.height      = unit(0.5, "cm"),
#     legend.spacing.y       = unit(0.3, "cm"),  # breathing room between entries
#     legend.margin          = margin(0, 0, 0, 10)
#   )


# ggsave(
#   filename = here::here(
#     'code',
#     'output',
#     "images",
#     "vehicle_analysis",
#     "BEV_probability_age_with_depreciation1.png"    
#   ),
#   width = 8,
#   height = 6,
#   dpi = 300
# )
