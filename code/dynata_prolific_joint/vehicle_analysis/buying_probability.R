source(here::here('code', 'setup.R'))

# Load the estimated model
load(here("models", "model_car_low.RData"))
load(here("models", "model_car_high.RData"))
load(here("models", "model_suv_low.RData"))
load(here("models", "model_suv_high.RData"))


# Load Data :

predicted_car_prices <- read_parquet(here('data', 'predicted_prices.parquet' )) |> 
    separate(id , into = c('model', 'make', 'powertrain', 'vehicle_type'),  sep = "_", remove = FALSE) 

bev_names <- predicted_car_prices  |> 
  filter(powertrain != 'cv') |> 
  select(model, make, powertrain) |> 
  distinct()

bev_names$range <- c(149, 114, 269, 170, 57, 258, 253, 33, 53, 26) # vehicle range from internet search

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


vehicle1 <- 'leaf_nissan_bev_car'
vehicle2 <- 'versa sedan_nissan_cv_car'

v1_data <- predicted_car_prices |> 
  filter(id == vehicle1)

v2_data <- predicted_car_prices |> 
  filter(id == vehicle2)

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


for (i in seq(nrow(v1_data)))
  {
    #i = 1
    v1 <- v1_data[i,]
    v2 <- v2_data[i,]
  
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
      model_car_low,
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




  
  placeholder_df |> 
  ggplot(aes(x = age_years, y = bev_choice_probability)) +
  # Main line with improved styling
  geom_line(
    color = "#2E86AB",
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
    limits = c(0.15, 0.50),
    labels = scales::percent_format(accuracy = 1L),
    breaks = seq(0.15, 0.50, by = 0.05),
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
    
    legend.position = "bottom",   # cleaner for papers
    legend.direction = "horizontal",
    
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines")
  ) 

