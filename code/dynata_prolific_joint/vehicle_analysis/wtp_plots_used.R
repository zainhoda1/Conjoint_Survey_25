source(here::here('code', 'setup.R'))

# Load the estimated model
load(here("models", "model_car_low.RData"))
load(here("models", "model_car_high.RData"))
load(here("models", "model_suv_low.RData"))
load(here("models", "model_suv_high.RData"))


model_car_low

model <- model_car_low


get_wtp_draws <- function(model) {
  coefs <- coef(model)
  # Get the model coefficients and covariance matrix
  covariance <- vcov(model)
  
  # Take 10,000 draws of the coefficients
  coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
  
  # Compute WTP for each coefficient draw
  wtp_draws = -1 * (coef_draws[,] / coef_draws[, 'price'])
  
  return(wtp_draws) 
  
}

get_cis <- function(df) {
  
  # Adding dollar values
  wtp_draws <- df %>%
    mutate(
      BEV = powertrainbev * 10^4,
      HEV = powertrainhev * 10^4,
      CV = 0,
      age_year = age * 10^4,
      mileage_10k = mileage * 10^4,
      range_bev = range_bev * 10^4
    )
  
  
  # For each coefficient, get the mean and 95% confidence interval of WTP
  wtp_ci <- ci(wtp_draws, level = 0.95)%>%
    mutate(across(everything(), ~ round(.x, 2)))

  return(wtp_ci)
}


conf_model_car_low <- get_cis(get_wtp_draws(model_car_low))
conf_model_car_high <- get_cis(get_wtp_draws(model_car_high))
conf_model_suv_low <- get_cis(get_wtp_draws(model_suv_low))
conf_model_suv_high <- get_cis(get_wtp_draws(model_suv_high))


predict(model_car_low)




all_models <- c("conf_model_car_low", "conf_model_car_high", "conf_model_suv_low", "conf_model_suv_high")

age_list <- seq(2,8)
mileage_list <- seq (1, 8, 0.5)
range_list <- seq(0.5, 3.5, 0.5)




age_list_df <- data.frame(
  current_age = age_list
) %>% 
  mutate(id = 1)


range_list_df <- data.frame(
  current_range = range_list
) %>% 
  mutate(id = 1)


plot_data <- data.frame(
  model_name = character(),
  vehicle_type = character(),
  mileage_10k = numeric(), # or double()
  age_year = numeric(),
  wtp = numeric(),
  id = numeric(),
  range_bev = numeric(),
  stringsAsFactors = FALSE
)

for (current in all_models)
{
  print(current)
  df <- get(current)
  glimpse(df)
  current_model <- as.data.frame(t(df %>%  
                                     select(mean) )) %>% 
    
    select('BEV', 'HEV', 'CV', 'age_year', 'mileage_10k', 'range_bev') %>% 
    pivot_longer(cols = c('BEV', 'HEV', 'CV'), names_to = 'vehicle_type', values_to = 'wtp') %>% 
    mutate (id = 1,
            model_name = current)
  
  plot_data <- rbind(plot_data, current_model)
}


df <- expand.grid(current_range = range_list, current_age = age_list) %>% 
  mutate (id = 1)

temp <- full_join(plot_data, df, by= 'id')

temp <- temp %>% 
  mutate (wtp_total = wtp +age_year* current_age + range_bev * current_range )





#Final wtp:
#############
temp %>%  
  mutate(
    model_type = case_when(
      model_name %in% c('conf_model_car_low', 'conf_model_car_high') ~ 'car',
      .default = 'suv'
    ),
    segment = case_when(
      model_name %in% c('conf_model_car_low', 'conf_model_suv_low') ~ 'Low range',
      TRUE ~ 'High range'
    )
  ) %>% 
  filter(
    vehicle_type == "BEV",
    current_age %in% c(2, 5, 8),
    (
      (segment == 'Low range' & current_range < 2.5) |
        (segment == 'High range' & current_range > 1.5)
    )
  ) %>% 
  mutate(
    current_age = factor(current_age, levels = c(2, 5, 8))
  ) %>% 
  ggplot(aes(
    x = current_range,
    y = wtp_total,
    group = interaction(current_age, segment),
    color = current_age,
    linetype = segment
  )) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ model_type, ncol = 2) +
 # scale_color_brewer(palette = "Blues", name = "Vehicle age\n(years)") +
  scale_color_manual(
    values = c(
      "2" = "#0072B2",  # blue
      "5" = "#E69F00",  # orange
      "8" = "#009E73"   # green
    ),
    name = "Vehicle age\n(years)"
  )+
  scale_linetype_manual(
    values = c("Low range" = "dashed", "High range" = "solid"),
    name = "Model segment"
  ) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    x = "Driving range (100s of miles)",
    y = "Willingness to pay ($)",
    title = "Willingness to Pay for BEVs by Range and Vehicle Age"
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
  ) +
  
  # Reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40")


ggsave(
  filename = here::here(
    'code',
    'output',
    "images",
    "vehicle_wtp_range.png"    
  ),
  width = 8,
  height = 6,
  dpi = 300
)


#######


plot_data2 <- data.frame(
  model_name = character(),
  vehicle_type = character(),
  mileage_10k = numeric(), # or double()
  age_year = numeric(),
  wtp = numeric(),
  id = numeric(),
  range_bev = numeric(),
  wtp_value_type = character(),
  stringsAsFactors = FALSE
)

for (current in all_models)
{
  print(current)
  df <- get(current)
  glimpse(df)
  current_model <- as.data.frame(t(df %>%  
                                     select(lower, mean, upper) ))
  
  current_model$wtp_value_type = rownames(current_model)
  
  
  current_model <- current_model %>% 
    select('BEV', 'HEV', 'CV', 'age_year', 'mileage_10k', 'range_bev', 'wtp_value_type') %>% 
    pivot_longer(cols = c('BEV', 'HEV', 'CV'), names_to = 'vehicle_type', values_to = 'wtp') %>% 
    mutate (id = 1,
            model_name = current)
  
  plot_data2 <- rbind(plot_data2, current_model)
}


df <- expand.grid(current_range = range_list, current_age = age_list) %>% 
  mutate (id = 1)

temp2 <- full_join(plot_data2, df, by= 'id')

temp2 <- temp2 %>% 
  mutate (wtp_total = wtp +age_year* current_age + range_bev * current_range )

temp3 <-  temp2 |> 
  mutate(
    model_type = case_when(
      model_name %in% c('conf_model_car_low', 'conf_model_car_high') ~ 'car',
      .default = 'suv'
    ),
    segment = case_when(
      model_name %in% c('conf_model_car_low', 'conf_model_suv_low') ~ 'Low range',
      TRUE ~ 'High range'
    )
  ) %>% 
  filter(
    vehicle_type == "BEV",
    current_age %in% c(2, 5, 8),
    (
      (segment == 'Low range' & current_range < 2.5) |
        (segment == 'High range' & current_range > 1.5)
    )
  ) %>% 
  mutate(
    current_age = factor(current_age, levels = c(2, 5, 8))
  ) %>% 
    select(model_name, model_type, current_range, current_age, wtp_total, wtp_value_type, segment )



temp4 <- temp3 |> 
  pivot_wider(names_from = wtp_value_type , values_from = wtp_total)



############

# Final Plot for WTP with confidence Interval

temp4 |> 
  ggplot(aes(
    x = current_range,
    y = mean,
    color = current_age,
    fill = current_age,
    linetype = segment
  )) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.25,
    color = NA
  ) +
  geom_line(linewidth = 1.1) +
  facet_grid(current_age ~ model_type) +
  #facet_wrap(~model_type) +
  scale_color_manual(
    values = c(
      "2" = "#E63946",    # Red
      "5" = "#06A77D",    # Green
      "8" = "#4A90E2"     # Blue
    ),
    name = "Current age"
  ) +
  scale_fill_manual(
    values = c(
      "2" = "#E63946",
      "5" = "#06A77D",
      "8" = "#4A90E2"
    ),
    name = "Current age",
    guide = "none"  # Hide fill legend, keep only color
  ) +
  labs(
    title = "Estimated WTP for Vehicle Range and Age",
    subtitle = "(95% Confidence Intervals)",
    x = "Vehicle range at year 0 (miles)",
    y = "WTP ($)"
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
  ) +
  
  # Reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40")


ggsave(
  filename = here::here(
    'code',
    'output',
    "images",
    "vehicle_analysis",
    "wtp_range_with_coef_int.png"    
  ),
  width = 8,
  height = 6,
  dpi = 300
)



library(scales)

temp4 |> 
  mutate(segment = case_when(
    segment == 'Low range' ~ 'Low Budget',
    .default = 'High Budget'),
    model_type = case_when (
      model_type == 'car'  ~ 'CAR',
      .default = 'SUV'
    )
) |> 
  ggplot(aes(
    x     = current_range,
    y     = mean,
    color = current_age,
    fill  = current_age,
    linetype = segment
  )) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 0.9) +
  facet_grid(
    current_age ~ model_type,
    scales   = "free_y",
    labeller = labeller(
      current_age = c("2" = "Age 2", "5" = "Age 5", "8" = "Age 8")
    )
  ) +
  scale_color_manual(
    values = c("2" = "#C0392B", "5" = "#1A7A5E", "8" = "#2E6DA4"),
    guide  = "none"   
    # name   = "Current age",
    # labels = c("2" = "2 years", "5" = "5 years", "8" = "8 years")
  ) +
  scale_fill_manual(
    values = c("2" = "#C0392B", "5" = "#1A7A5E", "8" = "#2E6DA4"),
    guide  = "none"
  ) +
  scale_linetype_manual(
    values = c("High Budget" = "solid", "Low Budget" = "dashed"),
    name   = "Market Segment"
  ) +
  scale_y_continuous(
    labels = label_dollar(scale = 1e-3, suffix = "k", accuracy = 1)
  ) +
  scale_x_continuous(breaks = 1:4, labels= c("100", "200", "300", "400")) +

  labs(
    title    = "Estimated WTP for Vehicle Range and Age",
    subtitle = "95% confidence intervals shown as shaded bands",
    x        = "Vehicle range (miles)",
    y        = "Willingness to pay (USD)"
  ) +
  theme_bw(base_size = 12, base_family = "sans") +
  theme(
    # Background
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(color = "grey80", fill = NA, linewidth = 0.5),

    # Grid — keep only major horizontal
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.35),

    # Titles
    plot.title    = element_text(face = "bold", size = 13, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 8)),
    plot.caption  = element_text(size = 8, color = "grey50", hjust = 0),

    # Axes
    axis.title   = element_text(size = 10, face = "bold"),
    axis.text    = element_text(size = 9, color = "grey30"),
    axis.ticks   = element_line(color = "grey70", linewidth = 0.3),

    # Facet strips
    strip.text       = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "grey95", color = "grey80", linewidth = 0.5),

    # Legend
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 9),
    legend.key.width = unit(1.8, "cm"),
    legend.spacing.x = unit(0.6, "cm"),

    # Margins
    plot.margin = margin(10, 14, 8, 10)
  )



ggsave(
  filename = here::here(
    'code',
    'output',
    "images",
    "vehicle_analysis",
    "wtp_range_with_coef_int.png"    
  ),
  width = 8,
  height = 6,
  dpi = 300
)
