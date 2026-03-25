source(here::here('code', 'setup.R'))

# Load the estimated model
load(here("models", "model_car_low.RData"))
load(here("models", "model_car_high.RData"))
load(here("models", "model_suv_low.RData"))
load(here("models", "model_suv_high.RData"))



conf_model_car_low <- create_confidence_intervals(model_car_low)
conf_model_car_high <- create_confidence_intervals(model_car_high)
conf_model_suv_low <- create_confidence_intervals(model_suv_low)
conf_model_suv_high <- create_confidence_intervals(model_suv_high)


all_models <- c("conf_model_car_low", "conf_model_car_high", "conf_model_suv_low", "conf_model_suv_high")

age_list <- seq(1,8)
mileage_list <- seq (1, 8, 1)

plot_data <- data.frame(
  model_name = character(),
  bev_type = character(),
  mileage_10k = numeric(), # or double()
  age_year = numeric(),
  wtp_bev = numeric(),
  id = numeric(),
  stringsAsFactors = FALSE
)

for (current in all_models)
  {
    print(current)
    df <- get(current)
    glimpse(df)
    current_model <- as.data.frame(t(df %>%  
      select(mean) )) %>% 
      select('BEV100', 'BEV200', 'BEV300', 'age_year', 'mileage_10k') %>% 
      pivot_longer(cols = c('BEV100', 'BEV200', 'BEV300'), names_to = 'bev_type', values_to = 'neg_bev') %>% 
      mutate (id = 1,
              model_name = current)
    
    plot_data <- rbind(plot_data, current_model)
  }


df <- expand.grid(current_age = age_list, current_mileage = mileage_list) %>% 
  mutate (id = 1)

temp <- full_join(plot_data, df, by= 'id')

temp <- temp %>%  
  mutate(
    WTP_BEV =  neg_bev + (current_age * age_year) + (current_mileage * mileage_10k ),
    vehicle_type = case_when(
      str_detect(model_name, 'car') ~ 'car',
      .default = 'suv')
      )

# Low Budget 
####################################
 
 low_budget_to_use <- temp %>%
   filter(
     !model_name %in% c('conf_model_car_high', 'conf_model_suv_high'),  
     bev_type %in% c('BEV100', 'BEV200')  
          )
 
 wtp_min <- min(low_budget_to_use$WTP_BEV)
 wtp_max <- max(low_budget_to_use$WTP_BEV)

 breaks = seq(wtp_min, wtp_max, length.out = 5)
  
 low_budget_to_use %>%
   ggplot(aes(current_age, current_mileage, fill = WTP_BEV)) +
   facet_grid(bev_type ~ vehicle_type) +
   geom_tile(color = "white", linewidth = 0.3) +
   scale_fill_viridis_c(
     option  = "plasma",           # try "viridis", "magma", "inferno"
     name    = "WTP (USD)",
     limits = c(wtp_min, wtp_max),
     breaks = unique(c(wtp_min, breaks, wtp_max)) ,
     labels = scales::label_dollar(scale = 1e-3, suffix = "k", accuracy = 0.1),
     guide   = guide_colorbar(
       barwidth  = 30,
       barheight = 0.8,
       title.position = "top"
     )
   ) +
   scale_x_continuous(breaks = 1:length(age_list), expand = c(0, 0)) +
   scale_y_continuous(breaks = seq(1, length(mileage_list), 1), expand = c(0, 0)) +
   labs(
     x    = "Vehicle age (years)",
     y    = "Annual mileage (10k miles)",
     fill = "WTP for BEV (USD)",
     title = 'Low budget models (Vehicle Range 100-200 miles)'  
   ) +
   theme_minimal(base_size = 11) +
   theme(
     plot.background  = element_rect(fill = "white", color = NA),
     panel.background = element_rect(fill = "white", color = NA),
     panel.grid   = element_blank(),
     strip.text   = element_text(face = "bold", size = 10),
     legend.position    = "bottom",
     legend.title.align = 0.5,
     axis.ticks   = element_line(linewidth = 0.3)
   )
  

 ggsave(
   filename = here::here(
     'code',
     'output',
     "images",
     "vehicle_wtp_low_budget.png"   
   ),
   width = 8,
   height = 6,
   dpi = 300
 )


####################
# High Budget  
#######################################
 
 
 high_budget_to_use <- temp %>%
   filter(
     model_name %in% c('conf_model_car_high', 'conf_model_suv_high'),   
     bev_type %in% c('BEV200', 'BEV300')  
   )
 
 wtp_min <- min(high_budget_to_use$WTP_BEV)
 wtp_max <- max(high_budget_to_use$WTP_BEV)
 

 breaks = seq(wtp_min, wtp_max, length.out = 5)
 
 high_budget_to_use %>%
   ggplot(aes(current_age, current_mileage, fill = WTP_BEV)) +
   facet_grid(bev_type ~ vehicle_type) +
   geom_tile(color = "white", linewidth = 0.3) +
   scale_fill_viridis_c(
     option  = "plasma",           
     name    = "WTP (USD)",
     limits = c(wtp_min, wtp_max),
     breaks = unique(c(wtp_min, breaks, wtp_max)) ,
     labels = scales::label_dollar(scale = 1e-3, suffix = "k", accuracy = 0.1),
     guide   = guide_colorbar(
       barwidth  = 30,
       barheight = 0.8,
       title.position = "top"
     )
   ) +
   scale_x_continuous(breaks = 1:length(age_list), expand = c(0, 0)) +
   scale_y_continuous(breaks = seq(1, length(mileage_list), 1), expand = c(0, 0)) +
   labs(
     x    = "Vehicle age (years)",
     y    = "Annual mileage (10k miles)",
     fill = "WTP for BEV (USD)",
     title = 'High budget models (Vehicle Range 200-300 miles)'  
   ) +
   theme_minimal(base_size = 11) +
   theme(
     plot.background  = element_rect(fill = "white", color = NA),
     panel.background = element_rect(fill = "white", color = NA),
     panel.grid   = element_blank(),
     strip.text   = element_text(face = "bold", size = 10),
     legend.position    = "bottom",
     legend.title.align = 0.5,
     axis.ticks   = element_line(linewidth = 0.3)
   )
 
 
 ggsave(
   filename = here::here(
     'code',
     'output',
     "images",
     "vehicle_wtp_high_budget.png"    
   ),
   width = 8,
   height = 6,
   dpi = 300
 )
 
#####################################
 
 
 