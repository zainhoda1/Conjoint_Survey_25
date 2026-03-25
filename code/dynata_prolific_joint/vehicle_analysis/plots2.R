source(here::here('code', 'setup.R'))

# Load the estimated model
load(here("models", "model_car_low.RData"))
load(here("models", "model_car_high.RData"))
load(here("models", "model_suv_low.RData"))
load(here("models", "model_suv_high.RData"))


model_car_low

model <- model_car_low


conf_interval_func <- function(model) {
  coefs <- coef(model)
  # Get the model coefficients and covariance matrix
  covariance <- vcov(model)
  
  # Take 10,000 draws of the coefficients
  coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
  
  # Compute WTP for each coefficient draw
  wtp_draws = -1 * (coef_draws[,] / coef_draws[, 'price'])
  
  # Adding dollar values
  wtp_draws <- wtp_draws %>%
    mutate(
      BEV = powertrainbev * 10^4,
      HEV = powertrainhev * 10^4,
      CV = 0,
      age_year = age * 10^4,
      mileage_10k = mileage * 10^4,
      Oper_cost_up_10cents = operating_cost * 10^4,
      range_bev = range_bev * 10^4
    )
  
  # For each coefficient, get the mean and 95% confidence interval of WTP
  wtp_ci <- ci(wtp_draws, level = 0.95)%>%
    mutate(across(everything(), ~ round(.x, 2)))

  return(wtp_ci)
}


conf_model_car_low <- conf_interval_func(model_car_low)
conf_model_car_high <- conf_interval_func(model_car_high)
conf_model_suv_low <- conf_interval_func(model_suv_low)
conf_model_suv_high <- conf_interval_func(model_suv_high)


all_models <- c("conf_model_car_low", "conf_model_car_high", "conf_model_suv_low", "conf_model_suv_high")

age_list <- seq(2,8)
mileage_list <- seq (1, 8, 0.5)
range_list <- seq(0.5, 3.5, 0.5)

age_list_df <- data.frame(
  current_age = age_list
) %>% 
  mutate(id = 1)

mileage_list_df <- data.frame(
  current_mileage = mileage_list
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


#df_try <- expand.grid( data = plot_data,current_age = age_list )

#df <- expand.grid(current_age = age_list, current_mileage = mileage_list) %>% 
  #mutate (id = 1)

age_data <- full_join(plot_data, age_list_df, by= 'id')

age_data <- age_data %>%  
  mutate(
    WTP_VEHICLE =  wtp + (current_age * age_year) ,
    vehicle_category = case_when(
      str_detect(model_name, 'car') ~ 'car',
      .default = 'suv')
  )


age_data %>% 
  filter(model_name == 'conf_model_car_low') %>% 
  ggplot( aes(x=current_age, y=WTP_VEHICLE , group=vehicle_type, color=vehicle_type)) +
  geom_line()


########################

mileage_data <- full_join(plot_data, mileage_list_df, by= 'id')

mileage_data <- mileage_data %>%  
  mutate(
    WTP_VEHICLE =  wtp + (current_mileage * mileage_10k) ,
    vehicle_category = case_when(
      str_detect(model_name, 'car') ~ 'car',
      .default = 'suv')
  )


mileage_data %>% 
  filter(model_name == 'conf_model_car_low') %>% 
  ggplot( aes(x=current_mileage, y=WTP_VEHICLE , group=vehicle_type, color=vehicle_type)) +
  geom_line()


###########################

range_data <- full_join(plot_data, range_list_df, by= 'id')

range_data <- range_data %>%  
  mutate(
    WTP_VEHICLE =  case_when(
      vehicle_type == 'BEV' ~  wtp + (current_range * range_bev) ,
      .default = wtp
    ),
    vehicle_category = case_when(
      str_detect(model_name, 'car') ~ 'car',
      .default = 'suv')
  )


range_data %>% 
  filter(model_name == 'conf_model_car_high') %>% 
  ggplot( aes(x=current_range, y=WTP_VEHICLE , group=vehicle_type, color=vehicle_type)) +
  geom_line()


###########################

