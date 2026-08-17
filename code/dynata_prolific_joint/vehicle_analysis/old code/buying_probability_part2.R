# This script calculates the predicted prices.

# Load functions, libraries, and other settings
source(here::here("code", "setup.R"))

library(fixest)

# Read in data

all_vehicles <-  read_parquet(here(
  "data",
  "vehicle_listing_prices.parquet"
))


vehicles_comparsion_list <- read_csv(here('data', 'vehicles_comparison_list.csv'))


all_vehicles$id <- paste0(all_vehicles$model, '_' ,
 all_vehicles$make, '_', all_vehicles$powertrain, '_', all_vehicles$vehicle_type)


temp <- all_vehicles |> 
  group_by(id, year) |> 
  summarise(total_cars = n(), .groups = "drop")


vehicle_ages <- all_vehicles |> 
  group_by(id) |> 
  summarise(
    earliest_year = min(year),
    latest_year = max(year),  
    earliest_age = min(age_years),
    latest_age = max(age_years),  
    total_cars = n(),
     .groups = "drop") 

vehicle_ages <- vehicle_ages |> 
  mutate(
    powertrain = case_when(
      str_detect(id, "_bev_") ~ "BEV",
      str_detect(id, "_phev_") ~ "PHEV",
      str_detect(id, "_hev_") ~ "HEV",
      TRUE ~ "CV"
    ),
    starting_year = earliest_year - round(earliest_age),
    no_years = (latest_year - starting_year) 
  ) 


vehicle_joint <- left_join(vehicles_comparsion_list,vehicle_ages , 
  by = c( 'bev_vehicle'='id') ) |>  
  select(id, bev_vehicle, other_vehicle, starting_year,
     latest_year, no_years, bev_range, other_vehicle_range )

vehicle_joint <- vehicle_joint |> 
  pivot_longer(cols = c('bev_vehicle', 'other_vehicle'),
 names_to = 'vehicle_type', values_to = 'vehicle_names')


# vehicle_ages_bev <- vehicle_ages |> 
#   filter(powertrain == 'BEV')

# vehicle_ages_rest <- vehicle_ages |> 
#   filter(powertrain != 'BEV')

# vehicle_joint <- left_join(vehicle_ages_bev, vehicle_ages_rest, by ='matching_id' )

# vehicle_joint <-  vehicle_joint |> 
#   mutate( use_earliest_year  = earliest_year.x,
#           use_latest_year  = latest_year.x
#   )

# vehicle_joint <- vehicle_joint %>%
#   select(id.x, id.y, use_earliest_year, use_latest_year) %>%
#   pivot_longer(cols = c(id.x, id.y), values_to = "id") %>%
#   distinct(id, .keep_all = TRUE)  

#vehicle_joint$no_years <- vehicle_joint$use_latest_year - vehicle_joint$use_earliest_year

#write_parquet( vehicle_ages,here('data', 'vehicle_ages.parquet'))

age_list = seq(0, 8)

df <- data.frame(
  age_years = age_list,
  miles = age_list * 10000
)

predictions <- data.frame(
  age_years = integer(),  
  miles = integer(),
  id = character()
)


run_model <- function(current_id, earliest_year, latest_year,  formula) {
  data <- all_vehicles %>%
    filter(id == current_id) |> 
    filter(year >=  earliest_year & year  <= latest_year)
    

  if (nrow(data) > 0) {
    model <- feols(fml = formula, data = data)
    return(model)
  }
  return(NULL)
}

# make_predictions <- function(model, pred_data) {
#   pred_vars <- names(pred_data)
#   if (is.null(model)) {
#     result <- data.frame(
#       estimate = rep(NA, nrow(pred_data))
#     )
#   } else {
#     result <- data.frame(
#       estimate = exp(predict(model, newdata = pred_data))
#     )
#   }
#   result[pred_vars] <- pred_data
#   return(result)
# }




ids <- unique(vehicle_joint$vehicle_names)

  # data <- all_vehicles %>%
  #   filter(id == 'versa sedan_nissan_cv')

for (i in ids){

  #i = 'versa sedan_nissan_cv'
  no_years =  vehicle_joint$no_years[vehicle_joint$vehicle_names == i]
  earliest_year =  vehicle_joint$starting_year[vehicle_joint$vehicle_names == i]
  latest_year =  vehicle_joint$latest_year[vehicle_joint$vehicle_names == i]
  print(paste0(i, '///',   no_years))

    model_used_vehicle <- run_model(
      current_id = i,
      earliest_year =  earliest_year,
      latest_year =  latest_year,
      formula = log(price) ~
        miles +
        age_years 
  )

  df_updated <- df |> filter(age_years <= no_years)

  df_updated$predicted_price <-round(exp(predict(model_used_vehicle,
     newdata = df_updated )), 2)

  df_updated <- df_updated |> 
    mutate(id = i)

  print(df_updated)
  predictions <- rbind(predictions, df_updated)
    
}

  write_parquet( predictions,here('data', 'predicted_prices.parquet'))
