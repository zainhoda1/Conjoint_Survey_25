# This script calculates the predicted prices.

# Load functions, libraries, and other settings
source(here::here("code", "setup.R"))

library(fixest)

# Read in data

all_vehicles <-  read_parquet(here(
  "data",
  "vehicle_listing_prices.parquet"
))

all_vehicles_2 <- all_vehicles |> 
  filter(age_years >=2.5 & age_years <3.5) |> 
  collect()



vehicles_comparsion_list <- read_csv(here('data', 'vehicle_pairs_2016_2024.csv')) |> 
  mutate(across(where(is.character), tolower))


vehicles_comparsion_list$id <- paste0(vehicles_comparsion_list$make,'_',
  vehicles_comparsion_list$model,'_',
  vehicles_comparsion_list$powertrain, '_',
  vehicles_comparsion_list$vehicle_type
)

all_vehicles$id <- paste0(all_vehicles$make, '_' ,
 all_vehicles$model, '_', all_vehicles$powertrain, '_', all_vehicles$vehicle_type)



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
    no_years = (latest_age - earliest_age) 
  ) 


vehicle_joint <- inner_join(vehicles_comparsion_list,vehicle_ages , 
  by = c( 'id') ) |>  
  select(id,  earliest_age,  latest_age, earliest_year,
     latest_year, no_years, total_cars, Pair_id )



age_list = seq(0, 9)

df <- data.frame(
  age_years = age_list,
  miles = age_list * 10000
)

predictions <- data.frame(
  age_years = integer(),  
  miles = integer(),
  id = character()
)


run_model <- function(current_id,   formula) { 
  data <- all_vehicles %>%
    filter(id == current_id)
    

  if (nrow(data) > 0) {
    model <- feols(fml = formula, data = data)
    return(model)
  }
  return(NULL)
}



ids <- unique(vehicle_joint$id)


for (i in ids){

  #i = 'kia_soul_cv_car'
  no_years =  round(vehicle_joint$no_years[vehicle_joint$id == i], 0)
  print(paste0(i, '///',   no_years))

    model_used_vehicle <- run_model(
      current_id = i,
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

