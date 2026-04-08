# Load functions, libraries, and other settings
source(here::here("code", "setup.R"))

library(fixest)

# Read in data

all_vehicles <-  read_parquet(here(
  "data",
  "vehicle_listing_prices.parquet"
))


all_vehicles$id <- paste0(all_vehicles$model, '_' ,
 all_vehicles$make, '_', all_vehicles$powertrain, '_', all_vehicles$vehicle_type)

age_list = seq(2, 6)

df <- data.frame(
  age_years = age_list,
  miles = age_list * 10000
)

predictions <- data.frame(
  age_years = integer(),  
  miles = integer(),
  id = character()
)


run_model <- function(current_id, formula) {
  data <- all_vehicles %>%
    filter(id == current_id)
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




ids <- unique(all_vehicles$id)

  # data <- all_vehicles %>%
  #   filter(id == 'versa sedan_nissan_cv')

for (i in ids){
  #i = 'versa sedan_nissan_cv'
    print(i)
  
    model_used_vehicle <- run_model(
      current_id = i,
      formula = log(price) ~
        miles +
        age_years 
  )

  df$predicted_price <-round(exp(predict(model_used_vehicle, newdata = df)), 2)

  df <- df |>  
    mutate(id = i)

  predictions <- rbind(predictions, df)
    
}

write_parquet( predictions,here('data', 'predicted_prices.parquet'))
