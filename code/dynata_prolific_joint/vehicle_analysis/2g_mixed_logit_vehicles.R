# After running both Dynata and Prolific
source(here::here('code', 'setup.R'))

# --------------------------------------------------------------------------

load(here("models", "mixed_model_1_car.RData"))
load(here("models", "mixed_model_1_suv.RData"))
load(here("models", "mixed_model_1_car_low.RData"))
load(here("models", "mixed_model_1_car_high.RData"))
load(here("models", "mixed_model_1_suv_low.RData"))
load(here("models", "mixed_model_1_suv_high.RData"))

load(here("models", "mixed_model_1_likely_bev_adopter.RData"))
load(here("models", "mixed_model_1_likely_bev_adopter_car.RData"))
load(here("models", "mixed_model_1_likely_bev_adopter_suv.RData"))


####################

summary(mixed_model_1_car)
summary(mixed_model_1_suv)
summary(mixed_model_1_car_low)
summary(mixed_model_1_car_high)
summary(mixed_model_1_suv_low)
summary(mixed_model_1_suv_high)


summary(mixed_model_1_likely_bev_adopter)
summary(mixed_model_1_likely_bev_adopter_car)
summary(mixed_model_1_likely_bev_adopter_suv)



