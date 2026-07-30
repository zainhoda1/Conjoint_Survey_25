# After running both Dynata and Prolific
source(here::here('code', 'setup.R'))

# --------------------------------------------------------------------------

load(here("models", "mixed_model_1_car_panel.RData"))
load(here("models", "mixed_model_1_suv_panel.RData"))
load(here("models", "mixed_model_1_car_low_panel.RData"))
load(here("models", "mixed_model_1_car_high_panel.RData"))
load(here("models", "mixed_model_1_suv_low_panel.RData"))
load(here("models", "mixed_model_1_suv_high_panel.RData"))


#load(here("models", "mixed_model_1_unlikely_bev_adopter.RData"))
load(here("models", "mixed_model_1_unlikely_bev_adopter_car.RData"))
load(here("models", "mixed_model_1_unlikely_bev_adopter_suv.RData"))
#load(here("models", "mixed_model_1_likely_bev_adopter.RData"))
load(here("models", "mixed_model_1_likely_bev_adopter_car.RData"))
load(here("models", "mixed_model_1_likely_bev_adopter_suv.RData"))


####################

summary(mixed_model_1_car_panel)
summary(mixed_model_1_suv_panel)

summary(mixed_model_1_car_low_panel)
summary(mixed_model_1_car_high_panel)
summary(mixed_model_1_suv_low_panel)
summary(mixed_model_1_suv_high_panel)


#summary(mixed_model_1_likely_bev_adopter)
#summary(mixed_model_1_unlikely_bev_adopter)
summary(mixed_model_1_likely_bev_adopter_car)
summary(mixed_model_1_unlikely_bev_adopter_car)
summary(mixed_model_1_likely_bev_adopter_suv)
summary(mixed_model_1_unlikely_bev_adopter_suv)



