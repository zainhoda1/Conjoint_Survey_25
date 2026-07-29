# After running both Dynata and Prolific
source(here::here('code', 'setup.R'))

# --------------------------------------------------------------------------

load(here("models", "mixed_model_1_car_panel.RData"))
load(here("models", "mixed_model_1_suv_panel.RData"))
load(here("models", "mixed_model_1_car_low_panel.RData"))
load(here("models", "mixed_model_1_car_high_panel.RData"))
load(here("models", "mixed_model_1_suv_low_panel.RData"))
load(here("models", "mixed_model_1_suv_high_panel.RData"))


load(here("models", "mixed_model_1_unlikely_bev_adopter_panel.RData"))
load(here("models", "mixed_model_1_unlikely_bev_adopter_car_panel.RData"))
load(here("models", "mixed_model_1_unlikely_bev_adopter_suv_panel.RData"))
load(here("models", "mixed_model_1_likely_bev_adopter_panel.RData"))
load(here("models", "mixed_model_1_likely_bev_adopter_car_panel.RData"))
load(here("models", "mixed_model_1_likely_bev_adopter_suv_panel.RData"))


####################

summary(mixed_model_1_car_panel)
summary(mixed_model_1_suv_panel)

summary(mixed_model_1_car_low_panel)
summary(mixed_model_1_car_high_panel)
summary(mixed_model_1_suv_low_panel)
summary(mixed_model_1_suv_high_panel)


summary(mixed_model_1_likely_bev_adopter_panel)
summary(mixed_model_1_unlikely_bev_adopter_panel)
summary(mixed_model_1_likely_bev_adopter_car_panel)
summary(mixed_model_1_unlikely_bev_adopter_car_panel)
summary(mixed_model_1_likely_bev_adopter_suv_panel)
summary(mixed_model_1_unlikely_bev_adopter_suv_panel)



