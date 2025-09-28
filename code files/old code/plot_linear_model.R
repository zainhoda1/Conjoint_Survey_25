# Visualize results of estimated multinomial logit (mnl) models

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)

# Load the data set:

pathToData <- here('data', "generated_data.csv")
data <- read_csv(pathToData)
head(data)



# Load estimated models
load(here("models", "model_linear.RData"))

# -----------------------------------------------------------------------------
# Some tips for working with model objects

# If you want to get the resulting model parameters, use the coef() function
coef(model_linear)

# If you want the standard errors, use se()
se(model_linear)

# If you want to get the full summary table of the model coefficients
# as a data frame, use coef(summary(model))
coef(summary(model_linear))

# -----------------------------------------------------------------------------
# Plot results

#######################
# profiles_used <- cbc_profiles(
#   powertrain     = c('Gasoline', 'Electric', 'Plug-in Hybrid', 'Hybrid'),
#   price          = seq(0.8, 1.1, 0.1),
#   range          = seq(50, 250, 25),
#   mileage        = seq(20000, 60000, 5000),
#   my             = seq(2015, 2023),
#   operating_cost = seq(3, 21, 3)
# )
############

# Get the estimated coefficients
coefs <- coef(model_linear)

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price <- data.frame(level = unique(data$price)) %>%
  mutate(
    diff    = level - min(level),
    utility = diff*coefs['price'])

df_range <- data.frame(level = c(50, 75, 100, 125, 150, 175, 200, 225, 250)) %>%
  mutate(
    diff    = level - min(level),
    utility = diff*coefs['range'])

df_mileage <- data.frame(level = c(20000, 25000, 30000, 35000, 40000, 45000, 50000, 55000, 60000)) %>%
  mutate(
    diff    = level - min(level),
    utility = diff*coefs['mileage'])

df_powertrain = data.frame(level = c("Gasoline", "Electric")) %>%
  mutate(utility = c(0, coefs['powertrain_electric']))

# Get upper and lower bounds (plots should have the same y-axis)
utility <- c(
  df_price$utility, df_range$utility,
  df_mileage$utility, df_powertrain$utility)
ymin <- floor(min(utility))
ymax <- ceiling(max(utility))

# Plot the utility for each attribute
plot_price <- df_price %>%
  ggplot() +
  geom_line(aes(x = level, y = utility)) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(x = 'Price ($1000)', y = 'Utility') +
  theme_bw()

plot_range <- df_range %>%
  ggplot() +
  geom_line(aes(x = level, y = utility)) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(x = 'Range', y = 'Utility') +
  theme_bw()

plot_mileage <- df_mileage %>%
  ggplot() +
  geom_line(aes(x = level, y = utility)) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(x = 'mileage', y = 'Utility') +
  theme_bw()

plot_powertrain <- df_powertrain %>%
  ggplot() +
  geom_point(aes(x = level, y = utility)) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(x = 'Powertrain', y = 'Utility') +
  theme_bw()

# Plot all plots in one figure
plot_model_linear <- plot_grid(
  plot_price, plot_range, plot_mileage, plot_powertrain,
  nrow = 1
)

# Save plots
ggsave(
  filename = here('figs', 'model_linear.png'),
  plot = plot_model_linear,
  width = 10, height = 2.3
)
