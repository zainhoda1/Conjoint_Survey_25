# Visualize results of estimated WTP space model

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)

# -----------------------------------------------------------------------------
# Get WTP estimates with 95% CI

# Method 1: Computed WTP from preference space model:
load(here("models", "model_linear.RData"))# Load pref space model
coefs <- coef(model_linear)
covariance <- vcov(model_linear)
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
wtp_draws = -1*(coef_draws[,c(2,3,6)] / coef_draws[,1])
wtp_ci1 <- ci(wtp_draws, level = 0.95)
wtp_ci1

# # Method 2: Estimate WTP in WTP space model:
# load(here("models", "model_linear.RData")) # Load estimated models
# coefs <- coef(model_wtp)
# covariance <- vcov(model_wtp)
# wtp_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
# wtp_ci2 <- ci(wtp_draws, level = 0.95)
# wtp_ci2 <- wtp_ci2[-1,] # Drop lambda (we won't plot this)
# wtp_ci2

# -----------------------------------------------------------------------------
# Plot results

wtp_ci <- wtp_ci1

# Separate coefficient CIs by attribute
wtp_ci$par <- row.names(wtp_ci)
wtp_range <- wtp_ci %>% filter(par == 'range')
wtp_mileage <- wtp_ci %>% filter(par == 'mileage')
wtp_powertrain_electric <- wtp_ci %>% filter(par == 'powertrain_electric')

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_range <- data.frame(level = c(50, 75, 100, 125, 150, 175, 200, 225, 250)) %>%
  mutate(
    diff  = level - min(level),
    mean  = diff*wtp_range$mean,
    lower = diff*wtp_range$lower,
    upper = diff*wtp_range$upper)

df_mileage <- data.frame(level = c(20000, 25000, 30000, 35000, 40000, 45000, 50000, 55000, 60000)) %>%
  mutate(
    diff  = level - min(level),
    mean  = diff*wtp_mileage$mean,
    lower = diff*wtp_mileage$lower,
    upper = diff*wtp_mileage$upper)

df_powertrain_electric <- data.frame(level = c("Gasoline", "Electric")) %>%
  mutate(
    mean  = c(0, wtp_powertrain_electric$mean),
    lower = c(0, wtp_powertrain_electric$lower),
    upper = c(0, wtp_powertrain_electric$upper))

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(c(
  df_range$lower,
  df_mileage$lower, df_powertrain_electric$lower)))
ymax <- ceiling(max(c(
  df_range$upper,
  df_mileage$upper, df_powertrain_electric$upper)))

# Plot the WTP for each attribute *with 95% CI*
plot_range <- df_range %>%
  ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(x = 'Range', y = 'WTP ($1,000)') +
  theme_bw()

plot_mileage <- df_mileage %>%
  ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(x = 'mileage', y = 'WTP ($1,000)') +
  theme_bw()

plot_powertrain_electric <- df_powertrain_electric %>%
  ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
  geom_point() +
  geom_errorbar(width = 0.3) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(x = 'Powertrain', y = 'WTP ($1,000)') +
  theme_bw()

# Plot all plots in one figure
plot_mnl_wtp <- plot_grid(
  plot_range, plot_mileage, plot_powertrain_electric,
  nrow = 1
)

# Save plots
ggsave(
  filename = here('figs', 'mnl_wtp.png'),
  plot = plot_mnl_wtp,
  width = 8, height = 2.3
)

# -----------------------------------------------------------------------------
# Compare WTP for changes in all attributes

# WTP for:
# 10 mpg improvement in fuel economy
# 3 sec improvement in acceleration time
# Gasoline vs Electric powertrain

df_compare <- wtp_ci
cols <- c('mean', 'lower', 'upper')
df_compare[1, cols] <- df_compare[1, cols]*10 # Fuel Economy
df_compare[2, cols] <- df_compare[2, cols]*10  # Acceleration Time
df_compare$label <- c(
  "range", "mileage", "Powertrain:\nElectric over Gasoline"
)

barplot_mnl_wtp <- df_compare %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper)) +
  geom_col(width = 0.5, fill = 'gray') +
  geom_errorbar(width = 0.3) +
  labs(x = 'WTP ($1,000)', y = 'Attribute') +
  theme_bw()

# Save plots
ggsave(
  filename = here('figs', 'mnl_wtp_barplot.png'),
  plot = barplot_mnl_wtp,
  width = 5, height = 3
)

