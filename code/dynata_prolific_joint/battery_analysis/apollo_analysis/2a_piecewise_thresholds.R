source(here::here('code', 'setup.R'))

data_doe <- read_parquet(here(
  "data",
  "doe",
  "02-06-26",
  "design_battery.parquet"
))

data_doe <- data_doe %>%
  mutate(
    battery_range_loss = (battery_range_year3 - battery_range_year8) /
      battery_range_year3 *
      100
  )

# histogram of battery range years3
ggplot(
  data_doe %>% filter(!is.na(battery_range_year3)),
  aes(x = battery_range_year3)
) +
  geom_histogram(binwidth = 5, fill = "#0170d8ff", color = "white") +
  labs(
    title = "Distribution of Battery Range (Year 3)",
    x = "Battery Range (year 3)",
    y = "Frequency"
  ) +
  theme_minimal()

quantile(
  data_doe$battery_range_year3,
  probs = c(0, 0.25, 0.33, 0.5, 0.66, 0.75, 1),
  na.rm = TRUE
)


ggplot(
  data_doe %>% filter(!is.na(battery_range_loss)),
  aes(x = battery_range_loss)
) +
  geom_histogram(binwidth = 1, fill = "#0170d8ff", color = "white") +
  labs(
    title = "Distribution of Battery Range Loss",
    x = "Battery Range Loss (%)",
    y = "Frequency"
  ) +
  theme_minimal()

round(
  quantile(
    data_doe$battery_range_loss[data_doe$battery_range_loss != 0],
    probs = c(0, 0.25, 0.33, 0.5, 0.66, 0.75, 1),
    na.rm = TRUE
  ),
  1
)
