source(here::here('code', 'setup.R'))
source(here::here('code', 'prolific_testing', 'approval_functions.R'))
library(patchwork)

data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint.parquet"
))


# Some special variables:
# session_id = a unique ID for the Run - should be the same across all surveys
# time_start = time stamp when survey was started
# time_end   = time stamp when survey ended
# time_p_*** = Time page *** was reached
# time_q_*** = Time question *** was last answered

data <- data_joint %>%
  select(
    starts_with('time_q_vehicle_cbc'),
    starts_with('time_q_battery_cbc'),
    starts_with('time_p_vehicle_page'),
    starts_with('time_p_battery_page'),
    time_p_cbc_intro,
    time_p_cbc_demo,
    time_p_battery_cbc_intro,
    time_p_battery_cbc_demo,
    time_start,
    time_end,
    session_id,
    prolific_pid,
    prolific_session_id,
    respID,
    completion_code,
    battery_respID,
    # current_page,
    next_veh_budget,
    # attention_check_toyota,
    next_veh_style,
    data_source
  )


# Compute time values for each page
data <- data %>%
  mutate(
    # Compute time through whole survey
    time_start = ymd_hms(time_start, tz = "UTC"),
    time_end = ymd_hms(time_end, tz = "UTC"),
    time_total = as.numeric(time_end - time_start, units = "secs"),
    # Compute time
    time_p_cbc_intro = ymd_hms(time_p_cbc_intro, tz = "UTC"),
    time_p_cbc_demo = ymd_hms(time_p_cbc_demo, tz = "UTC"),
    time_p_battery_cbc_intro = ymd_hms(time_p_battery_cbc_intro, tz = "UTC"),
    time_p_battery_cbc_demo = ymd_hms(time_p_battery_cbc_demo, tz = "UTC"),
    ## Vehicle
    time_p_vehicle_pageQ1_button = ymd_hms(
      time_p_vehicle_pageQ1_button,
      tz = "UTC"
    ),
    time_p_vehicle_pageQ2_button = ymd_hms(
      time_p_vehicle_pageQ2_button,
      tz = "UTC"
    ),
    time_p_vehicle_pageQ3_button = ymd_hms(
      time_p_vehicle_pageQ3_button,
      tz = "UTC"
    ),
    time_p_vehicle_pageQ4_button = ymd_hms(
      time_p_vehicle_pageQ4_button,
      tz = "UTC"
    ),
    time_p_vehicle_pageQ5_button = ymd_hms(
      time_p_vehicle_pageQ5_button,
      tz = "UTC"
    ),
    time_p_vehicle_pageQ6_button = ymd_hms(
      time_p_vehicle_pageQ6_button,
      tz = "UTC"
    ),
    ###########################
    time_q_vehicle_cbc_q0_button = ymd_hms(
      time_q_vehicle_cbc_q0_button,
      tz = "UTC"
    ),
    time_q_vehicle_cbc_q1_button = ymd_hms(
      time_q_vehicle_cbc_q1_button,
      tz = "UTC"
    ),
    time_q_vehicle_cbc_q2_button = ymd_hms(
      time_q_vehicle_cbc_q2_button,
      tz = "UTC"
    ),
    time_q_vehicle_cbc_q3_button = ymd_hms(
      time_q_vehicle_cbc_q3_button,
      tz = "UTC"
    ),
    time_q_vehicle_cbc_q4_button = ymd_hms(
      time_q_vehicle_cbc_q4_button,
      tz = "UTC"
    ),
    time_q_vehicle_cbc_q5_button = ymd_hms(
      time_q_vehicle_cbc_q5_button,
      tz = "UTC"
    ),
    time_q_vehicle_cbc_q6_button = ymd_hms(
      time_q_vehicle_cbc_q6_button,
      tz = "UTC"
    ),
    ## Battery
    time_p_battery_pageQ1_button = ymd_hms(
      time_p_battery_pageQ1_button,
      tz = "UTC"
    ),
    time_p_battery_pageQ2_button = ymd_hms(
      time_p_battery_pageQ2_button,
      tz = "UTC"
    ),
    time_p_battery_pageQ3_button = ymd_hms(
      time_p_battery_pageQ3_button,
      tz = "UTC"
    ),
    time_p_battery_pageQ4_button = ymd_hms(
      time_p_battery_pageQ4_button,
      tz = "UTC"
    ),
    time_p_battery_pageQ5_button = ymd_hms(
      time_p_battery_pageQ5_button,
      tz = "UTC"
    ),
    time_p_battery_pageQ6_button = ymd_hms(
      time_p_battery_pageQ6_button,
      tz = "UTC"
    ),
    ###########################
    time_q_battery_cbc_q1_button = ymd_hms(
      time_q_battery_cbc_q1_button,
      tz = "UTC"
    ),
    time_q_battery_cbc_q2_button = ymd_hms(
      time_q_battery_cbc_q2_button,
      tz = "UTC"
    ),
    time_q_battery_cbc_q3_button = ymd_hms(
      time_q_battery_cbc_q3_button,
      tz = "UTC"
    ),
    time_q_battery_cbc_q4_button = ymd_hms(
      time_q_battery_cbc_q4_button,
      tz = "UTC"
    ),
    time_q_battery_cbc_q5_button = ymd_hms(
      time_q_battery_cbc_q5_button,
      tz = "UTC"
    ),
    time_q_battery_cbc_q6_button = ymd_hms(
      time_q_battery_cbc_q6_button,
      tz = "UTC"
    ),

    ###########################   Calculations vehicle  ####

    calc_vehicle_intro_p_demo_p = as.numeric(
      time_p_cbc_demo - time_p_cbc_intro,
      units = "secs"
    ),
    calc_vehicle_intro_p_cbc_q0 = as.numeric(
      time_q_vehicle_cbc_q0_button - time_p_cbc_intro,
      units = "secs"
    ),
    calc_vehicle_cbc_q0_demo_p = -as.numeric(
      time_p_cbc_demo - time_q_vehicle_cbc_q0_button,
      units = "secs"
    ),

    calc_vehicle_demo_p_pageQ1 = as.numeric(
      time_p_vehicle_pageQ1_button - time_p_cbc_demo,
      units = "secs"
    ),
    calc_vehicle_demo_p_cbc_q1 = as.numeric(
      time_q_vehicle_cbc_q1_button - time_p_cbc_demo,
      units = "secs"
    ),
    calc_vehicle_cbc_q1_pageQ1 = -as.numeric(
      time_p_vehicle_pageQ1_button - time_q_vehicle_cbc_q1_button,
      units = "secs"
    ),

    calc_vehicle_pageQ1_pageQ2 = as.numeric(
      time_p_vehicle_pageQ2_button - time_p_vehicle_pageQ1_button,
      units = "secs"
    ),
    calc_vehicle_pageQ1_cbc_q2 = as.numeric(
      time_q_vehicle_cbc_q2_button - time_p_vehicle_pageQ1_button,
      units = "secs"
    ),
    calc_vehicle_cbc_q2_pageQ2 = -as.numeric(
      time_p_vehicle_pageQ2_button - time_q_vehicle_cbc_q2_button,
      units = "secs"
    ),

    calc_vehicle_pageQ2_pageQ3 = as.numeric(
      time_p_vehicle_pageQ3_button - time_p_vehicle_pageQ2_button,
      units = "secs"
    ),
    calc_vehicle_pageQ2_cbc_q3 = as.numeric(
      time_q_vehicle_cbc_q3_button - time_p_vehicle_pageQ2_button,
      units = "secs"
    ),
    calc_vehicle_cbc_q3_pageQ3 = -as.numeric(
      time_p_vehicle_pageQ3_button - time_q_vehicle_cbc_q3_button,
      units = "secs"
    ),

    calc_vehicle_pageQ3_pageQ4 = as.numeric(
      time_p_vehicle_pageQ4_button - time_p_vehicle_pageQ3_button,
      units = "secs"
    ),
    calc_vehicle_pageQ3_cbc_q4 = as.numeric(
      time_q_vehicle_cbc_q4_button - time_p_vehicle_pageQ3_button,
      units = "secs"
    ),
    calc_vehicle_cbc_q4_pageQ4 = -as.numeric(
      time_p_vehicle_pageQ4_button - time_q_vehicle_cbc_q4_button,
      units = "secs"
    ),

    calc_vehicle_pageQ4_pageQ5 = as.numeric(
      time_p_vehicle_pageQ5_button - time_p_vehicle_pageQ4_button,
      units = "secs"
    ),
    calc_vehicle_pageQ4_cbc_q5 = as.numeric(
      time_q_vehicle_cbc_q5_button - time_p_vehicle_pageQ4_button,
      units = "secs"
    ),
    calc_vehicle_cbc_q5_pageQ5 = -as.numeric(
      time_p_vehicle_pageQ5_button - time_q_vehicle_cbc_q5_button,
      units = "secs"
    ),

    calc_vehicle_pageQ5_pageQ6 = as.numeric(
      time_p_vehicle_pageQ6_button - time_p_vehicle_pageQ5_button,
      units = "secs"
    ),
    calc_vehicle_pageQ5_cbc_q6 = as.numeric(
      time_q_vehicle_cbc_q6_button - time_p_vehicle_pageQ5_button,
      units = "secs"
    ),
    calc_vehicle_cbc_q6_pageQ6 = -as.numeric(
      time_p_vehicle_pageQ6_button - time_q_vehicle_cbc_q6_button,
      units = "secs"
    ),

    ###########################   Calculations battery  ####

    calc_battery_demo_p_pageQ1 = as.numeric(
      time_p_battery_pageQ1_button - time_p_battery_cbc_demo,
      units = "secs"
    ),
    calc_battery_demo_p_cbc_q1 = as.numeric(
      time_q_battery_cbc_q1_button - time_p_battery_cbc_demo,
      units = "secs"
    ),
    calc_battery_cbc_q1_pageQ1 = -as.numeric(
      time_p_battery_pageQ1_button - time_q_battery_cbc_q1_button,
      units = "secs"
    ),

    calc_battery_pageQ1_pageQ2 = as.numeric(
      time_p_battery_pageQ2_button - time_p_battery_pageQ1_button,
      units = "secs"
    ),
    calc_battery_pageQ1_cbc_q2 = as.numeric(
      time_q_battery_cbc_q2_button - time_p_battery_pageQ1_button,
      units = "secs"
    ),
    calc_battery_cbc_q2_pageQ2 = -as.numeric(
      time_p_battery_pageQ2_button - time_q_battery_cbc_q2_button,
      units = "secs"
    ),

    calc_battery_pageQ2_pageQ3 = as.numeric(
      time_p_battery_pageQ3_button - time_p_battery_pageQ2_button,
      units = "secs"
    ),
    calc_battery_pageQ2_cbc_q3 = as.numeric(
      time_q_battery_cbc_q3_button - time_p_battery_pageQ2_button,
      units = "secs"
    ),
    calc_battery_cbc_q3_pageQ3 = -as.numeric(
      time_p_battery_pageQ3_button - time_q_battery_cbc_q3_button,
      units = "secs"
    ),

    calc_battery_pageQ3_pageQ4 = as.numeric(
      time_p_battery_pageQ4_button - time_p_battery_pageQ3_button,
      units = "secs"
    ),
    calc_battery_pageQ3_cbc_q4 = as.numeric(
      time_q_battery_cbc_q4_button - time_p_battery_pageQ3_button,
      units = "secs"
    ),
    calc_battery_cbc_q4_pageQ4 = -as.numeric(
      time_p_battery_pageQ4_button - time_q_battery_cbc_q4_button,
      units = "secs"
    ),

    calc_battery_pageQ4_pageQ5 = as.numeric(
      time_p_battery_pageQ5_button - time_p_battery_pageQ4_button,
      units = "secs"
    ),
    calc_battery_pageQ4_cbc_q5 = as.numeric(
      time_q_battery_cbc_q5_button - time_p_battery_pageQ4_button,
      units = "secs"
    ),
    calc_battery_cbc_q5_pageQ5 = -as.numeric(
      time_p_battery_pageQ5_button - time_q_battery_cbc_q5_button,
      units = "secs"
    ),

    calc_battery_pageQ5_pageQ6 = as.numeric(
      time_p_battery_pageQ6_button - time_p_battery_pageQ5_button,
      units = "secs"
    ),
    calc_battery_pageQ5_cbc_q6 = as.numeric(
      time_q_battery_cbc_q6_button - time_p_battery_pageQ5_button,
      units = "secs"
    ),
    calc_battery_cbc_q6_pageQ6 = -as.numeric(
      time_p_battery_pageQ6_button - time_q_battery_cbc_q6_button,
      units = "secs"
    ),

    calc_vehicle_cbc_total = as.numeric(
      time_p_vehicle_pageQ6_button - time_p_vehicle_pageQ1_button,
      units = "secs"
    ),
    ## Battery
    calc_battery_cbc_total = as.numeric(
      time_p_battery_pageQ6_button - time_p_battery_pageQ1_button,
      units = "secs"
    ),
    calc_min_total = time_total / 60,
    calc_min_vehicle_cbc = calc_vehicle_cbc_total / 60,
    calc_min_battery_cbc = calc_battery_cbc_total / 60
  ) %>%
  select(-starts_with('time'))

#######################################################################

write_csv(
  data,
  here(
    "data",
    "dynata_prolific_joint",
    "timestamps.csv"
  )
)


data_veh_duration <- data %>%
  select(
    starts_with('calc_vehicle_cbc_q') & contains('_cbc_q'),
    data_source
  ) %>%
  pivot_longer(
    cols = starts_with("calc_"),
    names_to = "metric_name",
    values_to = "value"
  )

hist_data_veh_duration <- ggplot(
  data_veh_duration %>% filter(value < 150),
  aes(x = value, fill = data_source)
) +
  # Create the histogram
  # position = "identity" allows overlaps (with transparency)
  # position = "dodge" places bars side-by-side
  geom_histogram(
    position = "identity",
    alpha = 0.6,
    bins = 30,
    color = "white"
  ) +

  # Split into panels (one for each calculated variable)
  facet_wrap(~metric_name, scales = "free", ncol = 1) +

  # Styling
  scale_fill_viridis_d(option = "viridis", name = "Data Source") +
  labs(
    title = "Distribution of Time/Metrics Across Survey Steps",
    subtitle = "Comparing calculated variables by Data Source",
    x = "Value (e.g., Duration in seconds)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

path_images <- 'code/output/images/'

ggsave(
  paste0(path_images, "hist_data_veh_duration.jpg"),
  plot = hist_data_veh_duration,
  width = 6,
  height = 15,
  dpi = 300
)

data_battery_duration <- data %>%
  select(
    starts_with('calc_battery_cbc_q') & contains('_cbc_q'),
    data_source
  ) %>%
  pivot_longer(
    cols = starts_with("calc_"),
    names_to = "metric_name",
    values_to = "value"
  )

hist_data_battery_duration <- ggplot(
  data_battery_duration %>% filter(value < 150),
  aes(x = value, fill = data_source)
) +
  # Create the histogram
  # position = "identity" allows overlaps (with transparency)
  # position = "dodge" places bars side-by-side
  geom_histogram(
    position = "identity",
    alpha = 0.6,
    bins = 30,
    color = "white"
  ) +

  # Split into panels (one for each calculated variable)
  facet_wrap(~metric_name, scales = "free", ncol = 1) +

  # Styling
  scale_fill_viridis_d(option = "viridis", name = "Data Source") +
  labs(
    title = "Distribution of Time/Metrics Across Survey Steps",
    subtitle = "Comparing calculated variables by Data Source",
    x = "Value (e.g., Duration in seconds)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

path_images <- 'code/output/images/'

ggsave(
  paste0(path_images, "hist_data_battery_duration.jpg"),
  plot = hist_data_battery_duration,
  width = 6,
  height = 15,
  dpi = 300
)
