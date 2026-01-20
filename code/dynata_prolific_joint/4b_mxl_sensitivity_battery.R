source(here::here('code', 'setup.R'))

# 0. Load smc_mxl_model
load(
  file = file.path(
    here(),
    "code",
    "main",
    "model_output",
    "logitr",
    "mxl_wtp_full.RData"
  )
)
summary(mxl_wtp_full)


# For the nexe study:
# Manually adjust the no_choice coefficient to test hypothetical bias correction
# coef(smc_mxl_model)
# smc_mxl_model$coefficients["no_choice"] <- 5

# ASC Adjustment for hypothetical bias correction
asc_adjustment <- 0

# 1. Sensitivity of user enrollment to changes in "enrollment_cash"

# Set a baseline scenario for all single-scenario sensitivity analysis
# Option 1 is provided with moderate values
# Option 2 is a no option
# We will change the enrollment_cash values for Option 1
smc_baseline <- data.frame(
  alt_id = c(1, 2),
  obs_id = c(1, 1),
  enrollment_cash = c(0, 0),
  monthly_cash = c(0, 0),
  override_days = c(0, 0),
  override_flag = c(0, 0),
  minimum_threshold = c(0, 0),
  guaranteed_threshold = c(0, 0),
  no_choice = c(0, 1 + asc_adjustment)
)

smc_baseline

# Set the sequence of enrollment cash levels from 0 to 300 USD
smc_enroll_levels <- seq(0, 300, by = 10)

# Count the numbers of enrollment scenarios
smc_enroll_numbers <- length(smc_enroll_levels)

# Create scenarios with different enrollment levels in option 1
smc_enroll_scenarios <- do.call(
  bind_rows,
  replicate(smc_enroll_numbers, smc_baseline, simplify = FALSE)
)
smc_enroll_scenarios$obs_id <- rep(seq(smc_enroll_numbers), each = 2)
smc_enroll_scenarios$enrollment_cash[which(
  smc_enroll_scenarios$alt_id == 1
)] <- smc_enroll_levels


# Simulate the enrollment cash sensitivity
smc_mxl_sens_enroll <- predict(
  smc_mxl_model,
  newdata = smc_enroll_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  # Keep only option 1
  filter(alt_id == 1) %>%
  # Keep only enrollment cash and predictions
  select(enrollment_cash, starts_with("predicted_"))

# Reveal the simulation
head(smc_mxl_sens_enroll)
tail(smc_mxl_sens_enroll)

# Plot of user enrollment change to "enrollment_cash"
smc_mxl_sens_enroll_plot <- smc_mxl_sens_enroll %>%
  ggplot(aes(
    x = enrollment_cash,
    y = predicted_prob,
    ymin = predicted_prob_lower,
    ymax = predicted_prob_upper
  )) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_line(
    data = smc_mxl_sens_enroll %>%
      filter(enrollment_cash <= 300, enrollment_cash >= 50),
    linetype = "solid"
  ) +
  expand_limits(x = c(0, 300), y = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Enrollment Cash (USD)",
    y = "Enrollment Rate",
    title = "SMC Enrollment Rate Sensitivity of Enrollment Cash"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(panel.grid.minor = element_blank())

smc_mxl_sens_enroll_plot

# 2. Sensitivity of user enrollment to changes in "monthly_cash"
smc_monthly_levels <- seq(0, 20, by = 1)
smc_monthly_numbers <- length(smc_monthly_levels)
smc_monthly_scenarios <- do.call(
  bind_rows,
  replicate(smc_monthly_numbers, smc_baseline, simplify = FALSE)
)
smc_monthly_scenarios$obs_id <- rep(seq(smc_monthly_numbers), each = 2)
smc_monthly_scenarios$monthly_cash[which(
  smc_monthly_scenarios$alt_id == 1
)] <- smc_monthly_levels

smc_mxl_sens_monthly <- predict(
  smc_mxl_model,
  newdata = smc_monthly_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  filter(alt_id == 1) %>%
  select(monthly_cash, starts_with("predicted_"))

# Plot of user enrollment change to "monthly_cash"
smc_mxl_sens_monthly_plot <- smc_mxl_sens_monthly %>%
  ggplot(aes(
    x = monthly_cash,
    y = predicted_prob,
    ymin = predicted_prob_lower,
    ymax = predicted_prob_upper
  )) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_line(
    data = smc_mxl_sens_monthly %>%
      filter(monthly_cash <= 20, monthly_cash >= 2),
    linetype = "solid"
  ) +
  expand_limits(x = c(0, 20), y = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Monthly Cash (USD)",
    y = "Enrollment Rate",
    title = "SMC Enrollment Rate Sensitivity of Monthly Cash"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(panel.grid.minor = element_blank())

smc_mxl_sens_monthly_plot

# 3. Sensitivity of user enrollment to changes in override_days
smc_override_levels <- seq(0, 5, by = 1)
smc_override_numbers <- length(smc_override_levels)
smc_override_scenarios <- do.call(
  bind_rows,
  replicate(smc_override_numbers, smc_baseline, simplify = FALSE)
)
smc_override_scenarios$obs_id <- rep(seq(smc_override_numbers), each = 2)
smc_override_scenarios$override_days[which(
  smc_override_scenarios$alt_id == 1
)] <- smc_override_levels
smc_override_scenarios$override_flag <- ifelse(
  smc_override_scenarios$override_days > 0,
  1,
  0
)

smc_mxl_sens_override <- predict(
  smc_mxl_model,
  newdata = smc_override_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  filter(alt_id == 1) %>%
  select(override_days, starts_with("predicted_"))

# Plot of user enrollment change to "override_days"
smc_mxl_sens_override_plot <- smc_mxl_sens_override %>%
  ggplot(aes(
    x = override_days,
    y = predicted_prob,
    ymin = predicted_prob_lower,
    ymax = predicted_prob_upper
  )) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_line(
    data = smc_mxl_sens_override %>%
      filter(override_days <= 5, override_days >= 0),
    linetype = "solid"
  ) +
  expand_limits(x = c(0, 5), y = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Override (Days)",
    y = "Enrollment Rate",
    title = "SMC Enrollment Rate Sensitivity of Override Days"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(panel.grid.minor = element_blank())

smc_mxl_sens_override_plot

# 4. Sensitivity of user enrollment to changes in "minimum_threshold"
smc_minimum_levels <- seq(0, 40, by = 1)
smc_minimum_numbers <- length(smc_minimum_levels)
smc_minimum_scenarios <- do.call(
  bind_rows,
  replicate(smc_minimum_numbers, smc_baseline, simplify = FALSE)
)
smc_minimum_scenarios$obs_id <- rep(seq(smc_minimum_numbers), each = 2)
smc_minimum_scenarios$minimum_threshold[which(
  smc_minimum_scenarios$alt_id == 1
)] <- smc_minimum_levels

smc_mxl_sens_minimum <- predict(
  smc_mxl_model,
  newdata = smc_minimum_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  filter(alt_id == 1) %>%
  select(minimum_threshold, starts_with("predicted_"))

# Plot of user enrollment change to "minimum_threshold"
smc_mxl_sens_minimum_plot <- smc_mxl_sens_minimum %>%
  ggplot(aes(
    x = minimum_threshold,
    y = predicted_prob,
    ymin = predicted_prob_lower,
    ymax = predicted_prob_upper
  )) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_line(
    data = smc_mxl_sens_minimum %>%
      filter(minimum_threshold <= 40, minimum_threshold >= 20),
    linetype = "solid"
  ) +
  expand_limits(x = c(0, 40), y = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Minimum Threshold (%)",
    y = "Enrollment Rate",
    title = "SMC Enrollment Rate Sensitivity of Minimum Threshold"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(panel.grid.minor = element_blank())

smc_mxl_sens_minimum_plot

# 5. Sensitivity of user enrollment to changes in "guaranteed_threshold"
smc_guaranteed_levels <- seq(0, 80, by = 1)
smc_guaranteed_numbers <- length(smc_guaranteed_levels)
smc_guaranteed_scenarios <- do.call(
  bind_rows,
  replicate(smc_guaranteed_numbers, smc_baseline, simplify = FALSE)
)
smc_guaranteed_scenarios$obs_id <- rep(seq(smc_guaranteed_numbers), each = 2)
smc_guaranteed_scenarios$guaranteed_threshold[which(
  smc_guaranteed_scenarios$alt_id == 1
)] <- smc_guaranteed_levels

smc_mxl_sens_guaranteed <- predict(
  smc_mxl_model,
  newdata = smc_guaranteed_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  filter(alt_id == 1) %>%
  select(guaranteed_threshold, starts_with("predicted_"))

# Plot of user enrollment change to "guaranteed_threshold"
smc_mxl_sens_guaranteed_plot <- smc_mxl_sens_guaranteed %>%
  ggplot(aes(
    x = guaranteed_threshold,
    y = predicted_prob,
    ymin = predicted_prob_lower,
    ymax = predicted_prob_upper
  )) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_line(
    data = smc_mxl_sens_guaranteed %>%
      filter(guaranteed_threshold <= 80, guaranteed_threshold >= 60),
    linetype = "solid"
  ) +
  expand_limits(x = c(0, 80), y = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Guaranteed Threshold (%)",
    y = "Enrollment Rate",
    title = "SMC Enrollment Rate Sensitivity of Guaranteed Threshold"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(panel.grid.minor = element_blank())

smc_mxl_sens_guaranteed_plot

# 6. Sensitivity of user enrollment to changes in multiple attributes

# We continue to work on Option 1
# This time, we modify all continuous attributes
# The attributes are: enrollment_cash, monthly_cash, override, minimum_threshold, and guaranteed_threshold

# Construct the smc_multiple_levels, like we constructed smc_enroll_levels
# But smc_multiple_levels is a df, while smc_enroll_levels is a sequence
smc_multiple_levels <- tribble(
  ~obs_id                ,
  ~alt_id                ,
  ~attribute             ,
  ~case                  ,
  ~value                 ,
                       2 ,
                       1 ,
  "enrollment_cash"      ,
  "high"                 ,
                     300 ,
                       3 ,
                       1 ,
  "enrollment_cash"      ,
  "low"                  ,
                      50 ,
                       4 ,
                       1 ,
  "monthly_cash"         ,
  "high"                 ,
                      20 ,
                       5 ,
                       1 ,
  "monthly_cash"         ,
  "low"                  ,
                       2 ,
                       6 ,
                       1 ,
  "override_days"        ,
  "high"                 ,
                       5 ,
                       7 ,
                       1 ,
  "override_days"        ,
  "mid"                  ,
                       3 ,
                       8 ,
                       1 ,
  "override_days"        ,
  "low"                  ,
                       1 ,
                       9 ,
                       1 ,
  "minimum_threshold"    ,
  "high"                 ,
                      40 ,
                      10 ,
                       1 ,
  "minimum_threshold"    ,
  "low"                  ,
                      20 ,
                      11 ,
                       1 ,
  "guaranteed_threshold" ,
  "high"                 ,
                      80 ,
                      12 ,
                       1 ,
  "guaranteed_threshold" ,
  "low"                  ,
                      60
)

# Reveal smc_multiple_levels
smc_multiple_levels

# Define the numbers of scenarios
smc_multiple_numbers <- 12

# Create scenarios with different upfront levels in option 2
smc_multiple_scenarios <- do.call(
  bind_rows,
  replicate(smc_multiple_numbers, smc_baseline, simplify = FALSE)
)
smc_multiple_scenarios$obs_id <- rep(seq(smc_multiple_numbers), each = 2)
smc_multiple_scenarios <- smc_multiple_scenarios %>%
  left_join(smc_multiple_levels, by = c("alt_id", "obs_id")) %>%
  mutate(
    attribute = ifelse(is.na(attribute), "other", attribute),
    case = ifelse(is.na(case), "base", case),
    enrollment_cash = ifelse(
      attribute == "enrollment_cash",
      value,
      enrollment_cash
    ),
    monthly_cash = ifelse(attribute == "monthly_cash", value, monthly_cash),
    override_days = ifelse(
      attribute == "override_days",
      value,
      override_days
    ),
    override_flag = ifelse(override_days == 0, 0, 1),
    minimum_threshold = ifelse(
      attribute == "minimum_threshold",
      value,
      minimum_threshold
    ),
    guaranteed_threshold = ifelse(
      attribute == "guaranteed_threshold",
      value,
      guaranteed_threshold
    )
  )

# Simulate the multiple-attribute sensitivity
smc_mxl_sens_mult <- predict(
  smc_mxl_model,
  newdata = smc_multiple_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  # Keep only option 1
  filter(alt_id == 1) %>%
  # Select the attributes and predictions
  select(attribute, case, value, predicted_prob)

# Combined Plot of user enrollment change to multiple attributes
smc_sens_mult_labels <- data.frame(
  attribute = c(
    "enrollment_cash",
    "monthly_cash",
    "override_days",
    "minimum_threshold",
    "guaranteed_threshold"
  ),
  label = c(
    "Enrollment Cash",
    "Monthly Cash",
    "Override Days",
    "Minimum Threshold",
    "Guaranteed Threshold"
  )
)

smc_mxl_sens_combined_data <- smc_mxl_sens_mult %>%
  filter(case != "base") %>%
  left_join(smc_sens_mult_labels, by = "attribute")

smc_mxl_sens_combined_data$value <- c(
  "$300",
  "$50",
  "$20",
  "$2",
  "5",
  "1",
  "0",
  "40%",
  "20%",
  "80%",
  "60%"
)

# Save all simulations
save(
  smc_mxl_sens_enroll,
  smc_mxl_sens_monthly,
  smc_mxl_sens_override,
  smc_mxl_sens_minimum,
  smc_mxl_sens_guaranteed,
  smc_mxl_sens_mult,
  file = file.path(
    processed_dir,
    "3_enrollment_sensitivity",
    "smc_mxl_sens.RData"
  )
)

################
# V2G Programs #
################

# 0. Load v2g_mxl_model
load(file.path(processed_dir, "2_models", "v2g_mxl_model.RData"))
summary(v2g_mxl_model)

# 1. Sensitivity of user enrollment to changes in "enrollment_cash"

# Set a baseline scenario for all single-scenario sensitivity analysis
# Option 1 is provided with moderate values
# Option 2 is a no option
# We will change the enrollment_cash values for Option 1
# Note: Using same asc_adjustment as SMC (defined above)
v2g_baseline <- data.frame(
  alt_id = c(1, 2),
  obs_id = c(1, 1),
  enrollment_cash = c(0, 0),
  occurrence_cash = c(1, 0),
  monthly_occurrence = c(1, 0),
  lower_bound = c(0, 0),
  guaranteed_threshold = c(0, 0),
  no_choice = c(0, 1 + asc_adjustment)
)

v2g_baseline

# Set the sequence of enrollment cash levels from 0 to 300 USD
v2g_enroll_levels <- seq(0, 300, by = 10)

# Count the numbers of enrollment scenarios
v2g_enroll_numbers <- length(v2g_enroll_levels)

# Create scenarios with different enrollment levels in option 1
v2g_enroll_scenarios <- do.call(
  bind_rows,
  replicate(v2g_enroll_numbers, v2g_baseline, simplify = FALSE)
)
v2g_enroll_scenarios$obs_id <- rep(seq(v2g_enroll_numbers), each = 2)
v2g_enroll_scenarios$enrollment_cash[which(
  v2g_enroll_scenarios$alt_id == 1
)] <- v2g_enroll_levels

# Simulate the enrollment cash sensitivity
v2g_mxl_sens_enroll <- predict(
  v2g_mxl_model,
  newdata = v2g_enroll_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  # Keep only option 1
  filter(alt_id == 1) %>%
  # Keep only enrollment cash and predictions
  select(enrollment_cash, starts_with("predicted_"))

# Reveal the simulation
head(v2g_mxl_sens_enroll)
tail(v2g_mxl_sens_enroll)

# Plot of user enrollment change to "enrollment_cash"
v2g_mxl_sens_enroll_plot <- v2g_mxl_sens_enroll %>%
  ggplot(aes(
    x = enrollment_cash,
    y = predicted_prob,
    ymin = predicted_prob_lower,
    ymax = predicted_prob_upper
  )) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_line(
    data = v2g_mxl_sens_enroll %>%
      filter(enrollment_cash <= 300, enrollment_cash >= 50),
    linetype = "solid"
  ) +
  expand_limits(x = c(0, 300), y = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Enrollment Cash (USD)",
    y = "Enrollment Rate",
    title = "V2G Enrollment Rate Sensitivity of Enrollment Cash"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(panel.grid.minor = element_blank())

v2g_mxl_sens_enroll_plot

# 2. Sensitivity of user enrollment to changes in "occurrence_cash"
v2g_occurrence_levels <- seq(0, 20, by = 1)
v2g_occurrence_numbers <- length(v2g_occurrence_levels)
v2g_occurrence_scenarios <- do.call(
  bind_rows,
  replicate(v2g_occurrence_numbers, v2g_baseline, simplify = FALSE)
)
v2g_occurrence_scenarios$obs_id <- rep(seq(v2g_occurrence_numbers), each = 2)
v2g_occurrence_scenarios$occurrence_cash[which(
  v2g_occurrence_scenarios$alt_id == 1
)] <- v2g_occurrence_levels

v2g_mxl_sens_occurrence <- predict(
  v2g_mxl_model,
  newdata = v2g_occurrence_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  filter(alt_id == 1) %>%
  select(occurrence_cash, starts_with("predicted_"))

# Plot of user enrollment change to "occurrence_cash"
v2g_mxl_sens_occurrence_plot <- v2g_mxl_sens_occurrence %>%
  ggplot(aes(
    x = occurrence_cash,
    y = predicted_prob,
    ymin = predicted_prob_lower,
    ymax = predicted_prob_upper
  )) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_line(
    data = v2g_mxl_sens_occurrence %>%
      filter(occurrence_cash <= 20, occurrence_cash >= 2),
    linetype = "solid"
  ) +
  expand_limits(x = c(0, 20), y = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Occurrence Cash (USD)",
    y = "Enrollment Rate",
    title = "V2G Enrollment Rate Sensitivity of Occurrence Cash"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(panel.grid.minor = element_blank())

v2g_mxl_sens_occurrence_plot

# 3. Sensitivity of user enrollment to changes in monthly_occurrence
v2g_monthly_levels <- seq(0, 5, by = 1)
v2g_monthly_numbers <- length(v2g_monthly_levels)
v2g_monthly_scenarios <- do.call(
  bind_rows,
  replicate(v2g_monthly_numbers, v2g_baseline, simplify = FALSE)
)
v2g_monthly_scenarios$obs_id <- rep(seq(v2g_monthly_numbers), each = 2)
v2g_monthly_scenarios$monthly_occurrence[which(
  v2g_monthly_scenarios$alt_id == 1
)] <- v2g_monthly_levels

v2g_mxl_sens_monthly <- predict(
  v2g_mxl_model,
  newdata = v2g_monthly_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  filter(alt_id == 1) %>%
  select(monthly_occurrence, starts_with("predicted_"))

# Plot of user enrollment change to "monthly_occurrence"
v2g_mxl_sens_monthly_plot <- v2g_mxl_sens_monthly %>%
  ggplot(aes(
    x = monthly_occurrence,
    y = predicted_prob,
    ymin = predicted_prob_lower,
    ymax = predicted_prob_upper
  )) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_line(
    data = v2g_mxl_sens_monthly %>%
      filter(monthly_occurrence <= 4, monthly_occurrence >= 1),
    linetype = "solid"
  ) +
  expand_limits(x = c(0, 5), y = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Monthly Occurrence",
    y = "Enrollment Rate",
    title = "V2G Enrollment Rate Sensitivity of Monthly Occurrence"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(panel.grid.minor = element_blank())

v2g_mxl_sens_monthly_plot

# 4. Sensitivity of user enrollment to changes in "lower_bound"
v2g_lower_levels <- seq(0, 40, by = 1)
v2g_lower_numbers <- length(v2g_lower_levels)
v2g_lower_scenarios <- do.call(
  bind_rows,
  replicate(v2g_lower_numbers, v2g_baseline, simplify = FALSE)
)
v2g_lower_scenarios$obs_id <- rep(seq(v2g_lower_numbers), each = 2)
v2g_lower_scenarios$lower_bound[which(
  v2g_lower_scenarios$alt_id == 1
)] <- v2g_lower_levels

v2g_mxl_sens_lower <- predict(
  v2g_mxl_model,
  newdata = v2g_lower_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  filter(alt_id == 1) %>%
  select(lower_bound, starts_with("predicted_"))

# Plot of user enrollment change to "lower_bound"
v2g_mxl_sens_lower_plot <- v2g_mxl_sens_lower %>%
  ggplot(aes(
    x = lower_bound,
    y = predicted_prob,
    ymin = predicted_prob_lower,
    ymax = predicted_prob_upper
  )) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_line(
    data = v2g_mxl_sens_lower %>%
      filter(lower_bound <= 40, lower_bound >= 20),
    linetype = "solid"
  ) +
  expand_limits(x = c(0, 40), y = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Lower Bound (%)",
    y = "Enrollment Rate",
    title = "V2G Enrollment Rate Sensitivity of Lower Bound"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(panel.grid.minor = element_blank())

v2g_mxl_sens_lower_plot

# 5. Sensitivity of user enrollment to changes in "guaranteed_threshold"
v2g_guaranteed_levels <- seq(0, 80, by = 1)
v2g_guaranteed_numbers <- length(v2g_guaranteed_levels)
v2g_guaranteed_scenarios <- do.call(
  bind_rows,
  replicate(v2g_guaranteed_numbers, v2g_baseline, simplify = FALSE)
)
v2g_guaranteed_scenarios$obs_id <- rep(seq(v2g_guaranteed_numbers), each = 2)
v2g_guaranteed_scenarios$guaranteed_threshold[which(
  v2g_guaranteed_scenarios$alt_id == 1
)] <- v2g_guaranteed_levels

v2g_mxl_sens_guaranteed <- predict(
  v2g_mxl_model,
  newdata = v2g_guaranteed_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  filter(alt_id == 1) %>%
  select(guaranteed_threshold, starts_with("predicted_"))

# Plot of user enrollment change to "guaranteed_threshold"
v2g_mxl_sens_guaranteed_plot <- v2g_mxl_sens_guaranteed %>%
  ggplot(aes(
    x = guaranteed_threshold,
    y = predicted_prob,
    ymin = predicted_prob_lower,
    ymax = predicted_prob_upper
  )) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_line(
    data = v2g_mxl_sens_guaranteed %>%
      filter(guaranteed_threshold <= 80, guaranteed_threshold >= 60),
    linetype = "solid"
  ) +
  expand_limits(x = c(0, 80), y = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Guaranteed Threshold (%)",
    y = "Enrollment Rate",
    title = "V2G Enrollment Rate Sensitivity of Guaranteed Threshold"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(panel.grid.minor = element_blank())

v2g_mxl_sens_guaranteed_plot

# 6. Sensitivity of user enrollment to changes in multiple attributes

# We continue to work on Option 1
# This time, we modify all continuous attributes
# The attributes are: enrollment_cash, occurrence_cash, monthly_occurrence, lower_bound, and guaranteed_threshold

# Construct the v2g_multiple_levels, like we constructed v2g_enroll_levels
# But v2g_multiple_levels is a df, while v2g_enroll_levels is a sequence
v2g_multiple_levels <- tribble(
  ~obs_id                ,
  ~alt_id                ,
  ~attribute             ,
  ~case                  ,
  ~value                 ,
                       2 ,
                       1 ,
  "enrollment_cash"      ,
  "high"                 ,
                     300 ,
                       3 ,
                       1 ,
  "enrollment_cash"      ,
  "low"                  ,
                      50 ,
                       4 ,
                       1 ,
  "occurrence_cash"      ,
  "high"                 ,
                      20 ,
                       5 ,
                       1 ,
  "occurrence_cash"      ,
  "low"                  ,
                       2 ,
                       6 ,
                       1 ,
  "monthly_occurrence"   ,
  "high"                 ,
                       4 ,
                       7 ,
                       1 ,
  "monthly_occurrence"   ,
  "low"                  ,
                       1 ,
                       8 ,
                       1 ,
  "lower_bound"          ,
  "high"                 ,
                      40 ,
                       9 ,
                       1 ,
  "lower_bound"          ,
  "low"                  ,
                      20 ,
                      10 ,
                       1 ,
  "guaranteed_threshold" ,
  "high"                 ,
                      80 ,
                      11 ,
                       1 ,
  "guaranteed_threshold" ,
  "low"                  ,
                      60
)

# Reveal v2g_multiple_levels
v2g_multiple_levels

# Define the numbers of scenarios
v2g_multiple_numbers <- 11

# Create scenarios with different upfront levels in option 2
v2g_multiple_scenarios <- do.call(
  bind_rows,
  replicate(v2g_multiple_numbers, v2g_baseline, simplify = FALSE)
)
v2g_multiple_scenarios$obs_id <- rep(seq(v2g_multiple_numbers), each = 2)
v2g_multiple_scenarios <- v2g_multiple_scenarios %>%
  left_join(v2g_multiple_levels, by = c("alt_id", "obs_id")) %>%
  mutate(
    attribute = ifelse(is.na(attribute), "other", attribute),
    case = ifelse(is.na(case), "base", case),
    enrollment_cash = ifelse(
      attribute == "enrollment_cash",
      value,
      enrollment_cash
    ),
    occurrence_cash = ifelse(
      attribute == "occurrence_cash",
      value,
      occurrence_cash
    ),
    monthly_occurrence = ifelse(
      attribute == "monthly_occurrence",
      value,
      monthly_occurrence
    ),
    lower_bound = ifelse(attribute == "lower_bound", value, lower_bound),
    guaranteed_threshold = ifelse(
      attribute == "guaranteed_threshold",
      value,
      guaranteed_threshold
    )
  )

# Simulate the multiple-attribute sensitivity
v2g_mxl_sens_mult <- predict(
  v2g_mxl_model,
  newdata = v2g_multiple_scenarios,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  # Keep only option 1
  filter(alt_id == 1) %>%
  # Select the attributes and predictions
  select(attribute, case, value, predicted_prob)

# Combined Plot of user enrollment change to multiple attributes
v2g_sens_mult_labels <- data.frame(
  attribute = c(
    "enrollment_cash",
    "occurrence_cash",
    "monthly_occurrence",
    "lower_bound",
    "guaranteed_threshold"
  ),
  label = c(
    "Enrollment Cash",
    "Occurrence Cash",
    "Monthly Occurrence",
    "Lower Bound",
    "Guaranteed Threshold"
  )
)

v2g_mxl_sens_combined_data <- v2g_mxl_sens_mult %>%
  filter(case != "base") %>%
  left_join(v2g_sens_mult_labels, by = "attribute")

v2g_mxl_sens_combined_data$value <- c(
  "$300",
  "$50",
  "$20",
  "$2",
  "4",
  "1",
  "40%",
  "20%",
  "80%",
  "60%"
)

# Save all simulations
save(
  v2g_mxl_sens_enroll,
  v2g_mxl_sens_occurrence,
  v2g_mxl_sens_monthly,
  v2g_mxl_sens_lower,
  v2g_mxl_sens_guaranteed,
  v2g_mxl_sens_mult,
  file = file.path(
    processed_dir,
    "3_enrollment_sensitivity",
    "v2g_mxl_sens.RData"
  )
)

####################
# Plot Combination #
####################

# 1. SMC Plot Combination
# 1.1 Create a theme with empty y-axis and title
plot_theme <- theme(
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.title = element_blank()
)

# 1.2 Remove unnecessary elements
smc_mxl_sens_enroll_plot <-
  smc_mxl_sens_enroll_plot +
  theme(plot.title = element_blank())
smc_mxl_sens_monthly_plot <-
  smc_mxl_sens_monthly_plot +
  plot_theme
smc_mxl_sens_override_plot <-
  smc_mxl_sens_override_plot +
  plot_theme
smc_mxl_sens_minimum_plot <-
  smc_mxl_sens_minimum_plot +
  plot_theme
smc_mxl_sens_guaranteed_plot <-
  smc_mxl_sens_guaranteed_plot +
  plot_theme

# 1.3 Combine the plots
smc_mxl_sens_plots <-
  smc_mxl_sens_enroll_plot +
  smc_mxl_sens_monthly_plot +
  smc_mxl_sens_override_plot +
  smc_mxl_sens_minimum_plot +
  smc_mxl_sens_guaranteed_plot +
  plot_layout(ncol = 5, guides = "collect") +
  plot_annotation(
    title = "A) Supplier Managed Charging (SMC)",
    theme = theme(
      plot.title = element_text(family = "Ubuntu", face = "bold")
    )
  )

smc_mxl_sens_plots

# 2. V2G Plot Combination

# 2.1 Remove unnecessary elements
v2g_mxl_sens_enroll_plot <-
  v2g_mxl_sens_enroll_plot +
  theme(plot.title = element_blank())
v2g_mxl_sens_occurrence_plot <-
  v2g_mxl_sens_occurrence_plot +
  plot_theme
v2g_mxl_sens_monthly_plot <-
  v2g_mxl_sens_monthly_plot +
  plot_theme
v2g_mxl_sens_lower_plot <-
  v2g_mxl_sens_lower_plot +
  plot_theme
v2g_mxl_sens_guaranteed_plot <-
  v2g_mxl_sens_guaranteed_plot +
  plot_theme

# 2.2 Combine the plots
v2g_mxl_sens_plots <-
  v2g_mxl_sens_enroll_plot +
  v2g_mxl_sens_occurrence_plot +
  v2g_mxl_sens_monthly_plot +
  v2g_mxl_sens_lower_plot +
  v2g_mxl_sens_guaranteed_plot +
  plot_layout(ncol = 5, guides = "collect") +
  plot_annotation(
    title = "B) Vehicle-to-Grid (V2G)",
    theme = theme(
      plot.title = element_text(family = "Ubuntu", face = "bold")
    )
  )

v2g_mxl_sens_plots

# 3. Stack both plots together
mxl_sens_plot <-
  (wrap_elements(full = smc_mxl_sens_plots)) /
  (wrap_elements(full = v2g_mxl_sens_plots))
mxl_sens_plot

ggsave(
  filename = file.path(
    processed_dir,
    "3_enrollment_sensitivity",
    "mxl_sens_plots.png"
  ),
  plot = mxl_sens_plot,
  width = 10,
  height = 10 / 2
)

#####################
# Sensitivity Table #
#####################

# 1. Function to find value that gives target probability
find_value_for_prob <- function(
  attribute,
  model,
  baseline_data,
  target_prob,
  tolerance = 0.0001
) {
  low <- 0
  high <- 300

  if (
    attribute %in%
      c("guaranteed_threshold", "minimum_threshold", "lower_bound")
  ) {
    low <- 0
    high <- 100
  }

  while ((high - low) > tolerance) {
    mid <- (low + high) / 2
    test_scenario <- baseline_data
    test_scenario[test_scenario$alt_id == 1, attribute] <- mid
    pred <- predict(
      model,
      newdata = test_scenario,
      obsID = "obs_id",
      type = "prob"
    )$predicted_prob[1]

    if (abs(pred - target_prob) < tolerance) {
      return(mid)
    }
    if (pred < target_prob) low <- mid else high <- mid
  }
  return(mid)
}

# 2. SMC
smc_prob_0 <- smc_mxl_sens_combined_data[2, 4]
smc_prob_1 <- smc_prob_0 + 0.05

smc_labels <- c(
  "Enrollment Cash",
  "Monthly Cash",
  "Override Days",
  "Minimum Threshold",
  "Guaranteed Threshold"
)
smc_values_0 <- c(50, 2, 1, 20, 60)
smc_units <- c("$", "$", "Days", "%", "%")

smc_values_1 <- sapply(1:5, function(i) {
  attr_name <- c(
    "enrollment_cash",
    "monthly_cash",
    "override_days",
    "minimum_threshold",
    "guaranteed_threshold"
  )[i]
  find_value_for_prob(
    attribute = attr_name,
    model = smc_mxl_model,
    baseline_data = smc_baseline,
    target_prob = smc_prob_1
  )
})

smc_mxl_sens_predict_table <- data.frame(
  label = smc_labels,
  prob_0 = rep(round(smc_prob_0, 2), 5),
  value_0 = smc_values_0,
  prob_1 = rep(round(smc_prob_1, 2), 5),
  value_1 = round(smc_values_1, 1),
  prob_diff = format(round(smc_prob_1 - smc_prob_0, 2), nsmall = 2),
  value_diff = round(smc_values_1 - smc_values_0, 1),
  unit = smc_units
)

smc_mxl_sens_predict_table

smc_attr_equiv <- smc_mxl_sens_predict_table %>%
  select(label, value_diff, unit) %>%
  rename(
    Attribute = label,
    "Equivalence Value" = value_diff,
    Unit = unit
  )

smc_attr_equiv

# 3. V2G
v2g_prob_0 <- v2g_mxl_sens_combined_data[2, 4]
v2g_prob_1 <- v2g_prob_0 + 0.05

v2g_labels <- c(
  "Enrollment Cash",
  "Occurrence Cash",
  "Monthly Occurrence",
  "Lower Bound",
  "Guaranteed Threshold"
)
v2g_values_0 <- c(50, 2, 1, 20, 60)
v2g_units <- c("$", "$", "Times", "%", "%")

v2g_values_1 <- sapply(1:5, function(i) {
  attr_name <- c(
    "enrollment_cash",
    "occurrence_cash",
    "monthly_occurrence",
    "lower_bound",
    "guaranteed_threshold"
  )[i]
  find_value_for_prob(
    attribute = attr_name,
    model = v2g_mxl_model,
    baseline_data = v2g_baseline,
    target_prob = v2g_prob_1
  )
})

v2g_mxl_sens_predict_table <- data.frame(
  label = v2g_labels,
  prob_0 = rep(round(v2g_prob_0, 2), 5),
  value_0 = v2g_values_0,
  prob_1 = rep(round(v2g_prob_1, 2), 5),
  value_1 = round(v2g_values_1, 1),
  prob_diff = format(round(v2g_prob_1 - v2g_prob_0, 2), nsmall = 2),
  value_diff = round(v2g_values_1 - v2g_values_0, 1),
  unit = v2g_units
)

v2g_mxl_sens_predict_table

v2g_attr_equiv <- v2g_mxl_sens_predict_table %>%
  select(label, value_diff, unit) %>%
  rename(
    Attribute = label,
    "Equivalence Value" = value_diff,
    Unit = unit
  )

v2g_attr_equiv
