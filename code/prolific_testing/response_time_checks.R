source(here::here('code', 'setup.R'))
source(here::here('code', 'prolific_testing', 'approval_functions.R'))
library(patchwork)

#pilot_start <- ymd_hms('2025-10-14 21:00:00')
#pilot_end <- ymd_hms('2025-10-21 16:00:00')

# Connect to database
#### surveydown::sd_db_config()
#db <- sd_db_connect()

# original db
#data_raw <- sd_get_data(db)

data_raw <- read_csv(here('data', 'prolific_testing', 'data_raw.csv'))

# removing testing entries
data_raw <- data_raw %>%
  filter(!is.na(prolific_pid), nchar(prolific_pid) >= 10)

nrow(data_raw)

# Check for approvals

data_approval <- check_all_approvals(data_raw)

data_approval %>%
  count(status, reason)

# Some special variables:
# session_id = a unique ID for the Run - should be the same across all surveys
# time_start = time stamp when survey was started
# time_end   = time stamp when survey ended
# time_p_*** = Time page *** was reached
# time_q_*** = Time question *** was last answered

data <- data_raw %>% 
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
    current_page,
    next_veh_budget,
    attention_check_toyota,
    next_veh_style

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
    time_p_vehicle_pageQ1_button = ymd_hms(time_p_vehicle_pageQ1_button, tz = "UTC"),
    time_p_vehicle_pageQ2_button = ymd_hms(time_p_vehicle_pageQ2_button, tz = "UTC"),
    time_p_vehicle_pageQ3_button = ymd_hms(time_p_vehicle_pageQ3_button, tz = "UTC"),
    time_p_vehicle_pageQ4_button = ymd_hms(time_p_vehicle_pageQ4_button, tz = "UTC"),
    time_p_vehicle_pageQ5_button = ymd_hms(time_p_vehicle_pageQ5_button, tz = "UTC"),
    time_p_vehicle_pageQ6_button = ymd_hms(time_p_vehicle_pageQ6_button, tz = "UTC"),
###########################    
    time_q_vehicle_cbc_q0_button = ymd_hms(time_q_vehicle_cbc_q0_button, tz = "UTC"),
    time_q_vehicle_cbc_q1_button = ymd_hms(time_q_vehicle_cbc_q1_button, tz = "UTC"),
    time_q_vehicle_cbc_q2_button = ymd_hms(time_q_vehicle_cbc_q2_button, tz = "UTC"),
    time_q_vehicle_cbc_q3_button = ymd_hms(time_q_vehicle_cbc_q3_button, tz = "UTC"),
    time_q_vehicle_cbc_q4_button = ymd_hms(time_q_vehicle_cbc_q4_button, tz = "UTC"),
    time_q_vehicle_cbc_q5_button = ymd_hms(time_q_vehicle_cbc_q5_button, tz = "UTC"),
    time_q_vehicle_cbc_q6_button = ymd_hms(time_q_vehicle_cbc_q6_button, tz = "UTC"),
    ## Battery
    time_p_battery_pageQ1_button = ymd_hms(time_p_battery_pageQ1_button, tz = "UTC"),
    time_p_battery_pageQ2_button = ymd_hms(time_p_battery_pageQ2_button, tz = "UTC"),
    time_p_battery_pageQ3_button = ymd_hms(time_p_battery_pageQ3_button, tz = "UTC"),
    time_p_battery_pageQ4_button = ymd_hms(time_p_battery_pageQ4_button, tz = "UTC"),
    time_p_battery_pageQ5_button = ymd_hms(time_p_battery_pageQ5_button, tz = "UTC"),
    time_p_battery_pageQ6_button = ymd_hms(time_p_battery_pageQ6_button, tz = "UTC"),
###########################   
    time_q_battery_cbc_q1_button = ymd_hms(time_q_battery_cbc_q1_button, tz = "UTC"),
    time_q_battery_cbc_q2_button = ymd_hms(time_q_battery_cbc_q2_button, tz = "UTC"),
    time_q_battery_cbc_q3_button = ymd_hms(time_q_battery_cbc_q3_button, tz = "UTC"),
    time_q_battery_cbc_q4_button = ymd_hms(time_q_battery_cbc_q4_button, tz = "UTC"),
    time_q_battery_cbc_q5_button = ymd_hms(time_q_battery_cbc_q5_button, tz = "UTC"),
    time_q_battery_cbc_q6_button = ymd_hms(time_q_battery_cbc_q6_button, tz = "UTC"),

###########################   Calculations vehicle  ####

    calc_vehicle_intro_p_demo_p = as.numeric(time_p_cbc_demo - time_p_cbc_intro , units = "secs"),
    calc_vehicle_intro_p_cbc_q0 = as.numeric(time_q_vehicle_cbc_q0_button - time_p_cbc_intro , units = "secs"),
    calc_vehicle_cbc_q0_demo_p = -as.numeric(time_p_cbc_demo - time_q_vehicle_cbc_q0_button , units = "secs"),
    
    calc_vehicle_demo_p_pageQ1 = as.numeric(time_p_vehicle_pageQ1_button - time_p_cbc_demo , units = "secs"),
    calc_vehicle_demo_p_cbc_q1 = as.numeric(time_q_vehicle_cbc_q1_button - time_p_cbc_demo , units = "secs"),
    calc_vehicle_cbc_q1_pageQ1 = -as.numeric(time_p_vehicle_pageQ1_button - time_q_vehicle_cbc_q1_button , units = "secs"),
    
    calc_vehicle_pageQ1_pageQ2 = as.numeric(time_p_vehicle_pageQ2_button - time_p_vehicle_pageQ1_button , units = "secs"),
    calc_vehicle_pageQ1_cbc_q2 = as.numeric(time_q_vehicle_cbc_q2_button - time_p_vehicle_pageQ1_button , units = "secs"),
    calc_vehicle_cbc_q2_pageQ2 = -as.numeric(time_p_vehicle_pageQ2_button - time_q_vehicle_cbc_q2_button , units = "secs"),
    
    calc_vehicle_pageQ2_pageQ3 = as.numeric(time_p_vehicle_pageQ3_button - time_p_vehicle_pageQ2_button , units = "secs"),
    calc_vehicle_pageQ2_cbc_q3 = as.numeric(time_q_vehicle_cbc_q3_button - time_p_vehicle_pageQ2_button , units = "secs"),
    calc_vehicle_cbc_q3_pageQ3 = -as.numeric(time_p_vehicle_pageQ3_button - time_q_vehicle_cbc_q3_button , units = "secs"),
    
    calc_vehicle_pageQ3_pageQ4 = as.numeric(time_p_vehicle_pageQ4_button - time_p_vehicle_pageQ3_button , units = "secs"),
    calc_vehicle_pageQ3_cbc_q4 = as.numeric(time_q_vehicle_cbc_q4_button - time_p_vehicle_pageQ3_button , units = "secs"),
    calc_vehicle_cbc_q4_pageQ4 = -as.numeric(time_p_vehicle_pageQ4_button - time_q_vehicle_cbc_q4_button , units = "secs"),
    
    calc_vehicle_pageQ4_pageQ5 = as.numeric(time_p_vehicle_pageQ5_button - time_p_vehicle_pageQ4_button , units = "secs"),
    calc_vehicle_pageQ4_cbc_q5 = as.numeric(time_q_vehicle_cbc_q5_button - time_p_vehicle_pageQ4_button , units = "secs"),
    calc_vehicle_cbc_q5_pageQ5 = -as.numeric(time_p_vehicle_pageQ5_button - time_q_vehicle_cbc_q5_button , units = "secs"),
    
    calc_vehicle_pageQ5_pageQ6 = as.numeric(time_p_vehicle_pageQ6_button - time_p_vehicle_pageQ5_button , units = "secs"),
    calc_vehicle_pageQ5_cbc_q6 = as.numeric(time_q_vehicle_cbc_q6_button - time_p_vehicle_pageQ5_button , units = "secs"),
    calc_vehicle_cbc_q6_pageQ6 = -as.numeric(time_p_vehicle_pageQ6_button - time_q_vehicle_cbc_q6_button , units = "secs"),

###########################   Calculations battery  ####

    calc_battery_demo_p_pageQ1 = as.numeric(time_p_battery_pageQ1_button - time_p_battery_cbc_demo , units = "secs"),
    calc_battery_demo_p_cbc_q1 = as.numeric(time_q_battery_cbc_q1_button - time_p_battery_cbc_demo , units = "secs"),
    calc_battery_cbc_q1_pageQ1 = -as.numeric(time_p_battery_pageQ1_button - time_q_battery_cbc_q1_button , units = "secs"),
    
    calc_battery_pageQ1_pageQ2 = as.numeric(time_p_battery_pageQ2_button - time_p_battery_pageQ1_button , units = "secs"),
    calc_battery_pageQ1_cbc_q2 = as.numeric(time_q_battery_cbc_q2_button - time_p_battery_pageQ1_button , units = "secs"),
    calc_battery_cbc_q2_pageQ2 = -as.numeric(time_p_battery_pageQ2_button - time_q_battery_cbc_q2_button , units = "secs"),
    
    calc_battery_pageQ2_pageQ3 = as.numeric(time_p_battery_pageQ3_button - time_p_battery_pageQ2_button , units = "secs"),
    calc_battery_pageQ2_cbc_q3 = as.numeric(time_q_battery_cbc_q3_button - time_p_battery_pageQ2_button , units = "secs"),
    calc_battery_cbc_q3_pageQ3 = -as.numeric(time_p_battery_pageQ3_button - time_q_battery_cbc_q3_button , units = "secs"),
    
    calc_battery_pageQ3_pageQ4 = as.numeric(time_p_battery_pageQ4_button - time_p_battery_pageQ3_button , units = "secs"),
    calc_battery_pageQ3_cbc_q4 = as.numeric(time_q_battery_cbc_q4_button - time_p_battery_pageQ3_button , units = "secs"),
    calc_battery_cbc_q4_pageQ4 = -as.numeric(time_p_battery_pageQ4_button - time_q_battery_cbc_q4_button , units = "secs"),
    
    calc_battery_pageQ4_pageQ5 = as.numeric(time_p_battery_pageQ5_button - time_p_battery_pageQ4_button , units = "secs"),
    calc_battery_pageQ4_cbc_q5 = as.numeric(time_q_battery_cbc_q5_button - time_p_battery_pageQ4_button , units = "secs"),
    calc_battery_cbc_q5_pageQ5 = -as.numeric(time_p_battery_pageQ5_button - time_q_battery_cbc_q5_button , units = "secs"),
    
    calc_battery_pageQ5_pageQ6 = as.numeric(time_p_battery_pageQ6_button - time_p_battery_pageQ5_button , units = "secs"),
    calc_battery_pageQ5_cbc_q6 = as.numeric(time_q_battery_cbc_q6_button - time_p_battery_pageQ5_button , units = "secs"),
    calc_battery_cbc_q6_pageQ6 = -as.numeric(time_p_battery_pageQ6_button - time_q_battery_cbc_q6_button , units = "secs"),

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
  select (-starts_with('time'))

#######################################################################

data <- data %>%
  mutate(
    budget = case_when(
      next_veh_budget %in% c("5000", "10000", "15000", "20000") ~ 'low',
      TRUE ~ 'high'
    )
  )

nrow(data)

# Drop those who missed attention checks
data <- data %>%
  # Bot
  filter(is.na(attention_check_toyota))

nrow(data)

# Drop people who got screened out

data <- data %>%
  filter(!is.na(current_page), current_page == "end") %>%
  
  # Drop those who completed before the adjustments
  #filter(time_start >= pilot_start) %>% 
  #filter(time_start <= pilot_end) %>% 
  
  select(-current_page)

nrow(data)

# Drop people whose next vehicle is NUll

data <- data %>% 
  filter(!is.na(next_veh_style ))
# Save

write_csv(
  data,
  here(
    "data",
    "prolific_testing",
    "timestamps.csv"
  )
)

data1 <- data %>% 
  select(starts_with('calc_vehicle_'))


data2 <- data %>% 
  select(starts_with('calc_battery_'))

hist(data2$calc_battery_cbc_q6_pageQ6)

bq6 <- data2 %>% 
  ggplot(aes(x=calc_battery_cbc_q6_pageQ6)) +
  geom_histogram(binwidth = 3,  fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 30") +
  coord_cartesian(xlim = c(0, 30))+
  theme(
    plot.title = element_text(size=15)
  )


bq1 <- data2 %>% 
  ggplot(aes(x=calc_battery_cbc_q1_pageQ1)) +
  geom_histogram(binwidth = 3,  fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 30") +
  coord_cartesian(xlim = c(0, 30))+
  theme(
    plot.title = element_text(size=15)
  )

bq2 <- data2 %>% 
  ggplot(aes(x=calc_battery_cbc_q2_pageQ2)) +
  geom_histogram(binwidth = 3,  fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 30") +
  coord_cartesian(xlim = c(0, 30))+
  theme(
    plot.title = element_text(size=15)
  )

bq3 <- data2 %>% 
  ggplot(aes(x=calc_battery_cbc_q3_pageQ3)) +
  geom_histogram(binwidth = 3,  fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 30") +
  coord_cartesian(xlim = c(0, 30))+
  theme(
    plot.title = element_text(size=15)
  )


bq4 <- data2 %>% 
  ggplot(aes(x=calc_battery_cbc_q4_pageQ4)) +
  geom_histogram(binwidth = 3,  fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 30") +
  coord_cartesian(xlim = c(0, 30))+
  theme(
    plot.title = element_text(size=15)
  )

bq5 <- data2 %>% 
  ggplot(aes(x=calc_battery_cbc_q5_pageQ5)) +
  geom_histogram(binwidth = 3,  fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 30") +
  coord_cartesian(xlim = c(0, 30))+
  theme(
    plot.title = element_text(size=15)
  )


bq1 + bq2 / bq3 + bq4 / bq5 + bq6
