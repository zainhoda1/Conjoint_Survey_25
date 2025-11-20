
library(DT)
library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(janitor)




data <- read.csv(here('round2_rows.csv'))
# dropouts <- read_excel(here('new_dropouts.xlsx')) %>%
#   clean_names()


data_joined <- data
 # inner_join(dropouts, by = c("psid" = "drop_out_i_ds"))



# data_joined <- data %>%
#   mutate(get_start_datetime = ymd_hms(time_start, tz = "UTC")) %>%
#   filter(get_start_datetime > as.POSIXct("2025-10-21 00:00:00", tz = "UTC"))



# user_status <- data_joined %>%
#   group_by(current_page) %>%
#   summarise(count = n()) %>%
#   arrange(desc(count))

data$current_page[data$current_page == ""] <- "first_page"

# dropout_results <- data_joined %>%
#   mutate(get_start_datetime = ymd_hms(time_start, tz = "UTC"),
#          get_start_date = as_date(get_start_datetime)) %>%
#   filter(get_start_datetime > as.POSIXct("2025-11-11 00:00:00", tz = "UTC")) %>%
#   #filter(get_start_datetime < as.POSIXct("2025-10-27 00:00:00", tz = "UTC")) %>%
#   mutate(
#     # survey_status = case_when(current_page == 'screenout' ~ 'screenout',
#     #                                current_page == 'end' ~ 'successfully completed',
#     #                                .default = 'left the survey'),
#     page_position = case_when(current_page == 'first_page' ~ 'first_page',
#                                    current_page == 'consent' ~ 'second_page',
#                                    current_page == 'screenout' ~ 'screenout',
#                                    .default = 'other_pages')) %>%
#   #group_by(survey_status, get_start_date ) %>%
#   group_by(page_position, get_start_date ) %>%
#   summarise(count = n()) %>%
#   pivot_wider(names_from = page_position, values_from = count)



all_results <- data  %>%
  mutate(get_start_datetime = ymd_hms(time_start, tz = "UTC"),
         get_start_date = as_date(get_start_datetime)) %>%
  filter(get_start_datetime > as.POSIXct("2025-11-11 00:00:00", tz = "UTC")) %>%
  #filter(get_start_datetime < as.POSIXct("2025-10-27 00:00:00", tz = "UTC")) %>%
  mutate(
    # survey_status = case_when(current_page == 'screenout' ~ 'screenout',
    #                                current_page == 'end' ~ 'successfully completed',
    #                                .default = 'left the survey')
    # page_position = case_when(current_page == 'first_page' ~ 'first_page',
    #                           current_page == 'consent' ~ 'second_page',
    #                           current_page == 'screenout' ~ 'screenout',
    #                           .default = 'other_pages')
    ) %>%
  #group_by(survey_status, get_start_date ) %>%
  #group_by(page_position, get_start_date ) %>%
  group_by(current_page ) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = current_page, values_from = count)



summary_results <- data  %>%
  mutate(get_start_datetime = ymd_hms(time_start, tz = "UTC"),
         get_start_date = as_date(get_start_datetime)) %>%
  filter(get_start_datetime > as.POSIXct("2025-11-11 00:00:00", tz = "UTC")) %>%
  #filter(get_start_datetime < as.POSIXct("2025-10-27 00:00:00", tz = "UTC")) %>%
  mutate(
    survey_status = case_when(current_page == 'screenout' ~ 'screenout',
                                   current_page == 'end' ~ 'successfully completed',
                                   .default = 'left the survey')
    # page_position = case_when(current_page == 'first_page' ~ 'first_page',
    #                           current_page == 'consent' ~ 'second_page',
    #                           current_page == 'screenout' ~ 'screenout',
    #                           .default = 'other_pages')
  ) %>%
  group_by(survey_status ) %>%
  #group_by(page_position, get_start_date ) %>%
  #group_by(current_page ) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = survey_status, values_from = count)


data %>%



time_start = ymd_hms(time_start, tz = "UTC"),
time_end = ymd_hms(time_end, tz = "UTC"),
time_total = as.numeric(time_end - time_start, units = "secs"),

