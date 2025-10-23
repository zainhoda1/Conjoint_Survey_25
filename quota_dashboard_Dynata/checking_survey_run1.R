
library(DT)
library(tidyverse)
library(lubridate)
library(here)
library(readxl)


data <- read.csv(here('round2_rows.csv'))
# dropouts <- read_excel(here('dropouts.xlsx'))
#
# unique(dropouts$`Drop Out IDs`)
#
# data_joined <- data %>%
#   inner_join(dropouts, by = c("psid" = "Drop Out IDs"))
#
#
#
# data_joined <- data_joined %>%
#   mutate(get_start_datetime = ymd_hms(time_start, tz = "UTC")) %>%
#   filter(get_start_datetime > as.POSIXct("2025-10-02 01:22:57", tz = "UTC"))



user_status <- data_joined %>%
  group_by(current_page) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


data %>%
  mutate(get_start_datetime = ymd_hms(time_start, tz = "UTC")) %>%
  filter(get_start_datetime > as.POSIXct("2025-10-10 20:00:00", tz = "UTC")) %>%
  group_by(current_page) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



<<<<<<< Updated upstream

=======
>>>>>>> Stashed changes
