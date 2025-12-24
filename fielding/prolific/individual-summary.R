library(tidyverse)

options(dplyr.width = Inf)

files <- list.files("prolific-studies", full.names = TRUE)

read_file <- function(file) {
  study <- str_replace(str_split(file, '_')[[1]][4], '.csv', '')
  read_csv(file) %>%
    janitor::clean_names() %>%
    filter(status %in% c("APPROVED", "SCREENED OUT")) %>%
    mutate(
      date = date(completed_at),
      description = "",
      study = study
    ) %>%
    select(date, description = status, participant_id, study) %>%
    mutate(
      reward = ifelse(description == "APPROVED", 2.50, 0.14),
      fee = ifelse(description == "APPROVED", 0.83, 0.05)
    )
}

results <- map_df(files, read_file)
write_csv(results, "individual-summary.csv")
