# Approval checking functions ----

# Check for incomplete vehicle CBC questions
check_vehicle_incomplete <- function(data) {
  data %>%
    mutate(
      vehicle_incomplete = is.na(vehicle_cbc_q1_button) |
        is.na(vehicle_cbc_q2_button) |
        is.na(vehicle_cbc_q3_button) |
        is.na(vehicle_cbc_q4_button) |
        is.na(vehicle_cbc_q5_button) |
        is.na(vehicle_cbc_q6_button)
    ) %>%
    select(prolific_pid, vehicle_incomplete)
}

# Check for incomplete battery CBC questions
check_battery_incomplete <- function(data) {
  data %>%
    mutate(
      battery_incomplete = is.na(battery_cbc_q1_button) |
        is.na(battery_cbc_q2_button) |
        is.na(battery_cbc_q3_button) |
        is.na(battery_cbc_q4_button) |
        is.na(battery_cbc_q5_button) |
        is.na(battery_cbc_q6_button)
    ) %>%
    select(prolific_pid, battery_incomplete)
}

# Check for same answer on all vehicle CBC questions
check_vehicle_same_answer <- function(data) {
  data %>%
    mutate(
      vehicle_all_same = (vehicle_cbc_q1_button == vehicle_cbc_q2_button) &
        (vehicle_cbc_q2_button == vehicle_cbc_q3_button) &
        (vehicle_cbc_q3_button == vehicle_cbc_q4_button) &
        (vehicle_cbc_q4_button == vehicle_cbc_q5_button) &
        (vehicle_cbc_q5_button == vehicle_cbc_q6_button),
      vehicle_all_option4 = vehicle_all_same & (vehicle_cbc_q1_button == "4")
    ) %>%
    select(prolific_pid, vehicle_all_same, vehicle_all_option4)
}

# Check for same answer on all battery CBC questions
check_battery_same_answer <- function(data) {
  data %>%
    mutate(
      battery_all_same = (battery_cbc_q1_button == battery_cbc_q2_button) &
        (battery_cbc_q2_button == battery_cbc_q3_button) &
        (battery_cbc_q3_button == battery_cbc_q4_button) &
        (battery_cbc_q4_button == battery_cbc_q5_button) &
        (battery_cbc_q5_button == battery_cbc_q6_button),
      battery_all_option4 = battery_all_same & (battery_cbc_q1_button == "4")
    ) %>%
    select(prolific_pid, battery_all_same, battery_all_option4)
}

# Check for failed attention check (bot check)
check_attention_check <- function(data) {
  data %>%
    mutate(
      failed_attention_check = !is.na(attention_check_toyota)
    ) %>%
    select(prolific_pid, failed_attention_check)
}

# Check for screened out respondents
check_screened_out <- function(data) {
  data %>%
    mutate(
      screened_out = is.na(current_page) | current_page != "end"
    ) %>%
    select(prolific_pid, screened_out)
}

# Combined approval check function
check_all_approvals <- function(data) {
  # Initialize approval status data frame
  approval_status <- data %>%
    select(prolific_pid) %>%
    mutate(
      status = "good",
      reason = "Passed all checks"
    )

  # Track remaining good IDs to check
  remaining_data <- data

  # Check 1: Failed attention check (bot)
  attention_check_result <- check_attention_check(remaining_data)
  failed_ids <- attention_check_result %>%
    filter(failed_attention_check) %>%
    pull(prolific_pid)

  if (length(failed_ids) > 0) {
    approval_status <- approval_status %>%
      mutate(
        status = ifelse(prolific_pid %in% failed_ids, "bad", status),
        reason = ifelse(
          prolific_pid %in% failed_ids,
          "Failed attention check (bot)",
          reason
        )
      )
    remaining_data <- remaining_data %>%
      filter(!prolific_pid %in% failed_ids)
  }

  # Check 2: Screened out
  screened_out_check <- check_screened_out(remaining_data)
  screened_ids <- screened_out_check %>%
    filter(screened_out) %>%
    pull(prolific_pid)

  if (length(screened_ids) > 0) {
    approval_status <- approval_status %>%
      mutate(
        status = ifelse(prolific_pid %in% screened_ids, "bad", status),
        reason = ifelse(
          prolific_pid %in% screened_ids,
          "Screened out (did not complete survey)",
          reason
        )
      )
    remaining_data <- remaining_data %>%
      filter(!prolific_pid %in% screened_ids)
  }

  # Check 3: Incomplete vehicle CBC questions
  vehicle_incomplete_check <- check_vehicle_incomplete(remaining_data)
  vehicle_incomplete_ids <- vehicle_incomplete_check %>%
    filter(vehicle_incomplete) %>%
    pull(prolific_pid)

  if (length(vehicle_incomplete_ids) > 0) {
    approval_status <- approval_status %>%
      mutate(
        status = ifelse(
          prolific_pid %in% vehicle_incomplete_ids,
          "bad",
          status
        ),
        reason = ifelse(
          prolific_pid %in% vehicle_incomplete_ids,
          "Incomplete vehicle CBC questions",
          reason
        )
      )
    remaining_data <- remaining_data %>%
      filter(!prolific_pid %in% vehicle_incomplete_ids)
  }

  # Check 4: Incomplete battery CBC questions
  battery_incomplete_check <- check_battery_incomplete(remaining_data)
  battery_incomplete_ids <- battery_incomplete_check %>%
    filter(battery_incomplete) %>%
    pull(prolific_pid)

  if (length(battery_incomplete_ids) > 0) {
    approval_status <- approval_status %>%
      mutate(
        status = ifelse(
          prolific_pid %in% battery_incomplete_ids,
          "bad",
          status
        ),
        reason = ifelse(
          prolific_pid %in% battery_incomplete_ids,
          "Incomplete battery CBC questions",
          reason
        )
      )
    remaining_data <- remaining_data %>%
      filter(!prolific_pid %in% battery_incomplete_ids)
  }

  # Check 5: Same answer for all vehicle CBC questions
  vehicle_same_check <- check_vehicle_same_answer(remaining_data)
  vehicle_same_bad_ids <- vehicle_same_check %>%
    filter(vehicle_all_same & !vehicle_all_option4) %>%
    pull(prolific_pid)

  if (length(vehicle_same_bad_ids) > 0) {
    approval_status <- approval_status %>%
      mutate(
        status = ifelse(prolific_pid %in% vehicle_same_bad_ids, "bad", status),
        reason = ifelse(
          prolific_pid %in% vehicle_same_bad_ids,
          "Same answer for all vehicle CBC questions",
          reason
        )
      )
    remaining_data <- remaining_data %>%
      filter(!prolific_pid %in% vehicle_same_bad_ids)
  }

  # Note: Check for option 4 pattern (may be acceptable)
  vehicle_option4_ids <- vehicle_same_check %>%
    filter(vehicle_all_same & vehicle_all_option4) %>%
    pull(prolific_pid)

  if (length(vehicle_option4_ids) > 0) {
    approval_status <- approval_status %>%
      mutate(
        reason = ifelse(
          prolific_pid %in% vehicle_option4_ids,
          "All vehicle questions answered with option 4 (may be acceptable)",
          reason
        )
      )
  }

  # Check 6: Same answer for all battery CBC questions
  battery_same_check <- check_battery_same_answer(remaining_data)
  battery_same_bad_ids <- battery_same_check %>%
    filter(battery_all_same & !battery_all_option4) %>%
    pull(prolific_pid)

  if (length(battery_same_bad_ids) > 0) {
    approval_status <- approval_status %>%
      mutate(
        status = ifelse(prolific_pid %in% battery_same_bad_ids, "bad", status),
        reason = ifelse(
          prolific_pid %in% battery_same_bad_ids,
          "Same answer for all battery CBC questions",
          reason
        )
      )
    remaining_data <- remaining_data %>%
      filter(!prolific_pid %in% battery_same_bad_ids)
  }

  # Note: Check for option 4 pattern (may be acceptable)
  battery_option4_ids <- battery_same_check %>%
    filter(battery_all_same & battery_all_option4) %>%
    pull(prolific_pid)

  if (length(battery_option4_ids) > 0) {
    approval_status <- approval_status %>%
      mutate(
        reason = ifelse(
          prolific_pid %in% battery_option4_ids,
          "All battery questions answered with option 4 (may be acceptable)",
          reason
        )
      )
  }

  return(distinct(approval_status))
}
