source(here::here('code', 'setup.R'))

# ----Load the data set----

data <- data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint.parquet"
))
# head(data)

# ----Socio-demographics----

data <- data %>%
  mutate(
    age_num = 2025 - birth_year,
    gender_cate = case_when(
      gender == "prefer_not_answer" ~ "prefer_not_answer",
      gender %notin% c("male", "female") ~ "other",
      T ~ gender
    ),
    ethnicity_cate = case_when(
      ethnicity == "no" ~ "non-hispanic",
      T ~ "hispanic"
    ),
    race_cate = case_when(
      race == "white" ~ "white_only",
      race == "african_american" ~ "african_american_only",
      T ~ "other"
    ),
    education_cate = case_when(
      education == "prefer_not_answer" ~ "prefer_not_answer",
      education %in% c("below_high_school", "high_school") ~ "high_school",
      education %in%
        c("college_less", "college_more", "technical") ~ "some_college",
      education %in% c("bachelor", "Postgraduate courses") ~ "bachelor",
      T ~ "graduate"
    ),
    student_cate = case_when(
      str_detect(employment, "student") ~ "student",
      employment == "prefer_not_answer" ~ "prefer_not_answer",
      T ~ "non-student"
    ),
    employment_cate = case_when(
      employment == "prefer_not_answer" ~ "prefer_not_answer",
      str_detect(employment, "full_time") ~ "full_time",
      str_detect(employment, "part_time") ~ "part_time",
      T ~ "not_employed"
    ),
    hhincome_num = as.numeric(hh_income),
    hhsize_num = case_when(
      hh_size == "more_than_5" ~ 6,
      T ~ as.numeric(hh_size)
    ),
    hhtenure_cate = case_when(
      housing_tenure %in% c("own", "rent") ~ housing_tenure,
      housing_tenure == "do_not_know" ~ "prefer_not_answer",
      T ~ "other"
    ),
    hhtype_cate = case_when(
      housing_type %in% c("sf_detached", "sf_attached") ~ housing_type,
      str_detect(housing_type, "apart") ~ "apart",
      T ~ "other"
    )
  ) %>%
  mutate(final_weights = 1)

# ----Attitudes----
## ----BEV (factor analysis)----
### Explortory Factor analysis
### (http://www.di.fc.ul.pt/~jpn/r/factoranalysis/factoranalysis.html)
att <- data %>%
  # filter(attitudes_2_a_attention_check_agree=="strongly_agree")%>%
  select(!attitudes_2_a_attention_check_agree) %>%
  select(
    session_id,
    attitudes_1_a_ev_norm:attitudes_2_a_ev_battery_function,
    -(contains("battery"))
  ) %>%
  mutate(across(
    starts_with("attitudes_"),
    ~ recode_values(
      .,
      "strongly_disagree" ~ 1L,
      "somewhat_disagree" ~ 2L,
      "neutral" ~ 3L,
      "somewhat_agree" ~ 4L,
      "strongly_agree" ~ 5L
    )
  ))

# summary(att)
att <- as.data.frame(att[complete.cases(att), ])


# ##Bartlett’s test is highly significant,  p-val < .05, and therefore factor analysis is appropriate. Then, we could get the determinant
# bartl_test = cortest.bartlett(att[,c(2:ncol(att))])
# bartl_test
#
# ## The determinant value is greater than the necessary value of 0.00001. As such, the determinant does not seem problematic.
# ncol<-as.numeric(ncol(att[,c(2:ncol(att))]))
# corr <- round(cor(att[,c(2:ncol)]), 1)
# det(corr)
#
#
# # Determine Number of Factors to Extract
# ev <- eigen(cor(att[,c(2:ncol(att))])) # get eigenvalues
# ap <- parallel(subject=nrow(att[,c(2:ncol(att))]),var=ncol(att[,c(2:ncol(att))]),
#                rep=100,cent=.05)
# nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
# plotnScree(nS)

fit <- factanal(
  att[, c(2:ncol(att))],
  2,
  rotation = "promax",
  score = "Bartlett"
)


# print(fit, digits = 2, cutoff = 0.3, sort = TRUE)

l <- fit$loadings
# l<-fit_oblimin$loadings
l.table <- data.frame(matrix(
  as.numeric(l),
  attributes(l)$dim,
  dimnames = attributes(l)$dimnames
))
l.table <- cbind(row.names(l.table), l.table)


######### Factor Loading
scores <- data.frame(fit$scores)
att_a_FC <- cbind(att, scores) %>%
  select(session_id, Factor1, Factor2)

colnames(att_a_FC) <- c("session_id", "FA_EV_benefit", "FA_EV_anxiety")

data$FA_EV_benefit <- NULL
data$FA_EV_anxiety <- NULL

data <- data %>%
  merge(att_a_FC, by = "session_id", all.x = T)

## ----Other----
data <- data %>%
  mutate(
    ATT_range_anxiety = attitudes_1_b_ev_range,
    ATT_EVB_environment = attitudes_2_a_ev_battery_environment,
    ATT_EVB_function = attitudes_2_a_ev_battery_function,
    ATT_techsavvy = attitudes_2_b_tech_savvy,
    ATT_risktaker = attitudes_2_b_risk_taker,
    ATT_price_sensitive = attitudes_2_b_price_sensitive,
    ATT_climate = climate_change,
    ATT_political = political_view,
    ATT_voting = party_voting
  )

# ----Knowledge----
data <- data %>%
  mutate(
    knowledge_gas = case_when(
      run_on_gasoline == "hev, phev" ~ 2,
      str_detect(run_on_gasoline, "hev") |
        str_detect(run_on_gasoline, "phev") ~ 1,
      T ~ 0
    ),
    knowledge_plugin = case_when(
      plugged_in == "phev, bev" ~ 2,
      str_detect(plugged_in, "bev") | str_detect(run_on_gasoline, "phev") ~ 1,
      T ~ 0
    ),
    knowledge_ev = case_when(know_electric_vehicle == "yes" ~ 1, T ~ 0), # need more data cleaning
    knowledge_subsidy = case_when(max_subsidy == "7500" ~ 1, T ~ 0)
  )

# ----Vehicle Attributes----
data <- data %>%
  mutate(
    Veh_hh_count = household_veh_count,
    Veh_hh_fuel = case_when(
      is.na(household_veh_fuel) ~ NA,
      household_veh_count == 0 ~ "no_vehicle",
      # any BEV in household → highest priority
      str_detect(household_veh_fuel, "bev") ~ "has_bev",
      # HEV/PHEV but no BEV
      str_detect(household_veh_fuel, "phev") |
        str_detect(household_veh_fuel, "hev") ~ "has_phev_hev",
      # pure ICEV
      str_detect(household_veh_fuel, "icev") ~ "icev_only",
      # catch-all: "other", unknown
      TRUE ~ "other"
    ),
    EV_charger = charger_access,
    EV_neighbor = neighbor_ev_info,

    Veh_primary_fuel = case_when(
      is.na(primary_veh_fuel) ~ household_veh_fuel,
      T ~ primary_veh_fuel
    ),
    Veh_primary_fuel = case_when(
      Veh_primary_fuel == "bev" ~ "bev",
      str_detect(Veh_primary_fuel, "phev") |
        str_detect(Veh_primary_fuel, "hev") ~ "phev_hev",
      TRUE ~ "icev"
    ),
    Veh_primary_refuel_monthly = primary_veh_refuel,
    Veh_primary_range = primary_veh_range
  )

# ----Information treatment----
data <- data %>%
  mutate(
    battery_info_treat = case_when(
      prime_group_label == "prime_long" ~ 1,
      T ~ 0
    )
  )


# ---- Flag issues ----
data <- data %>%
  mutate(
    flag_attention_check = attitudes_2_a_attention_check_agree !=
      "strongly_agree",
    survey_duration = difftime(time_end, time_start, units = "sec"),
    flag_speeding = survey_duration < 300,
    flag_veh_inconsistent = case_when(
      primary_veh_fuel == "bev" &
        !str_detect(household_veh_fuel, "bev") ~ 1,
      primary_veh_fuel == "hev" &
        !str_detect(household_veh_fuel, "hev") ~ 1,
      primary_veh_fuel == "phev" & !str_detect(household_veh_fuel, "phev") ~ 1,
      primary_veh_fuel == "icev" &
        !str_detect(household_veh_fuel, "icev") ~ 1,
      T ~ 0
    ),
    flag_veh_knowledge_inconsistent = case_when(
      primary_veh_fuel == "hev" &
        !str_detect(run_on_gasoline, "hev") ~ 1,
      primary_veh_fuel == "phev" &
        (!str_detect(run_on_gasoline, "phev") |
          !str_detect(plugged_in, "phev")) ~ 1,
      primary_veh_fuel == "bev" &
        !str_detect(plugged_in, "bev") ~ 1,
      T ~ 0
    ),
    flag_bev_name_dontknow = case_when(
      primary_veh_fuel == "bev" &
        know_electric_vehicle == "no" ~ 1,
      T ~ 0
    ),
    hh_veh_household_size = case_when(
      !is.na(household_veh_count) & !is.na(hhsize_num) ~ household_veh_count /
        hhsize_num,
      T ~ NA
    ),
    flag_veh_household_size = case_when(
      hh_veh_household_size > 3 ~ 1,
      T ~ 0
    ),
    flag_veh_income = case_when(
      hh_veh_household_size > 2 &
        hhincome_num < 50000 ~ 1,
      T ~ 0
    ),
    flag_budget_income = case_when(
      !is.na(next_veh_budget) &
        is.na(hhincome_num) &
        next_veh_budget - hhincome_num > 20000 ~ 1,
      T ~ 0
    ),
    flag_opentext = case_when(
      str_detect(
        next_veh_nobev,
        regex(
          "\\bnone\\b|\\bn/a\\b|\\bna\\b|\\bidk\\b",
          ignore_case = TRUE
        )
      ) ~ 1,
      str_detect(
        attention_check_survey_content,
        regex(
          "\\bnone\\b|\\bn/a\\b|\\bna\\b|\\bidk\\b",
          ignore_case = TRUE
        )
      ) ~ 1,
      T ~ 0
    ),
    flag_total = rowSums(select(., starts_with("flag_")), na.rm = TRUE)
  ) %>%
  filter(flag_total <= 1) # remove respondents with 3 or more flags (sensitivity check: can also try 2+ flags)


# b <- data %>%
#   select(-starts_with("time_")) %>%
#   filter(flag_total==2)

# # summarize open-text and flag low-quality answers
# opentext_next_veh_nobev <- data %>%
#   select(respID, next_veh_nobev)

# opentext_survey <- data %>%
#   select(respID, attention_check_survey_content)

# library(stringdist)

# flag_opentext <- function(
#   df,
#   id_col = "respID",
#   text_col,
#   near_dup_thresh = 0.15,
#   dup_count_cutoff = 3
# ) {
#   df2 <- df %>%
#     select(all_of(id_col), text = all_of(text_col)) %>%
#     filter(!is.na(text)) %>%
#     mutate(
#       n_chars = nchar(text),
#       n_words = str_count(text, "\\S+"),
#       repeated_chars = str_detect(text, "(.)\\1{4,}"),
#       alpha_prop = ifelse(
#         n_chars > 0,
#         str_count(text, "[A-Za-z]") / n_chars,
#         0
#       ),
#       generic = str_detect(
#         str_to_lower(text),
#         "^\\s*(survey|about survey|about this survey|no idea|no|none|n/a|na|idk|dont know|don't know|nothing|test|asdf|qwerty)\\b"
#       ),
#       very_short = n_words <= 2 | n_chars <= 5,
#       nonalpha_heavy = alpha_prop < 0.5
#     )
#   if (nrow(df2) == 0) {
#     return(tibble::tibble(respID = character(), text = character()))
#   }
#   # exact duplicate counts
#   dup_counts <- df2 %>%
#     group_by(text) %>%
#     summarise(n_same = n(), .groups = "drop")
#   df2 <- df2 %>% left_join(dup_counts, by = "text")
#   # near-duplicate counts via cosine distance
#   texts <- df2$text
#   dist_mat <- stringdist::stringdistmatrix(texts, texts, method = "cosine")
#   neardup_counts <- rowSums(dist_mat < near_dup_thresh) - 1
#   df2 <- df2 %>% mutate(neardup_count = neardup_counts)
#   # flags
#   df2 <- df2 %>%
#     mutate(
#       flag_exact_duplicate_majority = n_same >= dup_count_cutoff,
#       flag_many_near_duplicates = neardup_count >= dup_count_cutoff,
#       flag_gibberish = repeated_chars | nonalpha_heavy,
#       flag_low_content = very_short | generic,
#       flag_bad = flag_exact_duplicate_majority |
#         flag_many_near_duplicates |
#         flag_gibberish |
#         flag_low_content
#     ) %>%
#     rename(respID = all_of(id_col))
#   df2
# }

# # run flags for both questions (uses existing objects opentext_next_veh_nobev and opentext_survey)
# flags_next <- flag_opentext(
#   opentext_next_veh_nobev,
#   text_col = "next_veh_nobev"
# )
# flags_survey <- flag_opentext(
#   opentext_survey,
#   text_col = "attention_check_survey_content"
# )

# # combine per-respondent: mark respondent bad if flagged on either question
# bad_combined <- bind_rows(
#   flags_next %>% select(respID, source = "text", everything()),
#   flags_survey %>% select(respID, source = "text", everything())
# ) %>%
#   group_by(respID) %>%
#   summarize(
#     flagged_in_next = any(source == "text" & flag_bad),
#     flagged_in_survey = any(source == "text" & flag_bad),
#     flagged_any = flagged_in_next | flagged_in_survey,
#     n_flags = sum(flag_bad),
#     .groups = "drop"
#   ) %>%
#   arrange(desc(flagged_any), desc(n_flags))

# a<-list(
#   flags_next = flags_next,
#   flags_survey = flags_survey,
#   bad_respondents = bad_combined
# )

# b<-data %>%
#   select(respID, next_veh_nobev, attention_check_survey_content) %>%
#   filter(! (is.na(next_veh_nobev) & is.na(attention_check_survey_content))) %>%
#   right_join(bad_combined, by = c("respID")) %>%
#   mutate(flagged_any = ifelse(is.na(flagged_any), FALSE, flagged_any))

# ----Data Output----
write_parquet(
  data,
  here(
    "data",
    "dynata_prolific_joint",
    "data_clean_variables.parquet"
  )
)
