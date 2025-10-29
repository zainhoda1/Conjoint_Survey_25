source(here::here('code', 'setup.R'))

# ----Load the data set----

data <- read_csv(here(
  "data",
  "pilot",
  "survey_data.csv"
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
    ~ case_match(
      .,
      "strongly_disagree" ~ 1,
      "somewhat_disagree" ~ 2,
      "neutral" ~ 3,
      "somewhat_agree" ~ 4,
      "strongly_agree" ~ 5,
      .default = NA_integer_
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


# print(fit, digits=2, cutoff=0.3, sort=TRUE)

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
    ATT_EVB_environment = attitudes_2_a_ev_battery_environment,
    ATT_EVB_function = attitudes_2_a_ev_battery_function,
    ATT_techsavvy = attitudes_2_b_tech_savvy,
    ATT_risktaker = attitudes_2_b_risk_taker,
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
      household_veh_fuel == "other" ~ "other",
      household_veh_fuel == "icev" ~ "icev_only",
      str_detect(household_veh_fuel, "icev") ~ "ev_mix",
      T ~ "ev_only"
    ),
    EV_charger = charger_access,
    EV_neighbor = neighbor_ev_info,

    Veh_primary_fuel = case_when(
      is.na(primary_veh_fuel) ~ household_veh_fuel,
      T ~ primary_veh_fuel
    )
  )


# ----Data Output----
write_csv(
  data,
  here(
    "data",
    "main",
    "data_covariates.csv"
  )
)
