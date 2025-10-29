source(here::here('code', 'setup.R'))

# ----Load the data set----

data <- read_csv(here(
  "data",
  "main",
  "data_clean_variables.csv"
))

summary_dt <- data %>%
  select(
    ends_with("_num"),
    ends_with("_cate"),
    starts_with("FA_"),
    starts_with("ATT_"),
    starts_with("knowledge_"),
    starts_with("Veh_"),
    starts_with("EV_"),

    next_veh_fuel_new_bev,
    next_veh_fuel_used_bev,

    final_weights
  )

# ----Recode----
summary_dt <- summary_dt %>%
  mutate(across(
    c(
      ends_with("_cate"),
      starts_with("ATT_"),
      starts_with("knowledge_"),
      Veh_hh_fuel,
      starts_with("EV_"),
      next_veh_fuel_used_bev,
      next_veh_fuel_new_bev
    ),
    factor
  ))

varfactor_names <- names(select(
  data,
  ends_with("_cate"),
  starts_with("ATT_"),
  starts_with("knowledge_"),
  Veh_hh_fuel,
  starts_with("EV_"),
  next_veh_fuel_used_bev,
  next_veh_fuel_new_bev
))

# lapply(summary_dt[varfactor_names], levels)

summary_dt <- summary_dt %>%
  mutate(
    next_veh_fuel_used_bev = factor(
      next_veh_fuel_used_bev,
      levels = c(
        "very_unlikely",
        "somewhat_unlikely",
        "neutral",
        "somewhat_likely",
        "very_likely"
      ),
      ordered = TRUE
    ),
    next_veh_fuel_new_bev = factor(
      next_veh_fuel_new_bev,
      levels = c(
        "very_unlikely",
        "somewhat_unlikely",
        "neutral",
        "somewhat_likely",
        "very_likely"
      ),
      ordered = TRUE
    ),
    gender_cate = factor(
      gender_cate,
      levels = c("female", "male", "other"),
      labels = c("female", "male", "gender_other"),
      ordered = TRUE
    ),
    ethnicity_cate = factor(
      ethnicity_cate,
      labels = c("hispanic", "non-hispanic"),
      ordered = TRUE
    ),
    race_cate = factor(
      race_cate,
      levels = c("white_only", "african_american_only", "other"),
      labels = c("White only", "African American only", "race_other"),
      ordered = TRUE
    ),
    education_cate = factor(
      education_cate,
      levels = c("high_school", "some_college", "bachelor", "graduate"),
      labels = c(
        "high school or under",
        "some college",
        "bachelor",
        "master or above"
      ),
      ordered = TRUE
    ),
    student_cate = factor(
      student_cate,
      levels = c("student", "non-student", "prefer_not_answer"),
      labels = c("student", "non-student", "stu_prefer not to answer"),
      ordered = TRUE
    ),
    employment_cate = factor(
      employment_cate,
      levels = c("full_time", "part_time", "not_employed", "prefer_not_answer"),
      labels = c(
        "full-time",
        "part-time",
        "not employed",
        "employ_prefer not to answer"
      ),
      ordered = TRUE
    ),
    hhtenure_cate = factor(
      hhtenure_cate,
      levels = c("own", "rent", "other"),
      labels = c("own", "rent", "hhtenure_other"),
      ordered = TRUE
    ),
    hhtype_cate = factor(
      hhtype_cate,
      levels = c("sf_detached", "sf_attached", "apart", "other"),
      labels = c(
        "detached single family house",
        "attached single family house",
        "apartment",
        "hhtype_other"
      ),
      ordered = TRUE
    ),
    ATT_EVB_environment = factor(
      ATT_EVB_environment,
      levels = c(
        "strongly_disagree",
        "somewhat_disagree",
        "neutral",
        "somewhat_agree",
        "strongly_agree"
      ),
      labels = c(
        "EVBe_strongly disagree",
        "EVBe_somewhat disagree",
        "EVBe_neither agree nor disagree",
        "EVBe_somewhat agree",
        "EVBe_strongly agree"
      ),
      ordered = TRUE
    ),
    ATT_EVB_function = factor(
      ATT_EVB_function,
      levels = c(
        "strongly_disagree",
        "somewhat_disagree",
        "neutral",
        "somewhat_agree",
        "strongly_agree"
      ),
      labels = c(
        "EVBf_strongly disagree",
        "EVBf_somewhat disagree",
        "EVBf_neither agree nor disagree",
        "EVBf_somewhat agree",
        "EVBf_strongly agree"
      ),
      ordered = TRUE
    ),
    ATT_techsavvy = factor(
      ATT_techsavvy,
      levels = c(
        "strongly_disagree",
        "somewhat_disagree",
        "neutral",
        "somewhat_agree",
        "strongly_agree"
      ),
      labels = c(
        "ATTt_strongly disagree",
        "ATTt_somewhat disagree",
        "ATTt_neither agree nor disagree",
        "ATTt_somewhat agree",
        "ATTt_strongly agree"
      ),
      ordered = TRUE
    ),
    ATT_risktaker = factor(
      ATT_risktaker,
      levels = c(
        "strongly_disagree",
        "somewhat_disagree",
        "neutral",
        "somewhat_agree",
        "strongly_agree"
      ),
      labels = c(
        "ATTr_strongly disagree",
        "ATTr_somewhat disagree",
        "ATTr_neither agree nor disagree",
        "ATTr_somewhat agree",
        "ATTr_strongly agree"
      ),
      ordered = TRUE
    ),
    ATT_climate = factor(
      ATT_climate,
      levels = c("not_at_all", "a_little", "moderate", "a_lot", "great_deal"),
      labels = c(
        "not at all",
        "a little",
        "a moderate amount",
        "a lot",
        "a great_deal"
      ),
      ordered = TRUE
    ),
    ATT_political = factor(
      ATT_political,
      levels = c(
        "very_conservative",
        "conservative",
        "moderate",
        "liberal",
        "very_liberal",
        "other",
        "prefer_not_answer"
      ),
      labels = c(
        "very conservative",
        "conservative",
        "moderate",
        "liberal",
        "very liberal",
        "other",
        "political_prefer not to answer"
      ),
      ordered = TRUE
    ),
    ATT_voting = factor(
      ATT_voting,
      levels = c(
        "democratic",
        "republican",
        "independent",
        "prefer_not_answer"
      ),
      labels = c(
        "Democratic",
        "Republican",
        "I am independent",
        "voting_prefer not to answer"
      ),
      ordered = TRUE
    ),
    knowledge_gas = factor(
      knowledge_gas,
      levels = c("0", "1", "2"),
      labels = c(
        "gas_no knowledge",
        "gas_partial knowledge",
        "gas_full knowledge"
      ),
      ordered = TRUE
    ),
    knowledge_plugin = factor(
      knowledge_plugin,
      levels = c("0", "1", "2"),
      labels = c(
        "plugin_no knowledge",
        "plugin_partial knowledge",
        "plugin_full knowledge"
      ),
      ordered = TRUE
    ),
    knowledge_ev = factor(
      knowledge_ev,
      levels = c("0", "1"),
      labels = c("ev_no knowledge", "ev_full knowledge"),
      ordered = TRUE
    ),
    knowledge_subsidy = factor(
      knowledge_subsidy,
      levels = c("0", "1"),
      labels = c("subsidy_no knowledge", "subsidy_full knowledge"),
      ordered = TRUE
    ),

    Veh_hh_fuel = factor(
      Veh_hh_fuel,
      levels = c("icev_only", "ev_only", "ev_mix", "other"),
      labels = c("ICEV-only", "EV-only", "EV-mixed-fuel", "Vehfuel_other"),
      ordered = TRUE
    ),
    EV_charger = factor(
      EV_charger,
      levels = c("no", "yes", "not_sure"),
      labels = c("EVc_no", "EVc_yes", "EVc_not sure"),
      ordered = TRUE
    ),
    EV_neighbor = factor(
      EV_neighbor,
      levels = c("no", "yes", "not_sure"),
      labels = c("EVn_no", "EVn_yes", "EVn_not sure"),
      ordered = TRUE
    )
  )

# ----Summary Stats----
## ----Adopting new & used BEV----
### ----New BEV----
#### ----Row-wise %----
varfactor <- summary_dt %>%
  select(
    ends_with("_cate"),
    starts_with("ATT_"),
    starts_with("knowledge_"),
    Veh_hh_fuel,
    starts_with("EV_")
  ) %>%
  names() %>%
  syms()

cross_tabs_cat <- data.frame()
for (var in seq_along(varfactor)) {
  new_cross_tab <- summary_dt %>%
    group_by(!!varfactor[[var]], next_veh_fuel_new_bev) %>% # Unquote with !!
    # summarise(n=sum(final_weights)) %>%
    summarise(n = sum(final_weights)) %>%
    filter(!is.na(!!varfactor[[var]])) %>%
    mutate(perc = round(n / sum(n), 3)) %>%
    setNames(., c("Variables","next_veh_fuel_new_bev", "n", "perc"))
  cross_tabs_cat <- rbind(
    as.data.frame(cross_tabs_cat),
    as.data.frame(new_cross_tab)
  )
}

cross_tabs_cat_wide_perc <- cross_tabs_cat %>%
  select(-n) %>%
  # mutate(rowid=c(1:nrow(cross_tabs_cat))) %>%
  pivot_wider(names_from = next_veh_fuel_new_bev, values_from = perc)

cross_tabs_cat_wide_n <- cross_tabs_cat %>%
  select(-perc) %>%
  # mutate(rowid=c(1:nrow(cross_tabs_cat))) %>%
  pivot_wider(names_from = next_veh_fuel_new_bev, values_from = n)


cross_tabs_cat_pop <- data.frame()
for (var in seq_along(varfactor)) {
  new_cross_tab <- summary_dt %>%
    group_by(!!varfactor[[var]]) %>% # Unquote with !!
    summarise(n = sum(final_weights)) %>%
    filter(!is.na(!!varfactor[[var]])) %>%
    mutate(perc = round(n / sum(n), 3)) %>%
    setNames(., c("Variables", "n", "perc"))
  cross_tabs_cat_pop <- rbind(
    as.data.frame(cross_tabs_cat_pop),
    as.data.frame(new_cross_tab)
  )
}


cross_tabs_cat_pop_wide_n <- cross_tabs_cat_pop %>%
  select(!perc)

cross_tabs_cat_pop_wide_perc <- cross_tabs_cat_pop %>%
  select(!n)


## Continuous vehicle

varcon <- data %>%
  select(ends_with("_num"), starts_with("FA_"), Veh_hh_count) %>%
  names() %>%
  syms()

varcon_names <- names(select(
  data,
  ends_with("_num"),
  starts_with("FA_"),
  starts_with("knowledge_"),
  Veh_hh_count
))
varcon_names_list <- paste0('"', varcon_names, '"', collapse = ", ")


cross_tabs_con <- data.frame()

for (var in seq_along(varcon)) {
  new_cross_tab <- summary_dt %>%
    filter(!is.na(!!varcon[[var]])) %>%
    group_by(next_veh_fuel_new_bev) %>% # Unquote with !!
    summarise(
      n = sum(!!varcon[[var]] * final_weights),
      pop = sum(final_weights)
    ) %>%
    ungroup() %>%
    mutate(perc = round(n / pop, 3))

  cross_tabs_con <- rbind(
    as.data.frame(cross_tabs_con),
    as.data.frame(new_cross_tab)
  )
}

cross_tabs_con_wide <- cross_tabs_con %>%
  mutate(
    Variables = c(rep(
      c(
        "age_num",
        "hhincome_num",
        "hhsize_num",
        "FA_EV_benefit",
        "FA_EV_anxiety",
        "Veh_hh_count"
      ),
      each = 5
    ))
  ) %>%
  select(next_veh_fuel_new_bev, Variables, perc) %>%
  pivot_wider(names_from = next_veh_fuel_new_bev, values_from = perc)

cross_tabs_con_pop <- data.frame()
for (var in seq_along(varcon)) {
  new_cross_tab <- summary_dt %>%
    filter(!is.na(!!varcon[[var]])) %>%
    summarise(
      n = sum(!!varcon[[var]] * final_weights),
      pop = sum(final_weights)
    ) %>%
    ungroup() %>%
    mutate(perc = round(n / pop, 3))
  cross_tabs_con_pop <- rbind(
    as.data.frame(cross_tabs_con_pop),
    as.data.frame(new_cross_tab)
  )
}

cross_tabs_con_pop_wide <- cross_tabs_con_pop %>%
  mutate(
    Variables = c(
      "age_num",
      "hhincome_num",
      "hhsize_num",
      "FA_EV_benefit",
      "FA_EV_anxiety",
      "Veh_hh_count"
    )
  ) %>%
  select(Variables, perc) %>%
  setNames(c("Variables", "n"))

cross_tabs <- rbind(cross_tabs_cat_wide_perc, cross_tabs_con_wide)

cross_tabs_pop <- rbind(cross_tabs_cat_pop_wide_n, cross_tabs_con_pop_wide)
# cross_tabs[is.na(cross_tabs)]<-0

cross_tabs_new_bev <- cross_tabs_pop %>%
  inner_join(cross_tabs, by = "Variables")

cate_vars <- summary_dt %>%
  select_if(~ is.factor(.) || is.character(.)) %>%
  select(!starts_with("next_veh"))
vars <- lapply(cate_vars, function(x) levels(as.factor(x)))

df_levels <- tibble(variable = names(vars), levels_list = vars) %>%
  unnest(cols = c(levels_list)) %>%
  rename(level = levels_list)

cross_tabs_new_bev <- df_levels %>%
  full_join(cross_tabs_new_bev, by = c("level" = "Variables"))



### ----Used BEV----
#### ----Row-wise %----
varfactor <- summary_dt %>%
  select(
    ends_with("_cate"),
    starts_with("ATT_"),
    starts_with("knowledge_"),
    Veh_hh_fuel,
    starts_with("EV_")
  ) %>%
  names() %>%
  syms()

cross_tabs_cat <- data.frame()
for (var in seq_along(varfactor)) {
  new_cross_tab <- summary_dt %>%
    group_by(!!varfactor[[var]], next_veh_fuel_used_bev) %>% # Unquote with !!
    # summarise(n=sum(final_weights)) %>%
    summarise(n = sum(final_weights)) %>%
    filter(!is.na(!!varfactor[[var]])) %>%
    mutate(perc = round(n / sum(n), 3)) %>%
    setNames(., c( "Variables","next_veh_fuel_used_bev", "n", "perc"))
  cross_tabs_cat <- rbind(
    as.data.frame(cross_tabs_cat),
    as.data.frame(new_cross_tab)
  )
}

cross_tabs_cat_wide_perc <- cross_tabs_cat %>%
  select(-n) %>%
  # mutate(rowid=c(1:nrow(cross_tabs_cat))) %>%
  pivot_wider(names_from = next_veh_fuel_used_bev, values_from = perc)

cross_tabs_cat_wide_n <- cross_tabs_cat %>%
  select(-perc) %>%
  # mutate(rowid=c(1:nrow(cross_tabs_cat))) %>%
  pivot_wider(names_from = next_veh_fuel_used_bev, values_from = n)


cross_tabs_cat_pop <- data.frame()
for (var in seq_along(varfactor)) {
  new_cross_tab <- summary_dt %>%
    group_by(!!varfactor[[var]]) %>% # Unquote with !!
    summarise(n = sum(final_weights)) %>%
    filter(!is.na(!!varfactor[[var]])) %>%
    mutate(perc = round(n / sum(n), 3)) %>%
    setNames(., c("Variables", "n", "perc"))
  cross_tabs_cat_pop <- rbind(
    as.data.frame(cross_tabs_cat_pop),
    as.data.frame(new_cross_tab)
  )
}


cross_tabs_cat_pop_wide_n <- cross_tabs_cat_pop %>%
  select(!perc)

cross_tabs_cat_pop_wide_perc <- cross_tabs_cat_pop %>%
  select(!n)

## Continuous vehicle

varcon <- data %>%
  select(ends_with("_num"), starts_with("FA_"), Veh_hh_count) %>%
  names() %>%
  syms()

varcon_names <- names(select(
  data,
  ends_with("_num"),
  starts_with("FA_"),
  starts_with("knowledge_"),
  Veh_hh_count
))
varcon_names_list <- paste0('"', varcon_names, '"', collapse = ", ")


cross_tabs_con <- data.frame()

for (var in seq_along(varcon)) {
  new_cross_tab <- summary_dt %>%
    filter(!is.na(!!varcon[[var]])) %>%
    group_by(next_veh_fuel_used_bev) %>% # Unquote with !!
    summarise(
      n = sum(!!varcon[[var]] * final_weights),
      pop = sum(final_weights)
    ) %>%
    ungroup() %>%
    mutate(perc = round(n / pop, 3))

  cross_tabs_con <- rbind(
    as.data.frame(cross_tabs_con),
    as.data.frame(new_cross_tab)
  )
}

cross_tabs_con_wide <- cross_tabs_con %>%
  mutate(
    Variables = c(rep(
      c(
        "age_num",
        "hhincome_num",
        "hhsize_num",
        "FA_EV_benefit",
        "FA_EV_anxiety",
        "Veh_hh_count"
      ),
      each = 5
    ))
  ) %>%
  select(next_veh_fuel_used_bev, Variables, perc) %>%
  pivot_wider(names_from = next_veh_fuel_used_bev, values_from = perc)

cross_tabs_con_pop <- data.frame()
for (var in seq_along(varcon)) {
  new_cross_tab <- summary_dt %>%
    filter(!is.na(!!varcon[[var]])) %>%
    summarise(
      n = sum(!!varcon[[var]] * final_weights),
      pop = sum(final_weights)
    ) %>%
    ungroup() %>%
    mutate(perc = round(n / pop, 3))
  cross_tabs_con_pop <- rbind(
    as.data.frame(cross_tabs_con_pop),
    as.data.frame(new_cross_tab)
  )
}

cross_tabs_con_pop_wide <- cross_tabs_con_pop %>%
  mutate(
    Variables = c(
      "age_num",
      "hhincome_num",
      "hhsize_num",
      "FA_EV_benefit",
      "FA_EV_anxiety",
      "Veh_hh_count"
    )
  ) %>%
  select(Variables, perc) %>%
  setNames(c("Variables", "n"))

cross_tabs <- rbind(cross_tabs_cat_wide_perc, cross_tabs_con_wide)

cross_tabs_pop <- rbind(cross_tabs_cat_pop_wide_n, cross_tabs_con_pop_wide)
# cross_tabs[is.na(cross_tabs)]<-0

cross_tabs_used_bev <- cross_tabs_pop %>%
  inner_join(cross_tabs, by = "Variables")

cate_vars <- summary_dt %>%
  select_if(~ is.factor(.) || is.character(.)) %>%
  select(!starts_with("next_veh"))
vars <- lapply(cate_vars, function(x) levels(as.factor(x)))

df_levels <- tibble(variable = names(vars), levels_list = vars) %>%
  unnest(cols = c(levels_list)) %>%
  rename(level = levels_list)

cross_tabs_used_bev <- df_levels %>%
  full_join(cross_tabs_used_bev, by = c("level" = "Variables"))


### ----Combined----
cross_tabs_combined<-cbind(cross_tabs_new_bev %>%
                             group_by(variable) %>% 
                             mutate(perc=n/sum(n)) %>% 
                             select(variable, level, n, perc, everything()) %>% 
                             setNames(c("variable","level","n","perc",
                                        "new_bev_very_unlikely",
                                        "new_bev_somewhat_unlikely",
                                        "new_bev_neutral",
                                        "new_bev_somewhat_likely",
                                        "new_bev_very_likely"
                                        )
                                      ), 
                           NA,
                          cross_tabs_used_bev %>%
                             select(very_unlikely:very_likely) %>% 
                             setNames(c(
                                        "used_bev_very_unlikely",
                                        "used_bev_somewhat_unlikely",
                                        "used_bev_neutral",
                                        "used_bev_somewhat_likely",
                                        "used_bev_very_likely"
                             )
                             )
                           )

write_csv(
  cross_tabs_combined,
  here(
    "code",
    "output",
    "bev_likelihood_cross_tabs.csv"
  )
)

# ----Archive (column-wise tables)----
# library(rlang)
#
# varfactor <- data %>%
#   select(ends_with("_cate"),
#          starts_with("ATT_"),
#          starts_with("knowledge_"),
#          Veh_hh_fuel,
#          starts_with("EV_")
#   ) %>%
#   names() %>%
#   syms()
#
# cross_tabs_cat<-data.frame()
# for (var in seq_along(varfactor)){
#   new_cross_tab<-summary_dt %>%
#     group_by(next_veh_fuel_new_bev,!! varfactor[[var]]) %>% # Unquote with !!
#     # summarise(n=sum(final_weights)) %>%
#     summarise(n=sum(final_weights)) %>%
#     filter(!is.na(!! varfactor[[var]])) %>%
#     mutate(perc=round(n/sum(n),3)) %>%
#     setNames(.,c("next_veh_fuel_new_bev","Variables","n","perc"))
#   cross_tabs_cat<-rbind(as.data.frame(cross_tabs_cat),as.data.frame(new_cross_tab))
# }
#
# cross_tabs_cat_wide_perc<-cross_tabs_cat %>%
#   select(-n) %>%
#   # mutate(rowid=c(1:nrow(cross_tabs_cat))) %>%
#   pivot_wider(names_from = next_veh_fuel_new_bev,values_from=perc)
#
# cross_tabs_cat_wide_n<-cross_tabs_cat %>%
#   select(-perc) %>%
#   # mutate(rowid=c(1:nrow(cross_tabs_cat))) %>%
#   pivot_wider(names_from = next_veh_fuel_new_bev,values_from=n)
#
#
# cross_tabs_cat_pop<-data.frame()
# for (var in seq_along(varfactor)){
#   new_cross_tab<-summary_dt %>%
#     group_by(!! varfactor[[var]]) %>% # Unquote with !!
#     summarise(n=sum(final_weights)) %>%
#     filter(!is.na(!! varfactor[[var]])) %>%
#     mutate(perc=round(n/sum(n),3)) %>%
#     setNames(.,c("Variables","n","perc"))
#   cross_tabs_cat_pop<-rbind(as.data.frame(cross_tabs_cat_pop),as.data.frame(new_cross_tab))
# }
#
#
# cross_tabs_cat_pop_wide_n<-cross_tabs_cat_pop %>%
#   select(!perc)
#
# cross_tabs_cat_pop_wide_perc<-cross_tabs_cat_pop %>%
#   select(!n)
#
# ## Continuous vehicle
#
# varcon <- data %>%
#   select(ends_with("_num"),
#          starts_with("FA_"),
#          Veh_hh_count) %>%
#   names() %>%
#   syms()
#
# varcon_names <- names(select(data, ends_with("_num"), starts_with("FA_"), starts_with("knowledge_"), Veh_hh_count))
# varcon_names_list<-paste0('"', varcon_names, '"', collapse = ", ")
#
#
# cross_tabs_con <- data.frame()
#
# for (var in seq_along(varcon)){
#   new_cross_tab<-summary_dt %>%
#     filter(!is.na(!! varcon[[var]])) %>%
#     group_by(next_veh_fuel_new_bev) %>% # Unquote with !!
#     summarise(n=sum(!! varcon[[var]]*final_weights),pop=sum(final_weights)) %>%
#     ungroup() %>%
#     mutate(perc=round(n/pop,3))
#
#   cross_tabs_con<-rbind(as.data.frame(cross_tabs_con),as.data.frame(new_cross_tab))
# }
#
# cross_tabs_con_wide<-cross_tabs_con %>%
#   mutate(Variables=c(rep(c("age_num", "hhincome_num", "hhsize_num", "FA_EV_benefit", "FA_EV_anxiety", "Veh_hh_count"
#   ),each=5))) %>%
#   select(next_veh_fuel_new_bev, Variables,perc) %>%
#   pivot_wider(names_from = next_veh_fuel_new_bev,values_from=perc)
#
# cross_tabs_con_pop <- data.frame()
# for (var in seq_along(varcon)){
#   new_cross_tab<-summary_dt %>%
#     filter(!is.na(!! varcon[[var]])) %>%
#     summarise(n=sum(!! varcon[[var]]*final_weights),pop=sum(final_weights)) %>%
#     ungroup() %>%
#     mutate(perc=round(n/pop,3))
#   cross_tabs_con_pop<-rbind(as.data.frame(cross_tabs_con_pop),as.data.frame(new_cross_tab))
# }
#
# cross_tabs_con_pop_wide<-cross_tabs_con_pop %>%
#   mutate(Variables=c("age_num", "hhincome_num", "hhsize_num", "FA_EV_benefit", "FA_EV_anxiety", "Veh_hh_count"
#   )) %>%
#   select(Variables, perc) %>%
#   setNames(c("Variables", "n"))
#
# cross_tabs<-rbind(cross_tabs_cat_wide_perc,cross_tabs_con_wide)
#
# cross_tabs_pop<-rbind(cross_tabs_cat_pop_wide_n,cross_tabs_con_pop_wide)
# # cross_tabs[is.na(cross_tabs)]<-0
#
# cross_tabs_new_bev<-cross_tabs_pop %>%
#   inner_join(cross_tabs,by="Variables")
#
# cate_vars <- summary_dt %>%
#   select_if(~ is.factor(.) || is.character(.)) %>%
#   select(!starts_with("next_veh"))
# vars <- lapply(cate_vars, function(x) levels(as.factor(x)))
#
# df_levels <- tibble(variable = names(vars), levels_list = vars) %>%
#   unnest(cols = c(levels_list)) %>%
#   rename(level = levels_list)
#
# cross_tabs_new_bev<-df_levels %>%
#   full_join(cross_tabs_new_bev, by=c("level"="Variables"))
