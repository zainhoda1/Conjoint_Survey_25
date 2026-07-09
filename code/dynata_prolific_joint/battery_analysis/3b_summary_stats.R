source(here::here('code', 'setup.R'))

# ----Load the data set----
data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery.parquet"
))

# data_model <- data_model %>%
#   filter(
#     !is.na(ATT_range_anxiety) &
#       !is.na(ATT_risktaker) &
#       # !is.na(next_veh_budget_k) &
#       !is.na(EV_charger) &
#       !is.na(Veh_hh_fuel) &
#       !is.na(Veh_primary_range) &
#       !is.na(ATT_EVB_environment) &
#       !is.na(ATT_EVB_function) &
#       !is.na(vehicle_typesuv)
#   )

# table(data_model$data_source)
length(unique(data_model$psid))
data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint.parquet"
))

data_joint <- data_joint %>%
  filter(psid %in% data_model$psid)

n_total_all <- nrow(data_joint)

# ----Respresentative----
data <- data %>%
  mutate(
    age_group = case_when(
      age_num <= 24 ~ "18 - 24",
      age_num <= 34 ~ "25 - 34",
      age_num <= 44 ~ "35 - 44",
      age_num <= 54 ~ "45 - 54",
      age_num <= 64 ~ "55 - 64",
      age_num >= 65 ~ "65+"
    ),
    hh_income_group = case_when(
      is.na(hhincome_num) ~ "prefer_not_answer",
      hhincome_num <= 15000 ~ "< $15,000",
      hhincome_num <= 25000 ~ "$15,000 - $24,999",
      hhincome_num <= 45000 ~ "$25,000 - $49,999",
      hhincome_num <= 75000 ~ "$50,000 - $74,999",
      hhincome_num <= 95000 ~ "$75,000 - $99,999",
      hhincome_num <= 145000 ~ "$100,000 - $149,999",
      TRUE ~ "$150,000+"
    )
  ) %>%
  mutate(
    age_group = factor(
      age_group,
      levels = c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65+")
    ),
    hh_income_group = factor(
      hh_income_group,
      levels = c(
        "< $15,000",
        "$15,000 - $24,999",
        "$25,000 - $49,999",
        "$50,000 - $74,999",
        "$75,000 - $99,999",
        "$100,000 - $149,999",
        "$150,000+",
        "prefer_not_answer"
      )
    )
  )

table(data$hh_income_group, data$data_source, useNA = "ifany")
table(data$age_group, data$data_source, useNA = "ifany")

# ----summary_dt----
summary_dt <- data %>%
  select(
    ends_with("_num"),
    ends_with("_cate"),
    starts_with("Veh_"),
    starts_with("EV_")
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
      next_veh_fuel_new_bev,
      data_source
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
  next_veh_fuel_new_bev,
  data_source
))

# lapply(summary_dt[varfactor_names], levels)

summary_dt <- summary_dt %>%
  mutate(
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
    )
  )


# ----Summary Stats----
## ----Data Source----
#### ----Col-wise %----
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
    group_by(data_source, !!varfactor[[var]]) %>% # Unquote with !!
    # summarise(n=sum(final_weights)) %>%
    summarise(n = sum(final_weights)) %>%
    filter(!is.na(!!varfactor[[var]])) %>%
    mutate(perc = round(n / sum(n), 3)) %>%
    setNames(., c("data_source", "Variables", "n", "perc"))
  cross_tabs_cat <- rbind(
    as.data.frame(cross_tabs_cat),
    as.data.frame(new_cross_tab)
  )
}

cross_tabs_cat_wide_perc <- cross_tabs_cat %>%
  select(-n) %>%
  # mutate(rowid=c(1:nrow(cross_tabs_cat))) %>%
  pivot_wider(names_from = data_source, values_from = perc)

cross_tabs_cat_wide_n <- cross_tabs_cat %>%
  select(-perc) %>%
  # mutate(rowid=c(1:nrow(cross_tabs_cat))) %>%
  pivot_wider(names_from = data_source, values_from = n)


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
    group_by(data_source) %>% # Unquote with !!
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
      each = 2
    ))
  ) %>%
  select(data_source, Variables, perc) %>%
  pivot_wider(names_from = data_source, values_from = perc)

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

cross_tabs_pop <- cross_tabs_pop %>% mutate(Variables = as.character(Variables))
cross_tabs_cat_wide_n <- cross_tabs_cat_wide_n %>%
  mutate(Variables = as.character(Variables))
cross_tabs <- cross_tabs %>% mutate(Variables = as.character(Variables))

cross_tabs_data_source <- cross_tabs_pop %>%
  inner_join(cross_tabs_cat_wide_n, by = "Variables") %>%
  inner_join(cross_tabs, by = "Variables")

cate_vars <- summary_dt %>%
  select_if(~ is.factor(.) || is.character(.)) %>%
  select(!starts_with("next_veh"), -data_source)
vars <- lapply(cate_vars, function(x) levels(as.factor(x)))

df_levels <- tibble(variable = names(vars), levels_list = vars) %>%
  unnest(cols = c(levels_list)) %>%
  rename(level = levels_list)

cross_tabs_data_source <- df_levels %>%
  full_join(cross_tabs_data_source, by = c("level" = "Variables"))

write.xlsx(
  list(
    cross_tabs_data_source
  ),
  paste0(here(), "/code/output/data_source_compare.xlsx")
)

# ============================================================
# ----Sample profile vs ACS 2020-2024 5-year benchmarks----
# ============================================================
# Benchmarks: ACS 2020-2024 5-year national estimates from the Census API.
# Tables: B01001 (sex by age, 18+), B01001A/B/I (sex by age for White alone,
# Black alone, and Hispanic; used for race/ethnicity among 18+),
# B19001 (household income), B15003 (education, 25+), B25003 (tenure),
# B25032 (units in structure, occupied units).
# Estimates are cached in code/output/acs2024_5yr_cache.csv (retrieved 2026-07-06).
# Delete the cache file to re-download (uses CENSUS_API_KEY from ~/.Renviron).
#
# Universe notes for the table:
# - age / gender / race / Hispanic origin: population 18+
# - education: population 25+
# - income, tenure, housing type: households (occupied units)
# Sample shares are unweighted and exclude item nonresponse ("prefer not to
# answer" / NA) from each variable's denominator.

acs_cache_path <- here("code", "output", "acs2024_5yr_cache.csv")
acs_tables <- c(
  "B01001",
  "B01001A",
  "B01001B",
  "B01001I",
  "B02001",
  "B03003",
  "B19001",
  "B15003",
  "B25003",
  "B25032",
  "B25010"
)

if (!file.exists(acs_cache_path)) {
  acs_key <- Sys.getenv("CENSUS_API_KEY")
  stopifnot("CENSUS_API_KEY not found in environment" = nzchar(acs_key))
  fetch_acs_group <- function(tbl) {
    url <- paste0(
      "https://api.census.gov/data/2024/acs/acs5?get=group(",
      tbl,
      ")&for=us:1&key=",
      acs_key
    )
    raw <- jsonlite::fromJSON(url)
    df <- setNames(as.data.frame(raw[-1, , drop = FALSE]), raw[1, ])
    df %>%
      select(matches("^B\\d+[A-Z]?_\\d{3}E$")) %>%
      pivot_longer(
        everything(),
        names_to = "variable",
        values_to = "estimate"
      ) %>%
      mutate(table = tbl, estimate = as.numeric(estimate)) %>%
      select(table, variable, estimate)
  }
  acs_raw <- map_dfr(acs_tables, fetch_acs_group)
  write.csv(acs_raw, acs_cache_path, row.names = FALSE)
}

acs_raw <- read.csv(acs_cache_path)
acs <- setNames(acs_raw$estimate, acs_raw$variable)
acs_sum <- function(tbl, cells) sum(acs[sprintf("%s_%03dE", tbl, cells)])

## ----ACS benchmark shares----
# Age and gender among population 18+ (B01001 male cells 7-25, female 31-49)
acs_18p <- acs_sum("B01001", 7:25) + acs_sum("B01001", 31:49)
acs_age <- c(
  "18 - 24" = acs_sum("B01001", 7:10) + acs_sum("B01001", 31:34),
  "25 - 34" = acs_sum("B01001", 11:12) + acs_sum("B01001", 35:36),
  "35 - 44" = acs_sum("B01001", 13:14) + acs_sum("B01001", 37:38),
  "45 - 54" = acs_sum("B01001", 15:16) + acs_sum("B01001", 39:40),
  "55 - 64" = acs_sum("B01001", 17:19) + acs_sum("B01001", 41:43),
  "65+" = acs_sum("B01001", 20:25) + acs_sum("B01001", 44:49)
) /
  acs_18p
acs_gender <- c(
  "Female" = acs_sum("B01001", 31:49),
  "Male" = acs_sum("B01001", 7:25)
) /
  acs_18p
# ACS sex is binary; sample "Other" has no ACS counterpart (shown as --)
acs_gender <- c(acs_gender, "Other" = NA_real_)

# Race and Hispanic origin among population 18+ (B01001A/B/I sex-by-age
# iteration tables: 18+ = male cells 7-16 + female cells 22-31)
acs_white18 <- acs_sum("B01001A", c(7:16, 22:31))
acs_black18 <- acs_sum("B01001B", c(7:16, 22:31))
acs_race <- c(
  "White alone" = acs_white18,
  "Black or African American alone" = acs_black18,
  "Other or multiple races" = acs_18p - acs_white18 - acs_black18
) /
  acs_18p
acs_hisp18 <- acs_sum("B01001I", c(7:16, 22:31))
acs_hisp <- c(
  "Hispanic or Latino" = acs_hisp18,
  "Not Hispanic or Latino" = acs_18p - acs_hisp18
) /
  acs_18p

# Household income (households)
acs_hh <- acs[["B19001_001E"]]
acs_income <- c(
  "Less than $25,000" = acs_sum("B19001", 2:5),
  "$25,000 - $49,999" = acs_sum("B19001", 6:10),
  "$50,000 - $74,999" = acs_sum("B19001", 11:12),
  "$75,000 - $99,999" = acs_sum("B19001", 13),
  "$100,000 - $149,999" = acs_sum("B19001", 14:15),
  "$150,000 or more" = acs_sum("B19001", 16:17)
) /
  acs_hh

# Education (population 25+)
acs_edu <- c(
  "High school or less" = acs_sum("B15003", 2:18),
  "Some college or associate" = acs_sum("B15003", 19:21),
  "Bachelor's degree" = acs_sum("B15003", 22),
  "Graduate or professional degree" = acs_sum("B15003", 23:25)
) /
  acs[["B15003_001E"]]

# Tenure and units in structure (occupied housing units)
acs_tenure <- c(
  "Own" = acs[["B25003_002E"]],
  "Rent" = acs[["B25003_003E"]]
) /
  acs[["B25003_001E"]]
acs_hhtype <- c(
  "Single-family detached" = acs_sum("B25032", c(3, 14)),
  "Single-family attached" = acs_sum("B25032", c(4, 15)),
  "Apartment (2+ units)" = acs_sum("B25032", c(5:10, 16:21)),
  "Mobile home or other" = acs_sum("B25032", c(11:12, 22:23))
) /
  acs[["B25032_001E"]]

acs_bench <- bind_rows(
  tibble(variable = "Age", category = names(acs_age), acs = acs_age),
  tibble(variable = "Gender", category = names(acs_gender), acs = acs_gender),
  tibble(variable = "Race", category = names(acs_race), acs = acs_race),
  tibble(
    variable = "Hispanic origin",
    category = names(acs_hisp),
    acs = acs_hisp
  ),
  tibble(
    variable = "Household income",
    category = names(acs_income),
    acs = acs_income
  ),
  tibble(variable = "Education", category = names(acs_edu), acs = acs_edu),
  tibble(
    variable = "Housing tenure",
    category = names(acs_tenure),
    acs = acs_tenure
  ),
  tibble(
    variable = "Housing type",
    category = names(acs_hhtype),
    acs = acs_hhtype
  )
)

## ----Sample shares (unweighted, ACS-aligned brackets)----
# Demographics come from 3a_variable_cleaning.R output, restricted to the
# final estimation sample (psid in data_apollo_battery.parquet).
data_clean <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_clean_variables.parquet"
)) %>%
  filter(psid %in% data_model$psid)

profile_dt <- data_clean %>%
  transmute(
    age_cat = case_when(
      age_num <= 24 ~ "18 - 24",
      age_num <= 34 ~ "25 - 34",
      age_num <= 44 ~ "35 - 44",
      age_num <= 54 ~ "45 - 54",
      age_num <= 64 ~ "55 - 64",
      age_num >= 65 ~ "65+"
    ),
    gender_cat = case_when(
      gender_cate == "female" ~ "Female",
      gender_cate == "male" ~ "Male",
      gender_cate == "other" ~ "Other"
    ),
    race_cat = case_when(
      race_cate == "white_only" ~ "White alone",
      race_cate == "african_american_only" ~ "Black or African American alone",
      race_cate == "other" ~ "Other or multiple races"
    ),
    hisp_cat = case_when(
      grepl("^non", ethnicity_cate) ~ "Not Hispanic or Latino",
      grepl("hisp", ethnicity_cate) ~ "Hispanic or Latino"
    ),
    income_cat = case_when(
      is.na(hhincome_num) ~ NA_character_,
      hhincome_num <= 25000 ~ "Less than $25,000",
      hhincome_num <= 45000 ~ "$25,000 - $49,999",
      hhincome_num <= 75000 ~ "$50,000 - $74,999",
      hhincome_num <= 95000 ~ "$75,000 - $99,999",
      hhincome_num <= 145000 ~ "$100,000 - $149,999",
      TRUE ~ "$150,000 or more"
    ),
    edu_cat = case_when(
      education_cate == "high_school" ~ "High school or less",
      education_cate == "some_college" ~ "Some college or associate",
      education_cate == "bachelor" ~ "Bachelor's degree",
      education_cate == "graduate" ~ "Graduate or professional degree"
    ),
    tenure_cat = case_when(
      hhtenure_cate == "own" ~ "Own",
      hhtenure_cate == "rent" ~ "Rent"
    ),
    hhtype_cat = case_when(
      hhtype_cate == "sf_detached" ~ "Single-family detached",
      hhtype_cate == "sf_attached" ~ "Single-family attached",
      hhtype_cate == "apart" ~ "Apartment (2+ units)",
      hhtype_cate == "other" ~ "Mobile home or other"
    )
  )

sample_share <- function(x, varname) {
  tibble(category = x) %>%
    filter(!is.na(category)) %>%
    count(category) %>%
    mutate(variable = varname, sample = n / sum(n)) %>%
    select(variable, category, n, sample)
}

sample_bench <- bind_rows(
  sample_share(profile_dt$age_cat, "Age"),
  sample_share(profile_dt$gender_cat, "Gender"),
  sample_share(profile_dt$race_cat, "Race"),
  sample_share(profile_dt$hisp_cat, "Hispanic origin"),
  sample_share(profile_dt$income_cat, "Household income"),
  sample_share(profile_dt$edu_cat, "Education"),
  sample_share(profile_dt$tenure_cat, "Housing tenure"),
  sample_share(profile_dt$hhtype_cat, "Housing type")
)

sample_vs_acs <- acs_bench %>%
  left_join(sample_bench, by = c("variable", "category")) %>%
  mutate(
    n = replace_na(n, 0L),
    sample = replace_na(sample, 0),
    sample_pct = round(100 * sample, 1),
    acs_pct = round(100 * acs, 1)
  ) %>%
  select(variable, category, n, sample_pct, acs_pct)

print(sample_vs_acs, n = 40)

write.xlsx(
  list(sample_vs_acs = sample_vs_acs),
  paste0(here(), "/code/output/sample_vs_acs.xlsx")
)

## ----LaTeX table----
tex_rows <- sample_vs_acs %>%
  group_by(variable) %>%
  mutate(cat_tex = gsub("%", "\\\\%", gsub("\\$", "\\\\$", category))) %>%
  summarise(
    block = paste0(
      "\\multicolumn{4}{l}{\\textit{",
      first(variable),
      "}} \\\\\n",
      paste0(
        "\\quad ",
        cat_tex,
        " & ",
        formatC(n, format = "d", big.mark = ","),
        " & ",
        formatC(sample_pct, format = "f", digits = 1),
        " & ",
        ifelse(
          is.na(acs_pct),
          "--",
          formatC(acs_pct, format = "f", digits = 1)
        ),
        " \\\\",
        collapse = "\n"
      )
    ),
    .groups = "drop"
  ) %>%
  # keep display order
  arrange(match(
    variable,
    c(
      "Age",
      "Gender",
      "Race",
      "Hispanic origin",
      "Household income",
      "Education",
      "Housing tenure",
      "Housing type"
    )
  ))

tex_table <- paste0(
  "\\begin{table}[pos=H]\n\\centering\n",
  "\\caption{Sample profile compared with the U.S. population (ACS 2020--2024 5-year estimates).}\n",
  "\\label{table:sample_profile}\n",
  "\\begin{tabular}{lrrr}\n\\toprule\n",
  " & Sample (n) & Sample (\\%) & ACS (\\%) \\\\\n\\midrule\n",
  paste(tex_rows$block, collapse = "\n\\addlinespace[2pt]\n"),
  "\n\\bottomrule\n\\end{tabular}\n",
  "\\begin{minipage}{0.8\\textwidth}\n\\vspace{4pt}\\footnotesize\n",
  "Notes: Sample size and shares are unweighted and exclude item nonresponse. ",
  "`--' indicates no comparable ACS category. ",
  "ACS universes: population 18+ (age, gender, race, Hispanic origin), ",
  "population 25+ (education), and households (income, tenure, housing type).\n",
  "\\end{minipage}\n\\end{table}\n"
)

attach_dir <- here("paper_writing", "battery_paper", "attachments")
dir.create(attach_dir, showWarnings = FALSE, recursive = TRUE)
writeLines(tex_table, file.path(attach_dir, "sample_vs_acs.tex"))
