source(here::here('code', 'setup.R'))

# ============================================================
# Sample profile vs ACS 2020-2024 5-year benchmarks
# Vehicle paper — analytic sample from data_joint_vehicle.parquet
#
# ACS tables used:
#   B01001  sex by age (18+)
#   B01001A/B/I  sex by age for White, Black, Hispanic (18+)
#   B19001  household income
#   B15003  educational attainment (25+)
#   B25003  housing tenure
#   B25032  units in structure (occupied)
#
# Estimates cached in code/output/acs2024_5yr_cache.csv.
# Delete cache to re-download (requires CENSUS_API_KEY in ~/.Renviron).
# ============================================================

# ---- Load vehicle model sample ----
data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_vehicle.parquet"
))

data_clean <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_clean_variables.parquet"
)) %>%
  filter(psid %in% unique(data_model$psid))

cat("Vehicle analytic sample n =", nrow(data_clean), "\n")

# ---- ACS benchmarks ----
acs_cache_path <- here("code", "output", "acs2024_5yr_cache.csv")
acs_tables <- c(
  "B01001", "B01001A", "B01001B", "B01001I",
  "B02001", "B03003", "B19001", "B15003", "B25003", "B25032"
)

if (!file.exists(acs_cache_path)) {
  acs_key <- Sys.getenv("CENSUS_API_KEY")
  stopifnot("CENSUS_API_KEY not found in environment" = nzchar(acs_key))
  fetch_acs_group <- function(tbl) {
    url <- paste0(
      "https://api.census.gov/data/2024/acs/acs5?get=group(",
      tbl, ")&for=us:1&key=", acs_key
    )
    raw <- jsonlite::fromJSON(url)
    df <- setNames(as.data.frame(raw[-1, , drop = FALSE]), raw[1, ])
    df %>%
      select(matches("^B\\d+[A-Z]?_\\d{3}E$")) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "estimate") %>%
      mutate(table = tbl, estimate = as.numeric(estimate)) %>%
      select(table, variable, estimate)
  }
  acs_raw <- map_dfr(acs_tables, fetch_acs_group)
  write.csv(acs_raw, acs_cache_path, row.names = FALSE)
}

acs_raw <- read.csv(acs_cache_path)
acs <- setNames(acs_raw$estimate, acs_raw$variable)
acs_sum <- function(tbl, cells) sum(acs[sprintf("%s_%03dE", tbl, cells)])

acs_18p <- acs_sum("B01001", 7:25) + acs_sum("B01001", 31:49)

acs_age <- c(
  "18 - 24" = acs_sum("B01001", 7:10)  + acs_sum("B01001", 31:34),
  "25 - 34" = acs_sum("B01001", 11:12) + acs_sum("B01001", 35:36),
  "35 - 44" = acs_sum("B01001", 13:14) + acs_sum("B01001", 37:38),
  "45 - 54" = acs_sum("B01001", 15:16) + acs_sum("B01001", 39:40),
  "55 - 64" = acs_sum("B01001", 17:19) + acs_sum("B01001", 41:43),
  "65+"     = acs_sum("B01001", 20:25) + acs_sum("B01001", 44:49)
) / acs_18p

acs_gender <- c(
  "Female" = acs_sum("B01001", 31:49),
  "Male"   = acs_sum("B01001", 7:25)
) / acs_18p
acs_gender <- c(acs_gender, "Other" = NA_real_)

acs_white18 <- acs_sum("B01001A", c(7:16, 22:31))
acs_black18 <- acs_sum("B01001B", c(7:16, 22:31))
acs_race <- c(
  "White alone"                     = acs_white18,
  "Black or African American alone" = acs_black18,
  "Other or multiple races"         = acs_18p - acs_white18 - acs_black18
) / acs_18p

acs_hisp18 <- acs_sum("B01001I", c(7:16, 22:31))
acs_hisp <- c(
  "Hispanic or Latino"     = acs_hisp18,
  "Not Hispanic or Latino" = acs_18p - acs_hisp18
) / acs_18p

acs_hh <- acs[["B19001_001E"]]
acs_income <- c(
  "Less than $25,000"    = acs_sum("B19001", 2:5),
  "$25,000 - $49,999"   = acs_sum("B19001", 6:10),
  "$50,000 - $74,999"   = acs_sum("B19001", 11:12),
  "$75,000 - $99,999"   = acs_sum("B19001", 13),
  "$100,000 - $149,999" = acs_sum("B19001", 14:15),
  "$150,000 or more"    = acs_sum("B19001", 16:17)
) / acs_hh

acs_edu <- c(
  "High school or less"             = acs_sum("B15003", 2:18),
  "Some college or associate"       = acs_sum("B15003", 19:21),
  "Bachelor's degree"               = acs_sum("B15003", 22),
  "Graduate or professional degree" = acs_sum("B15003", 23:25)
) / acs[["B15003_001E"]]

acs_tenure <- c(
  "Own"  = acs[["B25003_002E"]],
  "Rent" = acs[["B25003_003E"]]
) / acs[["B25003_001E"]]

acs_hhtype <- c(
  "Single-family detached" = acs_sum("B25032", c(3, 14)),
  "Single-family attached" = acs_sum("B25032", c(4, 15)),
  "Apartment (2+ units)"   = acs_sum("B25032", c(5:10, 16:21)),
  "Mobile home or other"   = acs_sum("B25032", c(11:12, 22:23))
) / acs[["B25032_001E"]]

acs_bench <- bind_rows(
  tibble(variable = "Age",              category = names(acs_age),    acs = acs_age),
  tibble(variable = "Gender",           category = names(acs_gender), acs = acs_gender),
  tibble(variable = "Race",             category = names(acs_race),   acs = acs_race),
  tibble(variable = "Hispanic origin",  category = names(acs_hisp),   acs = acs_hisp),
  tibble(variable = "Household income", category = names(acs_income), acs = acs_income),
  tibble(variable = "Education",        category = names(acs_edu),    acs = acs_edu),
  tibble(variable = "Housing tenure",   category = names(acs_tenure), acs = acs_tenure),
  tibble(variable = "Housing type",     category = names(acs_hhtype), acs = acs_hhtype)
)

# ---- Sample shares ----
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
      gender_cate == "male"   ~ "Male",
      gender_cate == "other"  ~ "Other"
    ),
    race_cat = case_when(
      race_cate == "white_only"            ~ "White alone",
      race_cate == "african_american_only" ~ "Black or African American alone",
      race_cate == "other"                 ~ "Other or multiple races"
    ),
    hisp_cat = case_when(
      grepl("^non", ethnicity_cate) ~ "Not Hispanic or Latino",
      grepl("hisp", ethnicity_cate) ~ "Hispanic or Latino"
    ),
    income_cat = case_when(
      is.na(hhincome_num)    ~ NA_character_,
      hhincome_num <= 25000  ~ "Less than $25,000",
      hhincome_num <= 45000  ~ "$25,000 - $49,999",
      hhincome_num <= 75000  ~ "$50,000 - $74,999",
      hhincome_num <= 95000  ~ "$75,000 - $99,999",
      hhincome_num <= 145000 ~ "$100,000 - $149,999",
      TRUE                   ~ "$150,000 or more"
    ),
    edu_cat = case_when(
      education_cate == "high_school"  ~ "High school or less",
      education_cate == "some_college" ~ "Some college or associate",
      education_cate == "bachelor"     ~ "Bachelor's degree",
      education_cate == "graduate"     ~ "Graduate or professional degree"
    ),
    tenure_cat = case_when(
      hhtenure_cate == "own"  ~ "Own",
      hhtenure_cate == "rent" ~ "Rent"
    ),
    hhtype_cat = case_when(
      hhtype_cate == "sf_detached" ~ "Single-family detached",
      hhtype_cate == "sf_attached" ~ "Single-family attached",
      hhtype_cate == "apart"       ~ "Apartment (2+ units)",
      hhtype_cate == "other"       ~ "Mobile home or other"
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
  sample_share(profile_dt$age_cat,    "Age"),
  sample_share(profile_dt$gender_cat, "Gender"),
  sample_share(profile_dt$race_cat,   "Race"),
  sample_share(profile_dt$hisp_cat,   "Hispanic origin"),
  sample_share(profile_dt$income_cat, "Household income"),
  sample_share(profile_dt$edu_cat,    "Education"),
  sample_share(profile_dt$tenure_cat, "Housing tenure"),
  sample_share(profile_dt$hhtype_cat, "Housing type")
)

sample_vs_acs <- acs_bench %>%
  left_join(sample_bench, by = c("variable", "category")) %>%
  mutate(
    n          = replace_na(n, 0L),
    sample     = replace_na(sample, 0),
    sample_pct = round(100 * sample, 1),
    acs_pct    = round(100 * acs, 1)
  ) %>%
  select(variable, category, n, sample_pct, acs_pct)

print(sample_vs_acs, n = 40)

# ---- LaTeX table ----
escape_tex <- function(x) gsub("\\$", "\\\\$", gsub("%", "\\\\%", x))

tex_rows <- sample_vs_acs %>%
  group_by(variable) %>%
  mutate(cat_tex = escape_tex(category)) %>%
  summarise(
    block = paste0(
      "\\multicolumn{4}{l}{\\textit{", first(variable), "}} \\\\\n",
      paste0(
        "\\quad ", cat_tex,
        " & ", formatC(n, format = "d", big.mark = ","),
        " & ", formatC(sample_pct, format = "f", digits = 1),
        " & ", ifelse(is.na(acs_pct), "--", formatC(acs_pct, format = "f", digits = 1)),
        " \\\\",
        collapse = "\n"
      )
    ),
    .groups = "drop"
  ) %>%
  arrange(match(variable, c(
    "Age", "Gender", "Race", "Hispanic origin",
    "Household income", "Education", "Housing tenure", "Housing type"
  )))

tex_table <- paste0(
  "\\begin{table}[pos=H]\n\\centering\n",
  "\\caption{Sample profile compared with the U.S. population (ACS 2020--2024 5-year estimates).}\n",
  "\\label{table:sample_profile_vehicle}\n",
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

attach_dir <- here("paper_writing", "vehicle_paper", "attachments")
dir.create(attach_dir, showWarnings = FALSE, recursive = TRUE)
writeLines(tex_table, file.path(attach_dir, "sample_vs_acs_vehicle.tex"))
cat("Written to", file.path(attach_dir, "sample_vs_acs_vehicle.tex"), "\n")
