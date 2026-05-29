source(here::here('code', 'setup.R'))

if (!requireNamespace("tidycensus", quietly = TRUE)) install.packages("tidycensus")
library(tidycensus)

# Set Census API key (register once at https://api.census.gov/data/key_signup.html)
# census_api_key("CENSUS_API_KEY", install = TRUE)
Sys.getenv("CENSUS_API_KEY")

data <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_clean_variables.parquet"
))

# ---- Identify ZIPs with missing income ----
zip_income_miss <- data |>
  filter(is.na(hhincome_num)) |>
  pull(home_zipcode)

cat("Respondents with missing income:", length(zip_income_miss), "\n")

# ---- Validate ZIP codes ----
# Standardize to 5-character strings, then check against known ZCTAs
zip_padded <- str_pad(as.character(zip_income_miss), width = 5, pad = "0")
valid_zips  <- unique(zip_padded[is_zcta(zip_padded)])
invalid_zips <- unique(zip_padded[!is_zcta(zip_padded)])

cat("Valid ZIPs:  ", length(valid_zips), "\n")
cat("Invalid ZIPs:", length(invalid_zips), "\n")
print(invalid_zips)

# ---- Pull latest ACS 5-year median household income (B19013_001) ----
# geography = "zcta" matches ZIP Code Tabulation Areas
# year = 2023 is the most recent ACS 5-year release
acs_income <- get_acs(
  geography = "zcta",
  variables = "B19013_001",
  year      = 2023,
  survey    = "acs5"
) |>
  transmute(
    zipcode          = str_remove(NAME, "ZCTA5 "),
    median_income_acs = estimate
  ) |>
  filter(zipcode %in% valid_zips)

cat("ZIPs matched in ACS:", nrow(acs_income), "\n")

# ---- Merge back and fill missing hhincome_num ----
data <- data |>
  mutate(home_zipcode_pad = str_pad(as.character(home_zipcode), width = 5, pad = "0")) |>
  left_join(acs_income, by = c("home_zipcode_pad" = "zipcode")) |>
  mutate(
    hhincome_num      = coalesce(hhincome_num, median_income_acs),
    income_source     = case_when(
      !is.na(hhincome_num) & !is.na(median_income_acs) ~ "census_filled",
      TRUE ~ "original"
    )
  ) |>
  select(-home_zipcode_pad, -median_income_acs)

cat("Remaining missing after fill:", sum(is.na(data$hhincome_num)), "\n")
table(data$income_source, useNA = "ifany")

# ---- Save updated dataset ----
write_parquet(
  data,
  here("data", "dynata_prolific_joint", "data_clean_variables.parquet")
)

# table(data$hhincome_num, useNA = "ifany")
