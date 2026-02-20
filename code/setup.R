# Load libraries

pkgs <- c(
  "fastDummies", "here", "lubridate", "tidyverse", "arrow", "dotenv",
  "surveydown", "logitr", "cbcTools", "janitor", "apollo", "psych",
  "nFactors", "rlang", "openxlsx", "xlsx", "zipcodeR", "viridis", "maps"
)

installed <- pkgs %in% rownames(installed.packages())
if (any(!installed)) install.packages(pkgs[!installed])

invisible(lapply(pkgs, library, character.only = TRUE))

`%notin%` <- Negate(`%in%`)

# Change dplyr settings so I can view all columns
options(dplyr.width = Inf)
