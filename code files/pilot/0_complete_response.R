# Load libraries
library(fastDummies)
library(here)
library(lubridate)
library(tidyverse)
library(arrow)
library(dotenv)
library(surveydown)
load_dot_env()

surveydown::sd_db_config()

db <- sd_db_connect()

db <- sd_db_connect(
  drv = RPostgres::Postgres(),
  host = Sys.getenv("SD_HOST"),
  port = Sys.getenv("SD_PORT"),
  dbname = Sys.getenv("SD_DBNAME"),
  user = Sys.getenv("SD_USER"),
  password = Sys.getenv("SD_PASSWORD"),
  sslmode = "require"
)

# Change dplyr settings so I can view all columns
options(dplyr.widtkh = Inf)


# Import raw data

data_raw <- read_csv(here(
  "code files",
  "pilot",
  "data",
  "survey_data.csv"
))