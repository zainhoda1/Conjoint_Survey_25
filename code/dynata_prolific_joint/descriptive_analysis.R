source(here::here('code', 'setup.R'))
# Load data
data <- read_parquet(here::here('data',"dynata_prolific_joint", 'data_clean_variables.parquet'))
