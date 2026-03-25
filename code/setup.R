# Load libraries

pkgs <- c(
  "fastDummies",
  "here",
  "lubridate",
  "tidyverse",
  "arrow",
  "dotenv",
  "surveydown",
  "logitr",
  "cbcTools",
  "janitor",
  "apollo",
  "psych",
  "nFactors",
  "rlang",
  "openxlsx",
  # "xlsx",
  "zipcodeR",
  "viridis",
  "maps",
  "ggridges",
  "purrr",
  "gt",
  "scales"
)

installed <- pkgs %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(pkgs[!installed])
}

invisible(lapply(pkgs, library, character.only = TRUE))

`%notin%` <- Negate(`%in%`)

# Change dplyr settings so I can view all columns
options(dplyr.width = Inf)

create_confidence_intervals <- function(model) {
  # Description:
  # This function takes logit model and returns confidence interval.

  coefs <- coef(model)
  # Get the model coefficients and covariance matrix
  covariance <- vcov(model)

  # Take 10,000 draws of the coefficients
  coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))

  # Compute WTP for each coefficient draw
  wtp_draws = -1 * (coef_draws[,] / coef_draws[, 'price'])

  # Adding dollar values
  wtp_draws <- wtp_draws %>%
    mutate(across(where(is.numeric), ~ .x * 10^4)) %>% 
    mutate(
      BEV100 = (powertrainbev + range_bev) ,
      BEV200 = (powertrainbev + range_bev * 2) ,
      BEV300 = (powertrainbev + range_bev * 3) 
    )

  # For each coefficient, get the mean and 95% confidence interval of WTP
  wtp_ci <- ci(wtp_draws, level = 0.95)%>%
    mutate(across(everything(), ~ round(.x, 2)))

  return(wtp_ci)
}
