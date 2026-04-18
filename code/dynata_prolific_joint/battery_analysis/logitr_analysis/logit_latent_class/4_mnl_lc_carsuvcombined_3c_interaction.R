source(here::here('code', 'setup.R'))

# Research question: How do preferences and WTP for BEV attributes differ across latent classes and between car vs. SUV segments?
# Do people who differ in whether they like BEVs also differ in how they value BEV attributes?
# Conditional on considering BEVs, individuals exhibit similar preferences for battery attributes across latent classes defined by powertrain preferences.

# Data Upload----

data_dce_full <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_logitr_dce_covariate_battery.parquet"
))
# length(unique(data_dce_full$respID)) # 1,629 respondents

vars <- c(
  "mileage",
  "battery_range_year0",
  "battery_degradation",
  "battery_refurbishpackreplace",
  "battery_refurbishcellreplace"
)

# Latent Class ----
## Repeat sampling many times and average results
data_class_prop <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "vehicle_analysis",
  "apollo",
  "0_Combined_3c_class_probabilities.parquet"
))

data_class_prop$prob_class_max_assign <- data_class_prop$prob_class_assign

n_sim <- 50

class_draws <- replicate(n_sim, {
  apply(
    data_class_prop[, c("prob_class1", "prob_class2", "prob_class3")],
    1,
    function(p) sample(1:3, size = 1, prob = p)
  )
})

class_draws <- as.data.frame(class_draws)

class_draws$prob_class_assign <- apply(class_draws, 1, function(x) {
  as.numeric(names(which.max(table(x))))
})

data_class_prop$prob_class_assign <- factor(
  class_draws$prob_class_assign,
  levels = 1:3,
  labels = c("class1", "class2", "class3")
)
data_class_prop$same_class_assign <- data_class_prop$prob_class_assign ==
  data_class_prop$prob_class_max_assign

# table(data_class_prop$same_class_assign) # 22 different

## Full sample
data_dce <- data_dce_full %>%
  left_join(data_class_prop, by = "respID") %>%
  filter(!is.na(prob_class1))

## Full sample with prop
# data_dce <- data_dce_full %>%
#   left_join(data_class_prop, by = "respID") %>%
#   filter(!is.na(prob_class1)) %>%
#   filter(prob_class_max >= 0.66)

create_interactions <- function(data, var) {
  data %>%
    mutate(
      !!paste0(var, "_c2") := .data[[var]] * (prob_class_assign == "class2"),
      !!paste0(var, "_c3") := .data[[var]] * (prob_class_assign == "class3"),

      !!paste0(var, "_suv") := .data[[var]] * (vehicle_typesuv == 1),
      !!paste0(var, "_infotreat") := .data[[var]] * (battery_info_treat == 1)

      # !!paste0(var, "_c2_suv") := .data[[var]] * (class == 2) * (vehicle_type == "SUV"),
      # !!paste0(var, "_c3_suv") := .data[[var]] * (class == 3) * (vehicle_type == "SUV")
    )
}

for (v in vars) {
  data_dce <- create_interactions(data_dce, v)
}

# ftable(data_dce$vehicle_typesuv) / 24
# ftable(data_dce$battery_info_treat) / 24
# ftable(data_dce$vehicle_typesuv, data_dce$battery_info_treat) / 24
# ftable(
#   data_dce$vehicle_typesuv,
#   data_dce$battery_info_treat,
#   data_dce$prob_class_assign
# ) /
#   24

## Estimate MNL Interaction model----
## By Class----
mnl_wtp <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "no_choice",
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace"
    ),
    scalePar = 'price',
    numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}


mnl_wtp_3c <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "no_choice",
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",

      # class interactions
      "mileage_c2",
      "mileage_c3",
      "battery_range_year0_c2",
      "battery_range_year0_c3",
      "battery_degradation_c2",
      "battery_degradation_c3",
      "battery_refurbishpackreplace_c2",
      "battery_refurbishpackreplace_c3",
      "battery_refurbishcellreplace_c2",
      "battery_refurbishcellreplace_c3"
    ),
    scalePar = 'price',
    numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}


# Model estimation ----
m_wtp <- mnl_wtp(data_dce)
summary(m_wtp)

m_wtp_3c <- mnl_wtp_3c(data_dce)
summary(m_wtp_3c)


# SUV, Infotreat interactions ----
data_dce <- data_dce_full

create_interactions <- function(data, var) {
  data %>%
    mutate(
      !!paste0(var, "_suv") := .data[[var]] * (vehicle_typesuv == 1),
      !!paste0(var, "_infotreat") := .data[[var]] * (battery_info_treat == 1),
      !!paste0(var, "_risktaking") := .data[[var]] *
        (ATT_risktaker %in% c("somewhat_agree", "strongly_agree"))
    )
}

for (v in vars) {
  data_dce <- create_interactions(data_dce, v)
}


mnl_wtp_suv <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "no_choice",
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",

      # class interactions
      "mileage_suv",
      "battery_range_year0_suv",
      "battery_degradation_suv",
      "battery_refurbishpackreplace_suv",
      "battery_refurbishcellreplace_suv"
    ),
    scalePar = 'price',
    numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}

mnl_wtp_infotreat <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "no_choice",
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",

      # class interactions
      "mileage_infotreat",
      "battery_range_year0_infotreat",
      "battery_degradation_infotreat",
      "battery_refurbishpackreplace_infotreat",
      "battery_refurbishcellreplace_infotreat"
    ),
    scalePar = 'price',
    numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}

mnl_wtp_risktaking <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    pars = c(
      "no_choice",
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",

      # class interactions
      "mileage_risktaking",
      "battery_range_year0_risktaking",
      "battery_degradation_risktaking",
      "battery_refurbishpackreplace_risktaking",
      "battery_refurbishcellreplace_risktaking"
    ),
    scalePar = 'price',
    numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}

##
m_wtp_suv <- mnl_wtp_suv(data_dce)
summary(m_wtp_suv)

m_wtp_infotreat <- mnl_wtp_infotreat(data_dce)
summary(m_wtp_infotreat)

m_wtp_risktaking <- mnl_wtp_risktaking(data_dce)
summary(m_wtp_risktaking)
