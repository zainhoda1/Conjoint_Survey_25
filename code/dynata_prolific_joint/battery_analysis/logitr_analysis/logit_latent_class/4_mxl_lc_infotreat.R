source(here::here('code', 'setup.R'))

# Data Upload----

data_dce_full <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_logitr_dce_covariate_battery.parquet"
))

data_car_class_prop <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "vehicle_analysis",
  "apollo",
  "0_Car_3c_class_probabilities.parquet"
))
data_suv_class_prop <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "vehicle_analysis",
  "apollo",
  "0_SUV_3c_class_probabilities.parquet"
))
data_class_prop <- rbind(data_car_class_prop, data_suv_class_prop)

data_dce <- data_dce_full %>%
  left_join(data_class_prop, by = "respID") %>%
  filter(!is.na(prob_class1)) %>%
  mutate(
    prob_class_assign = case_when(
      prob_class_assign == "class3" ~ "class1",
      TRUE ~ prob_class_assign
    )
  )

# ftable(data_dce$vehicle_typesuv) / 24
# ftable(data_dce$battery_info_treat) / 24
# ftable(data_dce$vehicle_typesuv, data_dce$battery_info_treat) / 24
ftable(
  data_dce$vehicle_typesuv,
  data_dce$battery_info_treat,
  data_dce$prob_class_assign
) /
  24

# Estimate MXL model----
## Define random parameters for MXL model
randPars <- c(
  mileage = "n",
  battery_range_year0 = "n",
  battery_degradation = "n",
  battery_refurbishpackreplace = "n",
  battery_refurbishcellreplace = "n"
)
numDraws <- 300 # increase for publication

mxl_model_pref <- function(data) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    panelID = "respID",
    pars = c(
      "price",
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",
      "no_choice"
    ),
    randPars = randPars,
    numMultiStarts = 10,
    robust = TRUE,
    drawType = "sobol",
    numDraws = numDraws,
    numCores = 5,
    set.seed(123)
  )
  # cat('n =', length(unique(data$respID)))
  return(model)
}

mxl_model_wtp <- function(data, wtp_pref_model) {
  model <- logitr(
    data = data,
    outcome = "choice",
    obsID = "obsID",
    panelID = "respID",
    pars = c(
      "mileage",
      "battery_range_year0",
      "battery_degradation",
      "battery_refurbishpackreplace",
      "battery_refurbishcellreplace",
      "no_choice"
    ),
    scalePar = 'price',
    randPars = randPars,
    numMultiStarts = 10,
    robust = TRUE,
    drawType = "sobol",
    numDraws = numDraws,
    numCores = 5,
    startVals = wtp_pref_model$Estimate,
    set.seed(6789)
  )
  cat('n =', length(unique(data$respID)))
  return(model)
}


## model estimation ----
# Fit pref and WTP models for all combinations of battery_info_treat x vehicle_typesuv x prob_class_assign
# Results kept in lists: pref_models, wtp_pref_models, wtp_models; summaries in model_meta

# classes <- intersect(c("class1", "class2"), unique(data_dce$prob_class_assign))
grid <- expand.grid(
  battery_info_treat = c(0, 1),
  vehicle_typesuv = c(0, 1),
  prob_class_assign = c("class1", "class2"),
  stringsAsFactors = FALSE
)

pref_models <- list()
wtp_pref_models <- list()
wtp_models <- list()
model_meta <- tibble::tibble()

for (i in seq_len(nrow(grid))) {
  grp <- grid[i, ]
  tag <- paste0(
    "treat",
    grp$battery_info_treat,
    "_",
    ifelse(grp$vehicle_typesuv == 0, "car", "suv"),
    "_",
    grp$prob_class_assign
  )
  data_sub <- data_dce %>%
    filter(
      battery_info_treat == grp$battery_info_treat,
      vehicle_typesuv == grp$vehicle_typesuv,
      prob_class_assign == grp$prob_class_assign
    )

  n_obs <- nrow(data_sub)
  n_resp <- dplyr::n_distinct(data_sub$respID)

  # skip empty groups
  if (n_obs == 0 || n_resp == 0) {
    model_meta <- model_meta %>%
      tibble::add_row(
        tag = tag,
        n_obs = n_obs,
        n_resp = n_resp,
        status = "skipped"
      )
    next
  }

  # Fit preference MXL
  pref_mod <- tryCatch(
    mxl_model_pref(data_sub),
    error = function(e) e
  )

  if (inherits(pref_mod, "error")) {
    model_meta <- model_meta %>%
      tibble::add_row(
        tag = tag,
        n_obs = n_obs,
        n_resp = n_resp,
        status = "pref_error",
        msg = pref_mod$message
      )
    next
  }

  # WTP from pref model
  wtp_pref <- tryCatch(
    wtp(pref_mod, scalePar = "price"),
    error = function(e) e
  )

  if (inherits(wtp_pref, "error")) {
    model_meta <- model_meta %>%
      tibble::add_row(
        tag = tag,
        n_obs = n_obs,
        n_resp = n_resp,
        status = "wtp_pref_error",
        msg = wtp_pref$message
      )
    next
  }

  # Fit WTP-scaled MXL
  wtp_mod <- tryCatch(
    mxl_model_wtp(data_sub, wtp_pref),
    error = function(e) e
  )

  if (inherits(wtp_mod, "error")) {
    model_meta <- model_meta %>%
      tibble::add_row(
        tag = tag,
        n_obs = n_obs,
        n_resp = n_resp,
        status = "wtp_model_error",
        msg = wtp_mod$message
      )
    next
  }

  # Store results
  pref_models[[tag]] <- pref_mod
  wtp_pref_models[[tag]] <- wtp_pref
  wtp_models[[tag]] <- wtp_mod
}

# Save all models in wtp_models ----
saveRDS(
  wtp_models,
  file = here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "latent_class",
    "ml_2c_wtp_treat.Rds"
  )
)

# for (tag in names(wtp_models)) {
#   model <- wtp_models[[tag]]
#   saveRDS(
#     model,
#     file = here(
#       "code",
#       "output",
#       "model_output",
#       "battery_analysis",
#       "logitr",
#       "latent_class",
#       paste0("ml_2c_wtp_", tag, ".Rds")
#     )
#   )
# }
