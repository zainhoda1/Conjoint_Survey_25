# rm(list = ls())

source(here::here('code', 'setup.R'))

output_dir <- here(
  "code", "output", "model_output", "battery_analysis", "apollo"
)

# ── Helper: compute entropy from class probability parquet ────────────────────
compute_entropy <- function(cp, K) {
  prob_cols <- paste0("prob_class", 1:K)
  pi_mat    <- as.matrix(cp[, prob_cols])
  H     <- -sum(pi_mat * log(pmax(pi_mat, 1e-300)), na.rm = TRUE)
  N     <- nrow(cp)
  H_max <- N * log(K)
  round(1 - H / H_max, 3)
}

# ── Helper: extract fit indices from Apollo model object ─────────────────────
extract_indices <- function(k, model_tag) {
  model_file <- file.path(output_dir, paste0(model_tag, "_model.rds"))
  cp_file    <- file.path(output_dir, paste0("0_Combined_", k, "c_class_probabilities.parquet"))

  if (!file.exists(model_file)) {
    message("Model not found: ", model_file)
    return(NULL)
  }

  m    <- readRDS(model_file)
  LL   <- m$finalLL
  Npar <- m$numParams
  AIC  <- m$AIC
  BIC  <- m$BIC

  # Adjusted Rho-squared vs observed shares (constants model): from Apollo output
  adj_rho2 <- round(m$adjRho2_C, 3)

  # Entropy (undefined for 1 class)
  entropy <- if (k == 1) {
    NA_real_
  } else if (file.exists(cp_file)) {
    cp <- read_parquet(cp_file)
    compute_entropy(cp, k)
  } else {
    NA_real_
  }

  # Class proportions from posterior means
  class_props <- if (k == 1) {
    "100.0%"
  } else if (file.exists(cp_file)) {
    cp    <- read_parquet(cp_file)
    props <- sapply(1:k, function(j) round(mean(cp[[paste0("prob_class", j)]], na.rm = TRUE), 3))
    paste0(sprintf("%.1f%%", props * 100), collapse = " / ")
  } else {
    NA_character_
  }

  tibble(
    `No. of Classes`  = k,
    Npar              = as.integer(round(Npar)),
    LL                = as.integer(round(LL)),
    `L²`              = NA_integer_,   # filled after all rows built
    AIC               = as.integer(round(AIC)),
    BIC               = as.integer(round(BIC)),
    `Adj.Rho²`        = adj_rho2,
    Entropy           = entropy,
    `Class proportions` = class_props
  )
}

# ── Null LL: read from the 1c model in the same series (consistent dataset) ───
null_model_file <- file.path(output_dir, "piecewise_rangeloss_car_suv_lc_1c_1_model.rds")
LL_null <- if (file.exists(null_model_file)) {
  readRDS(null_model_file)$finalLL
} else {
  NA_real_
}

# ── Get N (number of unique respondents from any class prob file) ────────────
n_distinct_respondents <- {
  cp_ref <- file.path(output_dir, "0_Combined_6c_class_probabilities.parquet")
  if (file.exists(cp_ref)) nrow(read_parquet(cp_ref)) else NA_integer_
}

# ── Model configurations ──────────────────────────────────────────────────────
model_configs <- list(
  list(k = 1, tag = "piecewise_rangeloss_car_suv_lc_1c_1"),
  list(k = 2, tag = "piecewise_rangeloss_car_suv_lc_2c_1"),
  list(k = 3, tag = "piecewise_rangeloss_car_suv_lc_3c_1"),
  list(k = 4, tag = "piecewise_rangeloss_car_suv_lc_4c_1"),
  list(k = 5, tag = "piecewise_rangeloss_car_suv_lc_5c_1"),
  list(k = 6, tag = "piecewise_rangeloss_car_suv_lc_6c_1"),
  list(k = 7, tag = "piecewise_rangeloss_car_suv_lc_7c_1"),
  list(k = 8, tag = "piecewise_rangeloss_car_suv_lc_8c_1")
)

# ── Build table ──────────────────────────────────────────────────────────────
fit_table <- map_dfr(model_configs, function(cfg) {
  extract_indices(cfg$k, cfg$tag)
}) %>%
  mutate(
    `L²` = if (!is.na(LL_null)) as.integer(round(-2 * (LL_null - LL))) else NA_integer_
  ) %>%
  select(
    `No. of Classes`, Npar, LL, `L²`, AIC, BIC, `Adj.Rho²`, Entropy,
    `Class proportions`
  )

fit_table

# ── GT table ─────────────────────────────────────────────────────────────────
gt_fit <- fit_table %>%
  gt() %>%
  tab_header(
    title    = md("**Latent Class Model Fit Index Comparison**"),
    subtitle = md(paste0(
      "Piecewise linear range and range loss model · N = ",
      n_distinct_respondents, " respondents"
    ))
  ) %>%
  tab_spanner(
    label   = md("**Information Criteria**"),
    columns = c(AIC, BIC, `Adj.Rho²`)
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left",   columns = `Class proportions`) %>%
  fmt_integer(columns = c(Npar, LL, `L²`, AIC, BIC)) %>%
  fmt_number(columns = `Adj.Rho²`, decimals = 3) %>%
  fmt_number(columns = Entropy,    decimals = 3) %>%
  tab_footnote(
    footnote  = md("Entropy = 1 − H/H_max, where H = −Σ π_ik log(π_ik). Values closer to 1 indicate cleaner class separation."),
    locations = cells_column_labels(Entropy)
  ) %>%
  tab_footnote(
    footnote  = paste0("L² = −2(LL_1c − LL_model). Null = 1-class MNL on the same dataset (LL = ", as.integer(round(LL_null)), ")."),
    locations = cells_column_labels(`L²`)
  ) %>%
  tab_footnote(
    footnote  = md("Adj.Rho² = adjusted rho-squared vs observed shares (constants model)."),
    locations = cells_column_labels(`Adj.Rho²`)
  ) %>%
  tab_options(
    table.font.size           = px(13),
    heading.align             = "left",
    column_labels.font.weight = "bold",
    table.border.top.color    = "#004D80",
    table.border.bottom.color = "#004D80"
  ) %>%
  opt_stylize(style = 1, color = "blue")

gt_fit

gtsave(
  gt_fit,
  file = here::here(
    "code", "output", "model_output", "battery_analysis", "apollo",
    "0_lc_fit_index_comparison.html"
  )
)
