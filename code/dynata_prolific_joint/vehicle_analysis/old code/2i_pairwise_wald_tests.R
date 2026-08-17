# 2i_pairwise_wald_tests.R
#
# Pairwise two-sample Wald Z-tests comparing mean MXL coefficients across
# subgroup models. Tests whether estimated WTP parameters differ significantly
# between groups within each stratification dimension.
#
# Method: Z = (β1 − β2) / sqrt(SE1² + SE2²), two-tailed, p = 2*(1−Φ(|Z|))
# Assumes independently estimated models on non-overlapping subsamples.
#
# Note on alternatives:
#   A stronger approach is an interaction model: pool subgroups and add
#   group-dummy × attribute interactions. The t/z statistics on those
#   interactions test the same hypothesis with greater power (joint
#   estimation, no per-attribute multiple-comparison concerns). The Wald
#   approach here is appropriate given models are already estimated separately.
#
# Groups tested:
#   Budget:    car_low, car_high, suv_low, suv_high       (6 pairs)
#   Adoption:  car_likely, car_unlikely, suv_likely, suv_unlikely  (6 pairs)
#
# Output: models/pairwise_wald_tests.xlsx  (two sheets)

source(here::here('code', 'setup.R'))

# ---- Load models -------------------------------------------------------
load(here("models", "mixed_model_1_car_low_panel.RData"))
load(here("models", "mixed_model_1_car_high_panel.RData"))
load(here("models", "mixed_model_1_suv_low_panel.RData"))
load(here("models", "mixed_model_1_suv_high_panel.RData"))
load(here("models", "mixed_model_1_likely_bev_adopter_car.RData"))
load(here("models", "mixed_model_1_unlikely_bev_adopter_car.RData"))
load(here("models", "mixed_model_1_likely_bev_adopter_suv.RData"))
load(here("models", "mixed_model_1_unlikely_bev_adopter_suv.RData"))

budget_models <- list(
  car_low = mixed_model_1_car_low_panel,
  car_high = mixed_model_1_car_high_panel,
  suv_low = mixed_model_1_suv_low_panel,
  suv_high = mixed_model_1_suv_high_panel
)

adoption_models <- list(
  car_likely = mixed_model_1_likely_bev_adopter_car,
  car_unlikely = mixed_model_1_unlikely_bev_adopter_car,
  suv_likely = mixed_model_1_likely_bev_adopter_suv,
  suv_unlikely = mixed_model_1_unlikely_bev_adopter_suv
)

# ---- Parameters to test (mean WTP parameters only) ---------------------
params <- c(
  "scalePar",
  "powertrainbev",
  "powertrainhev",
  "range_bev",
  "mileage",
  "age",
  "operating_cost",
  "no_choice"
)

param_labels <- c(
  scalePar = "λ (scale)",
  powertrainbev = "BEV powertrain",
  powertrainhev = "HEV powertrain",
  range_bev = "Electric range",
  mileage = "Mileage",
  age = "Vehicle age",
  operating_cost = "Operating cost",
  no_choice = "No choice"
)

# ---- Significance stars ------------------------------------------------
sig_stars <- function(p) {
  ifelse(
    p < 0.001,
    "***",
    ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.1, ".", "")))
  )
}

# ---- Wald Z-test for a single parameter between two models -------------
wald_z_test <- function(m1, m2, par) {
  ct1 <- summary(m1)$coefTable
  ct2 <- summary(m2)$coefTable
  if (!(par %in% rownames(ct1)) | !(par %in% rownames(ct2))) {
    return(data.frame(
      b1 = NA,
      se1 = NA,
      b2 = NA,
      se2 = NA,
      z_stat = NA,
      p_value = NA,
      sig = NA_character_
    ))
  }
  b1 <- ct1[par, "Estimate"]
  se1 <- ct1[par, "Std. Error"]
  b2 <- ct2[par, "Estimate"]
  se2 <- ct2[par, "Std. Error"]
  z <- (b1 - b2) / sqrt(se1^2 + se2^2)
  p <- 2 * (1 - pnorm(abs(z)))
  data.frame(
    b1 = b1,
    se1 = se1,
    b2 = b2,
    se2 = se2,
    z_stat = z,
    p_value = p,
    sig = sig_stars(p)
  )
}

# ---- Run all pairwise tests for a model list ---------------------------
run_pairwise <- function(model_list, params) {
  pairs <- combn(names(model_list), 2, simplify = FALSE)
  rows <- lapply(pairs, function(pr) {
    m1_name <- pr[1]
    m2_name <- pr[2]
    lapply(params, function(par) {
      res <- wald_z_test(model_list[[m1_name]], model_list[[m2_name]], par)
      cbind(
        data.frame(
          group1 = m1_name,
          group2 = m2_name,
          parameter = par,
          stringsAsFactors = FALSE
        ),
        res
      )
    })
  })
  do.call(rbind, unlist(rows, recursive = FALSE))
}

budget_results <- run_pairwise(budget_models, params)
adoption_results <- run_pairwise(adoption_models, params)

# Round numeric columns for display
fmt <- function(df) {
  df$b1 <- round(df$b1, 3)
  df$se1 <- round(df$se1, 3)
  df$b2 <- round(df$b2, 3)
  df$se2 <- round(df$se2, 3)
  df$z_stat <- round(df$z_stat, 3)
  df$p_value <- round(df$p_value, 4)
  df
}

budget_results <- fmt(budget_results)
adoption_results <- fmt(adoption_results)

# ---- Wide summary table: rows = parameters, columns = pairs ------------
make_summary_table <- function(results, param_labels) {
  results$pair <- paste0(results$group1, " vs. ", results$group2)
  results$param_label <- param_labels[results$parameter]
  pairs <- unique(results$pair)
  par_order <- param_labels[params[params %in% results$parameter]]

  mat <- sapply(pairs, function(pr) {
    sub <- results[results$pair == pr, ]
    sapply(names(par_order), function(par) {
      row <- sub[sub$parameter == par, ]
      if (nrow(row) == 0 || is.na(row$z_stat)) {
        return(NA_character_)
      }
      paste0(formatC(row$z_stat, format = "f", digits = 3), row$sig)
    })
  })

  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  rownames(df) <- unname(par_order)
  df
}

budget_summary <- make_summary_table(budget_results, param_labels)
adoption_summary <- make_summary_table(adoption_results, param_labels)

# ---- Print to console --------------------------------------------------
cat("\n========== Budget subgroups: Wald Z-test summary ==========\n")
cat("Cells: Z-statistic (* p<0.1, ** p<0.01, *** p<0.001)\n\n")
print(budget_summary)

cat("\n========== Adoption propensity: Wald Z-test summary ==========\n")
cat("Cells: Z-statistic (* p<0.1, ** p<0.01, *** p<0.001)\n\n")
print(adoption_summary)

# ---- Write to Excel ----------------------------------------------------
out_path <- here("models", "pairwise_wald_tests.xlsx")

wb <- createWorkbook()

write_sheet <- function(wb, sheet_name, summary_tbl, long_tbl) {
  addWorksheet(wb, sheet_name)

  # Summary block
  writeData(
    wb,
    sheet_name,
    data.frame(
      Parameter = rownames(summary_tbl),
      summary_tbl,
      check.names = FALSE
    ),
    startRow = 1,
    startCol = 1,
    rowNames = FALSE
  )

  # Separator + long-form detail below
  detail_start <- nrow(summary_tbl) + 4
  writeData(
    wb,
    sheet_name,
    "Full results (long form):",
    startRow = detail_start,
    startCol = 1
  )
  writeData(
    wb,
    sheet_name,
    long_tbl,
    startRow = detail_start + 1,
    startCol = 1,
    rowNames = FALSE
  )
}

write_sheet(wb, "Budget", budget_summary, budget_results)
write_sheet(wb, "Adoption", adoption_summary, adoption_results)

saveWorkbook(wb, out_path, overwrite = TRUE)
cat("\nResults saved to", out_path, "\n")
