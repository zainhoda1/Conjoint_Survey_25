source(here::here('code', 'setup.R'))

# Load all models ----
load(here(
  "code", "output", "model_output", "battery_analysis", "logitr",
  "mxl_pref_model_car_treat.RData"
))
load(here(
  "code", "output", "model_output", "battery_analysis", "logitr",
  "mxl_pref_model_suv_treat.RData"
))
load(here(
  "code", "output", "model_output", "battery_analysis", "logitr",
  "mxl_pref_model_car_notreat.RData"
))
load(here(
  "code", "output", "model_output", "battery_analysis", "logitr",
  "mxl_pref_model_suv_notreat.RData"
))
load(here(
  "code", "output", "model_output", "battery_analysis", "logitr",
  "mxl_wtp_model_car_treat.RData"
))
load(here(
  "code", "output", "model_output", "battery_analysis", "logitr",
  "mxl_wtp_model_suv_treat.RData"
))
load(here(
  "code", "output", "model_output", "battery_analysis", "logitr",
  "mxl_wtp_model_car_notreat.RData"
))
load(here(
  "code", "output", "model_output", "battery_analysis", "logitr",
  "mxl_wtp_model_suv_notreat.RData"
))

# Helpers ----

sig_stars <- function(z) {
  case_when(
    abs(z) >= 2.576 ~ "***",
    abs(z) >= 1.960 ~ "**",
    abs(z) >= 1.645 ~ "*",
    TRUE            ~ ""
  )
}

extract_coef_table <- function(model, model_name) {
  cf <- coef(model)
  se <- sqrt(diag(vcov(model)))
  z  <- cf / se
  tibble(
    parameter                       = names(cf),
    !!paste0(model_name, "_est")   := round(cf, 4),
    !!paste0(model_name, "_se")    := round(se, 4),
    !!paste0(model_name, "_sig")   := sig_stars(z)
  )
}

extract_fit_stats <- function(model, model_name) {
  ll      <- model$logLik
  ll_null <- model$nullLogLik
  n_obs   <- model$n$obs
  n_resp  <- model$n$panels %||% NA_real_
  k       <- length(coef(model))
  aic_val <- -2 * ll + 2 * k
  bic_val <- -2 * ll + log(n_obs) * k
  rho2    <- 1 - ll / ll_null

  tibble(
    parameter                      = c(
      "Log-Likelihood",
      "Null Log-Likelihood",
      "McFadden R2",
      "AIC",
      "BIC",
      "N Observations (choice occasions)",
      "N Respondents"
    ),
    !!paste0(model_name, "_est")  := round(
      c(ll, ll_null, rho2, aic_val, bic_val, n_obs, n_resp),
      4
    ),
    !!paste0(model_name, "_se")   := NA_real_,
    !!paste0(model_name, "_sig")  := NA_character_
  )
}

# Named model list (order: pref then wtp, car before suv, treat before notreat) ----
models <- list(
  pref_car_treat   = pref_model_car_treat,
  pref_suv_treat   = pref_model_suv_treat,
  pref_car_notreat = pref_model_car_notreat,
  pref_suv_notreat = pref_model_suv_notreat,
  wtp_car_treat    = wtp_model_car_treat,
  wtp_suv_treat    = wtp_model_suv_treat,
  wtp_car_notreat  = wtp_model_car_notreat,
  wtp_suv_notreat  = wtp_model_suv_notreat
)

# Build coefficient block (full join handles different parameter sets) ----
coef_combined <- imap(models, extract_coef_table) %>%
  reduce(full_join, by = "parameter")

# Build model fit block ----
fit_combined <- imap(models, extract_fit_stats) %>%
  reduce(full_join, by = "parameter")

# Stack: coefficients on top, fit stats on bottom ----
final_table <- bind_rows(coef_combined, fit_combined)

# Write Excel with formatting ----
out_path <- here(
  "code", "output", "model_output", "battery_analysis", "logitr",
  "0_battery_model_info_treat_combined.xlsx"
)

wb <- createWorkbook()
addWorksheet(wb, "Results")

# Styles
style_header <- createStyle(
  fontName = "Roboto Condensed", fontSize = 11, textDecoration = "bold",
  fgFill = "#1F4E79", fontColour = "white",
  halign = "center", valign = "center", wrapText = TRUE,
  border = "Bottom", borderColour = "#FFFFFF"
)
style_param <- createStyle(
  fontName = "Roboto Condensed", fontSize = 10,
  textDecoration = "bold"
)
style_est <- createStyle(
  fontName = "Roboto Condensed", fontSize = 10,
  numFmt = "0.0000", halign = "right"
)
style_se <- createStyle(
  fontName = "Roboto Condensed", fontSize = 10,
  numFmt = "0.0000", halign = "right", fontColour = "#666666"
)
style_sig <- createStyle(
  fontName = "Roboto Condensed", fontSize = 10,
  halign = "center"
)
style_fit_label <- createStyle(
  fontName = "Roboto Condensed", fontSize = 10,
  textDecoration = "italic", fgFill = "#F2F2F2"
)
style_fit_val <- createStyle(
  fontName = "Roboto Condensed", fontSize = 10,
  numFmt = "0.0000", halign = "right", fgFill = "#F2F2F2"
)
style_divider <- createStyle(
  border = "Top", borderColour = "#1F4E79"
)

# Write data
writeData(wb, "Results", final_table, startRow = 1, startCol = 1,
          headerStyle = style_header)

n_coef_rows <- nrow(coef_combined)
n_total     <- nrow(final_table)

# Column index helpers
est_cols <- grep("_est$",  names(final_table))
se_cols  <- grep("_se$",   names(final_table))
sig_cols <- grep("_sig$",  names(final_table))
param_col <- 1

# Apply styles to data rows (row 1 = header, so data starts row 2)
addStyle(wb, "Results", style_param,
         rows = 2:(n_coef_rows + 1), cols = param_col, gridExpand = TRUE)
addStyle(wb, "Results", style_est,
         rows = 2:(n_coef_rows + 1), cols = est_cols, gridExpand = TRUE)
addStyle(wb, "Results", style_se,
         rows = 2:(n_coef_rows + 1), cols = se_cols, gridExpand = TRUE)
addStyle(wb, "Results", style_sig,
         rows = 2:(n_coef_rows + 1), cols = sig_cols, gridExpand = TRUE)

# Fit stats rows
fit_start <- n_coef_rows + 2
fit_end   <- n_total + 1
addStyle(wb, "Results", style_fit_label,
         rows = fit_start:fit_end, cols = param_col, gridExpand = TRUE)
addStyle(wb, "Results", style_fit_val,
         rows = fit_start:fit_end, cols = c(est_cols, se_cols, sig_cols),
         gridExpand = TRUE)
# Divider between coef and fit blocks
addStyle(wb, "Results", style_divider,
         rows = fit_start, cols = 1:ncol(final_table), gridExpand = TRUE,
         stack = TRUE)

# Column widths
setColWidths(wb, "Results", cols = param_col, widths = 32)
setColWidths(wb, "Results", cols = est_cols,  widths = 11)
setColWidths(wb, "Results", cols = se_cols,   widths = 11)
setColWidths(wb, "Results", cols = sig_cols,  widths = 6)
setRowHeights(wb, "Results", rows = 1, heights = 30)

# Freeze panes: keep parameter column and header visible while scrolling
freezePane(wb, "Results", firstActiveRow = 2, firstActiveCol = 2)

saveWorkbook(wb, out_path, overwrite = TRUE)
cat("Saved to:", out_path, "\n")
cat("Rows:", nrow(final_table), "| Cols:", ncol(final_table), "\n")
cat("Note: *** p<0.01  ** p<0.05  * p<0.10  (two-tailed z-test)\n")
