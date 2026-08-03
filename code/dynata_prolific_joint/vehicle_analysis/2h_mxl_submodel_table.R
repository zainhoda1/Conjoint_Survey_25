source(here::here('code', 'setup.R'))

# ============================================================
# Combined MXL results table — budget submodels + adoption
# propensity submodels — for vehicle paper
# Output: paper_writing/vehicle_paper/attachments/mxl_results_vehicle.tex
# ============================================================

# ---- Load models ----
load(here("models", "mixed_model_1_car_low_panel.RData"))
load(here("models", "mixed_model_1_car_high_panel.RData"))
load(here("models", "mixed_model_1_suv_low_panel.RData"))
load(here("models", "mixed_model_1_suv_high_panel.RData"))
load(here("models", "mixed_model_1_likely_bev_adopter_car.RData"))
load(here("models", "mixed_model_1_unlikely_bev_adopter_car.RData"))
load(here("models", "mixed_model_1_likely_bev_adopter_suv.RData"))
load(here("models", "mixed_model_1_unlikely_bev_adopter_suv.RData"))

models <- list(
  car_low      = mixed_model_1_car_low_panel,
  car_high     = mixed_model_1_car_high_panel,
  suv_low      = mixed_model_1_suv_low_panel,
  suv_high     = mixed_model_1_suv_high_panel,
  car_likely   = mixed_model_1_likely_bev_adopter_car,
  car_unlikely = mixed_model_1_unlikely_bev_adopter_car,
  suv_likely   = mixed_model_1_likely_bev_adopter_suv,
  suv_unlikely = mixed_model_1_unlikely_bev_adopter_suv
)

# ---- Helper: significance stars ----
sig_stars <- function(p) {
  ifelse(p < 0.001, "***",
    ifelse(p < 0.01, "**",
      ifelse(p < 0.05, "*",
        ifelse(p < 0.1, ".", ""))))
}

# ---- Helper: format one cell as shortstack ----
fmt_cell <- function(ct, par) {
  if (!par %in% rownames(ct)) return("--")
  est   <- ct[par, "Estimate"]
  se    <- ct[par, "Std. Error"]
  pval  <- ct[par, "Pr(>|z|)"]
  stars <- sig_stars(pval)
  sprintf("\\shortstack[r]{%.3f\\\\(%.3f)%s}", est, se, stars)
}

# ---- Extract coef tables ----
coef_tabs <- lapply(models, function(m) summary(m)$coefTable)

# ---- Parameter order and display labels ----
mean_pars <- c(
  "scalePar"       = "$\\lambda$ (scale)",
  "powertrainbev"  = "BEV powertrain",
  "powertrainhev"  = "HEV powertrain",
  "range_bev"      = "Electric range",
  "mileage"        = "Mileage",
  "age"            = "Vehicle age",
  "operating_cost" = "Operating cost",
  "no_choice"      = "No choice"
)

sd_pars <- c(
  "sd_powertrainbev"  = "$|\\hat{\\sigma}|$: BEV powertrain",
  "sd_powertrainhev"  = "$|\\hat{\\sigma}|$: HEV powertrain",
  "sd_range_bev"      = "$|\\hat{\\sigma}|$: Electric range",
  "sd_mileage"        = "$|\\hat{\\sigma}|$: Mileage",
  "sd_age"            = "$|\\hat{\\sigma}|$: Vehicle age",
  "sd_operating_cost" = "$|\\hat{\\sigma}|$: Operating cost",
  "sd_no_choice"      = "$|\\hat{\\sigma}|$: No choice"
)

# ---- Build one parameter row across all models ----
make_row <- function(par, label, ct_list) {
  cells <- sapply(ct_list, fmt_cell, par = par)
  paste0("\\quad ", label, " & ", paste(cells, collapse = " & "), " \\\\")
}

# ---- Mean parameter rows ----
mean_rows <- mapply(
  make_row,
  par   = names(mean_pars),
  label = unname(mean_pars),
  MoreArgs = list(ct_list = coef_tabs),
  SIMPLIFY = TRUE
)

# ---- SD parameter rows ----
sd_rows <- mapply(
  make_row,
  par   = names(sd_pars),
  label = unname(sd_pars),
  MoreArgs = list(ct_list = coef_tabs),
  SIMPLIFY = TRUE
)

# ---- Fit statistics ----
fit_stat <- function(models, fn) sapply(models, fn)

n_resp    <- fit_stat(models, function(m) m$n$obs / 6)
n_obs     <- fit_stat(models, function(m) m$n$obs)
loglik    <- fit_stat(models, function(m) round(summary(m)$statTable["Log-Likelihood:", 1], 2))
null_ll   <- fit_stat(models, function(m) round(summary(m)$statTable["Null Log-Likelihood:", 1], 2))
aic       <- fit_stat(models, function(m) round(summary(m)$statTable["AIC:", 1], 2))
bic       <- fit_stat(models, function(m) round(summary(m)$statTable["BIC:", 1], 2))
r2        <- fit_stat(models, function(m) round(summary(m)$statTable["McFadden R2:", 1], 4))
adj_r2    <- fit_stat(models, function(m) round(summary(m)$statTable["Adj McFadden R2:", 1], 4))

fmt_stat <- function(label, vals, fmt = "f", digits = 2, big_mark = ",") {
  cells <- formatC(vals, format = fmt, digits = digits, big.mark = big_mark)
  paste0(label, " & ", paste(cells, collapse = " & "), " \\\\")
}

stat_rows <- c(
  fmt_stat("Respondents",          n_resp, fmt = "d", digits = 0),
  fmt_stat("Choice observations",  n_obs,  fmt = "d", digits = 0),
  fmt_stat("Log-Likelihood",       loglik, fmt = "f", digits = 2),
  fmt_stat("Null Log-Likelihood",  null_ll,fmt = "f", digits = 2),
  fmt_stat("AIC",                  aic,    fmt = "f", digits = 2),
  fmt_stat("BIC",                  bic,    fmt = "f", digits = 2),
  fmt_stat("McFadden $R^2$",       r2,     fmt = "f", digits = 4, big_mark = ""),
  fmt_stat("Adj.\\ McFadden $R^2$",adj_r2, fmt = "f", digits = 4, big_mark = "")
)

# ---- Assemble LaTeX table ----
col_spec   <- "l *{8}{>{\\centering\\arraybackslash}p{2.4cm}}"
header_top <- paste0(
  " & \\multicolumn{4}{c}{\\textbf{By Budget}} & ",
  "\\multicolumn{4}{c}{\\textbf{By BEV Adoption Propensity}} \\\\"
)
cmidrule    <- "\\cmidrule(lr){2-5}\\cmidrule(lr){6-9}"
header_sub  <- paste0(
  " & Car (Low) & Car (High) & SUV (Low) & SUV (High)",
  " & Car (Likely) & Car (Unlikely) & SUV (Likely) & SUV (Unlikely) \\\\"
)

body <- c(
  "\\midrule",
  "\\multicolumn{9}{l}{\\textbf{\\textit{Mean parameters}}} \\\\[1ex]",
  mean_rows,
  "\\addlinespace[1.5ex]",
  "\\multicolumn{9}{l}{\\textbf{\\textit{Standard deviation}}} \\\\[1ex]",
  sd_rows,
  "\\midrule",
  stat_rows,
  "\\bottomrule"
)

tex_table <- paste(c(
  "\\begin{landscape}",
  "\\begin{table}[pos=H]",
  "\\footnotesize",
  "\\caption{Mixed logit model estimates in WTP space: budget subgroups and BEV adoption propensity subgroups (price unit: \\$10,000).}",
  "\\label{table:mxl_results_vehicle}",
  "\\begin{adjustbox}{width=\\linewidth, center}",
  paste0("\\begin{tabular}{", col_spec, "}"),
  "\\toprule",
  header_top,
  cmidrule,
  header_sub,
  body,
  "\\end{tabular}",
  "\\end{adjustbox}",
  "\\begin{minipage}{\\linewidth}\\vspace{4pt}\\footnotesize",
  "Notes: WTP estimates in \\$10,000 units. Significance codes: $p<0.001$ `***', $p<0.01$ `**', $p<0.05$ `*', $p<0.1$ `.'.",
  "Standard errors in parentheses.",
  "\\end{minipage}",
  "\\end{table}",
  "\\end{landscape}"
), collapse = "\n")

attach_dir <- here("paper_writing", "vehicle_paper", "attachments")
dir.create(attach_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(attach_dir, "mxl_results_vehicle.tex")
writeLines(tex_table, out_path)
cat("Written to", out_path, "\n")
