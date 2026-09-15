source(here::here('code', 'setup.R'))

#######
# Load the Dataset:

data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint_vehicle.parquet"
 )) 
# |> 
#   filter(collection_round != 'round_4')


data_raw_dynata <- read_parquet(here(
  "data",
  "dynata_testing",
  "data.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'),
 -respID) %>%
  mutate(data_source = 'dynata')

data_raw_prolific <- read_parquet(here(
  "data",
  "prolific_testing",
  "data.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'), -respID) %>%
  mutate(psid = prolific_pid, data_source = 'prolific') %>%
  select(-study_id, -prolific_session_id, -prolific_pid, -current_page)


data_raw_prolific_round2 <- read_parquet(here(
  "data",
  "prolific_testing",
  "data_round_2+3+4_may_26.parquet"
)) %>%
  select(-starts_with('time'), -ends_with('button'), -respID) %>%
  mutate(psid = prolific_pid, data_source = 'prolific') %>%
  select(-study_id, -prolific_session_id, -prolific_pid, -current_page
  )


data_joint %>% 
  group_by( data_source) %>% 
  count()

#########

data <- data_joint %>%
  mutate(
    price = price / 10000, # 0.5-6
    range_bev = range_bev / 100, # 0.5 - 2.5
    mileage = mileage / 10000, # 2 - 6
    age = age, # 2 - 8
    operating_cost = operating_cost / 10 # 0.3 - 2.5,
  ) 

data_raw_joined <- rbind(
  rbind(data_raw_dynata, data_raw_prolific),
  data_raw_prolific_round2
)


data_raw_joined <- data_raw_joined |> 
  filter(psid %in% unique(data_joint$psid) )


############################################################
# ---- Table 3: Household vehicle context & used-BEV purchase interest ----
# For paper subsection 3.3, inserted after Table 2 (sample_vs_acs_vehicle).
############################################################

n_total <- nrow(data_raw_joined)
n_primary_fuel <- sum(!is.na(data_raw_joined$primary_veh_fuel))

hh_context_tab <- bind_rows(
data_raw_joined %>%
    filter(!is.na(household_veh_count)) %>%
    mutate(category = if_else(household_veh_count >= 4, "4 or more",
                               as.character(household_veh_count))) %>%
    count(category) %>%
    mutate(
      pct = round(100 * n / sum(n), 1),
      variable = "Household vehicle count",
      order = match(category, c("0", "1", "2", "3", "4 or more"))
    ) %>%
    arrange(order),

data_raw_joined %>%
    filter(!is.na(primary_veh_fuel)) %>%
    mutate(category = recode(primary_veh_fuel,
      "icev" = "Gasoline (ICEV)",
      "hev"  = "Hybrid (HEV)",
      "phev" = "Plug-in hybrid (PHEV)",
      "bev"  = "Battery electric (BEV)",
      "other" = "Other"
    )) %>%
    count(category) %>%
    mutate(
      pct = round(100 * n / sum(n), 1),
      variable = "Current primary vehicle fuel type",
      order = match(category, c("Gasoline (ICEV)", "Hybrid (HEV)",
                                "Plug-in hybrid (PHEV)", "Battery electric (BEV)", "Other"))
    ) %>%
    arrange(order),

data_raw_joined %>%
  group_by(neighbor_ev_info) %>%
  count()

data_raw_joined %>%
  group_by(household_veh_count) %>%
  count()


data_raw_joined %>%
  group_by(next_veh_fuel_new_bev) %>%
  count()

data_raw_joined %>%
  group_by(next_veh_fuel_used_bev) %>%
  count()


data_raw_joined %>%
  group_by(next_veh_fuel_new_bev) %>%
  count()


###########

# ------------------------------------------------------------------------
# Table~\ref{table:mxl_results_vehicle}: mean/SD coefficients, all six
# vehicle-segment x budget-tier panel models, for paper_writing/vehicle_paper.
------------------------------------------------------------------------

models_table <- list(
  car      = mixed_model_1_car_panel,
  suv      = mixed_model_1_suv_panel,
  car_low  = mixed_model_1_car_low_panel,
  car_high = mixed_model_1_car_high_panel,
  suv_low  = mixed_model_1_suv_low_panel,
  suv_high = mixed_model_1_suv_high_panel
)

# ---- Helper: significance stars ----
sig_stars <- function(p) {
  ifelse(p < 0.001, "***",
    ifelse(p < 0.01, "**",
      ifelse(p < 0.05, "*",
        ifelse(p < 0.1, ".", ""))))
}

# Per-parameter display rescale factor (display-only; does not affect
# significance, since z = estimate / se is invariant to a positive rescale).
rescale_factor <- c(
  scalePar          = 1,
  powertrainbev     = 1,
  powertrainhev     = 1,
  range_bev         = 100,
  mileage           = 10000,
  age               = 1,
  operating_cost    = 1,
  no_choice         = 1,
  sd_powertrainbev  = 1,
  sd_powertrainhev  = 1,
  sd_range_bev      = 100,
  sd_mileage        = 10000,
  sd_age            = 1,
  sd_operating_cost = 1,
  sd_no_choice      = 1
)

# ---- Helper: format one cell as shortstack ----
fmt_cell <- function(ct, par) {
  if (!par %in% rownames(ct)) return("--")
  scale <- rescale_factor[[par]]
  est   <- ct[par, "Estimate"] * scale
  se    <- ct[par, "Std. Error"] * scale
  pval  <- ct[par, "Pr(>|z|)"]
  stars <- sig_stars(pval)
  sprintf("\\shortstack[r]{%.3f\\\\(%.3f)%s}", est, se, stars)
}

# ---- Extract coef tables ----
coef_tabs <- lapply(models_table, function(m) summary(m)$coefTable)

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

mean_rows <- mapply(
  make_row,
  par   = names(mean_pars),
  label = unname(mean_pars),
  MoreArgs = list(ct_list = coef_tabs),
  SIMPLIFY = TRUE
)

sd_rows <- mapply(
  make_row,
  par   = names(sd_pars),
  label = unname(sd_pars),
  MoreArgs = list(ct_list = coef_tabs),
  SIMPLIFY = TRUE
)

# ---- Fit statistics ----
fit_stat <- function(models, fn) sapply(models, fn)

n_resp  <- fit_stat(models_table, function(m) m$n$obs / 6)
n_obs   <- fit_stat(models_table, function(m) m$n$obs)
loglik  <- fit_stat(models_table, function(m) round(summary(m)$statTable["Log-Likelihood:", 1], 2))
null_ll <- fit_stat(models_table, function(m) round(summary(m)$statTable["Null Log-Likelihood:", 1], 2))
aic     <- fit_stat(models_table, function(m) round(summary(m)$statTable["AIC:", 1], 2))
bic     <- fit_stat(models_table, function(m) round(summary(m)$statTable["BIC:", 1], 2))
r2      <- fit_stat(models_table, function(m) round(summary(m)$statTable["McFadden R2:", 1], 4))
adj_r2  <- fit_stat(models_table, function(m) round(summary(m)$statTable["Adj McFadden R2:", 1], 4))

fmt_stat <- function(label, vals, fmt = "f", digits = 2, big_mark = ",") {
  cells <- formatC(vals, format = fmt, digits = digits, big.mark = big_mark)
  paste0(label, " & ", paste(cells, collapse = " & "), " \\\\")
}

stat_rows <- c(
  fmt_stat("Respondents",           n_resp,  fmt = "d", digits = 0),
  fmt_stat("Choice observations",   n_obs,   fmt = "d", digits = 0),
  fmt_stat("Log-Likelihood",        loglik,  fmt = "f", digits = 2),
  fmt_stat("Null Log-Likelihood",   null_ll, fmt = "f", digits = 2),
  fmt_stat("AIC",                   aic,     fmt = "f", digits = 2),
  fmt_stat("BIC",                   bic,     fmt = "f", digits = 2),
  fmt_stat("McFadden $R^2$",        r2,      fmt = "f", digits = 4, big_mark = ""),
  fmt_stat("Adj.\\ McFadden $R^2$", adj_r2,  fmt = "f", digits = 4, big_mark = "")
)

# ---- Assemble LaTeX table ----
col_spec <- "l *{6}{>{\\centering\\arraybackslash}p{2.4cm}}"
header_top <- paste0(
  " & \\multicolumn{2}{c}{\\textbf{By Vehicle Type}} & ",
  "\\multicolumn{4}{c}{\\textbf{By Budget}} \\\\"
)
cmidrule   <- "\\cmidrule(lr){2-3}\\cmidrule(lr){4-7}"
header_sub <- paste0(
  " & Car & SUV & Car (Low) & Car (High) & SUV (Low) & SUV (High) \\\\"
)

body <- c(
  "\\midrule",
  "\\multicolumn{7}{l}{\\textbf{\\textit{Mean parameters}}} \\\\[1ex]",
  mean_rows,
  "\\addlinespace[1.5ex]",
  "\\multicolumn{7}{l}{\\textbf{\\textit{Standard deviation}}} \\\\[1ex]",
  sd_rows,
  "\\midrule",
  stat_rows,
  "\\bottomrule"
)

tex_table <- paste(c(
  "\\begin{landscape}",
  "\\begin{table}[pos=H]",
  "\\footnotesize",
  "\\caption{Mixed logit model estimates in WTP space: by vehicle type and budget subgroups.}",
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
  "Notes: mean and standard-deviation coefficients in \\$1,000 units, denominated per natural attribute unit: powertrain indicators and the no-choice constant per 1 (dummy); electric range per 100 miles; mileage per 10,000 miles; vehicle age per 1 year; operating cost per 1 cent/mile. The scale parameter $\\lambda$ is not a dollar-denominated WTP coefficient.",
  "Significance codes: $p<0.001$ `***', $p<0.01$ `**', $p<0.05$ `*', $p<0.1$ `.'.",
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

