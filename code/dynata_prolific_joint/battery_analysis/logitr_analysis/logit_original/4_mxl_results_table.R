source(here::here('code', 'setup.R'))
library(knitr)
library(kableExtra)

# ── Load WTP model ────────────────────────────────────────────────────────────
load(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "logitr",
  "mxl_wtp_model_all.RData"
))

s <- summary(wtp_model_all)
coef_raw <- as.data.frame(s$coefTable)

# Recompute p-values: stored as 0 in model object; derive from z-scores
coef_raw$p_val <- 2 * pnorm(-abs(coef_raw$`z-value`))

stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ ".",
    TRUE ~ ""
  )
}
fmt_p <- function(p) ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))

# ── Parameter labels ──────────────────────────────────────────────────────────
param_labels <- c(
  scalePar = "$\\lambda$ (scale)",
  mileage = "Mileage",
  battery_range_year3 = "Range (Year 3)",
  battery_range_loss = "Range loss (Year 3 to Year 8)",
  battery_refurbishpackreplace = "Battery Refurbishment: Pack Replace",
  battery_refurbishcellreplace = "Battery Refurbishment: Cell Replace",
  no_choice = "No choice (opt-out)",
  sd_mileage = "SD: Mileage",
  sd_battery_range_year3 = "SD: Range (Year 3)",
  sd_battery_range_loss = "SD: Range loss (Year 3 to Year 8)",
  sd_battery_refurbishpackreplace = "SD: Battery Refurbishment: Pack Replace",
  sd_battery_refurbishcellreplace = "SD: Battery Refurbishment: Cell Replace"
)

n_mean <- sum(!grepl("^sd_", rownames(coef_raw))) # 7 mean params
n_sd <- sum(grepl("^sd_", rownames(coef_raw))) # 5 SD params

# ── Draw quantiles from randParSummary ───────────────────────────────────────
# randParSummary rows: base param names (mileage, battery_range_year0, ...)
# Q values go on mean parameter rows; scalePar/no_choice (not random) get ""
rps <- as.data.frame(s$randParSummary)

mean_rownames <- rownames(coef_raw)[!grepl("^sd_", rownames(coef_raw))]

fmt_q <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))

# rps rows for non-random params (scalePar, no_choice) return NA -> ""
mean_q1 <- fmt_q(rps[mean_rownames, "1st Qu."])
mean_med <- fmt_q(rps[mean_rownames, "Median"])
mean_q3 <- fmt_q(rps[mean_rownames, "3rd Qu."])

# SD rows get empty Q cols (draws already represented via mean section)
empty_q_sd <- rep("", n_sd)

# ── Build display data frame (8 cols) ─────────────────────────────────────────
display_df <- data.frame(
  Parameter = param_labels[rownames(coef_raw)],
  Estimate = sprintf("%.3f", coef_raw$Estimate),
  SE = sprintf("%.3f", coef_raw$`Std. Error`),
  p = fmt_p(coef_raw$p_val),
  Sig = stars(coef_raw$p_val),
  Q1 = c(mean_q1, empty_q_sd),
  Median_ = c(mean_med, empty_q_sd),
  Q3 = c(mean_q3, empty_q_sd),
  stringsAsFactors = FALSE
)

# ── Fit indices ───────────────────────────────────────────────────────────────
N <- wtp_model_all$n$obs
k <- wtp_model_all$n$pars
LL <- wtp_model_all$logLik
LL0 <- wtp_model_all$nullLogLik
AIC_v <- -2 * LL + 2 * k
BIC_v <- -2 * LL + k * log(N)
rho2 <- 1 - LL / LL0
arho2 <- 1 - (LL - k) / LL0

# ── Generate tabular via kable ────────────────────────────────────────────────
n_cols <- ncol(display_df) # 8
n_left <- 5L # Parameter, Estimate, SE, p, Sig
n_right <- 3L # 1st Qu., Median, 3rd Qu.

col_names_latex <- c(
  "Parameter",
  "Estimate",
  "Std. Error",
  "$p$-value",
  "",
  "",
  "",
  ""
)

tabular_raw <- kable(
  display_df,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = col_names_latex,
  align = c("l", "r", "r", "r", "l", "r", "r", "r"),
  linesep = "",
  row.names = FALSE
)

# ── Inject section headers and fit indices ────────────────────────────────────
tbl_lines <- strsplit(as.character(tabular_raw), "\n")[[1]]
midrule_idx <- which(tbl_lines == "\\midrule")
br_idx <- which(tbl_lines == "\\bottomrule")
insert_sd_before <- midrule_idx + n_mean + 1

n_tasks <- 6L
N_resp <- N / n_tasks
half <- n_cols %/% 2 # 4

fit_rows <- c(
  "\\midrule",
  paste0(
    "\\multicolumn{",
    half,
    "}{l}{Observations}",
    " & \\multicolumn{",
    n_cols - half,
    "}{r}{",
    formatC(N_resp, format = "d", big.mark = ","),
    " $\\times$ ",
    n_tasks,
    " $=$ ",
    formatC(N, format = "d", big.mark = ","),
    "}\\\\"
  ),
  paste0(
    "\\multicolumn{",
    half,
    "}{l}{Parameters}",
    " & \\multicolumn{",
    n_cols - half,
    "}{r}{",
    k,
    "}\\\\"
  ),
  paste0(
    "\\multicolumn{",
    half,
    "}{l}{Log-Likelihood}",
    " & \\multicolumn{",
    n_cols - half,
    "}{r}{",
    formatC(round(LL, 2), format = "f", digits = 2, big.mark = ","),
    "}\\\\"
  ),
  paste0(
    "\\multicolumn{",
    half,
    "}{l}{Null Log-Likelihood}",
    " & \\multicolumn{",
    n_cols - half,
    "}{r}{",
    formatC(round(LL0, 2), format = "f", digits = 2, big.mark = ","),
    "}\\\\"
  ),
  paste0(
    "\\multicolumn{",
    half,
    "}{l}{AIC}",
    " & \\multicolumn{",
    n_cols - half,
    "}{r}{",
    formatC(round(AIC_v, 2), format = "f", digits = 2, big.mark = ","),
    "}\\\\"
  ),
  paste0(
    "\\multicolumn{",
    half,
    "}{l}{BIC}",
    " & \\multicolumn{",
    n_cols - half,
    "}{r}{",
    formatC(round(BIC_v, 2), format = "f", digits = 2, big.mark = ","),
    "}\\\\"
  ),
  paste0(
    "\\multicolumn{",
    half,
    "}{l}{McFadden $R^2$}",
    " & \\multicolumn{",
    n_cols - half,
    "}{r}{",
    sprintf("%.4f", rho2),
    "}\\\\"
  ),
  paste0(
    "\\multicolumn{",
    half,
    "}{l}{Adj.\\ McFadden $R^2$}",
    " & \\multicolumn{",
    n_cols - half,
    "}{r}{",
    sprintf("%.4f", arho2),
    "}\\\\ "
  )
)

# Mean parameters header: left label + "Summary of 10k Draws" span on right
# Then cmidrule under the span, then quantile col sub-headers
mean_hdr <- c(
  paste0(
    "\\multicolumn{",
    n_left,
    "}{l}{\\textbf{\\textit{Mean parameters}}}",
    " & \\multicolumn{",
    n_right,
    "}{c}{\\textit{Summary of 10k Draws}}",
    "\\\\"
  ),
  paste0("\\cmidrule(l){", n_left + 1, "-", n_cols, "}"),
  paste0(" & & & & & 1st Qu. & Median & 3rd Qu.\\\\[1.5ex]")
)

new_lines <- c(
  tbl_lines[seq_len(midrule_idx)],
  mean_hdr,
  tbl_lines[(midrule_idx + 1):(insert_sd_before - 1)],
  paste0(
    "\\addlinespace[1.5ex]\\multicolumn{",
    n_cols,
    "}{l}{\\textbf{\\textit{Standard deviation}}}\\\\[1.5ex]"
  ),
  tbl_lines[insert_sd_before:(br_idx - 1)],
  fit_rows,
  tbl_lines[br_idx:length(tbl_lines)]
)

tbl_str <- paste(
  c(
    "\\begin{table}[pos=H]",
    "\\fontfamily{ptm}\\selectfont",
    "\\caption{Mixed Logit Model Estimates in WTP Space (price unit: \\$10,000).}",
    "\\label{table:mxl_results}",
    "\\begin{adjustbox}{width=\\textwidth, center}",
    new_lines,
    "\\end{adjustbox}",
    "\\begin{minipage}{\\textwidth}\\footnotesize",
    "Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1.",
    "\\end{minipage}",
    "\\end{table}"
  ),
  collapse = "\n"
)

writeLines(
  tbl_str,
  here::here(
    "paper_writing",
    "battery_paper",
    "attachments",
    "mxl_results.tex"
  )
)
