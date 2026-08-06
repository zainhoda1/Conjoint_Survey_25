source(here::here("code", "setup.R"))

# ---- Load the saved 6-class model ----
model <- readRDS(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "piecewise_rangeloss_car_suv_lc_6c_1_model.rds"
))

# Apollo assigns classes letters a–f; mapped to new paper class order:
# a=old1(BEV-Skeptical)→new5, b=old2(BatHealth)→new1, c=old3(RangeFocus)→new3,
# d=old4(NonAttend)→new6, e=old5(MultiAttr)→new2, f=old6(BudgetConstr)→new4
class_letters <- c("a", "b", "c", "d", "e", "f")
class_labels <- c("Class 5", "Class 1", "Class 3", "Class 6", "Class 2", "Class 4")

# ---- Build delta-method expressions for one class ----
# WTP = b_attr / (-b_price) * 10000 (price is in $10k units)
# A significant WTP difference means the marginal valuation genuinely changes
# across attribute levels.
#
# Tested attributes:
#   Range piecewise: pw1 (<130 mi), pw2 (130–200 mi), pw3 (200+ mi)
#   Range loss rate: pw1 (<12%), pw2 (12–24%), pw3 (24%+)
#   Refurbishment:   pack replace vs cell replace

build_exprs <- function(cl) {
  p <- paste0("b_price_", cl)
  c(
    "Range: pw1 - pw2" = paste0(
      "(b_range_pw1_",
      cl,
      " - b_range_pw2_",
      cl,
      ") / (-",
      p,
      ") * 10000"
    ),
    "Range: pw1 - pw3" = paste0(
      "(b_range_pw1_",
      cl,
      " - b_range_pw3_",
      cl,
      ") / (-",
      p,
      ") * 10000"
    ),
    "Range: pw2 - pw3" = paste0(
      "(b_range_pw2_",
      cl,
      " - b_range_pw3_",
      cl,
      ") / (-",
      p,
      ") * 10000"
    ),
    "Range loss: pw1 - pw2" = paste0(
      "(b_loss_pw1_",
      cl,
      " - b_loss_pw2_",
      cl,
      ") / (-",
      p,
      ") * 10000"
    ),
    "Range loss: pw1 - pw3" = paste0(
      "(b_loss_pw1_",
      cl,
      " - b_loss_pw3_",
      cl,
      ") / (-",
      p,
      ") * 10000"
    ),
    "Range loss: pw2 - pw3" = paste0(
      "(b_loss_pw2_",
      cl,
      " - b_loss_pw3_",
      cl,
      ") / (-",
      p,
      ") * 10000"
    ),
    "Refurb: pack - cell" = paste0(
      "(b_packreplace_",
      cl,
      " - b_cellreplace_",
      cl,
      ") / (-",
      p,
      ") * 10000"
    )
  )
}

# ---- Run tests for all classes ----
all_results <- map2_dfr(class_letters, class_labels, function(cl, class_label) {
  exprs <- build_exprs(cl)

  dm <- tryCatch(
    apollo_deltaMethod(model, deltaMethod_settings = list(expression = exprs)),
    error = function(e) {
      message("Error for ", class_label, " (", cl, "): ", e$message)
      NULL
    }
  )
  if (is.null(dm)) {
    return(NULL)
  }

  # apollo_deltaMethod returns columns: Expression, Value, s.e., t-ratio (0)
  dm %>%
    as_tibble() %>%
    rename(
      comparison = Expression,
      wtp_diff = Value,
      se = `s.e.`,
      t_stat = `t-ratio (0)`
    ) %>%
    mutate(
      class = class_label,
      p_value = 2 * pnorm(-abs(t_stat)),
      sig = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        p_value < 0.1 ~ ".",
        TRUE ~ ""
      ),
      wtp_diff = round(wtp_diff, 0),
      se = round(se, 0),
      t_stat = round(t_stat, 3),
      p_value = round(p_value, 4),
      .before = 1
    ) %>%
    select(class, comparison, wtp_diff, se, t_stat, p_value, sig)
})

# ---- Print results by class ----
cat("\n=== Pairwise WTP difference tests (delta method, robust SE) ===\n")
cat("Null: WTP difference = 0  |  Positive = first level has higher WTP\n\n")

for (cl_label in class_labels) {
  cat(sprintf("--- %s ---\n", cl_label))
  all_results %>%
    filter(class == cl_label) %>%
    select(-class) %>%
    print(n = Inf)
  cat("\n")
}

# ---- Save results ----
write_csv(
  all_results,
  here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "apollo",
    "0_wtp_pairwise_tests_6c.csv"
  )
)
cat("Results saved to 0_wtp_pairwise_tests_6c.csv\n")

# ---- Generate LaTeX table (wtp_pairwise_tests.tex) ----

# Step 1: compute individual WTPs from raw estimates, round each to nearest $10.
# Step 2: compute pairwise differences of the rounded values (no further rounding).
# Significance comes from the delta method on raw (unrounded) values above.

est <- model$estimate

attr_params <- c(
  range_pw1 = "b_range_pw1",
  range_pw2 = "b_range_pw2",
  range_pw3 = "b_range_pw3",
  loss_pw1 = "b_loss_pw1",
  loss_pw2 = "b_loss_pw2",
  loss_pw3 = "b_loss_pw3",
  pack = "b_packreplace",
  cell = "b_cellreplace"
)

# Individual rounded WTPs per attribute per class
wtp_ind <- map_dfr(
  setNames(class_letters, class_labels),
  function(cl) {
    b_price <- est[[paste0("b_price_", cl)]]
    imap_dfr(attr_params, function(param, attr) {
      wtp_r <- round(est[[paste0(param, "_", cl)]] / (-b_price) * 10000 / 10) *
        10
      tibble(attr = attr, wtp = wtp_r)
    })
  },
  .id = "class"
)

# Pairwise differences of rounded WTPs
rounded_diffs <- wtp_ind %>%
  pivot_wider(names_from = attr, values_from = wtp) %>%
  transmute(
    class,
    "Range: pw1 - pw2" = range_pw1 - range_pw2,
    "Range: pw1 - pw3" = range_pw1 - range_pw3,
    "Range: pw2 - pw3" = range_pw2 - range_pw3,
    "Range loss: pw1 - pw2" = loss_pw1 - loss_pw2,
    "Range loss: pw1 - pw3" = loss_pw1 - loss_pw3,
    "Range loss: pw2 - pw3" = loss_pw2 - loss_pw3,
    "Refurb: pack - cell" = pack - cell
  ) %>%
  pivot_longer(-class, names_to = "comparison", values_to = "wtp_diff_display")

# Merge display values with significance from delta method
display_tbl <- all_results %>%
  select(class, comparison, sig) %>%
  left_join(rounded_diffs, by = c("class", "comparison"))

# Format one cell: value is already the difference of rounded WTPs
fmt_cell <- function(val, sig) {
  abs_fmt <- formatC(abs(val), format = "d", big.mark = ",")
  prefix <- if (val >= 0) "\\$" else "-\\$"
  paste0(prefix, abs_fmt, sig)
}

# Wide table: one column per class, one row per comparison
wide <- display_tbl %>%
  mutate(cell = map2_chr(wtp_diff_display, sig, fmt_cell)) %>%
  select(class, comparison, cell) %>%
  pivot_wider(names_from = class, values_from = cell)

# Display order, LaTeX row labels, and section grouping
row_meta <- tribble(
  ~comparison             , ~tex_label                        , ~section ,
  "Range: pw1 - pw2"      , "40--130 mi minus 130--200 mi"    , "range"  ,
  "Range: pw1 - pw3"      , "40--130 mi minus 200+ mi"        , "range"  ,
  "Range: pw2 - pw3"      , "130--200 mi minus 200+ mi"       , "range"  ,
  "Range loss: pw1 - pw2" , "$<$12\\% minus 12--24\\%"        , "loss"   ,
  "Range loss: pw1 - pw3" , "$<$12\\% minus 24\\%+"           , "loss"   ,
  "Range loss: pw2 - pw3" , "12--24\\% minus 24\\%+"          , "loss"   ,
  "Refurb: pack - cell"   , "Pack replace minus Cell replace" , "refurb"
)

tbl <- row_meta %>% left_join(wide, by = "comparison")
cls_cols <- paste0("Class ", 1:6)

make_data_row <- function(label, cells) {
  sprintf("\\hspace{1em}%s & %s\\\\", label, paste(cells, collapse = " & "))
}

section_block <- function(header, df_sec) {
  rows <- apply(df_sec, 1, function(r) {
    make_data_row(r["tex_label"], r[cls_cols])
  })
  c(
    "\\addlinespace[0.3em]",
    sprintf("\\multicolumn{7}{l}{\\textbf{%s}}\\\\", header),
    rows
  )
}

col_spec <- paste0(
  ">{\\raggedright\\arraybackslash}p{6.3cm}",
  paste(rep(">{\\raggedleft\\arraybackslash}p{1.2cm}", 6), collapse = "")
)

cls_headers <- c(
  "Class 1 (n=661, 22.7\\%)",
  "Class 2 (n=606, 20.8\\%)",
  "Class 3 (n=485, 16.6\\%)",
  "Class 4 (n=373, 12.8\\%)",
  "Class 5 (n=252, 8.7\\%)",
  "Class 6 (n=539, 18.5\\%)"
)
col_header <- paste0(" & ", paste(cls_headers, collapse = " & "), "\\\\")

caption <- paste0(
  "\\caption{Pairwise WTP differences across attribute levels, by latent class ",
  "}\\label{tab:wtp_pairwise_tests}\\\\"
)

tex_lines <- c(
  "\\begingroup\\fontfamily{ptm}\\fontsize{8}{10}\\selectfont",
  "",
  sprintf("\\begin{longtable}[t]{%s}", col_spec),
  caption,
  "\\toprule",
  col_header,
  "\\midrule",
  "\\endfirsthead",
  "\\caption[]{Pairwise WTP differences across attribute levels, by latent class \\textit{(continued)}}\\\\",
  "\\toprule",
  col_header,
  "\\midrule",
  "\\endhead",
  "",
  "\\endfoot",
  "\\bottomrule",
  "\\endlastfoot",
  section_block(
    "BEV Electric Range (Year 3, per 100 miles): Pairwise WTP Differences",
    tbl %>% filter(section == "range")
  ),
  section_block(
    "Range Loss Rate (per percentage point): Pairwise WTP Differences",
    tbl %>% filter(section == "loss")
  ),
  section_block(
    "Battery Refurbishment: Pairwise WTP Differences",
    tbl %>% filter(section == "refurb")
  ),
  "\\end{longtable}",
  "\\begin{minipage}{\\textwidth}\\footnotesize",
  "Each cell reports the WTP difference in dollars between two attribute levels; a positive value means the first level carries higher WTP (or lower disutility).\\\\",
  "Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1.",
  "\\end{minipage}",
  "\\endgroup{}"
)

writeLines(
  tex_lines,
  con = here(
    "paper_writing",
    "battery_paper",
    "attachments",
    "wtp_pairwise_tests.tex"
  )
)
cat(
  "LaTeX table written to paper_writing/battery_paper/attachments/wtp_pairwise_tests.tex\n"
)
