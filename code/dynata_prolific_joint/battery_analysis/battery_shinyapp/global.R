# global.R — runs once; all objects are shared by ui.R and server.R
library(shiny)
library(tidyverse)
library(arrow)
library(here)

setwd(
  "/Users/xnw17/Documents/GitHub/Conjoint_Survey_25/code/dynata_prolific_joint/battery_analysis/battery_shinyapp"
)

# ── Load data & models ────────────────────────────────────────────────────────
data_dce <- read_parquet(
  "data/data_logitr_dce_only_battery.parquet"
)
load(
  "data/mxl_wtp_model_car.RData"
) # → wtp_model_car
load(
  "data/mxl_wtp_model_suv.RData"
) # → wtp_model_suv

# ── Helper: safe coefficient extraction ──────────────────────────────────────
pull_coef <- function(coefs, name) {
  val <- coefs[name]
  if (is.null(val) || length(val) == 0 || is.na(val)) 0 else as.numeric(val)
}

get_model_pars <- function(model, segment_label) {
  coefs <- coef(model)
  tibble(
    segment = segment_label,
    mean_range = pull_coef(coefs, "battery_range_year0"),
    mean_degrad = pull_coef(coefs, "battery_degradation"),
    mean_pack = pull_coef(coefs, "battery_refurbishpackreplace"),
    mean_cell = pull_coef(coefs, "battery_refurbishcellreplace"),
    sd_range = abs(pull_coef(coefs, "sd_battery_range_year0")),
    sd_degrad = abs(pull_coef(coefs, "sd_battery_degradation")),
    sd_pack = abs(pull_coef(coefs, "sd_battery_refurbishpackreplace")),
    sd_cell = abs(pull_coef(coefs, "sd_battery_refurbishcellreplace"))
  )
}

pars_all <- bind_rows(
  get_model_pars(wtp_model_car, "Car"),
  get_model_pars(wtp_model_suv, "SUV")
)

# ── Build grid ────────────────────────────────────────────────────────────────
degrad_seq <- seq(
  range(data_dce$battery_degradation, na.rm = TRUE)[1],
  range(data_dce$battery_degradation, na.rm = TRUE)[2],
  length.out = 120
)

range_seq <- seq(
  range(data_dce$battery_range_year0, na.rm = TRUE)[1],
  range(data_dce$battery_range_year0, na.rm = TRUE)[2],
  length.out = 120
)

refurb_levels <- tribble(
  ~refurb , ~cell , ~pack ,
  "none"  ,     0 ,     0 ,
  "cell"  ,     1 ,     0 ,
  "pack"  ,     0 ,     1
)

calc_df <- expand_grid(pars_all, refurb_levels) |>
  crossing(expand_grid(
    battery_degradation = degrad_seq,
    battery_range_year0 = range_seq
  )) |>
  mutate(
    mean_wtp = mean_range *
      battery_range_year0 +
      mean_degrad * battery_degradation +
      mean_pack * pack +
      mean_cell * cell,
    mean_wtp_scaled = mean_wtp * 10, # unit: $1,000
    refurb = case_when(
      refurb == "none" ~ "No refurbishment",
      refurb == "cell" ~ "Cell-level refurbishment",
      refurb == "pack" ~ "Pack-level refurbishment"
    ) |>
      factor(
        levels = c(
          "No refurbishment",
          "Cell-level refurbishment",
          "Pack-level refurbishment"
        )
      ),
    segment = factor(segment, levels = c("Car", "SUV"))
  ) |>
  filter(!is.na(mean_wtp))

# ── Lookup table: one row per (segment × refurb × grid cell) ─────────────────
# used for nearest-neighbour tooltip in server
lookup_df <- calc_df |>
  select(
    segment,
    refurb,
    battery_degradation,
    battery_range_year0,
    mean_wtp_scaled
  )

# ── Axis limits ───────────────────────────────────────────────────────────────
xlim_plot <- range(degrad_seq)
ylim_plot <- range(range_seq) * 100 # y displayed as miles (*100)
