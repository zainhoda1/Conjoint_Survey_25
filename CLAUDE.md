# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based academic research project studying U.S. consumer preferences for battery electric vehicles (BEVs) using discrete choice experiments (DCE). Survey data from three sources (Dynata, Prolific Round 1, Prolific Round 2) are combined and analyzed using latent class multinomial logit (LC-MNL) models.

## Running Code

All scripts use `here::here()` for paths and must be run from the project root (`.Rproj` location). Every script begins with:

```r
source(here::here('code', 'setup.R'))
```

`setup.R` loads all packages and defines the `%notin%` helper. Run scripts interactively in RStudio or via:

```r
Rscript --vanilla code/path/to/script.R
```

Apollo model estimation is long-running (can take hours). The `nCores` parameter in `apollo_control` controls parallelism — currently set to 2.

## Architecture

### Data Pipeline (run in order)

1. **`code/dynata_prolific_joint/1_dynata_prolific_joint.R`** — Merges raw parquet data from all three sources, offsets `respID`/`obsID` to ensure uniqueness across sources, writes `data/dynata_prolific_joint/data_joint_battery.parquet` and `data_joint_vehicle.parquet`.

2. **`code/dynata_prolific_joint/3a_variable_cleaning.R`** — Cleans socio-demographic variables, writes `data_clean_variables.parquet`.

3. **Apollo data prep** (domain-specific, run before modeling):
   - Battery: `code/dynata_prolific_joint/battery_analysis/apollo_analysis/1_apollo_data.R` → writes `data_apollo_battery.parquet`
   - Vehicle: `code/dynata_prolific_joint/vehicle_analysis/apollo_analysis/1_apollo_data.R` → writes `data_apollo_vehicle.parquet`

   These scripts: dummy-encode DCE attributes using `cbcTools::cbc_encode()`, pivot wide (one row per respondent-question), run factor analysis on attitude items (`psych::fa()`), and join covariates.

### Modeling

**Battery analysis:** `code/dynata_prolific_joint/battery_analysis/apollo_analysis/`
- `2b_apollo_battery.R` — 1- and 2-class LC models
- `2b_apollo_battery_3c.R` — 3-class LC model (current main model), segmented by `vehicle_typesuv == 0` (car) or `== 1` (SUV) — toggle by commenting/uncommenting `apollo_control` and `database` blocks

**Vehicle analysis:** `code/dynata_prolific_joint/vehicle_analysis/apollo_analysis/`
- `2a_apollo_vehicle_1c.R`, `2b_apollo_vehicle_2c.R`, `2c_apollo_vehicle_3c.R`, `2d_apollo_vehicle_3c_no_BEV_attribute.R`

Each Apollo model script follows this structure:
1. `apollo_initialise()` + define `apollo_control` (includes `outputDirectory` and `modelName` which determines output file names)
2. Define `apollo_beta` (starting values) and `apollo_fixed` (held-constant parameters)
3. Optionally warm-start with `apollo_readBeta()` using a previously estimated model
4. Define `apollo_lcPars()` for class membership utilities (logit of covariates) and class-specific parameters
5. Define `apollo_probabilities()` with MNL utilities per class
6. `apollo_estimate()` → saves CSV estimates, RDS model, and TXT output automatically to `outputDirectory`
7. Manually `saveRDS()` the `apollo_inputs` and `apollo_probabilities` function for post-estimation use

**Model output location:** `code/output/model_output/apollo/{battery,vehicle}/`
- `{modelName}_estimates.csv` — parameter estimates (read by summary scripts)
- `{modelName}_model.rds` — fitted model object (read by class profile scripts)
- `{modelName}_apollo_inputs.rds` / `{modelName}_apollo_probabilities.rds` — needed for posterior class assignment

### Post-Estimation Summary Scripts

Run after modeling, in order:
- `3a_summarize_model_coef.R` — reads `*_estimates.csv` for car and SUV models, reshapes wide by class, creates a `gt` table saved as HTML to `code/output/model_output/apollo/{domain}/0_model_summary_cars_suvs_lc_3c.html`
- `3b_summarize_class_profile.R` — reads `*_model.rds` + `*_apollo_inputs.rds` + `*_apollo_probabilities.rds`, computes posterior class membership, characterizes classes by demographics/attitudes

### Logitr Analysis (alternative framework)

`code/dynata_prolific_joint/battery_analysis/logitr_analysis/` — MNL and MXL models using the `logitr` package. Numbered `1_` through `6_`. These use long-format data (`data_logitr_dce_only_battery.parquet`, `data_logitr_dce_covariate_battery.parquet`).

## Key Conventions

**Variable scaling:** DCE attributes are divided before modeling to improve numerical stability:
- `price` / 10,000 (units: 10k USD)
- `mileage` / 10,000
- `battery_range_*` / 100 (units: 100 miles)
- `hhincome_num` / 10,000 for class-allocation covariates

**Class labeling:** In 3-class models, suffix `_a` = class 3 (fixed/reference), `_b` = class 1, `_c` = class 2. The `3a_summarize_model_coef.R` remaps these explicitly.

**Segment switching:** Car vs. SUV models are controlled by filtering `database` on `vehicle_typesuv` (0 = car, 1 = SUV) and switching the `modelName` in `apollo_control`. The inactive segment is commented out in each script.

**Model warm-starting:** `apollo_readBeta()` reads a previously saved `*_estimates.csv` to use as starting values. The `modelName` argument must match the file in `outputDirectory`. Set `overwriteFixed = FALSE` to keep fixed parameters at their starting values.

**Data format:** Primary storage is Apache Parquet (read/write via `arrow::read_parquet()` / `write_parquet()`). Model objects use RDS.

## Paper Writing

LaTeX source at `paper_writing/battery_paper/battery_paper.tex` and `paper_writing/vehicle_paper/`. Compile with `latexmk` or via a LaTeX IDE.
