# Project Overview
This is an R-based academic research project studying U.S. consumer preferences for battery electric vehicles (BEVs) using discrete choice experiments (DCE). 


# Data Pipeline (run in order)
1. **`code/dynata_prolific_joint/1_dynata_prolific_joint.R`** — Merges raw parquet data from all data sources, offsets `respID`/`obsID` to ensure uniqueness across sources, writes `data/dynata_prolific_joint/data_joint_battery.parquet` and `data_joint_vehicle.parquet`.

2. **`code/dynata_prolific_joint/3a_variable_cleaning.R`** — Cleans socio-demographic variables, writes `data_clean_variables.parquet`.

3. **Apollo data prep** (domain-specific, run before modeling):
   - Battery: `code/dynata_prolific_joint/battery_analysis/apollo_analysis/1_apollo_data.R` → writes `data_apollo_battery.parquet`
   - Vehicle: `code/dynata_prolific_joint/vehicle_analysis/apollo_analysis/1_apollo_data.R` → writes `data_apollo_vehicle.parquet`


# Modeling
Survey data are analyzed using discrete choice models (logit R package), latent class models (Apollo R package).

## Battery analysis:
- `code/dynata_prolific_joint/battery_analysis/logitr_analysis/`
- `code/dynata_prolific_joint/battery_analysis/apollo_analysis/`

## Vehicle analysis: 
- `code/dynata_prolific_joint/vehicle_analysis/logitr_analysis/`
- `code/dynata_prolific_joint/vehicle_analysis/apollo_analysis/`


# Post-Estimation Summary Scripts
Run after modeling, in order:
- `3a_summarize_model_coef.R` — reads `*_estimates.csv` for car and SUV models, reshapes wide by class, creates a `gt` table saved as HTML to `code/output/model_output/apollo/{domain}/0_model_summary_cars_suvs_lc_3c.html`
- `3b_summarize_class_profile.R` — reads `*_model.rds` + `*_apollo_inputs.rds` + `*_apollo_probabilities.rds`, computes posterior class membership, characterizes classes by demographics/attitudes


# Key Conventions
- keep all datasets in `data/`, keep all codes in `code/`, keep all outputs in `output/`. 
- **Data format:** Primary storage is Apache Parquet (read/write via `arrow::read_parquet()` / `write_parquet()`). Model objects use RDS.


# Paper Writing
LaTeX source at `paper_writing/battery_paper/battery_paper.tex` and `paper_writing/vehicle_paper/`.


# Important Notes
- PULL from github at the start of each work session
- NEVER commit .env files
