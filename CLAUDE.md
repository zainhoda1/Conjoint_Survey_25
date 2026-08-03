# Conjoint Survey 25 — Project Overview

## Project Summary

Academic research project studying U.S. consumer preferences for battery electric vehicles (BEVs) via discrete choice experiments (DCE). Data come from three waves: Dynata (wave 1), Prolific Round 1, and Prolific Round 2 (Feb 2026). The project produces two papers from two DCE domains — Battery and Vehicle — run within the same survey instrument.

**Active focus:** Study 2 (vehicle paper) is now the primary writing task. Study 1 (battery paper) is complete.

---

## Study 1: Battery Health Paper (COMPLETE)

**Title:** Consumer Valuation of Battery Health Attributes in Used Battery Electric Vehicle Markets: A Discrete Choice Experiment

**File:** `paper_writing/battery_paper/battery_paper.tex`

**Research question:** How do consumers value battery-health attributes (state of health, refurbishment history, range, mileage) when purchasing used BEVs?

**Sample:** N = 3,072 used-vehicle-intending buyers in the U.S.

**Methods:** Mixed logit (MXL) + 6-class latent class choice model (Apollo)

**Key findings:**
- Consumers value additional range at ~$11,070/100 miles; discount mileage at ~$2,780/10k miles
- Battery degradation penalized at ~$980 per percentage point of annual range loss
- Pack- and cell-level refurbishment penalized by ~$4,060 and ~$4,480
- Six consumer segments (EV-engaged/range-focused, battery-quality-attentive, budget-constrained, BEV skeptics, etc.)
- Battery replacement cost/warranty information does not affect overall market engagement

**Paper status:** Writing complete. Introduction review delivered (29 items pending); compiles clean.

**Analysis code:** `code/dynata_prolific_joint/battery_analysis/`

---

## Study 2: Used Vehicle Adoption Paper (ACTIVE)

**Title:** Measuring User Preferences for Used EVs

**File:** `paper_writing/vehicle_paper/vehicle_paper_edited.tex`

**Co-authorship:** Being co-written with a coauthor.

**Research question:** What are consumers' WTP for powertrain type and vehicle attributes in the used vehicle market, and how does this vary by vehicle segment, budget, and BEV adoption propensity?

---

### Survey Design

The survey (`survey_prolific_round2_feb_26/survey.qmd` + `app.R`) has 6 sections:

1. **Current vehicle info** — household vehicle count/types, charger access, neighbor EV ownership, primary vehicle details (fuel type, purchase method, cost, payment, MPG, refuel frequency, range)
2. **Future vehicle info** — budget, payment method, BEV purchase likelihood (Likert matrix for new/used PHEV and new/used BEV), vehicle image selection (car or SUV style)
3. **Vehicle DCE** — 6 choice tasks (Section 3 of 6)
4. **Battery DCE** — 6 choice tasks with randomized information treatment (Section 4 of 6)
5. **EV knowledge & attitudes** — EV knowledge questions, attitude scales
6. **Demographics**

**Screener logic:**
- Screens out: purchase window > 2 years or "not sure"; new-vehicle-only buyers; van/truck/other body style
- Targets: car or SUV buyers planning a used vehicle purchase within 2 years

---

### Vehicle DCE Design

Each choice task presents 3 vehicle alternatives + a no-choice option ("Even if these were my best options, I would not choose any of these vehicles"). Design is personalized: the respondent first selects a vehicle image (car or SUV), which is shown throughout DCE tasks.

**Attributes and ranges (as displayed to respondents):**

| Attribute | Description | Range in data |
|---|---|---|
| Purchase price | Total cost incl. taxes/fees | $5,000–$60,000 |
| Powertrain | Conventional (ref.), Gas hybrid (HEV), Battery electric (BEV) | 3 levels |
| Range | Miles on full charge (BEV only) | Varies |
| Model year | Displayed as `2025 − age` | Approx. 2017–2023 |
| Mileage | Miles traveled | 20,000–60,000 |
| Operating cost | Cents per mile (+ MPG equivalent) | 0.3–2.5 c/mile |

**Design is stratified by:** vehicle type (car/SUV) × budget (low/high). Budget cutoff: ≤$20,000 = low; ≥$25,000 = high.

**Information treatment (battery section):** Respondents randomly assigned to `prime_short` (brief battery degradation info) or `prime_long` (adds battery maintenance costs + warranty details). This affects only the battery DCE, not the vehicle DCE.

---

### Model Specification (`2f_mixed_logit_vehicles.R`)

**Package:** `logitr`; data: `data/dynata_prolific_joint/data_joint_vehicle.parquet`

**Scaling:** Variables rescaled before estimation:
- `price` ÷ 10,000 (range: 0.5–6)
- `range_bev` ÷ 100 (range: 0.5–2.5)
- `mileage` ÷ 10,000 (range: 2–6)
- `operating_cost` ÷ 10 (range: 0.3–2.5)
- `age` unscaled (range: 2–8)

**Reference levels:** powertrain = gas, vehicle_type = car, budget = low, data_source = prolific

**Model structure:** WTP-space MXL with `scalePar = 'price'`, Sobol draws (5,000), 10 multi-starts, panel correction (`panelID = respID`)

**Random parameters (normal):** powertrainbev, powertrainhev, range_bev, mileage, age, operating_cost, no_choice

**Data source interactions created but not in main spec:** `price_dynata` (price × dynata dummy), `bev_dynata` (BEV × dynata dummy)

---

### Models Estimated

**Primary models — by vehicle segment:**
- `mixed_model_1_car_panel` — car buyers (all budgets)
- `mixed_model_1_suv_panel` — SUV buyers (all budgets)

**Subgroup 1 — budget within segment:**
- `mixed_model_1_car_low_panel` — car, low budget (≤$20k)
- `mixed_model_1_car_high_panel` — car, high budget (≥$25k)
- `mixed_model_1_suv_low_panel` — SUV, low budget
- `mixed_model_1_suv_high_panel` — SUV, high budget

**Subgroup 2 — BEV adoption propensity within segment:**
- `mixed_model_1_likely_bev_adopter_car` — car buyers likely to purchase a used BEV
- `mixed_model_1_unlikely_bev_adopter_car` — car buyers unlikely to purchase a used BEV
- `mixed_model_1_likely_bev_adopter_suv` — SUV buyers likely to purchase a used BEV
- `mixed_model_1_unlikely_bev_adopter_suv` — SUV buyers unlikely to purchase a used BEV

**BEV adoption classification** (from survey `next_veh_fuel` matrix): "likely" = somewhat/very likely for used BEV; "unlikely" = somewhat/very unlikely for both new and used BEV.

All models saved as `.RData` in `models/`. Results loaded and summarized in `2g_mixed_logit_vehicles.R`.

---

### Attitude/Covariate Measures Collected

**EV attitudes (5-point Likert, strongly disagree to strongly agree):**
- Social norm: "People important to me think I should buy an EV"
- Cost savings belief, environmental benefit belief
- Range anxiety, resale value concern, price concern
- Battery environmental benefit, battery function skepticism

**Psychographic traits:** price sensitivity, tech-savviness, risk tolerance

**EV knowledge:** can name a BEV make/model; knows federal tax credit max ($7,500)

**Sociodemographics:** birth year, gender, ethnicity/race, household size, employment, income, education, housing type/tenure, electricity bill, zip code

**Political:** political views (5-point), party affiliation, climate change concern (5-point)

---

## Shared Infrastructure

**Data pipeline:** `code/dynata_prolific_joint/1_dynata_prolific_joint.R` merges all sources → parquet

**Key R packages:** `apollo`, `logitr`, `arrow`, `tidyverse`, `psych`

**LaTeX build:** `latexmk` with `biber`. Use `[pos=H]` (not `[H]`) for figure environments. Biber occasionally crashes with Unicode::UCD PAR cache error — fix: `rm -rf /var/folders/.../par-*/cache-*` then rerun.

---

## Key Conventions

- Always work on the file where the user's cursor is located unless another file is explicitly named
- Always print proposed text edits in chat before editing files
- Save original section text to memory before making any edits
- Never rewrite content when asked to fix layout/spacing
- Never change, reorder, or remove findings the user has written — only improve language
- Use asterisk notation (`*`, `**`, `***`) for p-values; never write explicit $p < 0.05$ expressions
- Vehicle analysis paths: `vehicle_analysis/`; battery analysis paths: `battery_analysis/`
