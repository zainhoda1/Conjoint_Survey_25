# Vehicle operating costs (cents per mile) for `combined_table` vehicles

## Goal
For each make / model / powertrain in `combined_table` (built in `waterfall_style_chart_part2.R` from `vehicle_listing_prices.parquet` and `vehicle_pairs_2016_2024.csv`), get an operating cost in cents per mile for a vehicle that is **3 years old**.

## Source
**U.S. EPA / U.S. DOE, fueleconomy.gov** (Fuel Economy Guide data and public web service).
- Web service: <https://www.fueleconomy.gov/ws/rest/> (endpoints `vehicle/menu/options`, `vehicle/{id}`, `fuelprices`)
- Documentation: <https://www.fueleconomy.gov/feg/ws/>

Why this source: it is the official EPA-certified rating for every model year and trim (combined MPG / MPGe, kWh per 100 miles), and the same dataset behind the window sticker. It is public and reproducible through an API.

## What "operating cost" means here
**Energy/fuel cost only** (gasoline or electricity). It excludes insurance, maintenance, repairs, tires, depreciation and registration. If the survey attribute was meant to be a full per-mile cost, a different source (AAA *Your Driving Costs* or Edmunds True Cost to Own) would be needed. These are only available by size class or on a 5-year, new-vehicle basis.

## Method
1. Read `combined_table`, which gives 18 vehicles in 9 matched pairs: Pair 3 and 7 are filtered out later in the script, but I kept them here.
2. **Age 3 → model year 2022.** This follows the paper's convention (`model year = 2025 − age`).
3. For each vehicle, query fueleconomy.gov for all 2022 trims of that model (see the table for exact EPA model names). I excluded performance trims (TRD, N, M, JCW, Nismo) and non-matching body styles (Corolla Hatchback, Corolla Cross, Cooper Countryman, etc.).
4. Compute cents per mile for each trim, then average across trims (simple mean). The min and max across trims are in the CSV.
   - Gasoline: `100 × price per gallon ÷ combined MPG`
   - Electricity: `kWh per 100 mi ÷ 100 × price per kWh × 100`
5. Prices are the current EPA assumptions returned by the `fuelprices` endpoint on 2026-10-14: regular $4.47/gal, premium $5.48/gal, electricity $0.15/kWh. Using the same prices for every vehicle keeps the comparison consistent. The EPA `fuelCost08` field (annual cost at 15,000 mi) gives nearly identical per-mile values for the gasoline cars (e.g. Soul: $2,275 ÷ 15,000 = 15.2 ¢/mi).

Code: `fetch_operating_costs.py` (same folder). Output: `data/vehicle_operating_costs.csv`.

## Results (model year 2022, 3 years old)

| Pair | Make | Model | Powertrain | EPA model yr | Trims averaged | Fuel | Combined MPG / MPGe | kWh/100 mi | **Cents per mile** | Range across trims |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | KIA | SOUL | CV | 2022 | 2 | Regular | 29.5 | – | **15.16** | 14.90–15.41 |
| 1 | KIA | SOUL | BEV | **2020** | 1 | Electricity | 114.0 | 29.2 | **4.38** | 4.38 |
| 2 | HYUNDAI | KONA | CV | 2022 | 4 | Regular | 30.8 | – | **14.56** | 13.97–15.41 |
| 2 | HYUNDAI | KONA EV | BEV | 2022 | 1 | Electricity | 120.0 | 28.0 | **4.20** | 4.20 |
| 3 | MINI | COOPER | CV | 2022 | 4 | Premium | 30.2 | – | **18.19** | 17.12–20.30 |
| 3 | MINI | HARDTOP 2 DOOR | BEV | 2022 | 1 | Electricity | 110.0 | 30.7 | **4.61** | 4.61 |
| 4 | TOYOTA | CAMRY | CV | 2022 | 5 | Regular | 29.2 | – | **15.39** | 13.97–17.19 |
| 4 | TOYOTA | CAMRY | HEV | 2022 | 2 | Regular | 49.0 | – | **9.16** | 8.60–9.72 |
| 5 | TOYOTA | COROLLA | CV | 2022 | 6 | Regular | 33.0 | – | **13.55** | 13.15–13.97 |
| 5 | TOYOTA | COROLLA | HEV | 2022 | 1 | Regular | 52.0 | – | **8.60** | 8.60 |
| 6 | TOYOTA | RAV4 | CV | 2022 | 5 | Regular | 29.4 | – | **15.22** | 14.90–15.96 |
| 6 | TOYOTA | RAV4 | HEV | 2022 | 1 | Regular | 40.0 | – | **11.18** | 11.18 |
| 7 | FORD | ESCAPE | CV | 2022 | 3 | Regular | 28.0 | – | **16.02** | 14.90–17.19 |
| 7 | FORD | ESCAPE | HEV | 2022 | 2 | Regular | 40.5 | – | **11.04** | 10.90–11.18 |
| 8 | NISSAN | VERSA NOTE | CV | 2022 | 2 | Regular | 32.5 | – | **13.84** | 12.77–14.90 |
| 8 | NISSAN | LEAF | BEV | 2022 | 3 | Electricity | 107.7 | 31.3 | **4.70** | 4.56–4.84 |
| 9 | BMW | 4 SERIES | CV | 2022 | 3 | Premium | 27.7 | – | **19.81** | 19.57–20.30 |
| 9 | BMW | I4 | BEV | 2022 | 2 | Electricity | 104.0 | 32.4 | **4.87** | 4.64–5.09 |

(The "Trims averaged" counts are EPA configurations, such as FWD/AWD and engine/transmission variants, under the model names listed in `fetch_operating_costs.py`.)

## Caveats and judgment calls
- **Kia Soul BEV:** The Soul EV was not sold in the U.S. after 2020 (the CSV lists 2016–2020), so there is no 2022 model. I used the latest available year, **2020** (the 2022 vehicle would be 2 years older than the 3-year target).
- **Nissan Versa Note:** Nissan dropped the Versa Note hatchback after the 2019 U.S. model year, although the CSV lists 2006–2025. EPA's 2022 entries are listed under "Versa" (the sedan). I used those as a proxy, so treat this row as approximate.
- **MINI:** The CV "Cooper" row averages Cooper and Cooper S Hardtop 2-door (both premium fuel). The BEV is the Cooper SE Hardtop 2-door, the only electric MINI hardtop.
- **BMW 4 Series:** Averages 430i Coupe, 430i xDrive Coupe and 430i Gran Coupe. The M4 and M440i trims are excluded.
- **Trim mix:** The listings in `vehicle_listing_prices.parquet` do not break out trim, so the cents-per-mile figure is an unweighted average of EPA configurations, not a sales-weighted one.
- **Price sensitivity:** Gasoline cost scales with the pump price, so the CV and HEV values will move with the gas price used. Electricity is at the national EPA average ($0.15/kWh), and home electricity prices vary a lot by state.
- **Units vs. the survey:** The survey's `operating_cost` attribute is described in `CLAUDE.md` as 0.3–2.5 "cents per mile". These values (4–20 ¢/mi) are much larger. Before merging, check how that attribute was scaled in the survey and in `willingness_to_pay_attributes.R` (the open `operating_cost` scaling question already flagged in the script comments).
