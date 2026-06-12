source(here::here('code', 'setup.R'))

# ── Load full data (same as 4_optout_reason.R) ───────────────────────────────
data_full <- read_parquet(here("data", "dynata_prolific_joint", "data_joint.parquet"))

data_model <- read_parquet(here("data", "dynata_prolific_joint", "data_apollo_battery.parquet"))

data_model <- data_model %>%
  filter(
    !is.na(ATT_range_anxiety) &
      !is.na(ATT_risktaker) &
      !is.na(hhincome_num_10k) &
      !is.na(EV_charger) &
      !is.na(Veh_hh_fuel) &
      !is.na(Veh_primary_range) &
      !is.na(ATT_EVB_environment) &
      !is.na(ATT_EVB_function) &
      !is.na(vehicle_typesuv)
  )

data_nobev <- data_full %>%
  filter(psid %in% data_model$psid) %>%
  filter(!is.na(no_bev_selected0)) %>%
  select(psid, prime_group_label, no_bev_selected0) %>%
  mutate(
    treatment = case_when(
      prime_group_label == "prime_long"  ~ "Extended Info",
      prime_group_label == "prime_short" ~ "Basic Info",
      TRUE ~ NA_character_
    )
  )

cat("data_nobev rows:", nrow(data_nobev), "\n")

# ── Define themes vector ──────────────────────────────────────────────────────
themes <- c(
  "conventional_gas_vehicle_enthusiasm",
  "simply_not_interested",
  "price_cost",
  "range_anxiety",
  "charging_infrastructure",
  "battery_concern",
  "ev_skepticism",
  "ev_distrust",
  "used_vehicle_distrust",
  "environmental_doubt",
  "other"
)

# ── Load existing coded parquet ───────────────────────────────────────────────
existing_coded <- read_parquet(here(
  "code", "output", "model_output", "battery_analysis", "apollo",
  "0_nobev_themes_coded.parquet"
))
cat("Existing coded rows:", nrow(existing_coded), "\n")

# ── Build new coded data frame from LLM matrix ────────────────────────────────
new_coded_raw <- tribble(
  ~psid,                       ~conventional_gas_vehicle_enthusiasm, ~simply_not_interested, ~price_cost, ~range_anxiety, ~charging_infrastructure, ~battery_concern, ~ev_skepticism, ~ev_distrust, ~used_vehicle_distrust, ~environmental_doubt, ~other,
  "69ca4aa83fb819364415f6dd",  0,1,0,0,0,0,0,0,0,0,0,
  "69907e70c4054eeb8965b30d",  0,1,0,0,0,0,0,0,0,0,0,
  "69e0ae99beb07348718ee06d",  0,1,0,0,0,1,0,0,0,0,0,
  "673b79ba5d7681e718c277fc",  1,0,0,1,1,0,0,0,0,0,0,
  "69de95638988aeebd28245c4",  1,0,1,0,1,0,1,0,0,0,0,
  "69cc98ab241394f39f691454",  1,1,0,0,0,0,0,0,0,0,0,
  "6637f2807d667d4013989fc1",  0,0,1,1,1,0,1,0,0,0,0,
  "65a2a922bfa22dde98c0d604",  0,0,1,0,0,0,0,0,0,0,0,
  "69c3239a158bfee31783d53f",  0,0,0,0,0,1,0,1,0,0,0,
  "69f8eef2a6fbd2a08ed4ccf4",  0,1,1,0,0,0,0,0,0,0,0,
  "69cd5dcb8a87891b72f6ebf1",  0,0,0,1,1,1,0,1,0,0,0,
  "6a05d2a43ea4837743f28d8a",  0,1,0,0,0,0,0,0,0,0,0,
  "69f972198ab29387d981c907",  0,0,1,0,0,1,0,1,0,1,0,
  "655ffddf6682f3df8008b8f9",  0,1,0,0,0,0,0,0,0,0,0,
  "698e0d75e89fe7155bb2fd14",  1,1,0,0,1,1,1,0,0,0,0,
  "69e9f6a993985bfec2114d70",  0,1,0,0,0,0,0,0,0,0,0,
  "66d77183848780bed7462056",  0,0,0,1,1,1,1,0,0,0,0,
  "63af4399152d060c863a6484",  0,0,0,1,1,0,0,0,0,0,0,
  "66de61ebd22e266a86232075",  0,0,1,0,0,1,0,0,1,0,0,
  "69cc740df30eeb7d258e0dc9",  1,0,0,0,0,0,0,0,0,0,0,
  "663f6c71063220be73b4ba2f",  0,0,0,0,1,0,0,0,0,0,0,
  "6741437f3c0f685464c09139",  1,1,0,1,1,0,0,0,0,0,0,
  "69cfd3a8dc59402e42230ced",  0,0,1,0,0,0,0,0,0,0,0,
  "663a5c69296e5ab16fa30d00",  0,0,0,0,0,1,1,0,0,0,0,
  "69bf00b13882a8cf2f7400b3",  0,1,0,0,0,0,0,0,0,0,0,
  "65e0d81acca2780f6d77bbe2",  0,0,1,0,0,1,0,0,1,0,0,
  "615e4e76151c3bb90b6bd868",  0,0,1,0,0,0,0,0,0,0,0,
  "6a029599262229428b4fb20a",  0,1,0,0,0,0,0,0,0,0,0,
  "6730112557e35267b2274947",  0,1,0,0,1,0,0,0,0,0,0,
  "69960866041cb0eb7a4f6487",  0,0,0,1,1,1,0,0,0,0,0,
  "67abac2e77976bbccd35798b",  0,1,0,1,0,0,0,0,0,0,0,
  "64820b6c2c4ffe2a7f30b1dc",  0,0,0,1,0,0,0,1,0,0,0,
  "69adc9fb8215015327c2b342",  0,0,0,1,1,0,1,0,0,0,0,
  "69aa6fa9ac8d4eca02d5dd7b",  0,0,0,1,1,0,0,0,0,0,0,
  "5c337cb05b8ed100018a9b2e",  0,0,0,0,0,1,0,1,0,0,0,
  "69964193eb8ec313b470ec6c",  1,1,0,0,0,0,0,0,0,0,0,
  "695bdc063eacb21445424075",  0,0,0,0,1,0,0,0,0,0,0,
  "69da46223396714fc0d4427d",  0,0,0,0,0,0,1,0,0,0,0,
  "698f869ca948853644583ee2",  0,0,1,0,1,0,0,0,0,0,0,
  "6104c9954ebe1d41cf1c3d9f",  1,0,0,1,0,1,0,0,0,0,0,
  "69d480a9374b64399b0b276c",  0,0,0,1,1,0,0,0,0,0,0,
  "69a719ff0a09773981d56612",  0,1,0,0,0,0,0,0,0,0,0,
  "69992d4bd471ae1ac8d5d13b",  0,0,1,1,0,1,0,0,1,0,0,
  "69f51166a4b79fa522b9bc9a",  1,0,0,0,0,0,0,0,0,0,0,
  "695fe183c240e1fd35d3f1ef",  0,0,0,0,0,1,0,0,1,0,0,
  "6a0910707b57c547ae145a77",  0,1,0,1,1,1,0,0,0,0,0,
  "699c91e6dc40ecf071af9d13",  1,1,0,0,0,0,0,0,0,0,0,
  "698dfc12afe15931772f452e",  0,0,0,0,1,0,1,0,0,0,0,
  "6742afafe7ffabc756109eec",  0,0,0,0,0,0,1,0,0,0,1,
  "69a9efa0aa4d2f1a98435f39",  0,0,1,0,0,0,0,0,0,0,0,
  "6714457399a98b94cabd3bec",  1,0,0,0,1,1,0,0,0,1,0,
  "5fac561bfa81812030b9fee0",  0,1,0,0,0,0,0,0,0,0,0,
  "6a04b2b78043f4510945c845",  0,1,0,0,0,1,0,0,1,0,0,
  "69d6f79ee03a2de194479371",  0,1,0,0,0,0,0,0,0,0,0,
  "69fcc490e45bb5726737eb6e",  0,0,1,1,0,0,0,0,0,0,0,
  "69be18a7edf32a3fca008f2c",  0,1,0,0,0,0,0,1,0,0,0,
  "67c95fc1fefb0741ac3832b4",  0,1,0,0,0,0,0,0,0,0,0,
  "66311eece7cd682caa24b768",  0,0,0,1,1,0,0,0,0,0,0,
  "69ab75e828da1fa3fd63d816",  0,0,0,1,1,0,0,0,0,0,0,
  "69b1e73b9dbf1dec3b4174de",  0,0,0,0,0,1,0,1,0,0,0,
  "66f247b22b5fe367ca421d35",  0,0,0,0,1,0,0,0,0,0,0,
  "66432cfe824559e786003361",  0,0,0,0,0,1,1,0,0,1,0,
  "6989489cc139c6790863c43f",  0,0,0,1,0,0,0,0,0,0,0,
  "66b29d1ec3671b7ea728c771",  0,0,1,0,0,0,0,0,0,0,0,
  "697a5d5b10537642caf820dc",  0,0,0,1,0,0,0,0,0,0,0,
  "698314cfbc5b5149e7eff3cf",  0,1,0,1,0,0,0,0,0,0,0,
  "673e5f6b0c248822d47e064d",  0,0,0,1,1,0,0,0,0,0,0,
  "671c468e4744030347f73f70",  0,0,0,1,1,0,0,0,0,0,0,
  "69b704f9f5724767d87fa125",  0,1,0,0,0,0,0,0,0,0,0,
  "63f2cff7f0a6f682868f2ca0",  0,0,0,0,0,0,1,1,0,0,0,
  "69bdd4476bc8f30a2520c96d",  0,0,1,0,0,0,1,0,0,0,0,
  "675c6ac13aff0173b803dbec",  0,0,0,1,1,0,0,0,0,0,0,
  "69a8fbab64f103b79ec6a02b",  0,0,0,0,0,0,1,0,1,0,0,
  "66d616d0aad3bc7ce893c04d",  1,1,0,0,0,0,0,0,0,0,0,
  "6723d2acbc2d23b9506b2b46",  0,0,1,1,0,1,0,0,0,0,0,
  "69f84a1d3e029f3c32ce3818",  0,0,0,0,0,1,1,0,0,0,0,
  "697f93af0b8274ea279bb374",  1,1,1,0,0,0,0,0,0,0,0,
  "69eea2cb76066b446d9833eb",  0,0,0,1,1,0,0,0,0,0,0,
  "69c554c4669fbdcc2241932a",  0,0,0,0,0,0,0,1,0,0,1,
  "695d2db768ef0dc1e54f11c8",  0,1,0,0,0,0,0,0,0,0,0,
  "6a0102a61a9cddc2b509e682",  1,0,0,0,0,1,0,0,1,0,0,
  "6972f6b68ba5cb2b5cf718a9",  0,0,0,0,0,1,0,0,1,0,0,
  "69f64280ed4e3ec5e382b97b",  0,0,0,1,1,0,0,0,0,0,0,
  "697e62d589e17fcbb4ed408d",  0,1,0,0,0,0,0,0,0,0,0,
  "698fb2f1165d0fe11cc5f6b4",  0,0,0,1,1,0,0,0,0,0,0,
  "6111b04382b7b8effc8b2b27",  0,1,0,0,0,0,0,0,0,0,0,
  "69939b823c7d1ff0d5eecbb3",  0,1,0,0,0,0,0,0,0,0,0,
  "6962dbac88944946c07d1497",  1,1,0,0,0,0,0,0,0,0,0,
  "69929ba3656edf355205efdb",  1,1,0,0,0,0,0,0,0,0,0,
  "6985f6870d2de85554edac63",  1,1,0,0,0,0,0,0,0,0,0,
  "69a1ea2fb87a69e06221ba16",  0,0,0,0,0,1,0,0,1,0,0,
  "66b9b602bba4ebe205ce3e49",  0,1,0,0,0,0,0,0,0,0,0,
  "69bc340be433f0c0359ca134",  0,1,0,0,0,0,0,0,0,0,0,
  "epb85GQCt0bZBfHBsLlfs9oC",  0,1,0,0,0,0,0,0,0,0,0,
  "pZrS3hgW1oUFdJpa2RGSmzA8",  0,0,0,0,1,0,0,0,0,0,0,
  "R6qA1aKb1XVMF5mgIgAbXsgm",  0,1,0,0,0,0,0,0,0,0,0,
  "2j8TOmkHez3aaaWh01tyVu77",  0,0,0,0,1,0,0,0,0,0,0,
  "LlUTuiCJAQzEXkVpt5K8mJmr",  0,1,1,0,1,0,0,0,0,1,0,
  "v3HN2FoQjwgyFiius4oyKEvP",   0,0,1,0,0,0,0,1,0,0,0,
  "eaeh6LgBqafDQUL5QIHNrn40",  0,1,0,0,0,0,0,0,0,0,0,
  "MXv56fhZC1S9JkE2TncJ8Arl",  0,1,1,0,1,0,1,0,0,0,0,
  "c4cQL5j8wAIMvEejd37HExRb",  0,0,0,0,0,0,0,0,0,0,1,
  "5hOcYvtd6ta8VsB8JY7eG5P9",  1,0,0,0,0,0,0,0,0,0,1,
  "RdZm5bMrBIbDgCfRYaEOwMHS",  0,1,0,0,0,0,0,0,0,0,0,
  "nSTZIpoZF5FSl0HzWbC3sRES",  0,1,0,0,0,0,0,0,0,0,0
)

cat("New coded raw rows:", nrow(new_coded_raw), "\n")

# ── Join with data_nobev to get prime_group_label, no_bev_selected0, treatment ─
new_coded <- new_coded_raw %>%
  left_join(
    data_nobev %>% select(psid, prime_group_label, no_bev_selected0, treatment),
    by = "psid"
  ) %>%
  mutate(n_themes = rowSums(select(., all_of(themes)), na.rm = TRUE))

# Check how many matched
cat("Rows with treatment info:", sum(!is.na(new_coded$treatment)), "\n")
cat("Rows WITHOUT match in data_nobev:", sum(is.na(new_coded$treatment)), "\n")
if (sum(is.na(new_coded$treatment)) > 0) {
  cat("Unmatched psids:\n")
  print(new_coded$psid[is.na(new_coded$treatment)])
}

# ── Check for duplicates with existing ───────────────────────────────────────
already_in <- new_coded$psid %in% existing_coded$psid
cat("Already in existing coded:", sum(already_in), "\n")
new_coded_clean <- new_coded %>% filter(!psid %in% existing_coded$psid)
cat("Net new rows to append:", nrow(new_coded_clean), "\n")

# ── Reorder columns to match existing parquet ─────────────────────────────────
col_order <- names(existing_coded)
# n_themes not in existing; add it at end if not present
if (!"n_themes" %in% col_order) {
  col_order <- c(col_order, "n_themes")
}

new_coded_clean <- new_coded_clean %>%
  select(any_of(col_order))

# ── Append and save ───────────────────────────────────────────────────────────
combined <- bind_rows(existing_coded, new_coded_clean)
cat("Combined rows:", nrow(combined), "\n")
cat("Treatment distribution after append:\n")
print(table(combined$treatment, useNA = "always"))

write_parquet(
  combined,
  here(
    "code", "output", "model_output", "battery_analysis", "apollo",
    "0_nobev_themes_coded.parquet"
  )
)
cat("Saved updated parquet with", nrow(combined), "rows.\n")
