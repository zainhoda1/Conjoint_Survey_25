source(here::here('code', 'setup.R'))

# ── 25 sub-themes (column order matters) ──────────────────────────────────────
subthemes <- c(
  "charging_home_access", # 1
  "charging_public_lack", # 2
  "charging_time", # 3
  "range_daily_insufficient", # 4
  "range_long_trip", # 5
  "cost_upfront_purchase", # 6
  "cost_battery_replacement", # 7
  "cost_electricity_operating", # 8
  "cost_maintenance_insurance", # 9
  "cost_general_value", # 10
  "battery_degradation", # 11
  "battery_safety", # 12
  "battery_weather_cold", # 13
  "ev_tech_immature", # 14
  "ev_general_distrust", # 15
  "env_grid_source", # 16
  "env_mining_manufacturing", # 17
  "env_disposal_recycle", # 18
  "env_overall_skepticism", # 19
  "used_vehicle_distrust", # 20
  "simply_not_interested", # 21
  "gas_engine_love", # 22
  "missing_ICE_features", # 23
  "knowledge_limited", # 24
  "other" # 25
)

# Parent → child mapping (for rollup columns)
parent_map <- list(
  parent_charging = c(
    "charging_home_access",
    "charging_public_lack",
    "charging_time"
  ),
  parent_range = c("range_daily_insufficient", "range_long_trip"),
  parent_cost = c(
    "cost_upfront_purchase",
    "cost_battery_replacement",
    "cost_electricity_operating",
    "cost_maintenance_insurance",
    "cost_general_value"
  ),
  parent_battery_concern = c(
    "battery_degradation",
    "battery_safety",
    "battery_weather_cold"
  ),
  parent_ev_distrust = c("ev_tech_immature", "ev_general_distrust"),
  parent_environmental = c(
    "env_grid_source",
    "env_mining_manufacturing",
    "env_disposal_recycle",
    "env_overall_skepticism"
  ),
  parent_used_distrust = c("used_vehicle_distrust"),
  parent_simply_not_int = c("simply_not_interested"),
  parent_gas_enthusiasm = c("gas_engine_love", "missing_ICE_features"),
  parent_limited_knowledge = c("knowledge_limited"),
  parent_other = c("other")
)

# ── Per-respondent codings (psid -> comma-separated column indices that = 1) ──
# Coded by reading each no_bev_selected0 response against the 25-sub-theme codebook.
# Residual rule: 21 (simply_not_interested) and 25 (other) fire ONLY if no other
# sub-theme applies.
spec <- c(
  "6792aaa32a815f81e7ef6c00|21",
  "670763434c8fd3dbbf341cf2|21",
  "697b6a31c5be61f911835aaa|21",
  "69c1924e8539891b35bd9a5c|15",
  "6662926b4a9e6c43497fdaf3|3,19",
  "6629658113ec221fd75635bb|11",
  "669d7166f5b54b43345f470a|1",
  "6657b55f598e9524b0e2f0e7|4,6,11,20",
  "67c73d5782c0bc8887ebe092|15",
  "66a017dc4b008452171ea97e|8",
  "66da824cbc26079e9e3c06a1|22",
  "5e4874c85c308330c8110aa4|21",
  "69b1a380d1a73b12ea3dff15|21",
  "65ce19f79d8488ef10b52a3e|15",
  "66748d21df607e201f1b692e|21",
  "6998af19175ea049781b31fe|10",
  "69ad746adbaf4ed19d5b218f|3,4,11",
  "69bd7998ee91edb9364ba992|21",
  "69ab5ae0fbd4b07dd054e822|11,12",
  "698c671e1816c886609f6af8|4",
  "64d5060402e9e0c10ff81e41|2,3,4,5,9",
  "6944426d5defea5e83fbdadf|1,6,7,14",
  "6998bdae820cf4c0812316d6|2,22",
  "699382a137908d9fac312139|20",
  "66c57de9f300cdb31238a7ac|22",
  "66303835b8a3536d5e202e91|21",
  "66a02700042f0e4fab8b698f|2,9,18",
  "696299db338fdebee65fcc12|6",
  "67f5041d9f11df8c9ff67b4e|6",
  "699a48258e833a6a4374f433|21",
  "663a8a538ae7d0205b2fa8c9|2,4,5,22",
  "69b8c291b887f7511afccfbf|2,4,11,22",
  "5c52966eac3bca0001d21d12|21",
  "65e2a2bb637373a334e3dbb6|7",
  "6787027230a5a1f3f6adad4b|21",
  "6941c1f6fc0380d01c4e25a9|6",
  "69a64043252cccc6ac2ae475|2,5",
  "6307d354d53118467b329503|2,4,5",
  "697bfa7384833e969276e195|21",
  "66d9d1eb189369965f4b8704|22",
  "6960623a4027c433ad7447d3|11,15,22",
  "6701580c116225812fb34d99|2,12,14",
  "6277fd1a3a086f8af7867fa0|4,22",
  "69ce7d2297b4750b47987000|9",
  "6995128bd75ec09b6e8b6973|21",
  "699fd694c9f21eda4add191d|2,5,15,22",
  "69600b1433413eeef0775039|8,15,22",
  "ge65x9kMOEZSYFUe1IBVVNpA|10,15",
  "bB30UrowC7N7YzcBEgXDgK5B|22",
  "a63g0KpOEMTVuZcARrGn7U4C|20",
  "vLM1EB3wYrgHrngZ23gEylgB|21",
  "KbE4CByVU1uupRYh8disoskJ|11",
  "dWa9BzxZpLUj6oZGtmgvUBq2|21",
  "ihZo0nm1F9XzzA0U6dVCDXn5|22",
  "yRXru6XFujm1gm2DfbNYSOYm|6,19",
  "afLgpZhIHGDhPZKpF0xigcAw|2,6,9,22",
  "DATdqGMX8hYb95EvVbsSJzBe|20",
  "WXGKYxAdXGrLUv3mI3MfRK0Z|6",
  "MM7apPvHR2VnrX6Uo6cysVEs|4,11",
  "vJuZb06McuKG8an9BPoML3ML|4",
  "RwIZfLWFIttLKNJAAfam9hPO|2",
  "V2x4Ti7wb5OJUSZHZ2AS7pCk|21",
  "afMrgo19VZZxmXhyG1npYJ8E|2",
  "FzbfUkg05fRTqmotRnmNGEP9|21",
  "WeUmVlCNsTqHRu94Ipm0jMK1|1,15",
  "WkC24UKK3rV2XPPaBuTdxJQV|21",
  "zbNHWh0FC5IcvP3xgXYSKjmc|7",
  "y2JJ9ZCSvSDVcZa2opHt6zRs|12",
  "9mUvmVYxOsXHgsIC6wjkFtfl|21",
  "FTznnepVsenvIOcMC45ItYzW|21",
  "z3HiFPsHuRD2oAAZwibPtjP7|21",
  "rOBlzvkwp4fw9DvGguGr2jbw|4,11,20",
  "MmB0q2TUvYYeTophYZvSnoKq|4,6",
  "QdaU3OG9ovK0coN7fNP6ZHML|5",
  "xf23lJCMlVPLXOYQ0ZV1i8YM|20",
  "1pR2t6ZNq6VaFGv9Jma2lN9v|21",
  "3VC3OmUC7rVlvS4rSQdSOrl5|20",
  "PdwgjRnIbdsGU4VorgUm3OER|21",
  "XDxlMkNVphjq8zJlyOiStuFW|6,15",
  "kjQh8liNSJRK1S5Kh7Szm1xg|21",
  "792fkFEv4CKq9JThKkOX6kg1|14,15",
  "8ATx3FniIERgovCMUqHyOGvd|1,15",
  "yE8z3AAt6G4F0FJp3os3OTec|12,15",
  "0fBA7dGrO6htzWzxlAr2bpQi|7,11",
  "1ELnJDNiTl30BVZbsKWTrLtU|21",
  "62uO9VutOXrODSnf3mLHFG7s|21",
  "5FBibTDblsN1yUKTxUocb9eH|21",
  "Kl9tfvGHTOFX3XA5M5kWysOG|21",
  "69ca4aa83fb819364415f6dd|21",
  "69907e70c4054eeb8965b30d|21",
  "69e0ae99beb07348718ee06d|7",
  "673b79ba5d7681e718c277fc|2,3,5,22",
  "69de95638988aeebd28245c4|3,10,19,22",
  "69cc98ab241394f39f691454|22",
  "6637f2807d667d4013989fc1|2,4,6,14",
  "65a2a922bfa22dde98c0d604|6",
  "69c3239a158bfee31783d53f|9,15",
  "69f8eef2a6fbd2a08ed4ccf4|9",
  "69cd5dcb8a87891b72f6ebf1|8,12",
  "6a05d2a43ea4837743f28d8a|21",
  "69f972198ab29387d981c907|7,12,19",
  "655ffddf6682f3df8008b8f9|21",
  "698e0d75e89fe7155bb2fd14|3,12,22",
  "69e9f6a993985bfec2114d70|21",
  "66d77183848780bed7462056|2,5,7,14,15",
  "63af4399152d060c863a6484|2,5",
  "66de61ebd22e266a86232075|6,11,20",
  "69cc740df30eeb7d258e0dc9|22",
  "663f6c71063220be73b4ba2f|1",
  "6741437f3c0f685464c09139|3,4,22",
  "69cfd3a8dc59402e42230ced|6",
  "663a5c69296e5ab16fa30d00|12",
  "69bf00b13882a8cf2f7400b3|21",
  "65e0d81acca2780f6d77bbe2|10,11,20",
  "615e4e76151c3bb90b6bd868|6",
  "6a029599262229428b4fb20a|21",
  "6730112557e35267b2274947|2",
  "69960866041cb0eb7a4f6487|3,5,13",
  "67abac2e77976bbccd35798b|4",
  "64820b6c2c4ffe2a7f30b1dc|4,12",
  "69adc9fb8215015327c2b342|3,5,9,15",
  "69aa6fa9ac8d4eca02d5dd7b|3,4",
  "5c337cb05b8ed100018a9b2e|15",
  "69964193eb8ec313b470ec6c|22",
  "695bdc063eacb21445424075|1",
  "69da46223396714fc0d4427d|14,15",
  "698f869ca948853644583ee2|1,6",
  "6104c9954ebe1d41cf1c3d9f|4,11,22",
  "69d480a9374b64399b0b276c|5,12",
  "69a719ff0a09773981d56612|21",
  "69992d4bd471ae1ac8d5d13b|4,10,20",
  "69f51166a4b79fa522b9bc9a|23",
  "695fe183c240e1fd35d3f1ef|11",
  "6a0910707b57c547ae145a77|3,4,12",
  "699c91e6dc40ecf071af9d13|22",
  "698dfc12afe15931772f452e|2,3",
  "6742afafe7ffabc756109eec|24",
  "69a9efa0aa4d2f1a98435f39|6,7",
  "6714457399a98b94cabd3bec|2,13,19,22",
  "5fac561bfa81812030b9fee0|21",
  "6a04b2b78043f4510945c845|11,20",
  "69d6f79ee03a2de194479371|21",
  "69fcc490e45bb5726737eb6e|4,5,6",
  "69be18a7edf32a3fca008f2c|21",
  "67c95fc1fefb0741ac3832b4|21",
  "66311eece7cd682caa24b768|1,4",
  "69ab75e828da1fa3fd63d816|4",
  "69b1e73b9dbf1dec3b4174de|12",
  "66f247b22b5fe367ca421d35|1",
  "66432cfe824559e786003361|15,18,19",
  "6989489cc139c6790863c43f|4,5",
  "66b29d1ec3671b7ea728c771|6",
  "697a5d5b10537642caf820dc|4",
  "698314cfbc5b5149e7eff3cf|5",
  "673e5f6b0c248822d47e064d|2,4,5",
  "671c468e4744030347f73f70|3,4",
  "69b704f9f5724767d87fa125|21",
  "63f2cff7f0a6f682868f2ca0|15,22,24",
  "69bdd4476bc8f30a2520c96d|10",
  "675c6ac13aff0173b803dbec|2,5",
  "69a8fbab64f103b79ec6a02b|14,20",
  "66d616d0aad3bc7ce893c04d|22",
  "6723d2acbc2d23b9506b2b46|6,11",
  "69f84a1d3e029f3c32ce3818|11",
  "697f93af0b8274ea279bb374|6,15,22",
  "69eea2cb76066b446d9833eb|2,3,4",
  "69c554c4669fbdcc2241932a|12,15",
  "695d2db768ef0dc1e54f11c8|21",
  "6a0102a61a9cddc2b509e682|9,11,23",
  "6972f6b68ba5cb2b5cf718a9|11,20",
  "69f64280ed4e3ec5e382b97b|2",
  "697e62d589e17fcbb4ed408d|21",
  "698fb2f1165d0fe11cc5f6b4|2,4,5",
  "6111b04382b7b8effc8b2b27|21",
  "69939b823c7d1ff0d5eecbb3|21",
  "6962dbac88944946c07d1497|2,22",
  "69929ba3656edf355205efdb|22",
  "6985f6870d2de85554edac63|22",
  "69a1ea2fb87a69e06221ba16|10,11",
  "66b9b602bba4ebe205ce3e49|21",
  "69bc340be433f0c0359ca134|22",
  "epb85GQCt0bZBfHBsLlfs9oC|21",
  "pZrS3hgW1oUFdJpa2RGSmzA8|3",
  "R6qA1aKb1XVMF5mgIgAbXsgm|21",
  "2j8TOmkHez3aaaWh01tyVu77|3",
  "LlUTuiCJAQzEXkVpt5K8mJmr|2,6,19",
  "v3HN2FoQjwgyFiius4oyKEvP|10",
  "eaeh6LgBqafDQUL5QIHNrn40|21",
  "MXv56fhZC1S9JkE2TncJ8Arl|2,10,14",
  "c4cQL5j8wAIMvEejd37HExRb|24",
  "5hOcYvtd6ta8VsB8JY7eG5P9|23",
  "RdZm5bMrBIbDgCfRYaEOwMHS|21",
  "nSTZIpoZF5FSl0HzWbC3sRES|21",
  "60e5039426cd97f8e467fa52|2,15",
  "69926e2f979ee89b9eebc165|1,4,11",
  "649ef9707838238c00232254|7",
  "694580e0f1757dd9295e5cb9|10,11",
  "67230cd39476e5bf3c464823|12,15",
  "6941b9d827b9d207b59e9301|22",
  "69cc02fe97b347ab5334ae71|22",
  "66aa59612e8897c5ab69032a|6",
  "kW9yXKxPfS3k7yTz7cylldmr|1,7",
  "7OVhPuzQ3rSou6sBWCyk3Jp4|5",
  "KS1Wm16RqcmGsicHXwczctUw|21",
  "PJzkw4qcDZGRR2xgjMqztAMX|21",
  "1W1PgIMnv2WmH3HlymPgdf4V|22",
  "ktYzJl7RzBklACYA1aZYYPsi|21",
  "Yl7ZHUxLaDZ3Jp4eNhZ8fI6w|6",
  "csPMMVWSGdm1KPlxpYjqiL1A|22"
)

# ── Expand sparse spec into binary matrix ─────────────────────────────────────
parts <- strsplit(spec, "\\|", fixed = FALSE)
psids <- vapply(parts, `[`, character(1), 1)
codes_strs <- vapply(parts, `[`, character(1), 2)

mat <- matrix(
  0L,
  nrow = length(spec),
  ncol = length(subthemes),
  dimnames = list(psids, subthemes)
)
for (i in seq_along(codes_strs)) {
  idxs <- as.integer(strsplit(codes_strs[i], ",", fixed = TRUE)[[1]])
  mat[i, idxs] <- 1L
}

cat("Coded", nrow(mat), "responses across", ncol(mat), "sub-themes.\n")

# ── Within-parent exclusivity rule for EV Distrust ───────────────────────────
# `ev_general_distrust` captures generic "no faith" with no concrete reason;
# `ev_tech_immature` captures "not ready yet". When both fire for the same
# respondent the underlying concern is immaturity expressed two ways, not two
# separate concerns (e.g. "I don't trust them YET" or "too risky RIGHT NOW").
# Keep the more specific code and drop the residual.
both_evdist <- mat[, "ev_general_distrust"] == 1 &
  mat[, "ev_tech_immature"] == 1
if (any(both_evdist)) {
  mat[both_evdist, "ev_general_distrust"] <- 0
  cat(
    "Within-parent exclusivity (EV Distrust):",
    sum(both_evdist),
    "cases adjusted.\n"
  )
}

# Sanity checks
stopifnot(length(unique(psids)) == nrow(mat)) # no duplicate psids
n_subthemes <- rowSums(mat)
stopifnot(all(n_subthemes >= 1)) # every row has at least one code

# Residual rule audit: simply_not_interested (21) should be the ONLY code when it fires
sni_violations <- which(mat[, "simply_not_interested"] == 1 & n_subthemes > 1)
if (length(sni_violations) > 0) {
  cat(
    "WARN:",
    length(sni_violations),
    "rows have simply_not_interested + other codes.\n"
  )
  print(psids[sni_violations])
}
# Same for other (25)
oth_violations <- which(mat[, "other"] == 1 & n_subthemes > 1)
if (length(oth_violations) > 0) {
  cat("WARN:", length(oth_violations), "rows have other + other codes.\n")
}

# Coverage summary
cat("\nSub-theme marginal counts (top to bottom):\n")
print(sort(colSums(mat), decreasing = TRUE))

# ── Build final tibble + join treatment + parent rollups ──────────────────────
# Text + treatment come from the source-of-truth data_joint.parquet rather
# than the older 0_nobev_themes_coded.parquet, so newly added psids (those in
# the current model sample but not previously coded) are properly joined.
data_full <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint.parquet"
))
text_lookup <- data_full %>%
  mutate(
    treatment = dplyr::case_when(
      prime_group_label == "prime_long" ~ "Extended Info",
      prime_group_label == "prime_short" ~ "Basic Info",
      TRUE ~ NA_character_
    )
  ) %>%
  select(psid, no_bev_selected0, treatment)

detailed <- as_tibble(mat, rownames = "psid") %>%
  left_join(text_lookup, by = "psid") %>%
  relocate(psid, no_bev_selected0, treatment) %>%
  mutate(n_subthemes = n_subthemes)

# Parent rollups: 1 if any child sub-theme = 1
for (p in names(parent_map)) {
  detailed[[p]] <- as.integer(
    rowSums(detailed[, parent_map[[p]], drop = FALSE]) > 0
  )
}

cat("\nFinal rows:", nrow(detailed), " cols:", ncol(detailed), "\n")
cat("Treatment x has-text:\n")
print(table(detailed$treatment, !is.na(detailed$no_bev_selected0)))

# ── Save ──────────────────────────────────────────────────────────────────────
out_path <- here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "0_nobev_themes_coded_detailed.parquet"
)
write_parquet(detailed, out_path)
cat("Saved:", out_path, "\n")

# Print parent rollup totals
cat("\nParent rollup counts:\n")
parent_totals <- sapply(names(parent_map), function(p) sum(detailed[[p]]))
print(sort(parent_totals, decreasing = TRUE))

# ── Share of respondents by number of sub-themes cited ────────────────────────
subtheme_dist <- detailed %>%
  count(n_subthemes, name = "n_respondents") %>%
  mutate(share = round(n_respondents / sum(n_respondents), 3))
cat("\nRespondents by number of sub-themes cited:\n")
print(subtheme_dist)

# Companion view: by number of DISTINCT parent themes (a respondent citing two
# sub-themes of the same parent counts as one parent theme here).
n_parent_themes <- rowSums(detailed[, names(parent_map)])
parent_dist <- tibble(n_parent_themes = n_parent_themes) %>%
  count(n_parent_themes, name = "n_respondents") %>%
  mutate(share = round(n_respondents / sum(n_respondents), 3))
cat("\nRespondents by number of distinct parent themes cited:\n")
print(parent_dist)
