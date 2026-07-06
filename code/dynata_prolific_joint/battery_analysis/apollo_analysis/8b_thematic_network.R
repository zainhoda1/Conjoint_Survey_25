source(here::here('code', 'setup.R'))

if (!requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext", repos = "https://cloud.r-project.org")
}
library(showtext)
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 300)

# ── Load coded data ───────────────────────────────────────────────────────────
detailed <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "0_nobev_themes_coded_detailed.parquet"
))
N_total <- nrow(detailed)
cat("Loaded", N_total, "responses.\n")

# Combine cost_battery_replacement + cost_maintenance_insurance into a single
# "Maintenance / Repair" sub-theme. Battery replacement is itself a maintenance
# event, so this collapses the two cost sub-themes that share that semantics.
detailed <- detailed %>%
  mutate(
    cost_maintenance_repair = as.integer(
      cost_battery_replacement == 1 | cost_maintenance_insurance == 1
    )
  )

# ── Labels + palette ──────────────────────────────────────────────────────────
parent_meta <- tribble(
  ~parent_id                 , ~parent_label               , ~color    ,
  "parent_range"             , "Range Anxiety"             , "#2E8B57" ,
  "parent_charging"          , "Charging Inconvenience"    , "#4682B4" ,
  "parent_cost"              , "Economic Barriers"         , "#B22222" ,
  "parent_battery_concern"   , "Battery Concerns"          , "#1F3D5C" ,
  "parent_environmental"     , "Environmental Concerns"    , "#556B2F" ,
  "parent_ev_distrust"       , "EV Distrust"               , "#8B008B" ,
  "parent_used_distrust"     , "Used Vehicle Distrust"     , "#A0522D" ,
  "parent_simply_not_int"    , "Simply Not Interested"     , "#708090" ,
  "parent_gas_enthusiasm"    , "Gas Vehicle Enthusiasm"    , "#8B4513" ,
  "parent_limited_knowledge" , "Limited Knowledge on BEVs" , "#DAA520"
)

subtheme_meta <- tribble(
  ~subtheme                    , ~parent_id                 , ~label                                  ,
  "charging_home_access"       , "parent_charging"          , "Lack of Home Chargers"                 ,
  "charging_public_lack"       , "parent_charging"          , "Lack of Public Chargers"               ,
  "charging_time"              , "parent_charging"          , "Charging Time"                         ,
  "range_daily_insufficient"   , "parent_range"             , "Daily Range Insufficient"              ,
  "range_long_trip"            , "parent_range"             , "Long-Trip Range Insufficient"          ,
  "cost_upfront_purchase"      , "parent_cost"              , "Upfront Purchase"                      ,
  "cost_maintenance_repair"    , "parent_cost"              , "Maintenance / Repair"                  ,
  "cost_electricity_operating" , "parent_cost"              , "Operation Cost"                        ,
  "cost_general_value"         , "parent_cost"              , "Generic 'Not Worth It'"                ,
  "battery_degradation"        , "parent_battery_concern"   , "Degradation / Lifespan"                ,
  "battery_safety"             , "parent_battery_concern"   , "Safety (Fire, Crash)"                  ,
  "battery_weather_cold"       , "parent_battery_concern"   , "Cold-Weather Low Performance"          ,
  "ev_tech_immature"           , "parent_ev_distrust"       , "Technology Immature"                   ,
  "ev_general_distrust"        , "parent_ev_distrust"       , "General Distrust"                      ,
  "env_grid_source"            , "parent_environmental"     , "Grid Source"                           ,
  "env_mining_manufacturing"   , "parent_environmental"     , "Mining / Manufacturing"                ,
  "env_disposal_recycle"       , "parent_environmental"     , "Battery Disposal / Recycle"            ,
  "env_overall_skepticism"     , "parent_environmental"     , "Overall 'Green' Skepticism"            ,
  "used_vehicle_distrust"      , "parent_used_distrust"     , "Used Vehicle Distrust"                 ,
  "simply_not_interested"      , "parent_simply_not_int"    , "Simply Not Interested"                 ,
  "gas_engine_love"            , "parent_gas_enthusiasm"    , "Love of Gas Engines"                   ,
  "missing_ICE_features"       , "parent_gas_enthusiasm"    , "BEVs Missing Certain Vehicle Features" ,
  "knowledge_limited"          , "parent_limited_knowledge" , "Limited Knowledge on BEVs"
)

# Drop sub-themes with zero count
sub_counts <- sapply(subtheme_meta$subtheme, function(s) sum(detailed[[s]]))
subtheme_meta <- subtheme_meta %>%
  mutate(n = sub_counts[subtheme]) %>%
  filter(n > 0)
cat("Active sub-themes:", nrow(subtheme_meta), "\n")

# Parent n = unique respondents with at least one sub-theme in that category.
# A respondent who cites multiple sub-themes within the same parent counts
# ONCE toward the parent total but is counted in each sub-theme they cite.
# Consequence: sum(children_n) >= parent_n, with the gap equal to the amount
# of within-parent multi-labeling. This is the methodologically correct
# behavior; the visual mismatch is informative, not a bug, and the figure
# caption explains it.
parent_meta <- parent_meta %>%
  mutate(n = sapply(parent_id, function(p) sum(detailed[[p]]))) %>%
  filter(n > 0)
cat("Active parents:", nrow(parent_meta), "\n")

# Flag single-child parents — these will collapse into a single combined box
parent_kid_counts <- subtheme_meta %>% count(parent_id, name = "k")
parent_meta <- parent_meta %>%
  left_join(parent_kid_counts, by = "parent_id") %>%
  mutate(is_single = k == 1)

# ── Layout: hub-spoke ─────────────────────────────────────────────────────────
parents_in_order <- c(
  "parent_range",
  "parent_charging",
  "parent_cost",
  "parent_battery_concern",
  "parent_environmental",
  "parent_ev_distrust",
  "parent_used_distrust",
  "parent_simply_not_int",
  "parent_limited_knowledge",
  "parent_gas_enthusiasm"
)
parents_in_order <- intersect(parents_in_order, parent_meta$parent_id)

# Sector width: proportional to (visible items in that sector) + buffer.
# For collapsed single-child parents, sector still needs ~1 slot.
n_kids_per_parent <- sapply(parents_in_order, function(p) {
  sum(subtheme_meta$parent_id == p)
})
slot_per_parent <- pmax(n_kids_per_parent, 1)
sector_w <- 2 * pi * (slot_per_parent + 0.9) / sum(slot_per_parent + 0.9)
sector_start <- cumsum(c(0, head(sector_w, -1)))
sector_center_clock <- sector_start + sector_w / 2

# Convert clockwise-from-top angle to standard math angle
to_math <- function(a_clock) pi / 2 - a_clock

R_PARENT <- 4
R_CHILD <- 9.5 # pushed out further to reduce label crowding

parent_layout <- tibble(
  parent_id = parents_in_order,
  sector_w = sector_w,
  sector_center_clock = sector_center_clock,
  angle = to_math(sector_center_clock),
  x = R_PARENT * cos(angle),
  y = R_PARENT * sin(angle)
)

# Build child nodes ONLY for multi-child parents.
# Children are placed near their parent's radial direction but with deterministic
# jitter on both angle and radius — gives an organic mind-map feel and frees up
# space so labels can be bigger.
set.seed(7)
multi_parents <- parent_meta %>% filter(!is_single) %>% pull(parent_id)
child_layout <- map_dfr(seq_along(parents_in_order), function(i) {
  p <- parents_in_order[i]
  if (!(p %in% multi_parents)) {
    return(NULL)
  }
  kids <- subtheme_meta %>% filter(parent_id == p) %>% pull(subtheme)
  k <- length(kids)
  inner_w <- sector_w[i] * 0.62
  a_base <- seq(
    sector_center_clock[i] - inner_w / 2,
    sector_center_clock[i] + inner_w / 2,
    length.out = k
  )
  # Jitter: angular ±0.05 rad (~2.9°), radial ±1.4 units around R_CHILD
  a_clock <- a_base + runif(k, -0.05, 0.05)
  R <- R_CHILD + runif(k, -1.4, 1.4)
  a_math <- to_math(a_clock)
  tibble(
    subtheme = kids,
    parent_id = p,
    angle = a_math,
    x = R * cos(a_math),
    y = R * sin(a_math)
  )
})

# For single-child parents, push parent node further out (to where the child would be)
# so it stays visually balanced with multi-child clusters.
parent_layout <- parent_layout %>%
  left_join(
    parent_meta %>% select(parent_id, is_single, n, parent_label, color, k),
    by = "parent_id"
  ) %>%
  mutate(
    x = ifelse(is_single, (R_PARENT + R_CHILD) / 2 * cos(angle), x),
    y = ifelse(is_single, (R_PARENT + R_CHILD) / 2 * sin(angle), y)
  )

# Parent box labels: always show n. For multi-child parents, n = unique
# respondents with ≥1 sub-theme in that category (may be less than the sum of
# children counts because responses are multi-labeled within a category).
parent_nodes <- parent_layout %>%
  mutate(
    box_label = paste0(parent_label, "\n(n=", n, ")"),
    type = "parent"
  )

child_nodes <- child_layout %>%
  left_join(subtheme_meta, by = c("subtheme", "parent_id")) %>%
  left_join(parent_meta %>% select(parent_id, color), by = "parent_id") %>%
  mutate(
    box_label = paste0(label, "\n(n=", n, ")"),
    type = "child"
  )

center_node <- tibble(
  x = 0,
  y = 0,
  box_label = paste0(
    "Reasons for Not\nSelecting any Used\nBEVs\n(N=",
    N_total,
    ")"
  ),
  color = "#333333",
  type = "center"
)

# ── Hierarchy edges ───────────────────────────────────────────────────────────
# For single-child parents, parent_layout$x/y already sits at child distance,
# so center→parent edge spans the same length as multi-child parents' two-segment chain.
hier_edges <- bind_rows(
  parent_nodes %>%
    transmute(x_from = 0, y_from = 0, x_to = x, y_to = y, color = color),
  # parent → child only for multi-child parents
  child_nodes %>%
    inner_join(
      parent_layout %>%
        filter(!is_single) %>%
        # For multi-child parents we need the inner (R_PARENT) coords, recompute:
        mutate(px = R_PARENT * cos(angle), py = R_PARENT * sin(angle)) %>%
        select(parent_id, px, py),
      by = "parent_id"
    ) %>%
    transmute(x_from = px, y_from = py, x_to = x, y_to = y, color = color)
)
# Replace center→parent endpoint for multi-child parents back to R_PARENT (so the
# hub edge stops at the parent box, not at the (collapsed) child position).
hier_edges_center <- parent_layout %>%
  mutate(
    end_x = ifelse(is_single, x, R_PARENT * cos(angle)),
    end_y = ifelse(is_single, y, R_PARENT * sin(angle))
  ) %>%
  transmute(x_from = 0, y_from = 0, x_to = end_x, y_to = end_y, color = color)

hier_edges <- bind_rows(
  hier_edges_center,
  child_nodes %>%
    inner_join(
      parent_layout %>%
        filter(!is_single) %>%
        mutate(px = R_PARENT * cos(angle), py = R_PARENT * sin(angle)) %>%
        select(parent_id, px, py),
      by = "parent_id"
    ) %>%
    transmute(x_from = px, y_from = py, x_to = x, y_to = y, color = color)
)

# For multi-child parents, we ALSO need to update parent_nodes' display position
# back to the inner ring (R_PARENT) so the parent box doesn't sit on top of children.
parent_nodes <- parent_nodes %>%
  mutate(
    x = ifelse(is_single, x, R_PARENT * cos(angle)),
    y = ifelse(is_single, y, R_PARENT * sin(angle))
  )

# ── Cross-edges: co-occurrence between sub-themes from DIFFERENT parents ──────
# Use the combined sub-theme list (after merging cost_maintenance_repair).
active_subs <- subtheme_meta$subtheme
M <- as.matrix(detailed[, active_subs])
co_mat <- crossprod(M)
n_marg <- diag(co_mat)

jac <- co_mat
for (i in seq_along(active_subs)) {
  for (j in seq_along(active_subs)) {
    if (i == j) {
      jac[i, j] <- NA
      next
    }
    uni <- n_marg[i] + n_marg[j] - co_mat[i, j]
    jac[i, j] <- if (uni > 0) co_mat[i, j] / uni else 0
  }
}

parent_of <- setNames(subtheme_meta$parent_id, subtheme_meta$subtheme)
cross_pairs <- tibble()
for (i in seq_along(active_subs)) {
  for (j in seq_along(active_subs)) {
    if (j <= i) {
      next
    }
    a <- active_subs[i]
    b <- active_subs[j]
    if (parent_of[a] == parent_of[b]) {
      next
    }
    joint <- co_mat[i, j]
    if (joint < 6) {
      next
    }
    jacv <- jac[i, j]
    if (jacv < 0.08) {
      next
    }
    cross_pairs <- bind_rows(
      cross_pairs,
      tibble(from = a, to = b, joint = joint, jaccard = jacv)
    )
  }
}
cat(
  "Cross-edges (different-parent, joint>=6, jaccard>=0.08):",
  nrow(cross_pairs),
  "\n"
)
print(cross_pairs %>% arrange(desc(jaccard)))

# Within-parent co-occurrence: pairs of sub-themes in the SAME parent that any
# respondent cited together. Threshold is joint >= 1 because within-parent
# counts are small and any co-mention is informative about the shape of the
# parent. Drawn separately from cross-parent edges with the parent's color.
within_pairs <- tibble()
for (i in seq_along(active_subs)) {
  for (j in seq_along(active_subs)) {
    if (j <= i) next
    a <- active_subs[i]; b <- active_subs[j]
    if (parent_of[a] != parent_of[b]) next
    joint <- co_mat[i, j]
    if (joint < 1) next
    within_pairs <- bind_rows(
      within_pairs,
      tibble(from = a, to = b, joint = joint, parent_id = parent_of[a])
    )
  }
}
cat("Within-parent edges (joint>=1):", nrow(within_pairs), "\n")
print(within_pairs %>% arrange(desc(joint)))

# Cross-edge endpoint lookup: child nodes for multi-child parents; for
# single-child parents the "endpoint" is the (collapsed) parent node.
endpoint_lookup <- bind_rows(
  child_nodes %>% select(subtheme, x, y),
  # For single-child parents, the parent's collapsed box is the endpoint
  parent_meta %>%
    filter(is_single) %>%
    inner_join(subtheme_meta, by = "parent_id") %>%
    inner_join(parent_nodes %>% select(parent_id, x, y), by = "parent_id") %>%
    select(subtheme, x, y)
)

cross_edges <- cross_pairs %>%
  left_join(
    endpoint_lookup %>% rename(from = subtheme, x_from = x, y_from = y),
    by = "from"
  ) %>%
  left_join(
    endpoint_lookup %>% rename(to = subtheme, x_to = x, y_to = y),
    by = "to"
  )

# Compute label position for joint-count badges. Curves bend off the chord;
# offset the badge perpendicular to the chord in the same direction the curve
# bends so the label sits near the apex of the arc, not on the straight chord.
CURV <- 0.25
cross_edges <- cross_edges %>%
  mutate(
    dx = x_to - x_from,
    dy = y_to - y_from,
    L = sqrt(dx^2 + dy^2),
    # Perpendicular unit vector; sign chosen to match geom_curve(curvature > 0)
    # which bends to the LEFT of the directed chord — equivalent to rotating
    # the chord vector +90° (CCW): (-dy, dx)/L
    perp_x = -dy / L,
    perp_y = dx / L,
    # Apex of a circular-ish arc: ~ chord_midpoint + perp * L * (curvature / 2.2)
    lbl_x = (x_from + x_to) / 2 + perp_x * L * (CURV / 2.2),
    lbl_y = (y_from + y_to) / 2 + perp_y * L * (CURV / 2.2),
    badge = paste0("n=", joint)
  )

# Within-parent edges: same endpoint lookup; color by the parent's hex so they
# read as part of the parent cluster rather than as cross-cutting edges. Uses
# slightly less curvature so labels sit closer to the chord.
WITHIN_CURV <- 0.18
within_edges <- within_pairs %>%
  left_join(
    endpoint_lookup %>% rename(from = subtheme, x_from = x, y_from = y),
    by = "from"
  ) %>%
  left_join(
    endpoint_lookup %>% rename(to = subtheme, x_to = x, y_to = y),
    by = "to"
  ) %>%
  left_join(parent_meta %>% select(parent_id, color), by = "parent_id") %>%
  mutate(
    dx = x_to - x_from,
    dy = y_to - y_from,
    L  = sqrt(dx^2 + dy^2),
    perp_x = -dy / L,
    perp_y =  dx / L,
    lbl_x = (x_from + x_to) / 2 + perp_x * L * (WITHIN_CURV / 2.2),
    lbl_y = (y_from + y_to) / 2 + perp_y * L * (WITHIN_CURV / 2.2),
    badge = paste0("n=", joint)
  )

# ── Build figure ──────────────────────────────────────────────────────────────
xlim <- c(-15.5, 15.5)
ylim <- c(-15.5, 15.5)

p <- ggplot() +
  # Within-parent dashed edges (drawn first so they sit behind cross-edges).
  # Color = parent's hex; one badge per within-parent co-mention pair.
  geom_curve(
    data = within_edges,
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = color),
    linewidth = 0.6, alpha = 0.7, linetype = "22",
    curvature = WITHIN_CURV, ncp = 12
  ) +
  geom_label(
    data = within_edges,
    aes(x = lbl_x, y = lbl_y, label = badge, color = color),
    family = "Roboto Condensed", fontface = "bold",
    size = 2.6, fill = "white",
    label.padding = unit(0.10, "lines"), label.size = 0.25
  ) +
  geom_segment(
    data = hier_edges,
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
    color = "grey70",
    linewidth = 0.45
  ) +
  geom_curve(
    data = cross_edges,
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = jaccard),
    color = "#D62728",
    alpha = 0.55,
    linetype = "22",
    curvature = CURV,
    ncp = 12
  ) +
  scale_linewidth_continuous(range = c(0.4, 1.6), guide = "none") +
  # Joint-count badges at curve apexes
  geom_label(
    data = cross_edges,
    aes(x = lbl_x, y = lbl_y, label = badge),
    family = "Roboto Condensed",
    fontface = "bold",
    size = 3.0,
    color = "#7A1F1F",
    fill = "white",
    label.padding = unit(0.14, "lines"),
    label.size = 0.3
  ) +

  # Child nodes
  geom_label(
    data = child_nodes,
    aes(x = x, y = y, label = box_label, color = color),
    family = "Roboto Condensed",
    fontface = "plain",
    size = 3.6,
    lineheight = 0.95,
    label.padding = unit(0.22, "lines"),
    label.size = 0.45,
    fill = "white"
  ) +
  # Parent nodes
  geom_label(
    data = parent_nodes,
    aes(x = x, y = y, label = box_label, fill = color),
    family = "Roboto Condensed",
    fontface = "bold",
    size = 4.5,
    lineheight = 0.95,
    label.padding = unit(0.32, "lines"),
    label.size = 0.55,
    color = "white"
  ) +
  # Center node
  geom_point(
    data = center_node,
    aes(x = x, y = y),
    shape = 21,
    fill = "#FFE7A8",
    color = "#8A6E00",
    size = 46,
    stroke = 1.3
  ) +
  geom_text(
    data = center_node,
    aes(x = x, y = y, label = box_label),
    family = "Roboto Condensed",
    fontface = "bold",
    size = 4.0,
    lineheight = 1.0,
    color = "#5A4A00"
  ) +

  scale_color_identity() +
  scale_fill_identity() +
  coord_fixed(xlim = xlim, ylim = ylim, clip = "off") +
  labs(
    caption = "Parent n = unique respondents with ≥1 sub-theme in that category. A respondent who cites multiple sub-themes within the same parent is counted ONCE toward the parent total but is counted in each sub-theme they cite, so sum(children) ≥ parent n; the gap is the amount of within-parent multi-labeling.\nDashed lines indicate interdependencies among barriers. Parent-colored dashed lines between sibling sub-themes flag within-parent co-mention (the same respondent raised two distinct sub-themes inside one parent). Red dashed curves connect sub-themes from DIFFERENT parents that were co-mentioned (Jaccard ≥ 0.08, joint ≥ 6); curve thickness scales with the Jaccard overlap.\nBadges show the joint count = number of respondents mentioning BOTH sub-themes."
  ) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(8, 8, 8, 8),
    plot.caption = element_text(
      hjust = 0.5,
      size = 9,
      color = "grey30",
      margin = margin(t = 8)
    )
  )

p

out_path <- here(
  "code",
  "output",
  "images",
  "battery_analysis",
  "thematic_analysis",
  "thematic_network_nobev.jpg"
)
ggsave(p, filename = out_path, width = 13, height = 13, dpi = 300)
cat("Saved:", out_path, "\n")
