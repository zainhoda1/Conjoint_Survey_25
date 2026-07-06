source(here::here('code', 'setup.R'))

# ── Reuse the same data + layout as 6_thematic_network.R ──────────────────────
detailed <- read_parquet(here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "0_nobev_themes_coded_detailed.parquet"
))
N_total <- nrow(detailed)
detailed <- detailed %>%
  mutate(
    cost_maintenance_repair = as.integer(
      cost_battery_replacement == 1 | cost_maintenance_insurance == 1
    )
  )

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

sub_counts <- sapply(subtheme_meta$subtheme, function(s) sum(detailed[[s]]))
subtheme_meta <- subtheme_meta %>%
  mutate(n = sub_counts[subtheme]) %>%
  filter(n > 0)
# Parent n = unique respondents with at least one sub-theme in that category.
# A respondent who cites multiple sub-themes in the same parent counts ONCE
# toward the parent total but is counted in each sub-theme they cite.
parent_meta <- parent_meta %>%
  mutate(n = sapply(parent_id, function(p) sum(detailed[[p]]))) %>%
  filter(n > 0)
parent_kid_counts <- subtheme_meta %>% count(parent_id, name = "k")
parent_meta <- parent_meta %>%
  left_join(parent_kid_counts, by = "parent_id") %>%
  mutate(is_single = k == 1)

# Layout (same logic as figure script, deterministic seed)
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
n_kids <- sapply(parents_in_order, function(p) {
  sum(subtheme_meta$parent_id == p)
})
slot <- pmax(n_kids, 1)
sector_w <- 2 * pi * (slot + 0.9) / sum(slot + 0.9)
sector_start <- cumsum(c(0, head(sector_w, -1)))
sector_center_clock <- sector_start + sector_w / 2
to_math <- function(a) pi / 2 - a
R_PARENT <- 4
R_CHILD <- 9.5

multi_parents <- parent_meta %>% filter(!is_single) %>% pull(parent_id)
parent_layout <- tibble(
  parent_id = parents_in_order,
  angle = to_math(sector_center_clock),
  sector_w = sector_w,
  sector_center_clock = sector_center_clock
) %>%
  left_join(parent_meta %>% select(parent_id, is_single), by = "parent_id") %>%
  mutate(
    px = ifelse(
      is_single,
      (R_PARENT + R_CHILD) / 2 * cos(angle),
      R_PARENT * cos(angle)
    ),
    py = ifelse(
      is_single,
      (R_PARENT + R_CHILD) / 2 * sin(angle),
      R_PARENT * sin(angle)
    )
  )

set.seed(7)
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
  a_clock <- a_base + runif(k, -0.05, 0.05)
  R <- R_CHILD + runif(k, -1.4, 1.4)
  a_math <- to_math(a_clock)
  tibble(
    subtheme = kids,
    parent_id = p,
    cx = R * cos(a_math),
    cy = R * sin(a_math)
  )
})

# Cross-edges (same rule)
active_subs <- subtheme_meta$subtheme
M <- as.matrix(detailed[, active_subs])
co_mat <- crossprod(M)
n_marg <- diag(co_mat)
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
    uni <- n_marg[i] + n_marg[j] - joint
    jacv <- if (uni > 0) joint / uni else 0
    if (jacv < 0.08) {
      next
    }
    cross_pairs <- bind_rows(
      cross_pairs,
      tibble(from = a, to = b, joint = joint, jaccard = jacv)
    )
  }
}

# Within-parent co-occurrence pairs (same parent, joint >= 1).
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

# ── Convert layout to draw.io pixel coords ────────────────────────────────────
SCALE <- 65 # px per layout-unit
CX_PX <- 1000 # canvas center x
CY_PX <- 800 # canvas center y
NODE_W_PARENT <- 200
NODE_H_PARENT <- 78
NODE_W_CHILD <- 180
NODE_H_CHILD <- 62
NODE_W_CENTER <- 230
NODE_H_CENTER <- 130

to_px <- function(x, y, w, h) {
  list(
    x = round(CX_PX + x * SCALE - w / 2),
    y = round(CY_PX - y * SCALE - h / 2)
  )
}

xml_esc <- function(s) {
  s |>
    gsub("&", "&amp;", x = _, fixed = TRUE) |>
    gsub("<", "&lt;", x = _, fixed = TRUE) |>
    gsub(">", "&gt;", x = _, fixed = TRUE) |>
    gsub('"', "&quot;", x = _, fixed = TRUE)
}

# Multi-line labels: draw.io renders <br> in html=1 styles
two_line <- function(top, n) paste0(xml_esc(top), "&lt;br&gt;(n=", n, ")")

style_center <- paste0(
  "ellipse;whiteSpace=wrap;html=1;rounded=0;",
  "fillColor=#FFE7A8;strokeColor=#8A6E00;strokeWidth=2;",
  "fontFamily=Roboto Condensed;fontSize=15;fontStyle=1;fontColor=#5A4A00;align=center;verticalAlign=middle"
)
style_parent <- function(color) {
  paste0(
    "rounded=1;whiteSpace=wrap;html=1;arcSize=12;",
    "fillColor=",
    color,
    ";strokeColor=",
    color,
    ";strokeWidth=2;",
    "fontFamily=Roboto Condensed;fontSize=15;fontStyle=1;fontColor=#FFFFFF;align=center;verticalAlign=middle"
  )
}
style_child <- function(color) {
  paste0(
    "rounded=1;whiteSpace=wrap;html=1;arcSize=10;",
    "fillColor=#FFFFFF;strokeColor=",
    color,
    ";strokeWidth=1.6;",
    "fontFamily=Roboto Condensed;fontSize=13;fontStyle=0;fontColor=",
    color,
    ";align=center;verticalAlign=middle"
  )
}
style_hier_edge <- "endArrow=none;html=1;strokeColor=#999999;strokeWidth=1.5;edgeStyle=none;rounded=0"
style_cross_edge <- function(width) {
  paste0(
    "endArrow=none;html=1;strokeColor=#D62728;strokeWidth=",
    round(width, 2),
    ";dashed=1;dashPattern=6 4;edgeStyle=none;curved=1;exitX=0.5;exitY=0.5;entryX=0.5;entryY=0.5;",
    "exitDx=0;exitDy=0;entryDx=0;entryDy=0;exitPerimeter=0;entryPerimeter=0"
  )
}
style_within_edge <- function(color) {
  paste0(
    "endArrow=none;html=1;strokeColor=", color, ";strokeWidth=1.4;",
    "dashed=1;dashPattern=4 3;edgeStyle=none;curved=1;",
    "exitX=0.5;exitY=0.5;entryX=0.5;entryY=0.5;",
    "exitDx=0;exitDy=0;entryDx=0;entryDy=0;exitPerimeter=0;entryPerimeter=0"
  )
}

# ── Build XML ─────────────────────────────────────────────────────────────────
xml <- c(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<mxfile host="app.diagrams.net" type="device">',
  '  <diagram id="thematic_network" name="Thematic Network">',
  '    <mxGraphModel dx="2200" dy="1500" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="2000" pageHeight="1600" math="0" shadow="0">',
  '      <root>',
  '        <mxCell id="0" />',
  '        <mxCell id="1" parent="0" />'
)

# Center node
ctr <- to_px(0, 0, NODE_W_CENTER, NODE_H_CENTER)
ctr_label <- paste0(
  "Reasons for Not Selecting any Used BEVs&lt;br&gt;(N=",
  N_total,
  ")"
)
xml <- c(
  xml,
  sprintf(
    '        <mxCell id="center" value="%s" style="%s" vertex="1" parent="1"><mxGeometry x="%d" y="%d" width="%d" height="%d" as="geometry"/></mxCell>',
    ctr_label,
    style_center,
    ctr$x,
    ctr$y,
    NODE_W_CENTER,
    NODE_H_CENTER
  )
)

# Parent nodes
parent_nodes <- parent_layout %>%
  left_join(parent_meta, by = c("parent_id", "is_single"))
parent_id_to_xml <- setNames(
  paste0("p_", seq_len(nrow(parent_nodes))),
  parent_nodes$parent_id
)
for (i in seq_len(nrow(parent_nodes))) {
  r <- parent_nodes[i, ]
  pos <- to_px(r$px, r$py, NODE_W_PARENT, NODE_H_PARENT)
  label <- two_line(r$parent_label, r$n)
  xml <- c(
    xml,
    sprintf(
      '        <mxCell id="%s" value="%s" style="%s" vertex="1" parent="1"><mxGeometry x="%d" y="%d" width="%d" height="%d" as="geometry"/></mxCell>',
      parent_id_to_xml[r$parent_id],
      label,
      style_parent(r$color),
      pos$x,
      pos$y,
      NODE_W_PARENT,
      NODE_H_PARENT
    )
  )
}

# Child nodes
child_nodes <- child_layout %>%
  left_join(subtheme_meta, by = c("subtheme", "parent_id")) %>%
  left_join(parent_meta %>% select(parent_id, color), by = "parent_id")
sub_id_to_xml <- setNames(
  paste0("c_", seq_len(nrow(child_nodes))),
  child_nodes$subtheme
)
for (i in seq_len(nrow(child_nodes))) {
  r <- child_nodes[i, ]
  pos <- to_px(r$cx, r$cy, NODE_W_CHILD, NODE_H_CHILD)
  label <- two_line(r$label, r$n)
  xml <- c(
    xml,
    sprintf(
      '        <mxCell id="%s" value="%s" style="%s" vertex="1" parent="1"><mxGeometry x="%d" y="%d" width="%d" height="%d" as="geometry"/></mxCell>',
      sub_id_to_xml[r$subtheme],
      label,
      style_child(r$color),
      pos$x,
      pos$y,
      NODE_W_CHILD,
      NODE_H_CHILD
    )
  )
}

# Hierarchy edges: center → each parent
for (pid in parent_nodes$parent_id) {
  xml <- c(
    xml,
    sprintf(
      '        <mxCell id="e_ctr_%s" style="%s" edge="1" parent="1" source="center" target="%s"><mxGeometry relative="1" as="geometry"/></mxCell>',
      parent_id_to_xml[pid],
      style_hier_edge,
      parent_id_to_xml[pid]
    )
  )
}
# Hierarchy edges: parent → child (only for multi-child parents)
for (i in seq_len(nrow(child_nodes))) {
  r <- child_nodes[i, ]
  src <- parent_id_to_xml[r$parent_id]
  tgt <- sub_id_to_xml[r$subtheme]
  xml <- c(
    xml,
    sprintf(
      '        <mxCell id="e_h_%d" style="%s" edge="1" parent="1" source="%s" target="%s"><mxGeometry relative="1" as="geometry"/></mxCell>',
      i,
      style_hier_edge,
      src,
      tgt
    )
  )
}

# Cross-edges with joint-count label embedded on the edge
# For single-child parents, the endpoint is the parent box itself.
endpoint_id <- function(sub) {
  if (sub %in% names(sub_id_to_xml)) {
    return(sub_id_to_xml[[sub]])
  }
  # Fall back to parent node id (single-child collapsed)
  p <- parent_of[[sub]]
  parent_id_to_xml[[p]]
}
for (i in seq_len(nrow(cross_pairs))) {
  r <- cross_pairs[i, ]
  src <- endpoint_id(r$from)
  tgt <- endpoint_id(r$to)
  width <- 1.2 + r$jaccard * 4
  edge_label <- paste0("n=", r$joint)
  xml <- c(
    xml,
    sprintf(
      '        <mxCell id="e_x_%d" value="%s" style="%s" edge="1" parent="1" source="%s" target="%s"><mxGeometry relative="1" as="geometry"><Array as="points"/></mxGeometry></mxCell>',
      i,
      edge_label,
      style_cross_edge(width),
      src,
      tgt
    )
  )
}

# Within-parent edges: parent-colored dashed lines for any joint >= 1.
within_color_lookup <- setNames(parent_meta$color, parent_meta$parent_id)
for (i in seq_len(nrow(within_pairs))) {
  r <- within_pairs[i, ]
  src <- endpoint_id(r$from)
  tgt <- endpoint_id(r$to)
  edge_label <- paste0("n=", r$joint)
  color <- unname(within_color_lookup[r$parent_id])
  xml <- c(
    xml,
    sprintf(
      '        <mxCell id="e_w_%d" value="%s" style="%s" edge="1" parent="1" source="%s" target="%s"><mxGeometry relative="1" as="geometry"><Array as="points"/></mxGeometry></mxCell>',
      i,
      edge_label,
      style_within_edge(color),
      src,
      tgt
    )
  )
}

xml <- c(
  xml,
  '      </root>',
  '    </mxGraphModel>',
  '  </diagram>',
  '</mxfile>'
)

out_path <- here(
  "code",
  "output",
  "images",
  "battery_analysis",
  "thematic_analysis",
  "thematic_network_nobev.drawio"
)
writeLines(xml, out_path, useBytes = TRUE)
cat("Saved:", out_path, "\n")
cat("Open at https://app.diagrams.net/ and use File > Open > Device.\n")
