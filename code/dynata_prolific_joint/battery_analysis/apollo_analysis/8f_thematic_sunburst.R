source(here::here('code', 'setup.R'))

for (pkg in c("showtext", "ggforce", "colorspace", "svglite", "ggrepel", "shadowtext")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(showtext)
library(ggforce)
library(colorspace)
library(ggrepel)
library(shadowtext)

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

detailed <- detailed %>%
  mutate(
    cost_maintenance_repair = as.integer(
      cost_battery_replacement == 1 | cost_maintenance_insurance == 1
    )
  )

# ── Labels + palette (same as 8b) ─────────────────────────────────────────────
parent_meta <- tribble(
  ~parent_id                 , ~parent_label               , ~color    ,
  "parent_range"             , "Range Anxiety"             , "#2E8B57" ,
  "parent_charging"          , "Charging Inconvenience"    , "#4682B4" ,
  "parent_cost"              , "Economic Barriers"         , "#B22222" ,
  "parent_battery_concern"   , "Battery Concerns"          , "#1F3D5C" ,
  "parent_environmental"     , "Environmental Concerns"    , "#556B2F" ,
  "parent_ev_distrust"       , "EV Distrust"               , "#8B008B" ,
  "parent_used_distrust"     , "Used Vehicle Distrust"     , "#7A3F5C" ,
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
  "cost_general_value"         , "parent_cost"              , "Generic 'Not Worth'"                   ,
  "battery_degradation"        , "parent_battery_concern"   , "Degradation / Lifespan"                ,
  "battery_safety"             , "parent_battery_concern"   , "Safety (Fire, Crash)"                  ,
  "battery_weather_cold"       , "parent_battery_concern"   , "Cold-Weather Low Performance"          ,
  "ev_tech_immature"           , "parent_ev_distrust"       , "Technology Immature"                   ,
  "ev_general_distrust"        , "parent_ev_distrust"       , "General Distrust"                      ,
  "env_grid_source"            , "parent_environmental"     , "Grid Source"                           ,
  "env_mining_manufacturing"   , "parent_environmental"     , "Mining / Manufacturing"                ,
  "env_disposal_recycle"       , "parent_environmental"     , "Battery Disposal/Recycle"              ,
  "env_overall_skepticism"     , "parent_environmental"     , "Generic 'Green' Skepticism"            ,
  "used_vehicle_distrust"      , "parent_used_distrust"     , "Used Vehicle Distrust"                 ,
  "simply_not_interested"      , "parent_simply_not_int"    , "Simply Not Interested"                 ,
  "gas_engine_love"            , "parent_gas_enthusiasm"    , "Love of Gas Engines"                   ,
  "missing_ICE_features"       , "parent_gas_enthusiasm"    , "BEVs Missing Certain Vehicle Features" ,
  "knowledge_limited"          , "parent_limited_knowledge" , "Limited Knowledge on BEVs"
)

# Counts
subtheme_meta <- subtheme_meta %>%
  mutate(n = sapply(subtheme, function(s) sum(detailed[[s]]))) %>%
  filter(n > 0)

parent_meta <- parent_meta %>%
  mutate(n = sapply(parent_id, function(p) sum(detailed[[p]]))) %>%
  filter(n > 0)

# Single-child parents: no outer arc
parent_kid_counts <- subtheme_meta %>% count(parent_id, name = "k")
parent_meta <- parent_meta %>%
  left_join(parent_kid_counts, by = "parent_id") %>%
  mutate(k = coalesce(k, 0L), is_single = k == 1)

cat(
  "Active parents:",
  nrow(parent_meta),
  " active sub-themes:",
  nrow(subtheme_meta),
  "\n"
)

# ── Angular layout ────────────────────────────────────────────────────────────
# Standard sunburst sizing:
#   Inner ring: parent arc ∝ parent_n / sum(parent_n) → sums to 2π.
#   Outer ring: within each parent's arc, sub-themes divide the arc in
#     proportion to their WITHIN-PARENT frequency. Sub-theme boundaries
#     therefore align exactly with their parent's boundaries.
# Angles are radians CLOCKWISE from 12 o'clock (matches ggforce::geom_arc_bar).

parents_in_order <- c(
  "parent_range",
  "parent_charging",
  "parent_battery_concern",
  "parent_cost",
  "parent_environmental",
  "parent_ev_distrust",
  "parent_used_distrust",
  "parent_simply_not_int",
  "parent_limited_knowledge",
  "parent_gas_enthusiasm"
)
parents_in_order <- intersect(parents_in_order, parent_meta$parent_id)

parent_meta <- parent_meta %>%
  mutate(parent_id = factor(parent_id, levels = parents_in_order)) %>%
  arrange(parent_id) %>%
  mutate(parent_id = as.character(parent_id))

total_parent_n <- sum(parent_meta$n)
total_sub_n <- sum(subtheme_meta$n)

parent_arcs <- parent_meta %>%
  mutate(
    w = n / total_parent_n * 2 * pi,
    start = cumsum(w) - w,
    end = start + w,
    mid = (start + end) / 2
  )

# Sub-themes grouped by parent, descending by count within parent
subtheme_meta <- subtheme_meta %>%
  mutate(parent_idx = match(parent_id, parents_in_order)) %>%
  arrange(parent_idx, desc(n))

sub_arcs <- subtheme_meta %>%
  left_join(
    parent_arcs %>% select(parent_id, parent_start = start, parent_w_arc = w),
    by = "parent_id"
  ) %>%
  group_by(parent_id) %>%
  mutate(
    sub_n_in_parent = sum(n),
    frac_in_parent  = n / sub_n_in_parent,
    w               = frac_in_parent * parent_w_arc,
    cum_end         = cumsum(w),
    start           = parent_start + cum_end - w,
    end             = start + w,
    mid             = (start + end) / 2
  ) %>%
  ungroup()

# ── Colors ────────────────────────────────────────────────────────────────────
# Parent = darkest base color. All sub-themes get a lighter shade than parent.
# Largest sub-theme is the least light; smaller ones get progressively lighter.
# Uses a lightness ramp in HCL space so hue is preserved.
LIGHT_START <- 0.25 # largest sub-theme: 25% lighter than parent
LIGHT_END <- 0.65 # smallest sub-theme in a parent with many kids: 65% lighter

sub_arcs <- sub_arcs %>%
  left_join(
    parent_meta %>% select(parent_id, parent_color = color, k),
    by = "parent_id"
  ) %>%
  group_by(parent_id) %>%
  mutate(
    rank_in_parent = row_number(), # 1 = largest (already sorted desc by n)
    # Lightness fraction: rank 1 -> LIGHT_START; last -> LIGHT_END. If k==1, use LIGHT_START.
    lf = if (n() == 1) {
      LIGHT_START
    } else {
      LIGHT_START + (rank_in_parent - 1) * (LIGHT_END - LIGHT_START) / (n() - 1)
    },
    color = colorspace::lighten(
      parent_color,
      amount = lf,
      method = "relative",
      space = "HCL"
    )
  ) %>%
  ungroup()

# ── Radii ─────────────────────────────────────────────────────────────────────
# Ring widths widened to give more labels room inside. Sub-theme ring span
# (R_OUTER - R_INNER) is what controls how many characters can fit radially in
# each outer segment; parent span controls parent labels.
R_HOLE <- 2.7
R_INNER <- 7.5 # inner ring: parents [R_HOLE, R_INNER]   span = 4.8
R_OUTER <- 12.0 # outer ring: sub-themes [R_INNER, R_OUTER]  span = 4.5
R_ARC <- 12.9 # first shell for outside interdependency arcs
R_LBL <- 14.5 # outside label anchor (revised below to sit beyond arcs)
R_LBL2 <- 15.9

# ── Sub-theme centroids for cross-links ───────────────────────────────────────
# Placed at the MID-radius of the outer ring so dashed lines emerge from the arc
# body rather than its inner or outer edge.
R_LINK <- (R_INNER + R_OUTER) / 2
angle_to_xy <- function(theta_clock, r) {
  a <- pi / 2 - theta_clock # convert clock→math
  tibble(x = r * cos(a), y = r * sin(a))
}

sub_centroids <- sub_arcs %>%
  transmute(subtheme, parent_id, theta = mid, r = R_LINK) %>%
  bind_cols(angle_to_xy(.$theta, .$r))

# For single-child parents (no outer arc), use the parent's outer edge midpoint
# so within-parent links can still terminate cleanly if any exist.
single_child_endpoints <- parent_meta %>%
  filter(is_single) %>%
  inner_join(subtheme_meta, by = "parent_id") %>%
  inner_join(parent_arcs %>% select(parent_id, mid), by = "parent_id") %>%
  transmute(
    subtheme = subtheme,
    parent_id = parent_id,
    theta = mid,
    r = (R_HOLE + R_INNER) / 2
  ) %>%
  {
    bind_cols(., angle_to_xy(.$theta, .$r))
  }

# Rebuild centroids: outer-ring points for multi-child parents; inner-ring
# midpoint for single-child parents.
sub_centroids <- bind_rows(
  sub_centroids %>%
    filter(
      !(parent_id %in% (parent_meta %>% filter(is_single) %>% pull(parent_id)))
    ),
  single_child_endpoints %>% select(subtheme, parent_id, theta, r, x, y)
)

# ── Cross-theme co-occurrence (same as 8b) ────────────────────────────────────
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
within_pairs <- tibble()
for (i in seq_along(active_subs)) {
  for (j in seq_along(active_subs)) {
    if (j <= i) {
      next
    }
    a <- active_subs[i]
    b <- active_subs[j]
    joint <- co_mat[i, j]
    if (parent_of[a] != parent_of[b]) {
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
    } else {
      if (joint < 1) {
        next
      }
      within_pairs <- bind_rows(
        within_pairs,
        tibble(from = a, to = b, joint = joint, parent_id = parent_of[a])
      )
    }
  }
}
cat(
  "Cross-parent edges:",
  nrow(cross_pairs),
  " within-parent edges:",
  nrow(within_pairs),
  "\n"
)

# NOTE: interdependency arcs + their sample-size badges are disabled here per
# user request; the user will draw them manually in the final figure. The
# co-occurrence data structures (cross_pairs / within_pairs) above are still
# computed so the interdependency table can still be inspected in R if needed.
# Uncomment the block below to re-enable auto-drawn arcs.

# single_child_parents_v <- parent_meta %>% filter(is_single) %>% pull(parent_id)
# edge_endpoints <- sub_centroids %>%
#   select(subtheme, theta, parent_id) %>%
#   mutate(r_end = ifelse(parent_id %in% single_child_parents_v, R_INNER, R_OUTER))
# all_pairs <- bind_rows(
#   cross_pairs %>% mutate(kind = "cross"),
#   within_pairs %>% mutate(kind = "within")
# ) %>%
#   left_join(edge_endpoints %>% select(from = subtheme, ta = theta, r_from = r_end),
#             by = "from") %>%
#   left_join(edge_endpoints %>% select(to   = subtheme, tb = theta, r_to = r_end),
#             by = "to")
#
# ARC_CURVATURE <- -1.0   # negative = outward bulge for CCW-ordered chords
#
# if (nrow(all_pairs) > 0) {
#   all_pairs <- all_pairs %>%
#     mutate(
#       ta_math = (pi/2 - ta) %% (2*pi),
#       tb_math = (pi/2 - tb) %% (2*pi),
#       ccw_dist = (tb_math - ta_math) %% (2*pi),
#       swap = ccw_dist > pi,
#       start_math = ifelse(swap, tb_math, ta_math),
#       end_math   = ifelse(swap, ta_math, tb_math),
#       start_r    = ifelse(swap, r_to, r_from),
#       end_r      = ifelse(swap, r_from, r_to),
#       x_start = start_r * cos(start_math),
#       y_start = start_r * sin(start_math),
#       x_end   = end_r   * cos(end_math),
#       y_end   = end_r   * sin(end_math),
#       chord_mid_x = (x_start + x_end) / 2,
#       chord_mid_y = (y_start + y_end) / 2,
#       chord_mid_r = sqrt(chord_mid_x^2 + chord_mid_y^2),
#       chord_len   = sqrt((x_end - x_start)^2 + (y_end - y_start)^2),
#       apex_h      = abs(ARC_CURVATURE) * chord_len / 2,
#       lbl_x = chord_mid_x + (chord_mid_x / chord_mid_r) * apex_h,
#       lbl_y = chord_mid_y + (chord_mid_y / chord_mid_r) * apex_h,
#       badge = paste0("n=", joint)
#     )
#   R_ARC_MAX <- max(sqrt(all_pairs$lbl_x^2 + all_pairs$lbl_y^2))
# } else {
#   R_ARC_MAX <- R_OUTER
# }

# Empty stand-in so downstream code that checks nrow(all_pairs) still works
# without needing further edits.
all_pairs <- tibble()

# ── Label positioning ─────────────────────────────────────────────────────────
# Strategy: RADIAL text (reads outward from center along the radial line).
#   - Reading direction and length are radial → limited by the ring's radial
#     span (or by the space outside the outer ring, for narrow arcs).
#   - Tangential extent is the text HEIGHT (n_lines * line_height) → limited
#     by the arc width at the label's radius.
#   - Sideways reading at top/bottom is expected in sunburst diagrams.
# An arc "fits inside" if the ring's radial span accommodates the longest line
# and the arc's tangential extent at the label's mid-radius accommodates the
# stack of lines. Otherwise the label is pushed OUTSIDE at R_LBL with a leader
# line from the arc's outer edge to the label anchor.

deg <- function(rad) rad * 180 / pi

# Radial rotation with upright flip: text baseline points outward from center
# along the radial. For anchors in the "left half" (math angle 90°-270°) the
# baseline direction points leftward, so we rotate 180° so characters remain
# upright. hjust = 0.5 (center-anchored) so the label is centered on r_anchor
# radially, giving a uniform look regardless of label length.
radial_geom <- function(theta_clock, r_anchor, hjust_outside = FALSE) {
  d <- (deg(theta_clock) %% 360)
  m <- (90 - d) %% 360 # math angle in degrees
  right_half <- (m <= 90 | m >= 270)
  rot <- ifelse(right_half, m, m + 180)
  rot <- ((rot + 180) %% 360) - 180
  # For OUTSIDE labels we want the text to extend AWAY from the ring (i.e.,
  # from anchor going outward). Use hjust matching the radial direction so
  # the anchor sits at the inner (ring-facing) edge of the label.
  hjust <- if (hjust_outside) ifelse(right_half, 0, 1) else 0.5
  m_rad <- (pi / 2 - theta_clock)
  data.frame(
    rot = rot,
    hjust = hjust,
    x_anchor = r_anchor * cos(m_rad),
    y_anchor = r_anchor * sin(m_rad)
  )
}

# Character-width and line-height heuristics per showtext-Roboto Condensed.
# Both scale linearly with text size.
CW_PER_SIZE <- 0.034 # units per (char × size point)
LH_PER_SIZE <- 0.050 # units per (line × size point)
PARENT_SIZE_MAX <- 4.6
PARENT_SIZE_MIN <- 3.0
SUB_SIZE_MAX <- 3.6
SUB_SIZE_MIN <- 2.4

# Wrap a name to keep max-line-length <= max_chars, preferring word boundaries.
wrap_name <- function(name, max_chars) {
  words <- strsplit(name, " ", fixed = TRUE)[[1]]
  if (length(words) == 0) {
    return(name)
  }
  lines <- character()
  cur <- ""
  for (w in words) {
    trial <- if (nzchar(cur)) paste(cur, w) else w
    if (nchar(trial) > max_chars && nzchar(cur)) {
      lines <- c(lines, cur)
      cur <- w
    } else {
      cur <- trial
    }
  }
  lines <- c(lines, cur)
  paste(lines, collapse = "\n")
}

# Fitting rule for radial INSIDE placement, at a specific text size.
#   longest line * (CW_PER_SIZE * size)  <=  radial_span
#   n_lines * (LH_PER_SIZE * size)       <=  arc_length_at_r_mid
label_fits_at_size <- function(text, arc_w, r_mid, radial_span, size) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  max_chars <- max(nchar(lines))
  n_lines <- length(lines)
  char_w <- CW_PER_SIZE * size
  line_h <- LH_PER_SIZE * size
  radial_ok <- max_chars * char_w * 1.10 <= radial_span
  tangential_ok <- n_lines * line_h * 1.15 <= arc_w * r_mid
  radial_ok & tangential_ok
}

# Find the best layout: try multiple wrap widths (widest first, since fewer
# lines need less tangential space), and within each width find the largest
# size that fits. Return the (wrap, size) combination whose fitting size is
# the highest. If nothing fits even at (widest, min-size), return NULL and the
# caller falls back to the min size + widest wrap (label will overflow).
best_fit_layout <- function(name, n_val, arc_w, r_mid, radial_span,
                            size_max, size_min, step = 0.2) {
  wrap_options <- c(999L, 30L, 24L, 20L, 16L, 12L, 10L)
  best <- NULL
  for (ww in wrap_options) {
    wrapped <- wrap_name(name, ww)
    lbl <- paste0(wrapped, "\nn=", n_val)
    for (s in seq(size_max, size_min, by = -step)) {
      if (label_fits_at_size(lbl, arc_w, r_mid, radial_span, s)) {
        if (is.null(best) || s > best$size) {
          best <- list(size = s, box_label = lbl, wrap = ww, fits = TRUE)
        }
        break
      }
    }
  }
  if (!is.null(best)) return(best)
  # Fallback: render at the MAX size for its category, single-line format.
  # Label will overflow the ring boundary (radially and/or tangentially into
  # neighboring arcs), which is acceptable — the user wants consistent sizing
  # across parents (and sub-themes) even at the cost of some overflow.
  list(size = size_max,
       box_label = paste0(wrap_name(name, 999L), "\nn=", n_val),
       wrap = 999L, fits = FALSE)
}

# Radial spans available for label reading. For sub-themes we allow generous
# overflow beyond R_OUTER so labels can extend past the outer ring boundary
# when the arc is too narrow to fit at any wrap.
SPAN_PARENT     <- R_INNER - R_HOLE
SUB_OVERFLOW    <- 2.5                       # sub-theme labels may extend outward
SPAN_SUB        <- (R_OUTER - R_INNER) + SUB_OVERFLOW

# Helper that runs best_fit_layout for one row and returns a data frame.
run_fit <- function(name, n_val, arc_w, r_mid, radial_span, smax, smin) {
  best <- best_fit_layout(name, n_val, arc_w, r_mid, radial_span, smax, smin)
  data.frame(box_label = best$box_label, text_size = best$size,
             fits = best$fits, stringsAsFactors = FALSE)
}

parent_fits <- do.call(rbind, Map(
  run_fit,
  parent_arcs$parent_label, parent_arcs$n, parent_arcs$w,
  (R_HOLE + R_INNER) / 2,
  MoreArgs = list(radial_span = SPAN_PARENT,
                  smax = PARENT_SIZE_MAX, smin = PARENT_SIZE_MIN)
))
parent_arcs <- parent_arcs %>%
  mutate(r_mid = (R_HOLE + R_INNER) / 2) %>%
  bind_cols(parent_fits)
parent_arcs <- bind_cols(parent_arcs,
                         radial_geom(parent_arcs$mid, parent_arcs$r_mid))

# Single-child sub-themes: skip labels — the parent label already carries the
# same name and count. They still participate in edge lookups.
single_child_parents <- parent_meta %>% filter(is_single) %>% pull(parent_id)
sub_arcs_labeled <- sub_arcs %>% filter(!(parent_id %in% single_child_parents))

sub_fits <- do.call(rbind, Map(
  run_fit,
  sub_arcs_labeled$label, sub_arcs_labeled$n, sub_arcs_labeled$w,
  (R_INNER + R_OUTER) / 2,
  MoreArgs = list(radial_span = SPAN_SUB,
                  smax = SUB_SIZE_MAX, smin = SUB_SIZE_MIN)
))
sub_arcs_labeled <- sub_arcs_labeled %>%
  mutate(r_mid = (R_INNER + R_OUTER) / 2) %>%
  bind_cols(sub_fits)
sub_arcs_labeled <- bind_cols(sub_arcs_labeled,
                              radial_geom(sub_arcs_labeled$mid, sub_arcs_labeled$r_mid))

# All labels stay anchored to their arc's mid-radius. Labels whose ideal
# layout couldn't fit even at min-size will show up as fits==FALSE — they are
# still drawn (single line at min size) and may radially overflow the ring
# boundary. Per user request: no outside labels, no leader lines.
cat(
  "Labels: parents fitting cleanly:", sum(parent_arcs$fits), "/", nrow(parent_arcs),
  "  sub-themes fitting cleanly:", sum(sub_arcs_labeled$fits), "/", nrow(sub_arcs_labeled),
  "\n  (labels that don't fit are drawn at min size and may overflow the boundary)\n"
)

# Data frames for ggforce arc bars (needs x0, y0, r0, r, start, end, fill)
parent_bars <- parent_arcs %>%
  transmute(
    x0 = 0,
    y0 = 0,
    r0 = R_HOLE,
    r = R_INNER,
    start = start,
    end = end,
    fill = color
  )

sub_bars <- sub_arcs %>%
  # For single-child parents, do NOT draw the outer arc.
  filter(
    !(parent_id %in% (parent_meta %>% filter(is_single) %>% pull(parent_id)))
  ) %>%
  transmute(
    x0 = 0,
    y0 = 0,
    r0 = R_INNER,
    r = R_OUTER,
    start = start,
    end = end,
    fill = color
  )

# ── Print interdependency table to console ───────────────────────────────────
# Disabled together with the arc drawing. Re-enable by uncommenting.
# sub_label_lookup <- subtheme_meta %>%
#   select(subtheme, sub_label = label, parent_id)
# parent_label_lookup <- parent_meta %>% select(parent_id, parent_label)
# report_edges <- all_pairs %>%
#   select(from, to, joint, kind, jaccard, parent_id) %>%
#   left_join(
#     sub_label_lookup %>%
#       rename(from = subtheme, from_label = sub_label, from_parent = parent_id),
#     by = "from"
#   ) %>%
#   left_join(
#     sub_label_lookup %>%
#       rename(to = subtheme, to_label = sub_label, to_parent = parent_id),
#     by = "to"
#   ) %>%
#   left_join(
#     parent_label_lookup %>%
#       rename(from_parent = parent_id, from_parent_label = parent_label),
#     by = "from_parent"
#   ) %>%
#   left_join(
#     parent_label_lookup %>%
#       rename(to_parent = parent_id, to_parent_label = parent_label),
#     by = "to_parent"
#   ) %>%
#   arrange(kind, desc(joint)) %>%
#   select(
#     kind,
#     from_parent_label,
#     from_label,
#     to_parent_label,
#     to_label,
#     joint,
#     jaccard
#   )
#
# cat("\n=========================================================\n")
# cat("INTERDEPENDENCY ARCS (drawn outside the ring)\n")
# cat("Cross-parent: joint respondents >= 6 AND Jaccard >= 0.08\n")
# cat("Within-parent: joint respondents >= 1\n")
# cat("=========================================================\n")
# print(report_edges, n = Inf)
# cat("=========================================================\n\n")

# ── Compute a tight bounding box so the image isn't wasted on empty quadrants
xs <- c(-R_OUTER, R_OUTER)
ys <- c(-R_OUTER, R_OUTER)
# Extend by interdependency arc apex points (label positions) plus a small
# margin equal to the badge extent.
# Disabled with the arc drawing. Re-enable if arcs are re-enabled.
# if (nrow(all_pairs) > 0) {
#   xs <- c(xs, all_pairs$lbl_x, all_pairs$x_start, all_pairs$x_end)
#   ys <- c(ys, all_pairs$lbl_y, all_pairs$y_start, all_pairs$y_end)
# }
# Extend by radial reach of any label that couldn't fit inside (single-line
# fallback) — these overflow the outer ring boundary.
overflow_labels <- sub_arcs_labeled %>% filter(!fits)
if (nrow(overflow_labels) > 0) {
  reach <- sapply(strsplit(overflow_labels$box_label, "\n", fixed = TRUE),
                  function(v) max(nchar(v))) *
           CW_PER_SIZE * overflow_labels$text_size
  m_rad <- pi/2 - overflow_labels$mid
  # Anchor is at r_mid; label extends radially by ±reach/2 (centered hjust=0.5).
  outer_reach <- overflow_labels$r_mid + reach / 2 + 0.3
  xs <- c(xs, outer_reach * cos(m_rad))
  ys <- c(ys, outer_reach * sin(m_rad))
}
BUF <- 1.2
xlim_tight <- c(min(xs) - BUF, max(xs) + BUF)
ylim_tight <- c(min(ys) - BUF, max(ys) + BUF)
aspect <- diff(xlim_tight) / diff(ylim_tight)
save_h <- 14
save_w <- save_h * aspect
cat(sprintf(
  "Canvas: xlim=[%.1f, %.1f], ylim=[%.1f, %.1f], aspect=%.2f, save %.1f x %.1f in\n",
  xlim_tight[1],
  xlim_tight[2],
  ylim_tight[1],
  ylim_tight[2],
  aspect,
  save_w,
  save_h
))

cat("Dynamic label sizes — parents:\n")
print(parent_arcs %>% select(parent_id, w, text_size, fits))
cat("Dynamic label sizes — sub-themes:\n")
print(sub_arcs_labeled %>% select(subtheme, w, text_size, fits))

# ── Figure ────────────────────────────────────────────────────────────────────
# Disabled with the arc drawing.
# ARC_COLOR <- "#D62728"
# BADGE_COLOR <- "#7A1F1F"

p <- ggplot() +
  # Ring segments
  geom_arc_bar(
    data = parent_bars,
    aes(
      x0 = x0,
      y0 = y0,
      r0 = r0,
      r = r,
      start = start,
      end = end,
      fill = fill
    ),
    color = "white",
    linewidth = 0.6
  ) +
  geom_arc_bar(
    data = sub_bars,
    aes(
      x0 = x0,
      y0 = y0,
      r0 = r0,
      r = r,
      start = start,
      end = end,
      fill = fill
    ),
    color = "white",
    linewidth = 0.5
  ) +
  # INTERDEPENDENCY CURVES + badges: disabled per user request; the user will
  # add these to the final figure manually. Re-enable by uncommenting.
  # {
  #   if (nrow(all_pairs) > 0) {
  #     geom_curve(
  #       data = all_pairs,
  #       aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
  #       curvature = ARC_CURVATURE, ncp = 20,
  #       color = ARC_COLOR, linewidth = 0.75,
  #       alpha = 0.85, linetype = "22"
  #     )
  #   }
  # } +
  # {
  #   if (nrow(all_pairs) > 0) {
  #     geom_label(
  #       data = all_pairs,
  #       aes(x = lbl_x, y = lbl_y, label = badge),
  #       family = "Roboto Condensed", fontface = "bold",
  #       size = 2.9, color = BADGE_COLOR, fill = "white",
  #       label.padding = unit(0.14, "lines"), label.size = 0.3
  #     )
  #   }
  # } +
  # INSIDE labels: parents (all — those that fit AND those that overflow).
  # White fill with a dark outline so the label is legible on any parent color.
  geom_shadowtext(
    data = parent_arcs,
    aes(x = x_anchor, y = y_anchor, label = box_label,
        angle = rot, hjust = hjust, size = text_size),
    family = "Roboto Condensed", fontface = "bold",
    color = "white", bg.colour = "grey15", bg.r = 0.16,
    lineheight = 0.9, vjust = 0.5
  ) +
  # INSIDE labels: sub-themes (all — narrow-arc labels may overflow radially).
  geom_text(
    data = sub_arcs_labeled,
    aes(x = x_anchor, y = y_anchor, label = box_label,
        angle = rot, hjust = hjust, size = text_size),
    family = "Roboto Condensed", fontface = "plain",
    color = "grey12", lineheight = 0.9, vjust = 0.5
  ) +
  scale_size_identity() +
  # Center hub — an EXACT circle at R_HOLE so it never bleeds over the ring.
  geom_circle(
    data = data.frame(x0 = 0, y0 = 0),
    aes(x0 = x0, y0 = y0, r = R_HOLE),
    fill = "white",
    color = "grey35",
    linewidth = 0.6,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = 0,
    y = 0,
    label = paste0("Reasons for\nNot Selecting\nAny Used BEVs\nN=", N_total),
    family = "Roboto Condensed",
    fontface = "bold",
    color = "grey25",
    size = 4.4,
    lineheight = 1.0
  ) +

  scale_fill_identity() +
  scale_color_identity() +
  coord_fixed(xlim = xlim_tight, ylim = ylim_tight, clip = "off") +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(6, 6, 6, 6)
  )

out_dir <- here(
  "code",
  "output",
  "images",
  "battery_analysis",
  "thematic_analysis"
)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

png_path <- file.path(out_dir, "thematic_sunburst_nobev.png")
svg_path <- file.path(out_dir, "thematic_sunburst_nobev.svg")

ggsave(
  p,
  filename = png_path,
  width = save_w,
  height = save_h,
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)
ggsave(
  p,
  filename = svg_path,
  width = save_w,
  height = save_h,
  device = svglite::svglite,
  bg = "white",
  limitsize = FALSE
)
cat("Saved:\n  ", png_path, "\n  ", svg_path, "\n")
