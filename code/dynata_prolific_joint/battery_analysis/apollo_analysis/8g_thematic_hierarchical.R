source(here::here('code', 'setup.R'))

for (pkg in c("showtext", "colorspace", "svglite", "shadowtext")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(showtext)
library(colorspace)
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

# ── Meta (colors identical to the sunburst 8f) ────────────────────────────────
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

subtheme_meta <- subtheme_meta %>%
  mutate(n = sapply(subtheme, function(s) sum(detailed[[s]]))) %>%
  filter(n > 0)
parent_meta <- parent_meta %>%
  mutate(n = sapply(parent_id, function(p) sum(detailed[[p]]))) %>%
  filter(n > 0)
parent_kid_counts <- subtheme_meta %>% count(parent_id, name = "k")
parent_meta <- parent_meta %>%
  left_join(parent_kid_counts, by = "parent_id") %>%
  mutate(k = coalesce(k, 0L), is_single = k == 1)

# ── Order themes by sample size (descending) and greedy-balance across cols ──
parent_meta <- parent_meta %>% arrange(desc(n))

# Greedy: walk through the sorted parents; assign each to whichever column
# currently has the smaller cumulative height.
col_h <- c(0, 0)
col_assign <- integer(nrow(parent_meta))
for (i in seq_len(nrow(parent_meta))) {
  pick <- if (col_h[1] <= col_h[2]) 1L else 2L
  col_assign[i] <- pick
  col_h[pick] <- col_h[pick] + parent_meta$n[i]
}
parent_meta$col <- col_assign

# Within each column, keep the sample-size-descending order,
# BUT with a manual swap of Gas Vehicle Enthusiasm ↔ Used Vehicle Distrust
# when they end up in the same column: swapping shortens the Battery
# Concerns ↔ Gas Vehicle Enthusiasm cross-mention arc that runs across cols.
parent_meta <- parent_meta %>%
  group_by(col) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  ungroup()

gas_id <- "parent_gas_enthusiasm"
used_id <- "parent_used_distrust"
if (all(c(gas_id, used_id) %in% parent_meta$parent_id)) {
  same_col <- with(
    parent_meta,
    col[parent_id == gas_id] == col[parent_id == used_id]
  )
  if (same_col) {
    ordering <- parent_meta$parent_id
    g <- which(ordering == gas_id)
    u <- which(ordering == used_id)
    ordering[c(g, u)] <- ordering[c(u, g)]
    parent_meta <- parent_meta %>% arrange(match(parent_id, ordering))
  }
}
parent_meta <- parent_meta %>% mutate(idx = row_number())

cat(
  "Column heights (respondent-units): col1=",
  col_h[1],
  "  col2=",
  col_h[2],
  "\n",
  sep = ""
)

# ── Vertical layout (per column, from top down) ───────────────────────────────
# Larger gap between theme blocks stretches the figure vertically so it fills
# a full landscape letter page (11 x 8.5 in) and gives the denser arc set room.
PARENT_GAP <- 10

parent_layout <- parent_meta %>%
  group_by(col) %>%
  arrange(idx, .by_group = TRUE) %>%
  mutate(
    step_h = n + PARENT_GAP,
    cum_before = cumsum(step_h) - step_h,
    y_top = -cum_before,
    y_bot = y_top - n,
    y_mid = (y_top + y_bot) / 2
  ) %>%
  ungroup()

# Sub-theme layout
sub_layout <- subtheme_meta %>%
  arrange(match(parent_id, parent_meta$parent_id), desc(n)) %>%
  left_join(
    parent_layout %>% select(parent_id, col, p_y_top = y_top, p_n = n),
    by = "parent_id"
  ) %>%
  group_by(parent_id) %>%
  mutate(
    sum_n_in_parent = sum(n),
    height_scaled = (n / sum_n_in_parent) * p_n,
    cum_before = cumsum(height_scaled) - height_scaled,
    y_top = p_y_top - cum_before,
    y_bot = y_top - height_scaled,
    y_mid = (y_top + y_bot) / 2
  ) %>%
  ungroup()

# Suppress sub-theme rectangles for single-child parents
single_child_ids <- parent_meta %>% filter(is_single) %>% pull(parent_id)
sub_layout_multi <- sub_layout %>% filter(!parent_id %in% single_child_ids)

# ── Colors ────────────────────────────────────────────────────────────────────
LIGHT_START <- 0.25
LIGHT_END <- 0.65

sub_layout_multi <- sub_layout_multi %>%
  left_join(
    parent_meta %>% select(parent_id, parent_color = color),
    by = "parent_id"
  ) %>%
  group_by(parent_id) %>%
  mutate(
    rank_in_parent = row_number(),
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

# ── X coordinates (wider themes and sub-themes so labels fit) ────────────────
W_THEME <- 85
W_SUB <- 80
GAP <- 5
INTER_GAP <- 40 # space between the two sub-theme blocks — arc territory

X_P1_LEFT <- 0
X_P1_RIGHT <- W_THEME
X_J1 <- X_P1_RIGHT + GAP
X_S1_LEFT <- X_J1 + GAP
X_S1_RIGHT <- X_S1_LEFT + W_SUB

X_S2_LEFT <- X_S1_RIGHT + INTER_GAP
X_S2_RIGHT <- X_S2_LEFT + W_SUB
X_J2 <- X_S2_RIGHT + GAP
X_P2_LEFT <- X_J2 + GAP
X_P2_RIGHT <- X_P2_LEFT + W_THEME

parent_layout <- parent_layout %>%
  mutate(
    x_left = ifelse(col == 1, X_P1_LEFT, X_P2_LEFT),
    x_right = ifelse(col == 1, X_P1_RIGHT, X_P2_RIGHT),
    x_mid = (x_left + x_right) / 2
  )

sub_layout_multi <- sub_layout_multi %>%
  mutate(
    x_left = ifelse(col == 1, X_S1_LEFT, X_S2_LEFT),
    x_right = ifelse(col == 1, X_S1_RIGHT, X_S2_RIGHT),
    x_mid = (x_left + x_right) / 2,
    x_inner = ifelse(col == 1, X_S1_RIGHT, X_S2_LEFT) # edge facing the gap
  )

# ── Bracket connectors ──────────────────────────────────────────────────────
brackets_parents <- parent_layout %>% filter(!parent_id %in% single_child_ids)

seg_theme_to_bracket <- brackets_parents %>%
  mutate(
    x = ifelse(col == 1, X_P1_RIGHT, X_P2_LEFT),
    xend = ifelse(col == 1, X_J1, X_J2)
  ) %>%
  transmute(x = x, y = y_mid, xend = xend, yend = y_mid)

seg_bracket_to_sub <- sub_layout_multi %>%
  mutate(
    x = ifelse(col == 1, X_J1, X_J2),
    xend = ifelse(col == 1, X_S1_LEFT, X_S2_RIGHT)
  ) %>%
  transmute(x = x, y = y_mid, xend = xend, yend = y_mid)

sub_y_ranges <- sub_layout_multi %>%
  group_by(parent_id, col) %>%
  summarise(
    y_top_mid = max(y_mid),
    y_bot_mid = min(y_mid),
    n_children = n(),
    .groups = "drop"
  ) %>%
  filter(n_children > 1)

bracket_verticals <- sub_y_ranges %>%
  mutate(x = ifelse(col == 1, X_J1, X_J2)) %>%
  transmute(x = x, y = y_bot_mid, xend = x, yend = y_top_mid)

# ── Cross-mention arcs between sub-themes ────────────────────────────────────
# Compute co-occurrence: how many respondents cited BOTH sub-themes.
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

# Single criterion for ALL connections (cross-theme and within-theme alike):
# drawn when cited together by at least five respondents. The Jaccard screen
# is disabled (set to 0) — at this sample size it never binds anyway.
JOINT_MIN <- 5
JACC_MIN <- 0
WITHIN_MIN <- 5

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
    if (parent_of[a] == parent_of[b]) {
      if (joint < WITHIN_MIN) {
        next
      }
      within_pairs <- bind_rows(
        within_pairs,
        tibble(from = a, to = b, joint = joint)
      )
    } else {
      if (joint < JOINT_MIN) {
        next
      }
      jacv <- jac[i, j]
      if (jacv < JACC_MIN) {
        next
      }
      cross_pairs <- bind_rows(
        cross_pairs,
        tibble(from = a, to = b, joint = joint, jaccard = jacv)
      )
    }
  }
}
cat(
  "Arcs to draw — cross-parent:",
  nrow(cross_pairs),
  "  within-parent:",
  nrow(within_pairs),
  "\n"
)

# Merge for arc drawing; keep just the fields shared by both.
all_edges <- bind_rows(
  cross_pairs %>% select(from, to, joint),
  within_pairs %>% select(from, to, joint)
)

# Endpoints for each sub-theme: the INNER edge (facing the middle gap).
# For single-child parents the sub-theme lives in the parent rectangle, so we
# use the parent's inner-edge midpoint instead.
endpoint_lookup <- bind_rows(
  sub_layout_multi %>% select(subtheme, col, y_mid, x_inner),
  # Single-child sub-themes: attach to the parent rectangle's inner edge midpoint
  subtheme_meta %>%
    filter(parent_id %in% single_child_ids) %>%
    inner_join(
      parent_layout %>% select(parent_id, col, p_y_mid = y_mid),
      by = "parent_id"
    ) %>%
    mutate(
      x_inner = ifelse(col == 1, X_P1_RIGHT, X_P2_LEFT),
      y_mid = p_y_mid
    ) %>%
    select(subtheme, col, y_mid, x_inner)
)

arc_data <- all_edges %>%
  left_join(
    endpoint_lookup %>%
      rename(from = subtheme, col_a = col, y_a = y_mid, x_a = x_inner),
    by = "from"
  ) %>%
  left_join(
    endpoint_lookup %>%
      rename(to = subtheme, col_b = col, y_b = y_mid, x_b = x_inner),
    by = "to"
  ) %>%
  left_join(
    subtheme_meta %>% select(from = subtheme, parent_a = parent_id),
    by = "from"
  ) %>%
  left_join(
    subtheme_meta %>% select(to = subtheme, parent_b = parent_id),
    by = "to"
  ) %>%
  mutate(
    is_within = (parent_a == parent_b),
    kind = case_when(
      col_a == 1 & col_b == 1 ~ "same_c1",
      col_a == 2 & col_b == 2 ~ "same_c2",
      TRUE ~ "cross"
    ),
    chord_x = (x_a + x_b) / 2,
    chord_y = (y_a + y_b) / 2,
    badge = paste0("n=", joint)
  )

# Explicit apex per arc so the bezier passes EXACTLY through the badge point.
#
# Same-column arcs bulge into the middle gap. Within-parent same-column arcs
# use a SMALL offset so they stay close to their parent's sub-theme column and
# don't overlap the cross-parent badges that live in the middle of the gap.
# Cross-column arcs stagger on apex_y (perpendicular to the mostly-horizontal
# chord) so their badges spread vertically without shifting the bezier control
# point off-chord (which was producing the "ear" cusp near endpoints).
WITHIN_APEX_OFFSET <- 3 # very small bulge for within-parent same-col arcs
# so they hug the parent column edge
XPARENT_APEX_OFFSET <- 16 # larger bulge for cross-parent same-col arcs
CROSS_STAGGER_STEP <- 5 # vertical apex offset between adjacent cross arcs

arc_data <- arc_data %>%
  group_by(kind) %>%
  arrange(chord_y, .by_group = TRUE) %>%
  mutate(within_group_idx = row_number(), n_in_group = n()) %>%
  ungroup() %>%
  mutate(
    apex_x = case_when(
      kind == "same_c1" & is_within ~ chord_x + WITHIN_APEX_OFFSET,
      kind == "same_c1" & !is_within ~ chord_x + XPARENT_APEX_OFFSET,
      kind == "same_c2" & is_within ~ chord_x - WITHIN_APEX_OFFSET,
      kind == "same_c2" & !is_within ~ chord_x - XPARENT_APEX_OFFSET,
      TRUE ~ chord_x
    ),
    apex_y = case_when(
      kind == "cross" ~ chord_y +
        (within_group_idx - (n_in_group + 1) / 2) *
          CROSS_STAGGER_STEP,
      TRUE ~ chord_y
    )
  )

# ── Bezier arc generator ────────────────────────────────────────────────────
# Quadratic Bezier P(t) = (1-t)^2 A + 2(1-t)t Q + t^2 B, midpoint at t=0.5
# equals 0.25*A + 0.5*Q + 0.25*B. Setting that midpoint to the chosen apex
# gives Q = 2*apex - 0.5*(A + B), which makes P(0.5) = apex exactly.
gen_bezier <- function(x_a, y_a, x_b, y_b, apex_x, apex_y, npts = 60) {
  Q_x <- 2 * apex_x - 0.5 * (x_a + x_b)
  Q_y <- 2 * apex_y - 0.5 * (y_a + y_b)
  t <- seq(0, 1, length.out = npts)
  x <- (1 - t)^2 * x_a + 2 * (1 - t) * t * Q_x + t^2 * x_b
  y <- (1 - t)^2 * y_a + 2 * (1 - t) * t * Q_y + t^2 * y_b
  data.frame(x = x, y = y)
}

if (nrow(arc_data) > 0) {
  arc_paths <- do.call(
    rbind,
    lapply(seq_len(nrow(arc_data)), function(i) {
      r <- arc_data[i, ]
      pts <- gen_bezier(r$x_a, r$y_a, r$x_b, r$y_b, r$apex_x, r$apex_y)
      pts$arc_id <- i
      pts
    })
  )
} else {
  arc_paths <- data.frame(x = numeric(0), y = numeric(0), arc_id = integer(0))
}

# ── Labels ────────────────────────────────────────────────────────────────────
SIZE_PARENT <- 7.0
SIZE_SUB <- 4.8

parent_layout <- parent_layout %>%
  mutate(
    lbl = paste0(
      parent_label,
      "\n(n=",
      n,
      " | ",
      sprintf("%.1f%%", 100 * n / N_total),
      ")"
    )
  )
sub_layout_multi <- sub_layout_multi %>%
  mutate(lbl = paste0(label, " (n=", n, ")"))

# ── Plot ──────────────────────────────────────────────────────────────────────
ARC_COLOR <- "#D62728"
BADGE_COLOR <- "#7A1F1F"

# Tight limits: figure edge sits at the block edges (tiny 0.2-unit guard so
# rectangle border strokes don't clip).
xlim <- c(-0.2, X_P2_RIGHT + 0.2)
ylim <- c(min(parent_layout$y_bot) - 0.2, max(parent_layout$y_top) + 0.2)

p <- ggplot() +
  # Parent rectangles
  geom_rect(
    data = parent_layout,
    aes(
      xmin = x_left,
      xmax = x_right,
      ymin = y_bot,
      ymax = y_top,
      fill = color
    ),
    alpha = 0.85,
    color = "white",
    linewidth = 0.6
  ) +
  # Sub-theme rectangles (multi-child parents only)
  geom_rect(
    data = sub_layout_multi,
    aes(
      xmin = x_left,
      xmax = x_right,
      ymin = y_bot,
      ymax = y_top,
      fill = color
    ),
    alpha = 0.85,
    color = "white",
    linewidth = 0.5
  ) +
  # Bracket connectors
  geom_segment(
    data = seg_theme_to_bracket,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black",
    linewidth = 0.7
  ) +
  {
    if (nrow(bracket_verticals) > 0) {
      geom_segment(
        data = bracket_verticals,
        aes(x = x, y = y, xend = xend, yend = yend),
        color = "black",
        linewidth = 0.7
      )
    }
  } +
  geom_segment(
    data = seg_bracket_to_sub,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black",
    linewidth = 0.7
  ) +
  # Cross-mention arcs — bezier curves that pass exactly through their apex
  {
    if (nrow(arc_paths) > 0) {
      geom_path(
        data = arc_paths,
        aes(x = x, y = y, group = arc_id),
        color = ARC_COLOR,
        linewidth = 0.7,
        linetype = "22",
        alpha = 1
      )
    }
  } +
  # Badges sit exactly at each arc's apex point
  {
    if (nrow(arc_data) > 0) {
      geom_label(
        data = arc_data,
        aes(x = apex_x, y = apex_y, label = badge),
        family = "Roboto Condensed",
        fontface = "bold",
        size = 3.7,
        color = BADGE_COLOR,
        fill = "white",
        label.padding = unit(0.14, "lines"),
        label.size = 0.3
      )
    }
  } +
  # Parent labels
  # For very short parent blocks the two-line label is taller than the block;
  # anchor those labels at the block's bottom edge so they grow upward into
  # the inter-theme gap instead of clipping at the figure edge.
  geom_shadowtext(
    data = parent_layout %>%
      mutate(
        lbl_y = ifelse(n < 8, y_bot + 0.3, y_mid),
        lbl_vjust = ifelse(n < 8, 0, 0.5)
      ),
    aes(x = x_mid, y = lbl_y, label = lbl, vjust = lbl_vjust),
    family = "Roboto Condensed",
    fontface = "bold",
    color = "white",
    bg.colour = "grey15",
    bg.r = 0.16,
    size = SIZE_PARENT
  ) +
  # Sub-theme labels
  geom_text(
    data = sub_layout_multi,
    aes(x = x_mid, y = y_mid, label = lbl),
    family = "Roboto Condensed",
    fontface = "plain",
    color = "black",
    bg.r = 0.12,
    size = SIZE_SUB
  ) +
  scale_fill_identity() +
  coord_cartesian(xlim = xlim, ylim = ylim, clip = "off", expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

# Landscape-letter aspect (11:8.5). Rendered at a larger canvas so the
# point-sized fonts keep the same proportion to the layout as the earlier
# renders; LaTeX scales the file to the page exactly because the aspect
# ratio already matches a full landscape page.
save_w <- 21
save_h <- 12.5
cat(sprintf(
  "Save size: %.1f x %.2f in (11:8.5 landscape aspect)\n",
  save_w,
  save_h
))

out_dir <- here(
  "code",
  "output",
  "images",
  "battery_analysis",
  "thematic_analysis"
)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
png_path <- file.path(out_dir, "thematic_hierarchical_nobev_all_blocks.png")
svg_path <- file.path(out_dir, "thematic_hierarchical_nobev_all_blocks.svg")

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
