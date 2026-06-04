# 5_visualize_lc6_profiles.R
# Six-class LC model: two-panel profile figure
# Panel A: Attribute-Sensitive   (Class 2, 3, 5)
# Panel B: Attribute-Insensitive (Class 1, 4, 6)

source(here::here('code', 'setup.R'))
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(showtext)

font_add_google("Roboto Condensed", "rc")
showtext_auto()
showtext_opts(dpi = 300)

# ── COLORS ────────────────────────────────────────────────────────────────────
COL_POS    <- "#2A9D8F"   # teal  (positive WTP)
COL_NEG    <- "#E07B54"   # coral (negative WTP)
COL_INSENS <- "#546E7A"   # header bg: insensitive
COL_SENS   <- "#1565C0"   # header bg: sensitive

# ── ATTRIBUTE METADATA ────────────────────────────────────────────────────────
attr_df <- tibble(
  attr = c(
    "optout",
    "mileage",
    "range_pw1",
    "range_pw2",
    "range_pw3",
    "loss_pw1",
    "loss_pw2",
    "loss_pw3",
    "pack",
    "cell"
  ),
  attr_disp = c(
    "Opt-Out",
    "Mileage (per 10k mi)",
    "Range at yr. 3: 40-130mi (per 100mi)",
    "Range at yr. 3: 130-200mi (per 100mi)",
    "Range at yr. 3: 200+mi (per 100mi)",
    "Range Loss (yr.3 - yr.8): 5-12% (per %)",
    "Range Loss (yr.3 - yr.8): 12-24% (per %)",
    "Range Loss (yr.3 - yr.8): 24+% (per %)",
    "Battery Refurbishment: Pack Replace",
    "Battery Refurbishment: Cell Replace"
  )
)

# ── WTP VALUES ($000) ─────────────────────────────────────────────────────────
# Order: optout, mileage, range_pw1, range_pw2, range_pw3,
#        loss_pw1, loss_pw2, loss_pw3, pack, cell
wtp_raw <- tibble(
  attr = attr_df$attr,
  c1 = c(-17.6, -3.2,  0.8,  12.8,   9.2, -1.6, -0.3,  0.2,  -6.4,  -7.3),
  c2 = c(-20.8, -1.8, 11.8,   9.5,   9.5, -0.8, -0.8, -0.4,  -5.8,  -6.1),
  c3 = c(-13.3, -2.1, 38.7,  20.8,  25.5, -0.9, -0.7, -0.5,  -0.5,  -0.7),
  c4 = c(-34.5, -0.6,  2.8,   1.8,   2.0, -0.2, -0.2, -0.2,   0.1,   0.3),
  c5 = c(-95.8, -2.9,  4.6,   4.6,   5.3, -2.3, -2.3, -2.1,  -2.7,  -3.8),
  c6 = c(-242.4,-10.0, 12.3, -10.1,  13.8,  0.3, -1.3, -1.4, -26.6, -32.6)
)

sig_raw <- tibble(
  attr = attr_df$attr,
  c1 = c(".",   ".",   "",    "",    "*",   "*",   "",    "",    "*",   "*"),
  c2 = c("***", "***", "***", "***", "***", "***", "***", "*",   "***", "***"),
  c3 = c("**",  "***", "***", "***", "***", "***", "***", "***", "",    ""),
  c4 = c("***", "***", "**",  "*",   "",    "**",  "***", "***", "",    ""),
  c5 = c("***", "***", ".",   ".",   "**",  "***", "***", "***", "*",   "**"),
  c6 = c("**",  "**",  "",    "",    ".",   "",    ".",   "*",   "**",  "**")
)

# ── CLASS METADATA ────────────────────────────────────────────────────────────
class_meta <- tibble(
  id    = c("c1", "c2", "c3", "c4", "c5", "c6"),
  num   = 1:6,
  n     = c(252, 606, 485, 373, 661, 539),
  share = c(9, 21, 17, 13, 23, 18),
  group = c(
    "insensitive",
    "sensitive",
    "sensitive",
    "insensitive",
    "sensitive",
    "insensitive"
  ),
  label = c(
    "BEV-Skeptical Resisters",
    "Range-Aware, Refurbishment-Averse",
    "High-Range Enthusiasts",
    "Budget-Constrained, Price-Dominant",
    "Loss-Averse, Battery-Sensitive",
    "Opt-Out Extreme, Uncertain"
  ),
  hdr_color = c(
    COL_INSENS,
    COL_SENS,
    COL_SENS,
    COL_INSENS,
    COL_SENS,
    COL_INSENS
  )
)

class_chars <- list(
  c1 = paste0(
    "Oldest avg. age 47 · Highest range anxiety 87%\n",
    "Most EV battery negative (30.5% enviro positive, 9.1% functional)\n",
    "Lowest outlet access 33% · Predominantly ICEV-only 89%\n",
    "Highest SUV preference 68% · Lowest BEV ownership 1.7%\n",
    "Lowest used BEV purchase intent (70% strongly disagree)\n",
    "Most conservative 33% · Most Republican 37%\n",
    "Lowest employment 38% · Lowest climate concern 22%"
  ),
  c2 = paste0(
    "Strong range WTP ($9.5k–$11.8k per 100mi), all ***\n",
    "Refurb-averse: pack −$5.8k***, cell −$6.1k***\n",
    "Moderate EV battery positivity 52% · Range anxiety 83%\n",
    "Moderate outlet access 42% · Avg. income $86.9k\n",
    "BEV ownership 7.5% · EV knowledge 77%\n",
    "Moderate loss sensitivity · Democratic 45%"
  ),
  c3 = paste0(
    "Highest range WTP: $38.7k / $20.8k / $25.5k per 100mi (all ***)\n",
    "Significant degradation aversion (all ***)\n",
    "Highest income $97.1k · Highest budget $26.9k\n",
    "Most EV positive: 66% enviro, 36% functional\n",
    "Highest outlet access 58% · Lowest SUV preference 43%\n",
    "Best EV knowledge 81% · Most liberal/Democratic 50%\n",
    "Highest used BEV purchase intent 36% agree · Youngest avg. 37.5"
  ),
  c4 = paste0(
    "Lowest income $70.0k · Lowest vehicle budget $19.7k\n",
    "Very high price sensitivity → small non-price WTPs\n",
    "Predominantly ICEV-only 87% · Lowest BEV ownership 3.4%\n",
    "Moderate outlet 40% · Range anxiety 78%\n",
    "Highest apartment rate 28% · Lowest homeowners 43%\n",
    "Moderate degradation sensitivity ($0.2k/%), all ***\n",
    "Lowest vehicle budget and household vehicle count"
  ),
  c5 = paste0(
    "Largest class 23% · Extreme opt-out aversion −$95.8k***\n",
    "Strongest degradation sensitivity: −$2.3k/% (all ***)\n",
    "Refurb-averse: pack −$2.7k*, cell −$3.8k**\n",
    "Moderate range WTP ($4.6k–$5.3k per 100mi)\n",
    "Highest risk-taking 44% · High EV positivity 63%\n",
    "High SUV preference 61% · Avg. income $91.3k\n",
    "Outlet access 46% · BEV ownership 7.5%"
  ),
  c6 = paste0(
    "Extreme opt-out aversion −$242.4k** (scale-driven)\n",
    "Extreme refurb aversion: pack −$26.6k**, cell −$32.6k**\n",
    "Incoherent range WTP signs (−$10.1k to +$13.8k)\n",
    "Lowest range anxiety 67% · Most risk-taking 52%\n",
    "Highest BEV/PHEV ownership: BEV 13.5%, PHEV/HEV 25%\n",
    "Highest budget $29.3k · Highest refuel frequency 5.5/mo\n",
    "Most diverse: white 59%, African American 22%"
  )
)

# ── SHARED AXIS LIMITS (same scale across all classes) ────────────────────────
BAR_CAP <- -120   # bar display cap (negative side)
y_lo    <- BAR_CAP * 1.22
y_hi    <- max(unlist(wtp_raw[-1])) * 1.25

# ── LONG DATA ─────────────────────────────────────────────────────────────────
wtp_long <- wtp_raw %>%
  pivot_longer(-attr, names_to = "id", values_to = "wtp") %>%
  left_join(
    sig_raw %>% pivot_longer(-attr, names_to = "id", values_to = "sig"),
    by = c("attr", "id")
  ) %>%
  left_join(attr_df,    by = "attr") %>%
  left_join(class_meta, by = "id") %>%
  mutate(
    attr_disp = factor(attr_disp, levels = rev(attr_df$attr_disp)),
    wtp_bar   = pmax(wtp, BAR_CAP),
    truncated = wtp < BAR_CAP,
    val_label = ifelse(
      truncated,
      paste0(sprintf("%.1f", wtp), sig, " ◄"),
      paste0(sprintf("%.1f", wtp), sig)
    ),
    fill_col  = ifelse(wtp >= 0, COL_POS, COL_NEG),
    text_pos  = ifelse(
      wtp >= 0,
      wtp_bar + abs(wtp_bar) * 0.04,
      wtp_bar - abs(wtp_bar) * 0.04
    ),
    text_hjust = ifelse(wtp >= 0, 0, 1)
  )

# ── THEME ─────────────────────────────────────────────────────────────────────
theme_lc <- theme_minimal(base_family = "rc", base_size = 12) %+replace%
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray91", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    axis.title         = element_blank(),
    axis.ticks         = element_blank(),
    axis.text.x        = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.margin        = margin(1, 4, 1, 2)
  )

# ── PLOT FUNCTIONS ────────────────────────────────────────────────────────────

make_header <- function(class_id) {
  info <- class_meta %>% filter(id == class_id)
  lbl  <- gsub("\n", ", ", info$label)

  ggplot() +
    annotate("rect",
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
      fill = info$hdr_color, color = NA
    ) +
    annotate("text",
      x = 0.5, y = 0.70,
      label    = paste0("Class ", info$num, ": ", lbl),
      family   = "rc", fontface = "bold",
      size     = 4.6, color = "white", hjust = 0.5
    ) +
    annotate("text",
      x = 0.5, y = 0.25,
      label  = paste0("n = ", info$n, "  •  ", info$share, "%"),
      family = "rc", size = 4.2, color = "white", alpha = 0.88, hjust = 0.5
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_void()
}

make_wtp <- function(class_id, show_y = TRUE) {
  d <- wtp_long %>% filter(id == class_id)

  ggplot(d, aes(x = attr_disp, y = wtp_bar)) +
    geom_col(aes(fill = fill_col), width = 0.68, show.legend = FALSE) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5) +
    geom_text(
      aes(y = text_pos, label = val_label, hjust = text_hjust),
      size = 4.2, family = "rc", color = "#1A202C"
    ) +
    scale_fill_identity() +
    scale_y_continuous(limits = c(y_lo, y_hi), expand = c(0, 0)) +
    coord_flip(clip = "off") +
    theme_lc +
    theme(
      axis.text.y = if (show_y) {
        element_text(size = 12, hjust = 1, color = "#444444")
      } else {
        element_blank()
      }
    )
}

make_chars <- function(class_id) {
  info <- class_meta %>% filter(id == class_id)
  txt  <- class_chars[[class_id]]

  ggplot() +
    annotate("rect",
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
      fill = "#F5F7FA", color = NA
    ) +
    annotate("text",
      x = 0.5, y = 0.88,
      label    = "CLASS CHARACTERISTICS",
      family   = "rc", fontface = "bold",
      size     = 4.2, color = info$hdr_color, hjust = 0.5
    ) +
    annotate("segment",
      x = 0.08, xend = 0.92, y = 0.78, yend = 0.78,
      color = info$hdr_color, alpha = 0.35, linewidth = 0.4
    ) +
    annotate("text",
      x = 0.5, y = 0.40,
      label      = txt,
      family     = "rc", size = 3.6,
      color      = "#333333", hjust = 0.5, vjust = 0.5, lineheight = 1.35
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(
        fill      = "#F5F7FA",
        color     = info$hdr_color,
        linewidth = 0.4
      ),
      plot.margin = margin(4, 4, 4, 4)
    )
}

make_col <- function(class_id, show_y = TRUE) {
  hdr   <- make_header(class_id)
  wtp   <- make_wtp(class_id, show_y = show_y)
  chars <- make_chars(class_id)
  (hdr / wtp / chars) + plot_layout(heights = c(0.5, 3.0, 2.0))
}

# ── ASSEMBLE ──────────────────────────────────────────────────────────────────

# Panel A: Sensitive (C2, C3, C5) — TOP
col_c2 <- make_col("c2", show_y = TRUE)
col_c3 <- make_col("c3", show_y = FALSE)
col_c5 <- make_col("c5", show_y = FALSE)

panel_a <- (col_c2 | col_c3 | col_c5) +
  plot_layout(widths = unit(c(10, 10, 10), "cm")) +
  plot_annotation(
    title = "Panel A  —  Attribute-Sensitive Classes (Class 2, 3, 5  •  61% of sample)",
    theme = theme(
      plot.title = element_text(
        family = "rc", face = "bold", size = 13,
        color = COL_SENS, hjust = 0, margin = margin(b = 3)
      ),
      plot.background = element_rect(
        fill = "white", color = "#BBDEFB", linewidth = 0.7
      ),
      plot.margin = margin(5, 5, 5, 5)
    )
  )

# Panel B: Insensitive (C1, C4, C6) — BOTTOM
col_c1 <- make_col("c1", show_y = TRUE)
col_c4 <- make_col("c4", show_y = FALSE)
col_c6 <- make_col("c6", show_y = FALSE)

panel_b <- (col_c1 | col_c4 | col_c6) +
  plot_layout(widths = unit(c(10, 10, 10), "cm")) +
  plot_annotation(
    title = "Panel B  —  Attribute-Insensitive Classes (Class 1, 4, 6  •  40% of sample)",
    theme = theme(
      plot.title = element_text(
        family = "rc", face = "bold", size = 13,
        color = COL_INSENS, hjust = 0, margin = margin(b = 3)
      ),
      plot.background = element_rect(
        fill = "white", color = "#CFD8DC", linewidth = 0.7
      ),
      plot.margin = margin(5, 5, 5, 5)
    )
  )

# Final figure
final_fig <- (panel_a / panel_b) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Six-Class Latent Class Model: Consumer Profiles for Used BEV Battery Attributes",
    caption = paste0(
      "WTP in $1,000 USD. Piecewise linear range and range loss. ",
      "Bar lengths capped at −$120k for display; ◄ indicates truncation with actual value shown. ",
      "Significance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1. ",
      "N = 2,916. Cars & SUVs combined."
    ),
    theme = theme(
      plot.title = element_text(
        family = "rc", face = "bold", size = 18,
        hjust = 0.5, margin = margin(b = 6)
      ),
      plot.caption = element_text(
        family = "rc", size = 10, color = "gray50", hjust = 0
      ),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(8, 8, 6, 8)
    )
  )

# ── SAVE ──────────────────────────────────────────────────────────────────────
output_path <- here(
  "code", "output", "model_output", "battery_analysis", "apollo",
  "0_lc6_class_profiles.pdf"
)

ggsave(
  output_path,
  plot   = final_fig,
  width  = 18,
  height = 16,
  device = "pdf"
)

cat("Saved:", output_path, "\n")
