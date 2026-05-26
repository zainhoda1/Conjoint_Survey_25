# 5_visualize_lc6_profiles.R
# Six-class LC model: two-panel profile figure
# Panel A: Attribute-Insensitive (Class 1, 4, 6)
# Panel B: Attribute-Sensitive   (Class 2, 3, 5)

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
COL_POS <- "#2A9D8F" # teal  (positive WTP)
COL_NEG <- "#E07B54" # coral (negative WTP)
COL_INSENS <- "#546E7A" # header bg: insensitive
COL_SENS <- "#1565C0" # header bg: sensitive

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
wtp_raw <- tibble(
  attr = attr_df$attr,
  c1 = c(-10.4, 8.7, 11.6, 11.0, -1.9, -0.3, -0.5, -5.6, -8.7, -4.9),
  c2 = c(-28.0, 11.3, 10.0, 7.9, -1.2, -0.7, -0.5, -7.5, -6.9, -2.3),
  c3 = c(-15.1, 42.7, 22.4, 24.3, -0.7, -0.7, -0.3, -1.0, -1.8, -2.6),
  c4 = c(-30.9, 1.5, 2.0, 1.9, -0.1, -0.2, -0.2, -0.1, 0.4, -0.4),
  c5 = c(-105.2, 1.0, 3.9, 5.4, -2.5, -2.5, -1.9, -5.5, -4.6, -3.3),
  c6 = c(-201.6, 2.6, -10.3, 12.3, -0.7, -1.2, -0.1, -11.8, -13.3, -5.3)
)

sig_raw <- tibble(
  attr = attr_df$attr,
  c1 = c("", "", "", ".", ".", "", "", "", ".", "."),
  c2 = c("***", "*", "***", "***", "***", "***", ".", "***", "***", "***"),
  c3 = c("*", "***", "***", "***", "**", "***", ".", "", "", "***"),
  c4 = c("***", ".", "", "", "", ".", ".", "", "", ""),
  c5 = c("***", "", "", "**", "***", "***", "***", "**", "*", "***"),
  c6 = c("*", "", "", "", "", "", "", "", ".", "*")
)

# ── CLASS METADATA ────────────────────────────────────────────────────────────
class_meta <- tibble(
  id = c("c1", "c2", "c3", "c4", "c5", "c6"),
  num = 1:6,
  n = c(202, 330, 227, 173, 297, 300),
  share = c(13, 22, 15, 11, 19, 20),
  group = c(
    "insensitive",
    "sensitive",
    "sensitive",
    "insensitive",
    "sensitive",
    "insensitive"
  ),
  label = c(
    "BEV Resisters, Opt-out Inclined",
    "Range-Aware,Refurbishment-Averse",
    "High Range Enthusiasts",
    "Budget-Constrained,Attributes Indifferent",
    "Loss-Averse,Battery-Sensitive",
    "Opt-Out Averse,Uncertain/Possible Noise"
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
  c1 = "Oldest avg.50  \u00b7 Highest range anxiety 89% \nMost negative towards EV batteries \nLowest electrical outlet access 33% \n High share of ICEV owners \nHigh share of SUV 59%  \u00b7Least EV subsidy knowledge  \nLowest used BEV purchase intent \u00b7  Lowest employment rate",
  c2 = "High share of current BEV owners 7.2% \nSomewhat concerned about the functionality of EV batteries \nRelatively low electrical outlet access 39%  \nHigh Range anxiety 81%\n Good EV knowledge",
  c3 = "Highest income $99.0k  \u00b7 Highest share of current BEV owners 8.9% \nHighest primary vehicle budget \nMost positive towards EV batteries \u00b7 High range anxiety 84% \nHighest electrical outlet access 56%\n High EV exposure \u00b7 Most EV knowledge 82% \nHighest used BEV purchase intent \u00b7 Lowest share of SUV 44% \nHighest eudcation attainment  \u00b7  Highest employment rate \nMost liberal and democratic",
  c4 = "Lowest income $64.2k, lowest vehicle budget $18.7k\nLeast EV knowledge \u00b7 Lowest vehicle ownership (dominately ICEVs) \nLowest share of housing owners  \u00b7 Highest share of apartment tenants",
  c5 = "High share of current BEV owners 8.1% \nSomewhat concerned about the functionality of EV batteries  \nHighest share of SUV 62%\nOutlet access 44%  \u00b7  Range anxiety 83%\nGood EV knowledge 77%",
  c6 = "Highest share of current BEV owners 10.4% \nHighest share of BEV primary users SUV 54%\nMost risk-taking \u00b7 Least range anxiety \u00b7 Highest EV exposure \nSome are predisposed to choose BEV,\nbut attribute trade-offs are insensitive  \nPossibly with some noisy responses without clear pattern"
)

# ── SHARED AXIS LIMITS (same scale across all classes) ────────────────────────
BAR_CAP <- -120 # bar display cap (negative side)
y_lo <- BAR_CAP * 1.22 # 22% padding beyond cap
y_hi <- max(unlist(wtp_raw[-1])) * 1.25 # 25% padding beyond global max

# ── LONG DATA ─────────────────────────────────────────────────────────────────
wtp_long <- wtp_raw %>%
  pivot_longer(-attr, names_to = "id", values_to = "wtp") %>%
  left_join(
    sig_raw %>% pivot_longer(-attr, names_to = "id", values_to = "sig"),
    by = c("attr", "id")
  ) %>%
  left_join(attr_df, by = "attr") %>%
  left_join(class_meta, by = "id") %>%
  mutate(
    attr_disp = factor(attr_disp, levels = rev(attr_df$attr_disp)),
    wtp_bar = pmax(wtp, BAR_CAP), # capped for bar length
    truncated = wtp < BAR_CAP,
    val_label = ifelse(
      truncated,
      paste0(sprintf("%.1f", wtp), sig, " \u25c4"), # ◄ marks truncated bars
      paste0(sprintf("%.1f", wtp), sig)
    ),
    fill_col = ifelse(wtp >= 0, COL_POS, COL_NEG),
    text_pos = ifelse(
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
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(1, 4, 1, 2)
  )

# ── PLOT FUNCTIONS ────────────────────────────────────────────────────────────

make_header <- function(class_id) {
  info <- class_meta %>% filter(id == class_id)
  lbl <- gsub("\n", ", ", info$label)

  ggplot() +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = info$hdr_color,
      color = NA
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.70,
      label = paste0("Class ", info$num, ": ", lbl),
      family = "rc",
      fontface = "bold",
      size = 4.6,
      color = "white",
      hjust = 0.5
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.25,
      label = paste0("n = ", info$n, "  \u2022  ", info$share, "%"),
      family = "rc",
      size = 4.2,
      color = "white",
      alpha = 0.88,
      hjust = 0.5
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
      size = 4.2,
      family = "rc",
      color = "#1A202C"
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
  txt <- class_chars[[class_id]]

  ggplot() +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = "#F5F7FA",
      color = NA
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.88,
      label = "CLASS CHARACTERISTICS",
      family = "rc",
      fontface = "bold",
      size = 4.2,
      color = info$hdr_color,
      hjust = 0.5
    ) +
    annotate(
      "segment",
      x = 0.08,
      xend = 0.92,
      y = 0.78,
      yend = 0.78,
      color = info$hdr_color,
      alpha = 0.35,
      linewidth = 0.4
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.40,
      label = txt,
      family = "rc",
      size = 3.6,
      color = "#333333",
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 1.35
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(
        fill = "#F5F7FA",
        color = info$hdr_color,
        linewidth = 0.4
      ),
      plot.margin = margin(4, 4, 4, 4)
    )
}

make_col <- function(class_id, show_y = TRUE) {
  hdr <- make_header(class_id)
  wtp <- make_wtp(class_id, show_y = show_y)
  chars <- make_chars(class_id)
  (hdr / wtp / chars) + plot_layout(heights = c(0.5, 3.0, 2.0))
}

# ── ASSEMBLE ──────────────────────────────────────────────────────────────────

# Panel A: Insensitive (C1, C4, C6)
col_c1 <- make_col("c1", show_y = TRUE)
col_c4 <- make_col("c4", show_y = FALSE)
col_c6 <- make_col("c6", show_y = FALSE)

panel_a <- (col_c1 | col_c4 | col_c6) +
  plot_layout(widths = unit(c(10, 10, 10), "cm")) +
  plot_annotation(
    title = "Panel A  \u2014  Attribute-Insensitive Classes (Class 1, 4, 6  \u2022  44% of sample)",
    theme = theme(
      plot.title = element_text(
        family = "rc",
        face = "bold",
        size = 13,
        color = COL_INSENS,
        hjust = 0,
        margin = margin(b = 3)
      ),
      plot.background = element_rect(
        fill = "white",
        color = "#CFD8DC",
        linewidth = 0.7
      ),
      plot.margin = margin(5, 5, 5, 5)
    )
  )

# Panel B: Sensitive (C2, C3, C5)
col_c2 <- make_col("c2", show_y = TRUE)
col_c3 <- make_col("c3", show_y = FALSE)
col_c5 <- make_col("c5", show_y = FALSE)

panel_b <- (col_c2 | col_c3 | col_c5) +
  plot_layout(widths = unit(c(10, 10, 10), "cm")) +
  plot_annotation(
    title = "Panel B  \u2014  Attribute-Sensitive Classes (Class 2, 3, 5  \u2022  56% of sample)",
    theme = theme(
      plot.title = element_text(
        family = "rc",
        face = "bold",
        size = 13,
        color = COL_SENS,
        hjust = 0,
        margin = margin(b = 3)
      ),
      plot.background = element_rect(
        fill = "white",
        color = "#BBDEFB",
        linewidth = 0.7
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
      "Bar lengths capped at \u2212$120k for display; \u25c4 indicates truncation with actual value shown. ",
      "Significance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1. ",
      "N = 1,529. Cars & SUVs combined."
    ),
    theme = theme(
      plot.title = element_text(
        family = "rc",
        face = "bold",
        size = 18,
        hjust = 0.5,
        margin = margin(b = 6)
      ),
      plot.caption = element_text(
        family = "rc",
        size = 10,
        color = "gray50",
        hjust = 0
      ),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(8, 8, 6, 8)
    )
  )

# ── SAVE ──────────────────────────────────────────────────────────────────────
output_path <- here(
  "code",
  "output",
  "model_output",
  "battery_analysis",
  "apollo",
  "0_lc6_class_profiles.pdf"
)

ggsave(
  output_path,
  plot = final_fig,
  width = 18,
  height = 16,
  device = "pdf"
)

cat("Saved:", output_path, "\n")
