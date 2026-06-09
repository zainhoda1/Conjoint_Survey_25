# 5_visualize_lc6_profiles.R
# Six-class LC model: two-panel profile figure
# Panel A: Attribute-Sensitive   (Class 2, 3, 5)
# Panel B: Limited-Sensitivity   (Class 1, 4, 6)

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
    "Range Loss Rate (yr.3 - yr.8): 5-12% (per %)",
    "Range Loss Rate (yr.3 - yr.8): 12-24% (per %)",
    "Range Loss Rate (yr.3 - yr.8): 24+% (per %)",
    "Battery Refurbishment: Pack Replace",
    "Battery Refurbishment: Cell Replace"
  )
)

# ── WTP VALUES ($000) ─────────────────────────────────────────────────────────
# WTP = (beta_attr / -beta_price) from unconditional class-level parameters
# Order: optout, mileage, range_pw1, range_pw2, range_pw3,
#        loss_pw1, loss_pw2, loss_pw3, pack, cell
wtp_raw <- tibble(
  attr = attr_df$attr,
  c1 = c(-17.6, -3.2, 0.8, 12.8, 9.2, -1.6, -0.3, 0.2, -6.4, -7.3),
  c2 = c(-95.8, -2.9, 4.6, 4.6, 5.3, -2.3, -2.3, -2.1, -2.7, -3.8),
  c3 = c(-13.3, -2.1, 38.7, 20.8, 25.5, -0.9, -0.7, -0.5, -0.5, -0.7),
  c4 = c(-242.4, -10.0, 12.3, -10.1, 13.8, 0.3, -1.3, -1.4, -26.6, -32.6),
  c5 = c(-20.8, -1.8, 11.8, 9.5, 9.5, -0.8, -0.8, -0.4, -5.8, -6.1),
  c6 = c(-34.5, -0.6, 2.8, 1.8, 2.0, -0.2, -0.2, -0.2, 0.1, 0.3)
)

sig_raw <- tibble(
  attr = attr_df$attr,
  c1 = c(".", ".", "", "", "*", "*", "", "", "*", "*"),
  c2 = c("***", "***", ".", ".", "**", "***", "***", "***", "*", "**"),
  c3 = c("**", "***", "***", "***", "***", "***", "***", "***", "", ""),
  c4 = c("**", "**", "", "", ".", "", ".", "*", "**", "**"),
  c5 = c("***", "***", "***", "***", "***", "***", "***", "*", "***", "***"),
  c6 = c("***", "***", "**", "*", "", "**", "***", "***", "", "")
)

# ── CLASS METADATA ────────────────────────────────────────────────────────────
class_meta <- tibble(
  id = c("c1", "c2", "c3", "c4", "c5", "c6"),
  num = 1:6,
  n = c(252, 661, 485, 539, 606, 373),
  share = c(9, 23, 17, 18, 21, 13),
  group = c(
    "insensitive",
    "sensitive",
    "sensitive",
    "insensitive",
    "sensitive",
    "insensitive"
  ),
  label = c(
    "Opt-Out Dominant, BEV-Skeptical",
    "High-range Enthusiast, Loss-Averse",
    "Range-Focused, EV-ready",
    "Price-Insensitive, Refurbishment-Averse",
    "Opt-Out Prone, Range & Loss Aware",
    "Budget-Constrained, Low WTP"
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
    "3/4 opts out in all 6 DCEs\n",
    "Oldest avg. age · Least risk-taking\n",
    "Highest range anxiety\n",
    "Most negative attitude toward EV batteries\n",
    "Lowest climate concern\n",
    "Dominately ICEV-only · Lowest BEV ownership\n",
    "Lowest electric outlet access\n",
    "Highest SUV preference"
  ),
  c2 = paste0(
    "Largest class\n",
    "Constant, or slightly increasing marginal returns to range.\n",
    "High concern about EV battery functionality\n",
    "High and almost consistent loss aversion, regardless of the rates\n",
    "High SUV preference\n"
  ),
  c3 = paste0(
    "Highest household income\n",
    "Highest primary vehicle range\n",
    "High share of current BEV owners\n",
    "Most positive towards EV batteries\n",
    "Highest electrical outlet access\n",
    "High EV exposure · Most EV knowledge\n",
    "High used BEV purchase intention\n",
    "Lowest SUV preference"
  ),
  c4 = paste0(
    "Strong opt-out aversion\n",
    "Smallest price coefficient, WTPs are ratio-inflated\n",
    "Highest share of information treatment · Refurbishment-averse\n",
    "High concern about functionality of EV batteries\n",
    "Lowest ICEV-only · Highest BEV households\n",
    "Highest vehicle budget · Most risk-taking\n",
    "Potentially inattentive respondents\n",
    "High tendency to agree with statements regardless of content"
  ),
  c5 = paste0(
    "Only 5.2% never opt out\n",
    "High concern about EV battery functionality\n",
    "Diminishing marginal returns to range, and then constant\n",
    "Median-level of household income and vehicle buget\n",
    "High concern about functionality of EV batteries\n",
    "High Range anxiety\n",
    "High Refurbishment-averse"
  ),
  c6 = paste0(
    "Most price-sensitive, small WTPs\n",
    "Lowest household income\n",
    "Accept low-range BEVs\n",
    "Range loss-averse\n",
    "Lowest vehicle ownership (predominately ICEVs)\n",
    "Lowest share of housing owners\n",
    "Highest share of apartment renters"
  )
)

# ── SHARED AXIS LIMITS (same scale across all classes) ────────────────────────
BAR_CAP <- -120 # bar display cap (negative side)
y_lo <- BAR_CAP * 1.22
y_hi <- max(unlist(wtp_raw[-1])) * 1.25

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
    wtp_bar = pmax(wtp, BAR_CAP),
    truncated = wtp < BAR_CAP,
    val_label = ifelse(
      truncated,
      paste0(sprintf("%.1f", wtp), sig, " ◄"),
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
theme_lc <- theme_minimal(base_family = "rc", base_size = 7) %+replace%
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray91", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0.5, 2, 0.5, 1)
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
      size = 2.8,
      color = "white",
      hjust = 0.5
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.25,
      label = paste0("n = ", info$n, "  •  ", info$share, "%"),
      family = "rc",
      size = 2.5,
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
      size = 2.5,
      family = "rc",
      color = "#1A202C"
    ) +
    scale_fill_identity() +
    scale_y_continuous(limits = c(y_lo, y_hi), expand = c(0, 0)) +
    coord_flip(clip = "off") +
    theme_lc +
    theme(
      axis.text.y = if (show_y) {
        element_text(size = 7, hjust = 1, color = "#444444")
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
      size = 2.5,
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
      size = 2.1,
      color = "#333333",
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 1.2
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
      plot.margin = margin(2, 2, 2, 2)
    )
}

make_col <- function(class_id, show_y = TRUE) {
  hdr <- make_header(class_id)
  wtp <- make_wtp(class_id, show_y = show_y)
  chars <- make_chars(class_id)
  (hdr / wtp / chars) + plot_layout(heights = c(0.5, 3.0, 2.0))
}

# ── ASSEMBLE ──────────────────────────────────────────────────────────────────

# Panel A: Sensitive (C2, C3, C5) — TOP
col_c2 <- make_col("c2", show_y = TRUE)
col_c3 <- make_col("c3", show_y = FALSE)
col_c5 <- make_col("c5", show_y = FALSE)

panel_a <- (col_c2 | col_c3 | col_c5) +
  plot_layout(widths = unit(c(7.5, 7.5, 7.5), "cm")) +
  plot_annotation(
    title = "Panel A  —  Attribute-Sensitive Classes (Class 2, 3, 5  •  61% of sample)",
    theme = theme(
      plot.title = element_text(
        family = "rc",
        face = "bold",
        size = 8,
        color = COL_SENS,
        hjust = 0,
        margin = margin(b = 2)
      ),
      plot.background = element_rect(
        fill = "white",
        color = "#BBDEFB",
        linewidth = 0.7
      ),
      plot.margin = margin(3, 3, 3, 3)
    )
  )

# Panel B: Limited-Sensitivity (C1, C4, C6) — BOTTOM
col_c1 <- make_col("c1", show_y = TRUE)
col_c4 <- make_col("c4", show_y = FALSE)
col_c6 <- make_col("c6", show_y = FALSE)

panel_b <- (col_c1 | col_c4 | col_c6) +
  plot_layout(widths = unit(c(7.5, 7.5, 7.5), "cm")) +
  plot_annotation(
    title = "Panel B  —  Limited-Sensitivity Classes (Class 1, 4, 6  •  40% of sample)",
    theme = theme(
      plot.title = element_text(
        family = "rc",
        face = "bold",
        size = 8,
        color = COL_INSENS,
        hjust = 0,
        margin = margin(b = 2)
      ),
      plot.background = element_rect(
        fill = "white",
        color = "#CFD8DC",
        linewidth = 0.7
      ),
      plot.margin = margin(3, 3, 3, 3)
    )
  )

# Final figure
final_fig <- (panel_a / panel_b) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Six-Class Latent Class Model: Consumer Profiles for Used BEV Battery Attributes",
    caption = paste0(
      "Bar lengths capped at −$120k for display;"
    ),
    theme = theme(
      plot.title = element_text(
        family = "rc",
        face = "bold",
        size = 11,
        hjust = 0.5,
        margin = margin(b = 4)
      ),
      plot.caption = element_text(
        family = "rc",
        size = 7,
        color = "gray50",
        hjust = 0
      ),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(4, 4, 3, 4)
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
  width = 11,
  height = 8.5,
  units = "in",
  device = "pdf"
)

cat("Saved:", output_path, "\n")
