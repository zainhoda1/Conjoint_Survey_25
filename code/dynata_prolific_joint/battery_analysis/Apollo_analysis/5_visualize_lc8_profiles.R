# 5_visualize_lc8_profiles.R
# Eight-class LC model: two-panel profile figure
# Panel A: Attribute-Sensitive   (Class 2, 3, 4, 5)
# Panel B: Attribute-Insensitive (Class 1, 6, 7, 8)

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
COL_POS    <- "#2A9D8F"
COL_NEG    <- "#E07B54"
COL_INSENS <- "#546E7A"
COL_SENS   <- "#1565C0"

# ── ATTRIBUTE METADATA ────────────────────────────────────────────────────────
attr_df <- tibble(
  attr = c(
    "optout", "mileage",
    "range_pw1", "range_pw2", "range_pw3",
    "loss_pw1",  "loss_pw2",  "loss_pw3",
    "pack", "cell"
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
  c1 = c(-54.2, -18.3, -2.9,  15.3,  14.3, -7.1, -2.1,  0.3, -42.5, -38.0),
  c2 = c(-11.4,  -2.3, 26.8,   5.6,  10.5, -1.2, -1.0, -0.7,  -2.3,  -2.3),
  c3 = c(-16.3,  -2.1, 37.8,  20.9,  25.2, -0.8, -0.7, -0.5,  -0.3,  -0.6),
  c4 = c(-34.7,  -0.6,  2.6,   1.9,   1.9, -0.2, -0.2, -0.2,  -0.1,   0.0),
  c5 = c(-106.0, -3.1,  4.9,   6.1,   5.2, -2.5, -2.6, -2.4,  -2.5,  -3.8),
  c6 = c(-246.4,-11.1,  9.7, -18.0,  11.4,  0.0, -0.8, -1.5,   1.6,  -5.5),
  c7 = c(-69.1,  -5.4, 28.6,  18.5,  12.8,  0.0, -2.6, -0.8, -77.4, -71.9),
  c8 = c(-14.6,  -0.1,  0.5,   5.9,   8.6, -0.1, -0.1,  0.0,  -1.2,  -1.5)
)

sig_raw <- tibble(
  attr = attr_df$attr,
  c1 = c("",    "",    "",    "",    "",    "",    "",    "",    "",    ""),
  c2 = c(".",   "***", "***", "",    "***", "***", "***", ".",   "",    ""),
  c3 = c("**",  "***", "***", "***", "***", "***", "***", "***", "",    ""),
  c4 = c("***", "***", "**",  "*",   "",    "**",  "***", "***", "",    ""),
  c5 = c("***", "***", "",    "*",   "*",   "***", "***", "***", "",    "*"),
  c6 = c("*",   "*",   "",    "",    "",    "",    "",    ".",   "",    ""),
  c7 = c("",    "",    "",    "",    "",    "",    "",    "",    "",    ""),
  c8 = c("**",  "",    "",    "*",   "",    "",    "",    "",    "",    ".")
)

# ── CLASS METADATA ────────────────────────────────────────────────────────────
class_meta <- tibble(
  id    = c("c1","c2","c3","c4","c5","c6","c7","c8"),
  num   = 1:8,
  n     = c(230, 441, 466, 373, 596, 470, 169, 170),
  share = c(8, 15, 16, 13, 20, 16, 6, 6),
  group = c(
    "insensitive",
    "sensitive",
    "sensitive",
    "sensitive",
    "sensitive",
    "insensitive",
    "insensitive",
    "insensitive"
  ),
  label = c(
    "BEV-Skeptical, Extreme Opt-Out",
    "Range-Aware, Moderate Opt-Out",
    "High-Range Enthusiasts",
    "Degradation-Sensitive, Budget-Constrained",
    "Loss-Averse, Opt-Out Averse",
    "Extreme Opt-Out, Scale-Dominant",
    "Refurb-Averse Niche, EV-Function Skeptics",
    "High-Opt-Out, Budget-Constrained"
  ),
  hdr_color = c(
    COL_INSENS, COL_SENS, COL_SENS, COL_SENS,
    COL_SENS,   COL_INSENS, COL_INSENS, COL_INSENS
  )
)

class_chars <- list(
  c1 = paste0(
    "Oldest avg. age 47 · Range anxiety 88%\n",
    "Most anti-EV battery (27.2% enviro positive, 8.4% functional)\n",
    "Lowest outlet 33% · ICEV-only ~90% · SUV 67%\n",
    "Lowest BEV ownership 1.5% · Budget $24.2k\n",
    "Extreme opt-out: 93-96% per task (mean 95.1%)\n",
    "71.5% strongly disagree used BEV · Conservative 34%"
  ),
  c2 = paste0(
    "Range WTP: $26.8k pw1***, $10.5k pw3***\n",
    "Degradation: −$1.2k pw1***, −$1.0k pw2***, −$0.7k pw3.\n",
    "Moderate opt-out: 27-42% per task (mean 36%)\n",
    "Range anxiety 83% · Outlet 45% · Income $94.8k\n",
    "EVB positive 54% · BEV ownership 7.6%\n",
    "EV knowledge 77% · Democratic 47%"
  ),
  c3 = paste0(
    "Highest range WTP: $37.8k / $20.9k / $25.2k per 100mi (all ***)\n",
    "Strongest degradation aversion all ***\n",
    "Highest income $97.8k · Highest budget $26.9k\n",
    "Most EV positive: 65.8% enviro, 35.3% functional\n",
    "Outlet 58% · Lowest SUV 43% · Youngest avg. 37.6\n",
    "Democratic 50% · Lowest opt-out rate 2.2%"
  ),
  c4 = paste0(
    "Significant degradation: **, ***, *** · Small WTP magnitudes\n",
    "Lowest income $70.7k · Lowest budget $19.9k\n",
    "High price sensitivity → economically small WTPs\n",
    "ICEV-dominant · BEV ownership 3.4%\n",
    "Highest apartment 27% · Lowest homeowners 43%\n",
    "Opt-out rate 1.9% mean · Range anxiety 78%"
  ),
  c5 = paste0(
    "Largest class 20% · Extreme opt-out aversion −$106.0k***\n",
    "Strongest degradation: −$2.5k/−$2.6k/−$2.4k per % (all ***)\n",
    "Cell replace −$3.8k* · Range WTP $4.9k-$6.1k\n",
    "Highest risk-taking 45% · Income $90.5k · Outlet 45%\n",
    "BEV ownership 7.8% · SUV 60% · EV positive 63%\n",
    "Lowest opt-out rate (mean 1.7%)"
  ),
  c6 = paste0(
    "Extreme opt-out −$246.4k* · Scale-driven effect\n",
    "Incoherent range WTP (pw2 negative: −$18.0k)\n",
    "Lowest range anxiety 65.6% · Most risk-taking 51%\n",
    "Highest BEV/PHEV: BEV 13.4%, PHEV/HEV 25.4%\n",
    "Highest refuel frequency 5.5/mo · Budget $28.8k\n",
    "Most diverse: white 60%, African American 22%"
  ),
  c7 = paste0(
    "Smallest class 6% · Extreme refurb: −$77.4k/−$71.9k (no sig)\n",
    "Most EV-function negative (87.5% do not disagree)\n",
    "Moderate range WTP $18.5k-$28.6k (not significant)\n",
    "Loss pw2 −$2.6k · Opt-out rate 15-21% (mean 18%)\n",
    "SUV 64% · Budget $30.4k · BEV ownership 11%\n",
    "All WTPs statistically insignificant"
  ),
  c8 = paste0(
    "Highest opt-out rate 49-60% per task (mean 55%)\n",
    "Opt-out aversion −$14.6k** but low attribute response\n",
    "Lowest income $68.2k · Lowest budget $18.9k\n",
    "Range anxiety 85% · Outlet 37% · ICEV-heavy 81%\n",
    "BEV ownership 4.7% · Renters 46%\n",
    "46.6% strongly disagree used BEV purchase"
  )
)

# ── SHARED AXIS LIMITS ────────────────────────────────────────────────────────
BAR_CAP <- -120
y_lo    <- BAR_CAP * 1.22
y_hi    <- max(unlist(wtp_raw[-1])) * 1.25

# ── LONG DATA ─────────────────────────────────────────────────────────────────
wtp_long <- wtp_raw %>%
  pivot_longer(-attr, names_to = "id", values_to = "wtp") %>%
  left_join(sig_raw %>% pivot_longer(-attr, names_to = "id", values_to = "sig"),
            by = c("attr","id")) %>%
  left_join(attr_df,    by = "attr") %>%
  left_join(class_meta, by = "id") %>%
  mutate(
    attr_disp  = factor(attr_disp, levels = rev(attr_df$attr_disp)),
    wtp_bar    = pmax(wtp, BAR_CAP),
    truncated  = wtp < BAR_CAP,
    val_label  = ifelse(truncated,
                        paste0(sprintf("%.1f", wtp), sig, " ◄"),
                        paste0(sprintf("%.1f", wtp), sig)),
    fill_col   = ifelse(wtp >= 0, COL_POS, COL_NEG),
    text_pos   = ifelse(wtp >= 0,
                        wtp_bar + abs(wtp_bar) * 0.04,
                        wtp_bar - abs(wtp_bar) * 0.04),
    text_hjust = ifelse(wtp >= 0, 0, 1)
  )

# ── THEME ─────────────────────────────────────────────────────────────────────
theme_lc <- theme_minimal(base_family = "rc", base_size = 11) %+replace%
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
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf,
             fill=info$hdr_color, color=NA) +
    annotate("text", x=0.5, y=0.70,
             label=paste0("Class ", info$num, ": ", lbl),
             family="rc", fontface="bold", size=4.0, color="white", hjust=0.5) +
    annotate("text", x=0.5, y=0.25,
             label=paste0("n = ", info$n, "  •  ", info$share, "%"),
             family="rc", size=3.8, color="white", alpha=0.88, hjust=0.5) +
    scale_x_continuous(limits=c(0,1)) + scale_y_continuous(limits=c(0,1)) +
    theme_void()
}

make_wtp <- function(class_id, show_y = TRUE) {
  d <- wtp_long %>% filter(id == class_id)
  ggplot(d, aes(x=attr_disp, y=wtp_bar)) +
    geom_col(aes(fill=fill_col), width=0.68, show.legend=FALSE) +
    geom_hline(yintercept=0, color="gray40", linewidth=0.5) +
    geom_text(aes(y=text_pos, label=val_label, hjust=text_hjust),
              size=3.6, family="rc", color="#1A202C") +
    scale_fill_identity() +
    scale_y_continuous(limits=c(y_lo, y_hi), expand=c(0,0)) +
    coord_flip(clip="off") +
    theme_lc +
    theme(axis.text.y = if (show_y) element_text(size=10, hjust=1, color="#444444")
                        else element_blank())
}

make_chars <- function(class_id) {
  info <- class_meta %>% filter(id == class_id)
  txt  <- class_chars[[class_id]]
  ggplot() +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf,
             fill="#F5F7FA", color=NA) +
    annotate("text", x=0.5, y=0.88, label="CLASS CHARACTERISTICS",
             family="rc", fontface="bold", size=3.8, color=info$hdr_color, hjust=0.5) +
    annotate("segment", x=0.08, xend=0.92, y=0.78, yend=0.78,
             color=info$hdr_color, alpha=0.35, linewidth=0.4) +
    annotate("text", x=0.5, y=0.40, label=txt,
             family="rc", size=3.2, color="#333333",
             hjust=0.5, vjust=0.5, lineheight=1.3) +
    scale_x_continuous(limits=c(0,1)) + scale_y_continuous(limits=c(0,1)) +
    theme_void() +
    theme(plot.background = element_rect(fill="#F5F7FA", color=info$hdr_color,
                                          linewidth=0.4),
          plot.margin = margin(4,4,4,4))
}

make_col <- function(class_id, show_y = TRUE) {
  (make_header(class_id) / make_wtp(class_id, show_y) / make_chars(class_id)) +
    plot_layout(heights = c(0.5, 3.0, 2.0))
}

# ── ASSEMBLE ──────────────────────────────────────────────────────────────────

# Panel A: Sensitive (C2, C3, C4, C5) — TOP
col_c2 <- make_col("c2", show_y = TRUE)
col_c3 <- make_col("c3", show_y = FALSE)
col_c4 <- make_col("c4", show_y = FALSE)
col_c5 <- make_col("c5", show_y = FALSE)

panel_a <- (col_c2 | col_c3 | col_c4 | col_c5) +
  plot_layout(widths = unit(c(10, 10, 10, 10), "cm")) +
  plot_annotation(
    title = "Panel A  —  Attribute-Sensitive Classes (Class 2, 3, 4, 5  •  64% of sample)",
    theme = theme(
      plot.title = element_text(family="rc", face="bold", size=13,
                                color=COL_SENS, hjust=0, margin=margin(b=3)),
      plot.background = element_rect(fill="white", color="#BBDEFB", linewidth=0.7),
      plot.margin = margin(5,5,5,5)
    )
  )

# Panel B: Insensitive (C1, C6, C7, C8) — BOTTOM
col_c1 <- make_col("c1", show_y = TRUE)
col_c6 <- make_col("c6", show_y = FALSE)
col_c7 <- make_col("c7", show_y = FALSE)
col_c8 <- make_col("c8", show_y = FALSE)

panel_b <- (col_c1 | col_c6 | col_c7 | col_c8) +
  plot_layout(widths = unit(c(10, 10, 10, 10), "cm")) +
  plot_annotation(
    title = "Panel B  —  Attribute-Insensitive Classes (Class 1, 6, 7, 8  •  36% of sample)",
    theme = theme(
      plot.title = element_text(family="rc", face="bold", size=13,
                                color=COL_INSENS, hjust=0, margin=margin(b=3)),
      plot.background = element_rect(fill="white", color="#CFD8DC", linewidth=0.7),
      plot.margin = margin(5,5,5,5)
    )
  )

# Final figure
final_fig <- (panel_a / panel_b) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title   = "Eight-Class Latent Class Model: Consumer Profiles for Used BEV Battery Attributes",
    caption = paste0(
      "WTP in $1,000 USD. Piecewise linear range and range loss. ",
      "Bar lengths capped at −$120k for display; ◄ indicates truncation with actual value shown. ",
      "Significance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1. ",
      "N = 2,916. Cars & SUVs combined."
    ),
    theme = theme(
      plot.title   = element_text(family="rc", face="bold", size=17,
                                  hjust=0.5, margin=margin(b=6)),
      plot.caption = element_text(family="rc", size=9.5, color="gray50", hjust=0),
      plot.background = element_rect(fill="white", color=NA),
      plot.margin  = margin(8,8,6,8)
    )
  )

# ── SAVE ──────────────────────────────────────────────────────────────────────
output_path <- here(
  "code","output","model_output","battery_analysis","apollo",
  "0_lc8_class_profiles.pdf"
)

ggsave(output_path, plot=final_fig, width=24, height=18, device="pdf")
cat("Saved:", output_path, "\n")
