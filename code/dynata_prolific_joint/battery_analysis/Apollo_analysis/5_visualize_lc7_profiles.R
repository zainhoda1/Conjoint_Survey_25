# 5_visualize_lc7_profiles.R
# Seven-class LC model: two-panel profile figure
# Panel A: Attribute-Sensitive   (Class 2, 3, 4, 5)
# Panel B: Attribute-Insensitive (Class 1, 6, 7)

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
  c1 = c(-17.4, -2.9,  0.6,  12.4,   9.1, -1.6, -0.3,  0.2,  -7.0,  -8.0),
  c2 = c(-18.1, -1.5, 11.2,   8.5,  10.0, -0.8, -0.6, -0.3,  -2.0,  -2.1),
  c3 = c(-14.0, -2.1, 38.3,  20.3,  25.7, -0.9, -0.7, -0.5,  -0.5,  -0.6),
  c4 = c(-35.1, -0.6,  2.6,   1.9,   1.9, -0.2, -0.2, -0.2,  -0.2,   0.0),
  c5 = c(-96.6, -3.0,  4.6,   4.9,   5.5, -2.3, -2.4, -2.2,  -2.3,  -3.4),
  c6 = c(-258.2,-10.8, 8.8, -18.2,  11.0,  0.2, -0.8, -1.6,   0.3,  -7.5),
  c7 = c(-88.6, -6.8, 23.0,  19.6,  13.4, -0.2, -3.0, -1.0, -86.7, -81.5)
)

sig_raw <- tibble(
  attr = attr_df$attr,
  c1 = c(".",   "",    "",    "",    ".",   ".",   "",    "",    ".",   "."),
  c2 = c("***", "***", "*",   "***", "***", "***", "***", "",    "",    ""),
  c3 = c("**",  "***", "***", "***", "***", "***", "***", "***", "",    ""),
  c4 = c("***", "***", "**",  "*",   "",    "**",  "***", "***", "",    ""),
  c5 = c("***", "***", "",    "*",   "**",  "***", "***", "***", "",    "*"),
  c6 = c(".",   ".",   "",    "",    "",    "",    "",    ".",   "",    ""),
  c7 = c("",    "",    "",    "",    "",    "",    "",    "",    "",    "")
)

# ── CLASS METADATA ────────────────────────────────────────────────────────────
class_meta <- tibble(
  id    = c("c1","c2","c3","c4","c5","c6","c7"),
  num   = 1:7,
  n     = c(258, 534, 481, 369, 647, 464, 163),
  share = c(9, 18, 16, 13, 22, 16, 6),
  group = c(
    "insensitive",
    "sensitive",
    "sensitive",
    "sensitive",
    "sensitive",
    "insensitive",
    "insensitive"
  ),
  label = c(
    "BEV-Skeptical Resisters",
    "Range-Aware, Loss-Aware",
    "High-Range Enthusiasts",
    "Degradation-Sensitive, Budget-Constrained",
    "Loss-Averse, Opt-Out Averse",
    "Extreme Opt-Out, Scale-Dominant",
    "Refurbishment-Averse, EVB Function Skeptics"
  ),
  hdr_color = c(
    COL_INSENS, COL_SENS, COL_SENS, COL_SENS,
    COL_SENS,   COL_INSENS, COL_INSENS
  )
)

class_chars <- list(
  c1 = paste0(
    "Oldest avg. age 47 · Highest range anxiety 87%\n",
    "Most EV battery negative (30.8% enviro positive, 9.1% functional)\n",
    "Lowest outlet access 33% · Predominantly ICEV-only 90%\n",
    "Highest SUV preference 68% · Lowest BEV ownership 1.7%\n",
    "Lowest used BEV purchase intent (70% strongly disagree)\n",
    "Most conservative 32% · Most Republican 37%\n",
    "Lowest climate concern 22%"
  ),
  c2 = paste0(
    "Range WTP $8.5k-$11.2k/100mi (*, ***) · Significant loss pw1-pw2\n",
    "High opt-out rate per task (33-47%) · range anxiety 84%\n",
    "Moderate outlet 43% · EV battery positive 55%\n",
    "Avg. income $87.1k · BEV ownership 7.3%\n",
    "EV knowledge 78% · Democratic 45%\n",
    "Moderate SUV 53%"
  ),
  c3 = paste0(
    "Highest range WTP: $38.3k / $20.3k / $25.7k per 100mi (all ***)\n",
    "Significant degradation aversion (all ***/***/***)  \n",
    "Highest income $97.4k · Highest budget $26.9k\n",
    "Most EV positive: 66% enviro, 35% functional\n",
    "Highest outlet 58% · Lowest SUV 42%\n",
    "Best EV knowledge 81% · Most liberal/Democratic 50%\n",
    "Highest BEV intent 36% agree · Youngest avg. 37.5"
  ),
  c4 = paste0(
    "Significant degradation aversion (**, ***, ***) · Small WTP magnitudes\n",
    "Lowest income $70.3k · Lowest budget $19.7k\n",
    "Very high price sensitivity → economically small non-price WTPs\n",
    "ICEV-dominant 87% · Lowest BEV ownership 3.4%\n",
    "Highest apartment 27% · Lowest homeowners 43%\n",
    "Range anxiety 78% · Moderate outlet 40%"
  ),
  c5 = paste0(
    "Largest class 22% · Extreme opt-out aversion −$96.6k***\n",
    "Strongest degradation: −$2.3k/−$2.4k/−$2.2k per % (all ***)\n",
    "Cell replace −$3.4k* · Moderate range WTP ($4.6k-$5.5k)\n",
    "High SUV 61% · Avg. income $91.5k · Outlet 46%\n",
    "BEV ownership 7.5% · EV positive 63%\n",
    "High risk-taking 44% · Democratic 48%"
  ),
  c6 = paste0(
    "Extreme opt-out −$258.2k (barely . sig) · Scale-driven\n",
    "Incoherent range WTP (pw2 negative: −$18.2k)\n",
    "Lowest range anxiety 65.5% · Most risk-taking\n",
    "Highest BEV/PHEV: BEV 13.5%, PHEV/HEV 25.7%\n",
    "Highest refuel frequency 5.5/mo · Budget $28.9k\n",
    "Most diverse: white 60.5%, African American 21.7%\n",
    "Cell replace −$7.5k (no sig) · Likely committed BEV adopters"
  ),
  c7 = paste0(
    "Smallest class 6% · Extreme refurb aversion −$86.7k/−$81.5k (no sig)\n",
    "Most negative about EV battery function (88.1% disagree-no)\n",
    "Moderate range WTP $19.6k-$23.0k (not significant)\n",
    "Strong loss pw2 −$3.0k · High opt-out rate 15-22% per task\n",
    "High SUV 65% · Highest budget $30.4k\n",
    "BEV ownership 10.5% · Income $87.2k · Not employed 36%\n",
    "High uncertainty: all WTPs statistically insignificant"
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
             family="rc", fontface="bold", size=4.2, color="white", hjust=0.5) +
    annotate("text", x=0.5, y=0.25,
             label=paste0("n = ", info$n, "  •  ", info$share, "%"),
             family="rc", size=3.9, color="white", alpha=0.88, hjust=0.5) +
    scale_x_continuous(limits=c(0,1)) + scale_y_continuous(limits=c(0,1)) +
    theme_void()
}

make_wtp <- function(class_id, show_y = TRUE) {
  d <- wtp_long %>% filter(id == class_id)
  ggplot(d, aes(x=attr_disp, y=wtp_bar)) +
    geom_col(aes(fill=fill_col), width=0.68, show.legend=FALSE) +
    geom_hline(yintercept=0, color="gray40", linewidth=0.5) +
    geom_text(aes(y=text_pos, label=val_label, hjust=text_hjust),
              size=3.8, family="rc", color="#1A202C") +
    scale_fill_identity() +
    scale_y_continuous(limits=c(y_lo, y_hi), expand=c(0,0)) +
    coord_flip(clip="off") +
    theme_lc +
    theme(axis.text.y = if (show_y) element_text(size=11, hjust=1, color="#444444")
                        else element_blank())
}

make_chars <- function(class_id) {
  info <- class_meta %>% filter(id == class_id)
  txt  <- class_chars[[class_id]]
  ggplot() +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf,
             fill="#F5F7FA", color=NA) +
    annotate("text", x=0.5, y=0.88, label="CLASS CHARACTERISTICS",
             family="rc", fontface="bold", size=3.9, color=info$hdr_color, hjust=0.5) +
    annotate("segment", x=0.08, xend=0.92, y=0.78, yend=0.78,
             color=info$hdr_color, alpha=0.35, linewidth=0.4) +
    annotate("text", x=0.5, y=0.40, label=txt,
             family="rc", size=3.3, color="#333333",
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
    title = "Panel A  —  Attribute-Sensitive Classes (Class 2, 3, 4, 5  •  69% of sample)",
    theme = theme(
      plot.title = element_text(family="rc", face="bold", size=13,
                                color=COL_SENS, hjust=0, margin=margin(b=3)),
      plot.background = element_rect(fill="white", color="#BBDEFB", linewidth=0.7),
      plot.margin = margin(5,5,5,5)
    )
  )

# Panel B: Insensitive (C1, C6, C7) — BOTTOM
col_c1 <- make_col("c1", show_y = TRUE)
col_c6 <- make_col("c6", show_y = FALSE)
col_c7 <- make_col("c7", show_y = FALSE)

panel_b <- (col_c1 | col_c6 | col_c7) +
  plot_layout(widths = unit(c(10, 10, 10), "cm")) +
  plot_annotation(
    title = "Panel B  —  Attribute-Insensitive Classes (Class 1, 6, 7  •  31% of sample)",
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
    title   = "Seven-Class Latent Class Model: Consumer Profiles for Used BEV Battery Attributes",
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
  "0_lc7_class_profiles.pdf"
)

ggsave(output_path, plot=final_fig, width=22, height=18, device="pdf")
cat("Saved:", output_path, "\n")
