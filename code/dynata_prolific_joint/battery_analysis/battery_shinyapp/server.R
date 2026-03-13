# server.R — full replacement

library(shiny)
library(ggplot2)
library(dplyr)


server <- function(input, output, session) {
  # ── Persist last valid snapped position ─────────────────────────────────
  # reactiveVal holds the last non-NULL snap so label doesn't disappear
  last_snap <- reactiveVal(NULL)

  observe({
    hv <- input$plot_hover
    if (is.null(hv) || is.na(hv$x) || is.na(hv$y)) {
      return()
    }

    x_snap <- degrad_seq[which.min(abs(degrad_seq - hv$x))]
    y_snap <- range_seq[which.min(abs(range_seq - (hv$y / 100)))]

    last_snap(list(
      x = x_snap,
      y = y_snap,
      x_disp = x_snap,
      y_disp = y_snap * 100
    ))
  })

  # ── Cache the base heatmap (no crosshair) — only redraws on resize ──────
  base_plot <- reactive({
    calc_df |>
      ggplot(aes(
        x = battery_degradation,
        y = battery_range_year0 * 100,
        fill = mean_wtp_scaled
      )) +
      geom_raster(interpolate = TRUE, na.rm = TRUE) +
      geom_contour(
        aes(z = mean_wtp_scaled),
        breaks = 0,
        color = "grey30",
        linetype = "dashed",
        linewidth = 0.4,
        na.rm = TRUE
      ) +
      # static red vertical line at x = 3.53
      geom_vline(
        xintercept = 3.53,
        color = "red",
        linetype = "solid",
        linewidth = 0.45,
        inherit.aes = FALSE
      ) +
      facet_grid(refurb ~ segment) +
      scale_fill_gradient2(
        name = "Mean WTP\n($1,000)",
        low = "#0170d8",
        mid = "white",
        high = "#da5f07",
        midpoint = 0,
        na.value = "transparent"
      ) +
      labs(
        x = "Battery degradation rate (% per year)",
        y = "Battery range — year 0 (miles)"
      ) +
      coord_cartesian(xlim = xlim_plot, ylim = ylim_plot, expand = FALSE) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey95", colour = NA),
        legend.position = "bottom"
      )
  })

  # ── Render plot: base + stable crosshair + label ─────────────────────────
  output$heatmap <- renderPlot(
    {
      p <- base_plot()
      s <- last_snap()

      if (!is.null(s)) {
        # crosshair lines (span all facets)
        p <- p +
          geom_vline(
            xintercept = s$x_disp,
            color = "grey",
            linetype = "dashed",
            linewidth = 0.45,
            inherit.aes = FALSE
          ) +
          geom_hline(
            yintercept = s$y_disp,
            color = "grey",
            linetype = "dashed",
            linewidth = 0.45,
            inherit.aes = FALSE
          )

        # per-facet WTP label at intersection
        wtp_at_cursor <- lookup_df |>
          filter(
            battery_degradation == s$x,
            battery_range_year0 == s$y
          ) |>
          mutate(
            label = paste0(
              "x = ",
              round(s$x_disp, 1),
              " %/yr\n",
              "y = ",
              round(s$y_disp, 1),
              " mi\n",
              "WTP = $",
              round(mean_wtp_scaled, 1),
              "k"
            )
          )

        # nudge direction: flip to left/below if near right/top edge
        x_range <- diff(xlim_plot)
        y_range <- diff(ylim_plot)
        h_just <- ifelse(s$x_disp > xlim_plot[1] + 0.7 * x_range, 1.08, -0.08)
        v_just <- ifelse(s$y_disp > ylim_plot[1] + 0.7 * y_range, 1.0, 0.0)

        # nudge_y: push label up by ~8% of y range (or down if near top)
        # base upward shift
        base_nudge <- 0.08 * y_range

        # if too close to bottom, push further up
        nudge_val <- ifelse(
          s$y_disp < ylim_plot[1] + 0.15 * y_range,
          0.18 * y_range,
          base_nudge
        )

        p <- p +
          geom_label(
            data = wtp_at_cursor,
            aes(
              x = s$x_disp,
              y = s$y_disp + nudge_val, # ← nudge in data units
              label = label
            ),
            hjust = h_just,
            vjust = v_just,
            size = 2.8,
            lineheight = 1.1,
            fill = alpha("white", 0.88),
            label.size = 0.25,
            color = "black",
            inherit.aes = FALSE
          )
      }

      p
    },
    res = 96
  )

  # ── Coordinate readout (sidebar) ─────────────────────────────────────────
  output$hover_coords <- renderText({
    s <- last_snap()
    if (is.null(s)) {
      return("Move cursor over plot")
    }
    paste0(
      "Degradation : ",
      round(s$x_disp, 1),
      " %/yr\n",
      "Range (yr 0): ",
      round(s$y_disp, 1),
      " miles"
    )
  })

  # ── Per-facet WTP table (sidebar) ────────────────────────────────────────
  output$hover_table <- renderTable(
    {
      s <- last_snap()
      if (is.null(s)) {
        return(NULL)
      }
      lookup_df |>
        filter(battery_degradation == s$x, battery_range_year0 == s$y) |>
        select(
          Segment = segment,
          Refurbishment = refurb,
          `WTP ($1k)` = mean_wtp_scaled
        ) |>
        mutate(`WTP ($1k)` = round(`WTP ($1k)`, 1)) |>
        arrange(Segment, Refurbishment)
    },
    striped = TRUE,
    bordered = FALSE,
    spacing = "xs",
    digits = 1
  )
}
