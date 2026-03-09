# ui.R
library(shiny)

ui <- fillPage(
  # ← fills viewport, no scrollbar
  padding = 8,
  tags$head(tags$style(HTML(
    "
    body { overflow: hidden; }
    #hover_table table { font-size: 11px; margin-top: 4px; }
    #hover_coords { font-size: 11px; color: #555; margin-bottom: 2px; }
  "
  ))),

  # title bar (fixed height)
  tags$div(
    style = "padding-left:4px; flex-shrink:0;",
    tags$div(
      style = "height:36px; line-height:36px; font-size:15px; font-weight:bold;",
      "Willingness to Pay (WTP) Heatmap for Battery Attributes in Used BEVs (hover for crosshair & WTP)"
    ),
    tags$div(
      style = "font-size:13px; color:#555; margin-top:6px;",
      tags$div(
        "1. The black dashed line indicates the threshold where WTP equals zero."
      ),
      tags$div(
        "2. The red solid line represents a battery degradation rate of 3.53%. At this rate, the battery's state of health (SoH) falls below 75% by Year 8, thereby qualifying for warranty replacement at no cost."
      )
    )
  ),

  # main area: plot left, tooltip right
  fillRow(
    flex = c(1, NA), # plot takes all remaining width; tooltip is fixed
    plotOutput(
      "heatmap",
      # height = "95%",
      height = "75vh", # 75% of viewport height
      hover = hoverOpts(
        id = "plot_hover",
        delay = 40,
        delayType = "debounce",
        nullOutside = TRUE,
        clip = TRUE
      )
    ),
    tags$div(
      style = "width:230px; padding-left:10px; padding-top:40px;",
      verbatimTextOutput("hover_coords"),
      tags$b("Mean WTP ($1,000) at cursor:", style = "font-size:11px;"),
      tableOutput("hover_table")
    )
  )
)
