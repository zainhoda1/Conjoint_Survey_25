# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(surveydown)
library(tidyverse)
library(here)
library(glue)

# Database setup

db <- sd_database(
  host   = "",
  dbname = "",
  port   = "",
  user   = "",
  table  = "",
  ignore = TRUE
)


# Server setup
server <- function(input, output, session) {

  survey <- read_csv(here('data', 'survey.csv'))
  respondentID <- sample(survey$respID, 1)
  df <- survey %>%
    filter(respID == respondentID)

  q1_alts <- df %>% filter(qID == 1)
  q1_alt1 <- q1_alts %>% filter(altID == 1)
  q1_alt2 <- q1_alts %>% filter(altID == 2)
  q1_alt3 <- q1_alts %>% filter(altID == 3)

  cost_com <- q1_alt1$price
  sd_store_value(cost_com)

  observeEvent(input$budget, {
      mult_val <- as.numeric(input$budget) * cost_com
      sd_store_value(mult_val)
  })

  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if()

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if(
    input$know_electric_vehicle == "Yes" ~ "write_electric_name"
  )

  # Database designation and other settings
  sd_server(
    db = db
  )

}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
