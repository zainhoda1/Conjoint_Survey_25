# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(surveydown)
library(tidyverse)
library(here)
library(glue)
library(dplyr)

# Database setup

db <- sd_db_connect()

# db <- sd_database(
#   host   = "",
#   dbname = "",
#   port   = "",
#   user   = "",
#   table  = "",
#   ignore = TRUE
# )


# Server setup
server <- function(input, output, session) {





  # Define vector of car image options

  sedan_images <- c("car_small_5", "car_mid_5", "car_large_5",
                  "car_small_4", "car_mid_4", "car_large_4")

  suv_images <- c("suv_small_4", "suv_mid_4", "suv_large_4",
                  "suv_small_5", "suv_mid_5", "suv_large_5")

  car_images <- c("car_small_5", "car_mid_5", "car_large_5",
                  "car_small_4", "car_mid_4", "car_large_4",
                  "suv_small_4", "suv_mid_4", "suv_large_4",
                  "suv_small_5", "suv_mid_5", "suv_large_5")


  #Choosing Car type

   chosen <- car_images
   #chosen1 <- input$next_vehicle_type

  # observeEvent({
  # chosen <- case_when(
  #   as.integer(input$next_vehicle_type) == 1 ~ 'sedan_images',
  #   as.integer(input$next_vehicle_type) == 2 ~ 'suv_images',
  #   .default = 'car_images'
  # )
  # })

  src <- glue::glue(
    '<img src="images/car-images/{chosen}.jpg" width=100%>'
  )
  names(chosen) <- src

  # Define first choice question
  sd_question(
    type  = 'mc_buttons',
    id    = 'images',
    label = "Which type of car are you planning to buy?",
    option = chosen
  )



  # Run observer that updates the chosen_image when an image is chosen
  observe({

    # This updates whenever input$images changes
    chosen_src <- paste0(
      'images/car-images/', input$images, '.jpg'
    )

    # Update the rendered image
    output$chosen_image <- renderImage({
      list(
        src = chosen_src,
        contentType = "image/jpg",
        width = "100%"
      )
    }, deleteFile = FALSE)

  })


  survey <- read_csv(here('data', 'choice_questions.csv'))
  respondentID <- sample(survey$respID, 1)
  df <- survey %>%
    filter(respID == respondentID)

  q1_alts <- df %>% filter(qID == 1)
  q1_alt1 <- q1_alts %>% filter(altID == 1)
  q1_alt2 <- q1_alts %>% filter(altID == 2)
  q1_alt3 <- q1_alts %>% filter(altID == 3)

  q2_alts <- df %>% filter(qID == 2)
  q2_alt1 <- q2_alts %>% filter(altID == 1)
  q2_alt2 <- q2_alts %>% filter(altID == 2)
  q2_alt3 <- q2_alts %>% filter(altID == 3)

  q3_alts <- df %>% filter(qID == 3)
  q3_alt1 <- q3_alts %>% filter(altID == 1)
  q3_alt2 <- q3_alts %>% filter(altID == 2)
  q3_alt3 <- q3_alts %>% filter(altID == 3)

  q4_alts <- df %>% filter(qID == 4)
  q4_alt1 <- q4_alts %>% filter(altID == 1)
  q4_alt2 <- q4_alts %>% filter(altID == 2)
  q4_alt3 <- q4_alts %>% filter(altID == 3)

  q5_alts <- df %>% filter(qID == 5)
  q5_alt1 <- q5_alts %>% filter(altID == 1)
  q5_alt2 <- q5_alts %>% filter(altID == 2)
  q5_alt3 <- q5_alts %>% filter(altID == 3)

  q6_alts <- df %>% filter(qID == 6)
  q6_alt1 <- q6_alts %>% filter(altID == 1)
  q6_alt2 <- q6_alts %>% filter(altID == 2)
  q6_alt3 <- q6_alts %>% filter(altID == 3)

  cost_com <- q1_alt2$price
  sd_store_value(cost_com)



  observeEvent(input$budget, {
    q1c1 <- as.numeric(input$budget) * q1_alt1$price
    sd_store_value(q1c1)
    q1c2 <- as.numeric(input$budget) * q1_alt2$price
    sd_store_value(q1c2)
    q1c3 <- as.numeric(input$budget) * q1_alt3$price
    sd_store_value(q1c3)

    q2c1 <- as.numeric(input$budget) * q2_alt1$price
    sd_store_value(q2c1)
    q2c2 <- as.numeric(input$budget) * q2_alt2$price
    sd_store_value(q2c2)
    q2c3 <- as.numeric(input$budget) * q2_alt3$price
    sd_store_value(q2c3)

  })

  observe(
    {
      q3c1 <- as.numeric(input$budget) * q3_alt1$price
      sd_store_value(q3c1)
      q3c2 <- as.numeric(input$budget) * q3_alt2$price
      sd_store_value(q3c2)
      q3c3 <- as.numeric(input$budget) * q3_alt3$price
      sd_store_value(q3c3)
    }
  )

  observe(
    {
      q4c1 <- as.numeric(input$budget) * q4_alt1$price
      sd_store_value(q4c1)
      q4c2 <- as.numeric(input$budget) * q4_alt2$price
      sd_store_value(q4c2)
      q4c3 <- as.numeric(input$budget) * q4_alt3$price
      sd_store_value(q4c3)

    }
  )

  observe(
    {
      q5c1 <- as.numeric(input$budget) * q5_alt1$price
      sd_store_value(q5c1)
      q5c2 <- as.numeric(input$budget) * q5_alt2$price
      sd_store_value(q5c2)
      q5c3 <- as.numeric(input$budget) * q5_alt3$price
      sd_store_value(q5c3)

    }
  )

  observe(
    {
      q6c1 <- as.numeric(input$budget) * q6_alt1$price
      sd_store_value(q6c1)
      q6c2 <- as.numeric(input$budget) * q6_alt2$price
      sd_store_value(q6c2)
      q6c3 <- as.numeric(input$budget) * q6_alt3$price
      sd_store_value(q6c3)

    }
  )



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
