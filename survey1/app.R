# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(surveydown)
library(tidyverse)
library(here)
library(glue)
library(dplyr)
library(kableExtra)

# Database setup

# surveydown stores data on a database that you define at https://supabase.com/
# To connect to a database, update the sd_database() function with details
# from your supabase database. For this demo, we set ignore = TRUE, which will
# ignore the settings and won't attempt to connect to the database. This is
# helpful for local testing if you don't want to record testing data in the
# database table. See the documentation for details:
# https://surveydown.org/store-data

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
    label = "Select a vehicle based on it’s segment / style that is most closely aligned with the next vehicle you are considering purchasing",
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
        width = "200px"
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
      oper_cost1 <- glue(" {q1_alt1$operating_cost}  cents per mile test")
      sd_store_value(oper_cost1)

      vehi_type1 <- q1_alt1$powertrain
      sd_store_value(vehi_type1)

      range_type1 <- q1_alt1$range
      sd_store_value(range_type1)

      range_type2 <- q1_alt1$range
      sd_store_value(range_type2)

      acc_type1 <- q1_alt1$accelTime
      sd_store_value(acc_type1)

      resp_id_send <- respondentID
      sd_store_value(resp_id_send)
    }
  )

  observe(
    {

      # Function to create the options table for a given choice question
      make_cbc_table <- function(df) {
        alts <- df %>%
          # mutate (
          #   pp = (as.numeric(input$budget)* price)
          #     ) %>%
          # Make nicer attribute labels
          select(
            `Option:` = altID,
            `Vehicle Type:` = powertrain,
            `Range(in miles)` = range,
            `Purchase Price` = price,
            `Operating Cost` = operating_cost,
            `acc time` =  accelTime,
            `Mileage` = mileage
          )
        row.names(alts) <- NULL # Drop row names

        table <- kbl(t(alts), escape = FALSE) %>%
          kable_styling(
            bootstrap_options = c("striped", "hover", "condensed"),
            full_width = FALSE,
            position = "center"
          )
        function() { table }
      }

      output$cbc1_table <- make_cbc_table(df |> filter(qID == 1))
      output$cbc2_table <- make_cbc_table(df |> filter(qID == 2))
      output$cbc3_table <- make_cbc_table(df |> filter(qID == 3))
      output$cbc4_table <- make_cbc_table(df |> filter(qID == 4))
      output$cbc5_table <- make_cbc_table(df |> filter(qID == 5))
      output$cbc6_table <- make_cbc_table(df |> filter(qID == 6))

    }
  )

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
  # sd_show_if(
  #   input$know_electric_vehicle == "Yes" ~ "write_electric_name"
  # )

  # Database designation and other settings
  sd_server(
    db = db,
    required_questions = c("images", "budget"),
    use_cookies = FALSE
  )

}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
