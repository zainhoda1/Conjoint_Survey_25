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

db <- sd_db_connect(gssencmode = "disable")

# db <- sd_database(
#   host   = "",
#   dbname = "",
#   port   = "",
#   user   = "",
#   table  = "",
#   ignore = TRUE
# )

# Define vector of car image options

car_images <- sample(c("1_new_car_sedan_compact", "2_new_car_hatchback_subcompact", "3_new_car_sedan_subcompact",
                  "4_new_car_sedan_fullsize", "5_new_car_sedan_fullsize_luxury", "6_new_car_hatchback_compact",
                  "7_new_car_hatchback_compact_luxury","8_new_car_sedan_midsize","9_new_car_hatchback_subcompact_luxury"))

suv_images <- sample(c("1_new_crossover_compact", "2_new_crossover_midsize", "3_new_suv_fullsize",
                "4_new_crossover_midsize", "5_new_suv_fullsize", "6_new_crossover_subcompact",
                "7_new_crossover_subcompact","8_new_suv_fullsize","9_new_crossover_compact"))


survey <- read_csv(here('data', 'choice_questions.csv'))
battery_survey <- read_csv(here('data', 'battery_choice_questions.csv'))
respondentID <- sample(survey$respID, 1)
battery_respondentID <- sample(battery_survey$respID, 1)


# Server setup
server <- function(input, output, session) {

  # Make a 10-digit random number completion code
  completion_code <- sd_completion_code(10)

  # Store the completion code in the survey data
  sd_store_value(completion_code)

  sd_store_value(respondentID,"respID")
  sd_store_value(battery_respondentID,"respID")

  # Create random assignment when session starts
  prime_groups <- c('prime_short', 'prime_long')
  prime_group_label <- sample(c('prime_short', 'prime_long'), 1)
  names(prime_groups) <- prime_group_label

  sd_store_value(prime_group_label, id = "prime_group_label")

  ## data
  df <- survey %>%
    filter(respID == respondentID) |>
    mutate(
    `range`=case_when(
      str_detect(`range`, "\\(") ~ str_replace(`range`, "\\s*\\(", "<br>("),
      TRUE ~ paste0(`range`, "<br>")
    ),
    operating_cost=case_when(
      str_detect(operating_cost, "\\(") ~ str_replace(operating_cost, "\\s*\\(", "<br>("),
      TRUE ~ paste0(operating_cost, "<br>")
    )
     )

  battery_df <- battery_survey |>
    filter(respID == battery_respondentID)|>
    # Paste on the "images/" path (images are stored in the "images" folder)
    mutate(
      # battery_refurbish= case_when(battery_refurbish=="Original" ~"Original",
      #                                   battery_refurbish=="Reconditioned" ~"Some battery cells replaced",
      #                                   battery_refurbish=="Replaced" ~"Entire battery pack replaced"
      #                                   ),
      image1 = paste0("images/", image_refurbishment),
      image2 = paste0("images/battery_choices_version2/", image_degradation)
      )

  #Choosing Car/SUV type
  car_chosen <- car_images
  suv_chosen <- suv_images

  car_src <- glue::glue(
    '<img src="images/car-images/{car_chosen}.png" class="responsive-image" style="max-width: 30%; height: auto; object-fit: contain;">'
  )
  suv_src <- glue::glue(
    '<img src="images/car-images/{suv_chosen}.png" class="responsive-image" style="max-width: 30%; height: auto; object-fit: contain;">'
  )
  names(car_chosen) <- car_src
  names(suv_chosen) <- suv_src


  # Define first choice question
  sd_question(
    type  = 'mc_buttons',
    id    = 'next_veh_car_images',
    label = "**Select a vehicle based on it’s segment / style that is most closely aligned with the next vehicle you are considering purchasing.**",
    option = car_chosen
  )

  sd_question(
    type  = 'mc_buttons',
    id    = 'next_veh_suv_images',
    label = "**Select a vehicle based on it’s segment / style that is most closely aligned with the next vehicle you are considering purchasing.**",
    option = suv_chosen
  )


  #  Function to add icons (gas pump, electric plug, or both) before text values in a vehicle’s powertrain description
  create_image_cell <- function(value) {
    # This checks if the word "Electric" appears in the value (case-insensitive)
    if (grepl("Battery Electric", value)) {
      return(sprintf(
        '<img src="images/electric_plug.png" style="width: 20px; height: 20px; vertical-align: middle; margin-right: 5px;">%s',
        value
      ))
    }
    # This checks if either "Gas" or "Gasoline" appears in the value
    else if (grepl("Gasoline", value)) {
      return(sprintf(
        '<img src="images/gas_pump.png" style="width: 20px; height: 20px; vertical-align: middle; margin-right: 5px;">%s',
        value
      ))
    }
    # This checks if either "Hybrid" appears in the value
    else if (grepl("Plug-in Hybrid", value)) {
      return(sprintf(
        '<img src="images/gas_pump.png" style="width: 20px; height: 20px; vertical-align: middle; margin-right: 5px;">
             <img src="images/electric_plug.png" style="width: 20px; height: 20px; vertical-align: middle; margin-right: 5px;">%s', value
      ))
    }
    else{
      return(sprintf(
        '<img src="images/gas_pump.png" style="width: 20px; height: 20px; vertical-align: middle; margin-right: 5px;">%s',
        value
      ))

    }
  }

  df_filtered_vehicle_type <- reactive({
    req(input$next_veh_style)  # ensures input is available

    df %>% {
      if (input$next_veh_style == "Car / sedan / hatchback") {
        filter(., vehicle_type == "car")
      } else {
        filter(., vehicle_type == "suv")
      }
    }
  }
    )

  # Vehicle DCE -- Button Format
  observe(
    {
      df<-df_filtered_vehicle_type()
      # Run observer that updates the chosen_image when an image is chosen

      # This updates whenever input$images changes
      chosen_input <- if (!is.null(input$next_veh_car_images)) {
        input$next_veh_car_images
      } else {
        input$next_veh_suv_images
      }

      chosen_src <- paste0(
        'images/car-images/', chosen_input, '.png'
      )

      # Update the rendered image - displays the vehicle the user previously selected
      output$chosen_image <- renderImage({
        list(
          src = chosen_src,
          contentType = "image/png",
          width = "80px"
        )
      }, deleteFile = FALSE)

      create_car_table_short <- function( chosen_src) {


        html_table <- sprintf('

 <link rel="stylesheet" href="css/testing_table1.css">

 <body>
  <div class="car-comparison">
    <div class="header-section">
    <div class="header-question">
      <b>Assume you are able to purchase a vehicle that looks like the one you selected (see photo). Which of the following versions of that vehicle would you be most likely to purchase?</b> <i>Please compare the key features shown for each option before making your selection.</i>
      </div>
        <img src="%s" alt="Selected vehicle" style="max-width: 300px; height: auto;"><br><br>
      </div>

      </body>

      ', chosen_src)
        function() { html_table }
      }

      output$make_table_short_0 <-create_car_table_short(chosen_src)
      output$make_table_short_1 <-create_car_table_short(chosen_src)
      output$make_table_short_2 <-create_car_table_short(chosen_src)
      output$make_table_short_3 <-create_car_table_short(chosen_src)
      output$make_table_short_4 <-create_car_table_short(chosen_src)
      output$make_table_short_5 <-create_car_table_short(chosen_src)
      output$make_table_short_6 <-create_car_table_short(chosen_src)

      ### Create button table

      domain <- shiny::getDefaultReactiveDomain()
      req(domain$input$next_veh_budget)
      budget <- as.numeric(domain$input$next_veh_budget)
      # df<-df %>%
      #   {if (domain$input$next_veh_style == "Car / sedan / hatchback"){
      #     filter(vehicle_type=="car")
      #   } else {
      #     filter(vehicle_type=="suv")
      #   }
      #   }

      ##### vehicle_cbc0_options

      vehicle_cbc0_options <- c("option_1", "option_2", "option_3")

      alt1 <- df |> filter(altID == 1) |> slice(1)
      alt2 <- df |> filter(altID == 2) |> slice(1)
      alt3 <- df |> filter(altID == 3) |> slice(1)

      alt1$price <- 1.1 * budget
      alt2$price <- 1.0 * budget
      alt3$price <- 0.5 * budget

      alt1$powertrain<-create_image_cell("Battery Electric")
      alt2$powertrain<-create_image_cell("Battery Electric")
      alt3$powertrain<-create_image_cell("Battery Electric")

      names(vehicle_cbc0_options) <- c(
        HTML(glue("
    <div style='text-align: left;'>
      <b>Option 1</b><br>
      <b><span style='font-size: 13px;'><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt1$powertrain}</span><br>
      <b><span style='font-size: 13px;'><u>Range:</u></span></b><br> <span style='font-size: 13px;'>100 miles on full charge</span><br>
      <b><span style='font-size: 13px;'><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt1$price)}</span><br>
      <b><span style='font-size: 13px;'><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>2018</span><br>
      <b><span style='font-size: 13px;'><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>30,000</span><br>
      <b><span style='font-size: 13px;'><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>7 cents per mile<br>(114.8 MPG equivalent)</span><br>
    </div>
  ")),
        HTML(glue("
    <div style='text-align: left;'>
      <b>Option 2</b><br>
      <b><span style='font-size: 13px;'><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt1$powertrain}</span><br>
      <b><span style='font-size: 13px;'><u>Range:</u></span></b><br> <span style='font-size: 13px;'>200 miles on full charge</span><br>
      <b><span style='font-size: 13px;'><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt2$price)}</span><br>
      <b><span style='font-size: 13px;'><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>2018</span><br>
      <b><span style='font-size: 13px;'><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>30,000</span><br>
      <b><span style='font-size: 13px;'><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>7 cents per mile<br>(114.8 MPG equivalent)</span><br>
    </div>
  ")),
        HTML(glue("
    <div style='text-align: left;'>
      <b>Option 3</b><br>
      <b><span style='font-size: 13px;'><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt1$powertrain}</span><br>
      <b><span style='font-size: 13px;'><u>Range:</u></span></b><br> <span style='font-size: 13px;'>300 miles on full charge</span><br>
      <b><span style='font-size: 13px;'><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt3$price)}</span><br>
      <b><span style='font-size: 13px;'><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>2018</span><br>
      <b><span style='font-size: 13px;'><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>30,000</span><br>
      <b><span style='font-size: 13px;'><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>7 cents per mile<br>(114.8 MPG equivalent)</span><br>
    </div>
  "))
      )

      ##### vehicle_cbc1_options -- vehicle_cbc6_options

      vehicle_cbc_options <- function(df, budget_select) {

        # df<-df %>%
        #   {if (domain$input$next_veh_style == "Car / sedan / hatchback"){
        #     filter(vehicle_type=="car")
        #   } else {
        #     filter(vehicle_type=="suv")
        #   }
        #   }

        alt1 <- df |> filter(altID == 1)
        alt2 <- df |> filter(altID == 2)
        alt3 <- df |> filter(altID == 3)

        alt1$price <- alt1$price * budget_select
        alt2$price <- alt2$price * budget_select
        alt3$price <- alt3$price * budget_select

        alt1$powertrain<-create_image_cell(alt1$powertrain)
        alt2$powertrain<-create_image_cell(alt2$powertrain)
        alt3$powertrain<-create_image_cell(alt3$powertrain)

        options <- c("option_1", "option_2", "option_3")


        names(options) <- c(
          HTML(glue("
      <div style='text-align: left;'>
        <b>Option 1</b><br>
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt1$powertrain}</span><br>
        <b><span style='font-size: 13px;' title='The maximum distance a vehicle can travel in a full tank/ fully charged battery.' ><u>Range:</u></span></b><br> <span style='font-size: 13px;'>{alt1$range}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt1$price)}</span><br>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt1$make_year}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt1$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt1$operating_cost}</span><br>
      </div>
    ")),
          HTML(glue("
      <div style='text-align: left;'>
        <b>Option 2</b><br>
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt2$powertrain}</span><br>
        <b><span style='font-size: 13px;' title='The maximum distance a vehicle can travel in a full tank/ fully charged battery.' ><u>Range:</u></span></b><br> <span style='font-size: 13px;'>{alt2$range}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt2$price)}</span><br>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt2$make_year}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt2$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt2$operating_cost}</span><br>
      </div>
    ")),
          HTML(glue("
      <div style='text-align: left;'>
        <b>Option 3</b><br>
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt3$powertrain}</span><br>
        <b><span style='font-size: 13px;' title='The maximum distance a vehicle can travel in a full tank/ fully charged battery.' ><u>Range:</u></span></b><br> <span style='font-size: 13px;'>{alt3$range}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt3$price)}</span><br>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt3$make_year}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt3$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt3$operating_cost}</span><br>
      </div>
    "))
        )
        return(options)
      }

      # Create the options for each choice question
      vehicle_cbc1_options <- vehicle_cbc_options(df |> filter(qID == 1), budget)
      vehicle_cbc2_options <- vehicle_cbc_options(df |> filter(qID == 2), budget)
      vehicle_cbc3_options <- vehicle_cbc_options(df |> filter(qID == 3), budget)
      vehicle_cbc4_options <- vehicle_cbc_options(df |> filter(qID == 4), budget)
      vehicle_cbc5_options <- vehicle_cbc_options(df |> filter(qID == 5), budget)
      vehicle_cbc6_options <- vehicle_cbc_options(df |> filter(qID == 6), budget)

      # Create each choice question - display these in your survey using sd_output()
      # Example: sd_output('cbc_q1', type = 'question')

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q0_button',
        label  = "If these were your only options, which would you choose?",
        option = vehicle_cbc0_options,
        width  = "70%",
        direction = "horizontal"
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q1_button',
        label  = "If these were your only options, which would you choose?",
        option = vehicle_cbc1_options,
        width  = "70%",
        direction = "horizontal"
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q2_button',
        label  = "If these were your only options, which would you choose?",
        option = vehicle_cbc2_options,
        width  = "70%",
        direction = "horizontal"
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q3_button',
        label  = "If these were your only options, which would you choose?",
        option = vehicle_cbc3_options,
        width  = "70%",
        direction = "horizontal"
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q4_button',
        label  = "If these were your only options, which would you choose?",
        option = vehicle_cbc4_options,
        width  = "70%",
        direction = "horizontal"
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q5_button',
        label  = "If these were your only options, which would you choose?",
        option = vehicle_cbc5_options,
        width  = "70%",
        direction = "horizontal"
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q6_button',
        label  = "If these were your only options, which would you choose?",
        option = vehicle_cbc6_options,
        width  = "70%"
      )

    }
  )

  # Battery DCE -- Button Format
  observe(
    {
    domain <- shiny::getDefaultReactiveDomain()
    req(domain$input$next_veh_budget)
    budget <- as.numeric(domain$input$next_veh_budget)

    ##### battery_cbc0_options

    battery_cbc0_options <- c("option_1", "option_2", "option_3")

    alt1 <- battery_df |> filter(altID == 1) |> slice(1)
    alt2 <- battery_df |> filter(altID == 2) |> slice(1)
    alt3 <- battery_df |> filter(altID == 3) |> slice(1)

    alt1$price <- 1.1 * budget
    alt2$price <- 1.0 * budget
    alt3$price <- 0.5 * budget

    names(battery_cbc0_options) <- c(
      HTML(glue("
      <div style='text-align: left;'>
        <b>Option 1</b><br>
        <b><span style='font-size: 13px;'>Mileage:</span></b><br> <span style='font-size: 13px;'>{scales::comma(30000)}</span><br>
        <b><span style='font-size: 13px;'>Purchase price:</span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt1$price)}</span><br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b><br> <img src='images/battery_survey_battery_original_text.png' style='width: 70px; vertical-align: middle;'><br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b><br>
        <img src='images/battery_choices_version2/Range_Degradation_200_8.png' style='width: 230px; vertical-align: middle;'>
      </div>
    ")),
      HTML(glue("
      <div style='text-align: left;'>
        <b>Option 2</b><br>
        <b><span style='font-size: 13px;'>Mileage:</span></b><br> <span style='font-size: 13px;'>{scales::comma(30000)}</span><br>
        <b><span style='font-size: 13px;'>Purchase price:</span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt2$price)}</span><br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b><br> <img src='images/battery_survey_battery_original_text.png' style='width: 70px; vertical-align: middle;'><br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b><br>
        <img src='images/battery_choices_version2/Range_Degradation_280_4.png' style='width: 230px; vertical-align: middle;'>
      </div>
    ")),
      HTML(glue("
      <div style='text-align: left;'>
        <b>Option 3</b><br>
        <b><span style='font-size: 13px;'>Mileage:</span></b><br> <span style='font-size: 13px;'>{scales::comma(30000)}</span><br>
        <b><span style='font-size: 13px;'>Purchase price:</span></b><br> <span style='font-size: 13px;'>$ {(scales::comma(alt3$price))}</span><br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b><br> <img src='images/battery_survey_battery_original_text.png' style='width: 70px; vertical-align: middle;'><br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b><br>
        <img src= 'images/battery_choices_version2/Range_Degradation_360_1.png' style='width: 230px;  vertical-align: middle;'>
      </div>
    "))
    )

    ##### battery_cbc1_options -- battery_cbc6_options

  battery_cbc_options <- function(df, budget_select) {

    alt1 <- df |> filter(altID == 1)
    alt2 <- df |> filter(altID == 2)
    alt3 <- df |> filter(altID == 3)

    alt1$price <- alt1$veh_price * budget_select
    alt2$price <- alt2$veh_price * budget_select
    alt3$price <- alt3$veh_price * budget_select

    options <- c("option_1", "option_2", "option_3")

    # <span style='font-size: 13px;'>{alt3$battery_refurbish}</span>
    names(options) <- c(
      HTML(glue("
      <div style='text-align: left;'>
        <b>Option 1</b><br>
        <b><span style='font-size: 13px;'>Mileage:</span></b><br> <span style='font-size: 13px;'>{scales::comma(alt1$veh_mileage)}</span><br>
        <b><span style='font-size: 13px;'>Purchase price:</span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt1$price)}</span><br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b><br> <img src='{alt1$image1}' style='width: 70px; vertical-align: left;'><br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b><br>
        <img src='{alt1$image2}' style='width: 230px; vertical-align: middle;'>
      </div>
    ")),
      HTML(glue("
      <div style='text-align: left;'>
        <b>Option 2</b><br>
        <b><span style='font-size: 13px;'>Mileage:</span></b><br> <span style='font-size: 13px;'>{scales::comma(alt2$veh_mileage)}</span><br>
        <b><span style='font-size: 13px;'>Purchase price:</span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt2$price)}</span><br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b><br> <img src='{alt2$image1}' style='width: 70px; vertical-align: left;'><br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b><br>
        <img src='{alt2$image2}' style='width: 230px; vertical-align: middle;'>
      </div>
    ")),
      HTML(glue("
      <div style='text-align: left;'>
        <b>Option 3</b><br>
        <b><span style='font-size: 13px;'>Mileage:</span></b><br> <span style='font-size: 13px;'>{scales::comma(alt3$veh_mileage)}</span><br>
        <b><span style='font-size: 13px;'>Purchase price:</span></b><br> <span style='font-size: 13px;'>$ {(scales::comma(alt3$price))}</span><br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b><br> <img src='{alt3$image1}' style='width: 70px; vertical-align: left;'></span><br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b><br>
        <img src= '{alt3$image2}' style='width: 230px;  vertical-align: middle;'>
      </div>
    "))
    )
    return(options)
  }

  # buget <- as.numeric(input$buget)
  # Create the options for each choice question
  battery_cbc1_options <- battery_cbc_options(battery_df |> filter(qID == 1), budget)
  battery_cbc2_options <- battery_cbc_options(battery_df |> filter(qID == 2), budget)
  battery_cbc3_options <- battery_cbc_options(battery_df |> filter(qID == 3), budget)
  battery_cbc4_options <- battery_cbc_options(battery_df |> filter(qID == 4), budget)
  battery_cbc5_options <- battery_cbc_options(battery_df |> filter(qID == 5), budget)
  battery_cbc6_options <- battery_cbc_options(battery_df |> filter(qID == 6), budget)

  # Create each choice question - display these in your survey using sd_output()
  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q0_button',
    label  = "If these were your only options, which would you choose?",
    option = battery_cbc0_options,
    width  = "70%",
    direction = "horizontal"
  )

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q1_button',
    label  = "If these were your only options, which would you choose?",
    option = battery_cbc1_options,
    width  = "70%",
    direction = "horizontal"
  )

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q2_button',
    label  = "If these were your only options, which would you choose?",
    option = battery_cbc2_options,
    width  = "70%"
  )

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q3_button',
    label  = "If these were your only options, which would you choose?",
    option = battery_cbc3_options,
    width  = "70%"
  )

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q4_button',
    label  = "If these were your only options, which would you choose?",
    option = battery_cbc4_options,
    width  = "70%"
  )

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q5_button',
    label  = "If these were your only options, which would you choose?",
    option = battery_cbc5_options,
    width  = "70%"
  )

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q6_button',
    label  = "If these were your only options, which would you choose?",
    option = battery_cbc6_options,
    width  = "70%"
  )

  }
)



  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_forward(
    input$next_veh_when %in% c("24","not_sure") ~ "screenout",
    input$next_veh_market %in% c("new","both") ~ "screenout",
    input$next_veh_style %in% c("van","truck","other") ~ "screenout",

    input$household_veh_count=="0" ~ "next_veh_info",

    !is.null(input$next_veh_car_images) ~ "cbc_intro",
    input$next_veh_style =="SUV / crossover" & (!is.null(input$next_veh_nobev_design)) ~ "next_veh_style_suv"

  )

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if(

    input$household_veh_count!="0" ~ "household_veh_fuel",
    !(input$household_veh_count=="1" & length(input$household_veh_fuel)==1) ~"primary_veh_fuel",

    input$primary_veh_obtain_how %in% c("bought_dealership","leased_dealership","bought_private") ~ "primary_veh_cost",
    input$primary_veh_obtain_how %in% c("bought_dealership","leased_dealership","bought_private") ~ "primary_veh_payment",

    input$primary_veh_fuel %in% c("gas","hev","phev") ~"primary_veh_mpg",

    input$next_veh_fuel_new_bev %in% c("very_unlikely","somewhat_unlikely") & input$next_veh_fuel_used_bev %in% c("very_unlikely","somewhat_unlikely") ~ "next_veh_info_nobev",


    input$know_electric_vehicle == "yes" ~ "write_electric_name",

    prime_group_label=="prime_short"  ~ "battery_prime_short",
    prime_group_label=="prime_long" ~ "battery_prime_long"

  )




  # Database designation and other settings
  sd_server(
    db = db,
    all_questions_required = TRUE,
    # required_questions = c("images", "budget", "next_vehicle_purchase",
    #                        "which_market", "next_car_payment_source", "know_electric_vehicle",
    #                        "cbc_q1",  "cbc_q2" , "cbc_q3",  "cbc_q4" , "cbc_q5",  "cbc_q6",
    #                        "battery_cbc_q1",  "battery_cbc_q2" , "battery_cbc_q3",  "battery_cbc_q4" , "battery_cbc_q5",  "battery_cbc_q6"),
    use_cookies = FALSE
  )

}


# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
