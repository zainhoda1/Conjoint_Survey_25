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

db <- sd_db_connect()

# db <- sd_database(
#   host   = "",
#   dbname = "",
#   port   = "",
#   user   = "",
#   table  = "",
#   ignore = TRUE
# )

# Define vector of car image options

sedan_images <- c("car_small_5", "car_mid_5", "car_large_5",
                  "car_small_4", "car_mid_4", "car_large_4")

suv_images <- c("suv_small_4", "suv_mid_4", "suv_large_4",
                "suv_small_5", "suv_mid_5", "suv_large_5")

car_images <- c("car_small_5", "car_mid_5", "car_large_5",
                "car_small_4", "car_mid_4", "car_large_4",
                "suv_small_4", "suv_mid_4", "suv_large_4",
                "suv_small_5", "suv_mid_5", "suv_large_5")

car_images1 <- c( "car_small_6", "car_small_7",
                  "car_mid_1", "car_mid_2",
                  "car_large_5", "car_large_9",
                  "suv_small_2", "suv_small_3",
                  "suv_mid_3","suv_mid_6",
                  "suv_large_1", "suv_large_8"

)

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
    mutate(battery_refurbish= case_when(battery_refurbish=="Original" ~"Original",
                                        battery_refurbish=="Reconditioned" ~"Some battery cells replaced",
                                        battery_refurbish=="Replaced" ~"Entire battery pack replaced"
                                        ),
      image2 = paste0("images/battery_choices_version2/", image),
      image3 = paste0("images/battery_choices_version3/", image)
      )

  #Choosing Car type
  chosen <- car_images1

  src <- glue::glue(
    #'<img src="images/car-images/{chosen}.jpg" width=100%>'
    '<img src="images/car-images/{chosen}.jpg" class="responsive-image" style="max-width: 100%; height: auto; object-fit: contain;">'
  )
  names(chosen) <- src


  # Define first choice question
  sd_question(
    type  = 'mc_buttons',
    id    = 'images',
    label = "Select a vehicle based on it’s segment / style that is most closely aligned with the next vehicle you are considering purchasing",
    option = chosen
  )


  #  Function to add icons (gas pump, electric plug, or both) before text values in a vehicle’s powertrain description
  create_image_cell <- function(value) {
    # This checks if the word "Electric" appears in the value (case-insensitive)
    if (grepl("Electric", value)) {
      return(sprintf(
        '<img src="images/electric_plug.png" style="width: 25px; height: 25px; vertical-align: middle; margin-right: 5px;">%s',
        value
      ))
    }
    # This checks if either "Gas" or "Gasoline" appears in the value
    else if (grepl("Gasoline", value)) {
      return(sprintf(
        '<img src="images/gas_pump.png" style="width: 25px; height: 25px; vertical-align: middle; margin-right: 5px;">%s',
        value
      ))
    }
    # This checks if either "Hybrid" appears in the value
    else if (grepl("Plug-in Hybrid", value)) {
      return(sprintf(
        '<img src="images/gas_pump.png" style="width: 25px; height: 25px; vertical-align: middle; margin-right: 5px;">
             <img src="images/electric_plug.png" style="width: 25px; height: 25px; vertical-align: middle; margin-right: 5px;">%s', value
      ))
    }
    else{
      return(sprintf(
        '<img src="images/gas_pump.png" style="width: 25px; height: 25px; vertical-align: middle; margin-right: 5px;">%s',
        value
      ))

    }
  }


  # Vehicle DCE -- Table Format
  observe(
    {
      # Run observer that updates the chosen_image when an image is chosen

      # This updates whenever input$images changes
      chosen_src <- paste0(
        'images/car-images/', input$images, '.jpg'
      )

      # Update the rendered image - displays the vehicle the user previously selected
      output$chosen_image <- renderImage({
        list(
          src = chosen_src,
          contentType = "image/jpg",
          width = "200px"
        )
      }, deleteFile = FALSE)


      create_car_table_detailed <- function(car_data, chosen_src) {

        # car_data should be a data frame with columns: model, range, price etc.
        car_data <- car_data %>%
          # mutate(
          #   price = as.numeric(price) * as.numeric(input$budget)
          #   #, image = paste0('<img src="', image, '" width=100>')
          #   ) %>%
          select(powertrain, range, price, make_year, mileage, operating_cost)

        rownames(car_data) <- c("Row1","Row2","Row3")
        car_data <- t(car_data)
        car_data <- as.data.frame(car_data)
        row.names(car_data) <- NULL


        car_data$names <- c('<span title="Does the vehicle run on gas or Electricity?" style="cursor: help; text-decoration: underline;">Powertrain</span>',
                            '<span title="The maximum distance a vehicle can travel in a full tank/ fully charged battery." style="cursor: help; text-decoration: underline;">Range</span>',
                            '<span title="The final price paid for the vehicle in dollars, including all taxes and fees." style="cursor: help; text-decoration: underline;">Price</span>',
                            '<span title="Model/Manufacturing year is the actual year the vehicle was built " style="cursor: help; text-decoration: underline;">Model Year</span>',
                            '<span title="The number of miles vehicle has travelled while in operation" style="cursor: help; text-decoration: underline;">Mileage</span>',
                            '<span title="Cost in cents per mile driven of fueling the vehicle" style="cursor: help; text-decoration: underline;">Operating Cost (Equivalent Gasoline Fuel Efficiency)</span>'

        )


        # Create the header part of the table
          # link rel="stylesheet": Loads a custom CSS file to style the table (testing_table1.css).
          # <body>, <div>s: Defines the structure and layout of the car comparison interface.
          # <div class="header-question">: Displays a question above the table to guide the user.
          # <img src="%s"...>: Dynamically inserts an image of the selected car. This is where %s gets replaced.
          # <table>: Sets up the HTML table with headers for three car options (Option 1, 2, 3).
          # <thead> and <tbody>: The table head defines the column titles; the body will later be appended with rows containing attribute values (range, price, powertrain, etc.).

      html_table <- sprintf('

    <link rel="stylesheet" href="css/testing_table1.css">

    <body>

  <div class="car-comparison">
    <div class="header-section">
    <div class="header-question">
      Assuming that you were able to purchase a vehicle that looked like the one you selected (see photo), which of these versions of that vehicle would you be most likely to purchase Compare the key features below:
      </div>
        <img src="%s" alt="Selected vehicle" style="max-width: 400px; height: auto;"><br><br>
      </div>
    <table>
        <thead>
            <tr>
                <th>Attribute</th>
                <th>Option 1</th>
                <th>Option 2</th>
                <th>Option 3</th>
            </tr>
        </thead>
        <tbody>
      ', chosen_src)

        # Add rows for each car in the data frame
        for(i in 1:nrow(car_data)) {

          # powertrain
          if (i == 1)
          {
            car_data$Row1[i] <- create_image_cell(car_data$Row1[i])
            car_data$Row2[i] <- create_image_cell(car_data$Row2[i])
            car_data$Row3[i] <- create_image_cell(car_data$Row3[i])

          }

          # price
          if (i ==3)
          {
            car_data$Row1[i] <- scales::dollar(ifelse(is.na(car_data$Row1[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row1[i])))
            car_data$Row2[i] <- scales::dollar(ifelse(is.na(car_data$Row2[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row2[i])))
            car_data$Row3[i] <- scales::dollar(ifelse(is.na(car_data$Row3[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row3[i])))

          }

          # mileague
          if (i ==5)
          {
            car_data$Row1[i] <- scales::comma(as.numeric(car_data$Row1[i]))
            car_data$Row2[i] <- scales::comma(as.numeric(car_data$Row2[i]))
            car_data$Row3[i] <- scales::comma(as.numeric(car_data$Row3[i]))

          }

          row <- sprintf('
            <tr>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>

            </tr>',
                         car_data$names[i],
                         car_data$Row1[i],
                         car_data$Row2[i],
                         car_data$Row3[i]

          )
          html_table <- paste0(html_table, row)
        }

        # Close the table
        # html_table <- paste0(html_table, '
        #     </tbody>
        # </table>')

        html_table <- paste0(html_table, '
        </tbody>
      </table>
      <div class="footnote">
        *To view an attribute description, hover over it. <br>
      </div>
    </div>')



        function() { html_table }
      }

      df_temp0 <- df |> filter(qID == 1)
      df_temp1 <- df |> filter(qID == 1)
      df_temp2 <- df |> filter(qID == 2)
      df_temp3 <- df |> filter(qID == 3)
      df_temp4 <- df |> filter(qID == 4)
      df_temp5 <- df |> filter(qID == 5)
      df_temp6 <- df |> filter(qID == 6)

     df_temp0$powertrain = c('Electric', 'Electric', 'Electric')
     df_temp0$price = c(1.1, 1.0, 0.5)
     df_temp0$make_year = c(2018, 2018, 2018)
     df_temp0$mileage = c(30000, 30000, 30000)
     df_temp0$operating_cost = c('12 cents per mile (27.5 MPG equivalent)', '12 cents per mile (27.5 MPG equivalent)', '12 cents per mile (27.5 MPG equivalent)')
     df_temp0$range = c('100 mile range on full charge', '200 mile range on full charge', '300 mile range on full charge')


      output$make_table_detailed_0 <-create_car_table_detailed(df_temp0, chosen_src)
      output$make_table_detailed_1 <-create_car_table_detailed(df_temp1, chosen_src)
      output$make_table_detailed_2 <-create_car_table_detailed(df_temp2, chosen_src)
      output$make_table_detailed_3 <-create_car_table_detailed(df_temp3, chosen_src)
      output$make_table_detailed_4 <-create_car_table_detailed(df_temp4, chosen_src)
      output$make_table_detailed_5 <-create_car_table_detailed(df_temp5, chosen_src)
      output$make_table_detailed_6 <-create_car_table_detailed(df_temp6, chosen_src)

    }
  )

  # Vehicle DCE -- Button Format
  observe(
    {
      # Run observer that updates the chosen_image when an image is chosen

      # This updates whenever input$images changes
      chosen_src <- paste0(
        'images/car-images/', input$images, '.jpg'
      )

      # Update the rendered image - displays the vehicle the user previously selected
      output$chosen_image <- renderImage({
        list(
          src = chosen_src,
          contentType = "image/jpg",
          width = "200px"
        )
      }, deleteFile = FALSE)

      create_car_table_short <- function( chosen_src) {


        html_table <- sprintf('

 <link rel="stylesheet" href="css/testing_table1.css">

 <body>
  <div class="car-comparison">
    <div class="header-section">
    <div class="header-question">
      Assuming that you were able to purchase a vehicle that looked like the one you selected (see photo), which of these versions of that vehicle would you be most likely to purchase Compare the key features below:
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
      req(domain$input$budget)
      budget <- as.numeric(domain$input$budget)

      vehicle_cbc_options <- function(df, budget_select) {

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
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or Electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt1$powertrain}</span><br>
        <b><span style='font-size: 13px;' title='The maximum distance a vehicle can travel in a full tank/ fully charged battery.' ><u>Range:</u></span></b><br> <span style='font-size: 13px;'>{alt1$range}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt1$price)}</span><br>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt1$make_year}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt1$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt1$operating_cost}</span><br>
      </div>
    ")),
          HTML(glue("
      <div style='text-align: left;'>
        <b>Option 2</b><br>
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or Electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt2$powertrain}</span><br>
        <b><span style='font-size: 13px;' title='The maximum distance a vehicle can travel in a full tank/ fully charged battery.' ><u>Range:</u></span></b><br> <span style='font-size: 13px;'>{alt2$range}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt2$price)}</span><br>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt2$make_year}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt2$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt2$operating_cost}</span><br>
      </div>
    ")),
          HTML(glue("
      <div style='text-align: left;'>
        <b>Option 3</b><br>
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or Electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt3$powertrain}</span><br>
        <b><span style='font-size: 13px;' title='The maximum distance a vehicle can travel in a full tank/ fully charged battery.' ><u>Range:</u></span></b><br> <span style='font-size: 13px;'>{alt3$range}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt3$price)}</span><br>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt3$make_year}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt3$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt3$operating_cost}</span><br>
      </div>
    "))
        )
        return(options)
      }

      # buget <- as.numeric(input$buget)
      # Create the options for each choice question
      # vehicle_cbc0_options <- vehicle_cbc_options(df |> filter(qID == 1), budget)
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
        id     = 'vehicle_cbc_q1_button',
        label  = "(1 of 6) If these were your only options, which would you choose?",
        option = vehicle_cbc1_options,
        width  = "70%",
        direction = "horizontal"
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q2_button',
        label  = "(2 of 6) If these were your only options, which would you choose?",
        option = vehicle_cbc2_options,
        width  = "70%",
        direction = "horizontal"
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q3_button',
        label  = "(3 of 6) If these were your only options, which would you choose?",
        option = vehicle_cbc3_options,
        width  = "70%",
        direction = "horizontal"
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q4_button',
        label  = "(4 of 6) If these were your only options, which would you choose?",
        option = vehicle_cbc4_options,
        width  = "70%",
        direction = "horizontal"
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q5_button',
        label  = "(5 of 6) If these were your only options, which would you choose?",
        option = vehicle_cbc5_options,
        width  = "70%",
        direction = "horizontal"
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'vehicle_cbc_q6_button',
        label  = "(6 of 6) If these were your only options, which would you choose?",
        option = vehicle_cbc6_options,
        width  = "70%"
      )

    }
  )

  # Battery DCE -- Table Format
  observe(
    {
      create_battery_table1 <- function(car_data) {

        # car_data should be a data frame with columns: model, range, price etc.
        car_data <- car_data %>%
          select(veh_mileage,battery_refurbish,image,veh_price)

        rownames(car_data) <- c("Row1","Row2","Row3")
        car_data <- t(car_data)
        car_data <- as.data.frame(car_data)
        row.names(car_data) <- NULL


        car_data$names <- c('<span title="The number of miles vehicle has travelled while in operation over the past three years." style="cursor: help; text-decoration: underline;">Mileage</span>',
                            '<span title="The refurbishment history of the battery (original vs. reconditioned vs. fully-replaced)." style="cursor: help; text-decoration: underline;">Refurbishment history</span>',
                            '<span title="Range and state-of-heath of the battery." style="cursor: help; text-decoration: underline;">Range in mile\n(SOH in %)</span>',
                            '<span title="" style="cursor: help; text-decoration: underline;">Purchase price</span>'
        )


        # Create the header part of the table
        # link rel="stylesheet": Loads a custom CSS file to style the table (testing_table1.css).
        # <body>, <div>s: Defines the structure and layout of the car comparison interface.
        # <div class="header-question">: Displays a question above the table to guide the user.
        # <img src="%s"...>: Dynamically inserts an image of the selected car. This is where %s gets replaced.
        # <table>: Sets up the HTML table with headers for three car options (Option 1, 2, 3).
        # <thead> and <tbody>: The table head defines the column titles; the body will later be appended with rows containing attribute values (range, price, powertrain, etc.).

        html_table <- sprintf('

    <link rel="stylesheet" href="css/testing_table1.css">

    <body>

  <div class="battery-comparison">
    <div class="header-section">
    <div class="header-question">
      Which of these versions of that vehicle would you be most likely to purchase comparing the key features below:
    <table>
        <thead>
            <tr>

                <th>Attribute</th>
                <th>Option 1</th>
                <th>Option 2</th>
                <th>Option 3</th>
            </tr>
        </thead>
        <tbody>
      ')

        # Add rows for each car in the data frame
        # for(i in 1:nrow(car_data[, -((ncol(car_data)-1):ncol(car_data))])) {
          for(i in 1:nrow(car_data)) {

          # mileague
          if (i ==1)
          {
            car_data$Row1[i] <- scales::comma(as.numeric(car_data$Row1[i]))
            car_data$Row2[i] <- scales::comma(as.numeric(car_data$Row2[i]))
            car_data$Row3[i] <- scales::comma(as.numeric(car_data$Row3[i]))

          }


          # range & SOH
          if (i ==3)
          {

            car_data$Row1[i] <- paste0('<img src= "images/battery_choices_version2/',car_data$Row1[i], ' "style="width: 300px; vertical-align: middle; ">')
            car_data$Row2[i] <- paste0('<img src= "images/battery_choices_version2/',car_data$Row2[i], ' "style="width: 300px; vertical-align: middle; ">')
            car_data$Row3[i] <- paste0('<img src= "images/battery_choices_version2/',car_data$Row3[i], ' "style="width: 300px; vertical-align: middle; ">')
          }

          # price
          if (i ==4)
          {
            car_data$Row1[i] <- scales::dollar(ifelse(is.na(car_data$Row1[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row1[i])))
            car_data$Row2[i] <- scales::dollar(ifelse(is.na(car_data$Row2[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row2[i])))
            car_data$Row3[i] <- scales::dollar(ifelse(is.na(car_data$Row3[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row3[i])))

          }


          row <- sprintf('
            <tr>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>

            </tr>',
                         car_data$names[i],
                         car_data$Row1[i],
                         car_data$Row2[i],
                         car_data$Row3[i]

          )
          html_table <- paste0(html_table, row)
        }

        # Close the table
        # html_table <- paste0(html_table, '
        #     </tbody>
        # </table>')

        html_table <- paste0(html_table, '
        </tbody>
      </table>
      <div class="footnote">
        *To view an attribute description, hover over it. <br>
      </div>
    </div>')



        function() { html_table }
      }

      df_temp0 <- battery_df |> filter(qID == 1)
      df_temp1 <- battery_df |> filter(qID == 1)
      df_temp2 <- battery_df |> filter(qID == 2)
      df_temp3 <- battery_df |> filter(qID == 3)
      df_temp4 <- battery_df |> filter(qID == 4)
      df_temp5 <- battery_df |> filter(qID == 5)
      df_temp6 <- battery_df |> filter(qID == 6)


      df_temp0$veh_mileage = c(30000, 30000, 30000)
      df_temp0$battery_refurbish = c('Original','Original','Original')
      df_temp0$image = c('<img src="images/battery_choices_version2/Range_Degradation_320_4.png" style="width: 200px; height: 150px; vertical-align: middle;">',
                         '<img src="images/battery_choices_version2/Range_Degradation_320_4.png" style="width: 200px; height: 150px; vertical-align: middle;">',
                         '<img src="images/battery_choices_version2/Range_Degradation_320_4.png" style="width: 200px; height: 150px; vertical-align: middle;">')
      df_temp0$veh_price = c(1.0, 0.8, 1.2)

      output$battery_table0 <-create_battery_table1(df_temp0)
      output$battery_table1 <-create_battery_table1(df_temp1)
      output$battery_table2 <-create_battery_table1(df_temp2)
      output$battery_table3 <-create_battery_table1(df_temp3)
      output$battery_table4 <-create_battery_table1(df_temp4)
      output$battery_table5 <-create_battery_table1(df_temp5)
      output$battery_table6 <-create_battery_table1(df_temp6)

    }
  )

  # Battery DCE -- Button Format -- version 1
  observe(
    {
    domain <- shiny::getDefaultReactiveDomain()
    req(domain$input$budget)
    budget <- as.numeric(domain$input$budget)

  battery_cbc_options <- function(df, budget_select) {

    alt1 <- df |> filter(altID == 1)
    alt2 <- df |> filter(altID == 2)
    alt3 <- df |> filter(altID == 3)

    alt1$price <- alt1$veh_price * budget_select
    alt2$price <- alt2$veh_price * budget_select
    alt3$price <- alt3$veh_price * budget_select

    options <- c("option_1", "option_2", "option_3")

    names(options) <- c(
      HTML(glue("
      <div style='text-align: left;'>
        <b>Option 1</b><br>
        <b><span style='font-size: 13px;'>Vehicle mileage:</span></b><br> <span style='font-size: 13px;'>{scales::comma(alt1$veh_mileage)}</span><br>
        <b><span style='font-size: 13px;'>Vehicle price:</span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt1$price)}</span><br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b><br> <span style='font-size: 13px;'>{alt1$battery_refurbish}</span><br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b><br>
        <img src='{alt1$image2}' style='width: 180px; vertical-align: middle;'>
      </div>
    ")),
      HTML(glue("
      <div style='text-align: left;'>
        <b>Option 2</b><br>
        <b><span style='font-size: 13px;'>Vehicle mileage:</span></b><br> <span style='font-size: 13px;'>{scales::comma(alt2$veh_mileage)}</span><br>
        <b><span style='font-size: 13px;'>Vehicle price:</span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt2$price)}</span><br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b><br> <span style='font-size: 13px;'>{alt2$battery_refurbish}</span><br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b><br>
        <img src='{alt2$image2}' style='width: 180px; vertical-align: middle;'>
      </div>
    ")),
      HTML(glue("
      <div style='text-align: left;'>
        <b>Option 3</b><br>
        <b><span style='font-size: 13px;'>Vehicle mileage:</span></b><br> <span style='font-size: 13px;'>{scales::comma(alt3$veh_mileage)}</span><br>
        <b><span style='font-size: 13px;'>Vehicle price:</span></b><br> <span style='font-size: 13px;'>$ {(scales::comma(alt3$price))}</span><br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b><br> <span style='font-size: 13px;'>{alt3$battery_refurbish}</span><br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b><br>
        <img src= '{alt3$image2}' style='width: 180px;  vertical-align: middle;'>
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
  # Example: sd_output('cbc_q1', type = 'question')

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q1_button',
    label  = "(1 of 6) If these were your only options, which would you choose?",
    option = battery_cbc1_options,
    width  = "70%",
    direction = "horizontal"
  )

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q2_button',
    label  = "(2 of 6) If these were your only options, which would you choose?",
    option = battery_cbc2_options,
    width  = "70%"
  )

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q3_button',
    label  = "(3 of 6) If these were your only options, which would you choose?",
    option = battery_cbc3_options,
    width  = "70%"
  )

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q4_button',
    label  = "(4 of 6) If these were your only options, which would you choose?",
    option = battery_cbc4_options,
    width  = "70%"
  )

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q5_button',
    label  = "(5 of 6) If these were your only options, which would you choose?",
    option = battery_cbc5_options,
    width  = "70%"
  )

  sd_question(
    type   = 'mc_buttons',
    id     = 'battery_cbc_q6_button',
    label  = "(6 of 6) If these were your only options, which would you choose?",
    option = battery_cbc6_options,
    width  = "70%"
  )

  }
)

  # Battery DCE -- Button Format -- version 2
  observe(
    {
      domain <- shiny::getDefaultReactiveDomain()
      req(domain$input$budget)
      budget <- as.numeric(domain$input$budget)

      battery_cbc_options_v3 <- function(df, budget_select) {

        alt1 <- df |> filter(altID == 1)
        alt2 <- df |> filter(altID == 2)
        alt3 <- df |> filter(altID == 3)

        alt1$price <- alt1$veh_price * budget_select
        alt2$price <- alt2$veh_price * budget_select
        alt3$price <- alt3$veh_price * budget_select

        options <- c("option_1", "option_2", "option_3")

        names(options) <- c(
          HTML(glue("
      <div style='text-align: left;'>
        <b>Option 1</b><br>
        <b><span style='font-size: 13px;'>Vehicle mileage:</span></b> {scales::comma(alt1$veh_mileage)}<br>
        <b><span style='font-size: 13px;'>Vehicle price:</span></b> $ {scales::comma(alt1$price)}<br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b> {alt1$battery_refurbish}<br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b>
        <img src='{alt1$image3}' style='width: 300px; vertical-align: middle;'>
      </div>
    ")),
          HTML(glue("
      <div style='text-align: left;'>
        <b>Option 2</b><br>
        <b><span style='font-size: 13px;'>Vehicle mileage:</span></b> {scales::comma(alt2$veh_mileage)}<br>
        <b><span style='font-size: 13px;'>Vehicle price:</span></b> $ {scales::comma(alt2$price)}<br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b> {alt2$battery_refurbish}<br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b>
        <img src='{alt2$image3}' style='width: 300px; vertical-align: middle;'>
      </div>
    ")),
          HTML(glue("
      <div style='text-align: left;'>
        <b>Option 3</b><br>
        <b><span style='font-size: 13px;'>Vehicle mileage:</span></b> {scales::comma(alt3$veh_mileage)}<br>
        <b><span style='font-size: 13px;'>Vehicle price:</span></b> $ {(scales::comma(alt3$price))}<br>
        <b><span style='font-size: 13px;'>Battery refurbishment:</span></b> {alt3$battery_refurbish}<br>
        <b><span style='font-size: 13px;'>Battery range and health:</span></b>
        <img src= '{alt3$image3}' style='width: 300px;  vertical-align: middle;'>
      </div>
    "))
        )
        return(options)
      }

      # buget <- as.numeric(input$buget)
      # Create the options for each choice question
      battery_cbc1_options_v3 <- battery_cbc_options_v3(battery_df |> filter(qID == 1), budget)
      battery_cbc2_options_v3 <- battery_cbc_options_v3(battery_df |> filter(qID == 2), budget)
      battery_cbc3_options_v3 <- battery_cbc_options_v3(battery_df |> filter(qID == 3), budget)
      battery_cbc4_options_v3 <- battery_cbc_options_v3(battery_df |> filter(qID == 4), budget)
      battery_cbc5_options_v3 <- battery_cbc_options_v3(battery_df |> filter(qID == 5), budget)
      battery_cbc6_options_v3 <- battery_cbc_options_v3(battery_df |> filter(qID == 6), budget)

      # Create each choice question - display these in your survey using sd_output()
      # Example: sd_output('cbc_q1', type = 'question')

      sd_question(
        type   = 'mc_buttons',
        id     = 'battery_cbc_q1_button_v3',
        label  = "(1 of 6) If these were your only options, which would you choose?",
        option = battery_cbc1_options_v3
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'battery_cbc_q2_button_v3',
        label  = "(2 of 6) If these were your only options, which would you choose?",
        option = battery_cbc2_options_v3
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'battery_cbc_q3_button_v3',
        label  = "(3 of 6) If these were your only options, which would you choose?",
        option = battery_cbc3_options_v3
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'battery_cbc_q4_button_v3',
        label  = "(4 of 6) If these were your only options, which would you choose?",
        option = battery_cbc4_options_v3
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'battery_cbc_q5_button_v3',
        label  = "(5 of 6) If these were your only options, which would you choose?",
        option = battery_cbc5_options_v3
      )

      sd_question(
        type   = 'mc_buttons',
        id     = 'battery_cbc_q6_button_v3',
        label  = "(6 of 6) If these were your only options, which would you choose?",
        option = battery_cbc6_options_v3
      )

    }
  )



  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if(
    input$next_vehicle_purchase == "not_sure" ~ "screenout",
    input$which_market == "New" ~ "screenout"
  )

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if(
    input$know_electric_vehicle == "Yes" ~ "write_electric_name",
    input$next_car_payment_source == "finance" ~ "next_car_down_payment"
  )




  # Database designation and other settings
  sd_server(
    db = db,
    # required_questions = c("images", "budget", "next_vehicle_purchase",
    #                        "which_market", "next_car_payment_source", "know_electric_vehicle",
    #                        "cbc_q1",  "cbc_q2" , "cbc_q3",  "cbc_q4" , "cbc_q5",  "cbc_q6",
    #                        "battery_cbc_q1",  "battery_cbc_q2" , "battery_cbc_q3",  "battery_cbc_q4" , "battery_cbc_q5",  "battery_cbc_q6"),
    use_cookies = FALSE
  )

}


# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
