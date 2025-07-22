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


  # Vehicle DCE
  observe(
    {

      #  Function to add icons (gas pump, electric plug, or both) before text values in a vehicle’s powertrain description
      create_image_cell <- function(value) {
        # This checks if the word "Electric" appears in the value (case-insensitive)
        if (grepl("Electric", value)) {
          return(sprintf(
            '<img src="images/electric_plug.png" style="width: 30px; height: 30px; vertical-align: middle; margin-right: 5px;">%s',
            value
          ))
        }
        # This checks if either "Gas" or "Gasoline" appears in the value
        else if (grepl("Gasoline", value)) {
          return(sprintf(
            '<img src="images/gas_pump.png" style="width: 30px; height: 30px; vertical-align: middle; margin-right: 5px;">%s',
            value
          ))
        }
        # This checks if either "Hybrid" appears in the value
        else if (grepl("Plug-in Hybrid", value)) {
          return(sprintf(
            '<img src="images/gas_pump.png" style="width: 30px; height: 30px; vertical-align: middle; margin-right: 5px;">
             <img src="images/electric_plug.png" style="width: 30px; height: 30px; vertical-align: middle; margin-right: 5px;">%s', value
          ))
        }
        else{
          return(sprintf(
              '<img src="images/gas_pump.png" style="width: 30px; height: 30px; vertical-align: middle; margin-right: 5px;">%s',
              value
          ))

        }
      }

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


      create_car_table1 <- function(car_data, chosen_src) {

        # car_data should be a data frame with columns: model, range, price etc.
        car_data <- car_data %>%
          # mutate(
          #   price = as.numeric(price) * as.numeric(input$budget)
          #   #, image = paste0('<img src="', image, '" width=100>')
          #   ) %>%
          select(powertrain, range, price, make_year, mileage, operating_cost  )

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


      output$make_table0 <-create_car_table1(df_temp0, chosen_src)
      output$make_table1 <-create_car_table1(df_temp1, chosen_src)
      output$make_table2 <-create_car_table1(df_temp2, chosen_src)
      output$make_table3 <-create_car_table1(df_temp3, chosen_src)
      output$make_table4 <-create_car_table1(df_temp4, chosen_src)
      output$make_table5 <-create_car_table1(df_temp5, chosen_src)
      output$make_table6 <-create_car_table1(df_temp6, chosen_src)

    }
  )


  # Battery DCE -- Image
  observe(
    {

      # #  Function to add icons (gas pump, electric plug, or both) before text values in a vehicle’s powertrain description
      # create_image_cell <- function(value) {
      #   # This checks if the word "Electric" appears in the value (case-insensitive)
      #   if (grepl("Electric", value)) {
      #     return(sprintf(
      #       '<img src="images/electric_plug.png" style="width: 30px; height: 30px; vertical-align: middle; margin-right: 5px;">%s',
      #       value
      #     ))
      #   }
      #   # This checks if either "Gas" or "Gasoline" appears in the value
      #   else if (grepl("Gasoline", value)) {
      #     return(sprintf(
      #       '<img src="images/gas_pump.png" style="width: 30px; height: 30px; vertical-align: middle; margin-right: 5px;">%s',
      #       value
      #     ))
      #   }
      #   # This checks if either "Hybrid" appears in the value
      #   else if (grepl("Plug-in Hybrid", value)) {
      #     return(sprintf(
      #       '<img src="images/gas_pump.png" style="width: 30px; height: 30px; vertical-align: middle; margin-right: 5px;">
      #        <img src="images/electric_plug.png" style="width: 30px; height: 30px; vertical-align: middle; margin-right: 5px;">%s', value
      #     ))
      #   }
      #   else{
      #     return(sprintf(
      #       '<img src="images/gas_pump.png" style="width: 30px; height: 30px; vertical-align: middle; margin-right: 5px;">%s',
      #       value
      #     ))
      #
      #   }
      # }
      #
      # # Run observer that updates the chosen_image when an image is chosen
      #
      # # This updates whenever input$images changes
      # chosen_src <- paste0(
      #   'images/car-images/', input$images, '.jpg'
      # )
      #
      # Update the rendered image - displays the vehicle the user previously selected
      output$chosen_image <- renderImage({
        list(
          src = chosen_src,
          contentType = "image/jpg",
          width = "200px"
        )
      }, deleteFile = FALSE)


      create_battery_table1 <- function(car_data) {

        # car_data should be a data frame with columns: model, range, price etc.
        car_data <- car_data %>%
          select(veh_mileage,battery_condition,battery_refurbish,image,charge_freq,charge_DCFC,veh_price)

        rownames(car_data) <- c("Row1","Row2","Row3")
        car_data <- t(car_data)
        car_data <- as.data.frame(car_data)
        row.names(car_data) <- NULL


        car_data$names <- c('<span title="The number of miles vehicle has travelled while in operation over the past three years." style="cursor: help; text-decoration: underline;">Mileage</span>',
                            '<span title="The condition of the battery (like-new vs. used)." style="cursor: help; text-decoration: underline;">Condition</span>',
                            '<span title="The refurbishment history of the battery (original vs. reconditioned vs. fully-replaced)." style="cursor: help; text-decoration: underline;">Refurbishment history</span>',
                            '<span title="Range and state-of-heath of the battery." style="cursor: help; text-decoration: underline;">Range in mile\n(SOH in %)</span>',
                            '<span title="Average weekly charging frequency over the past three years." style="cursor: help; text-decoration: underline;">Weekly charging frequency</span>',
                            '<span title="Percentage of charging sessions using DCFC" style="cursor: help; text-decoration: underline;">% of DCFC charging</span>',
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
          if (i ==4)
          {

            car_data$Row1[i] <- paste0('<img src= "images/battery_choices/',car_data$Row1[i], ' "style="width: 200px; height: 150px; vertical-align: middle; ">')
            car_data$Row2[i] <- paste0('<img src= "images/battery_choices/',car_data$Row2[i], ' "style="width: 200px; height: 150px; vertical-align: middle; ">')
            car_data$Row3[i] <- paste0('<img src= "images/battery_choices/',car_data$Row3[i], ' "style="width: 200px; height: 150px; vertical-align: middle; ">')
          }

          # price
          if (i ==7)
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
      df_temp0$battery_condition = c('Used', 'Used','Used')
      df_temp0$battery_refurbish = c('Original','Original','Original')


      df_temp0$image = c('<img src="images/battery_choices/Range_Degradation_320_4.png" style="width: 200px; height: 150px; vertical-align: middle;">',
                         '<img src="images/battery_choices/Range_Degradation_320_4.png" style="width: 200px; height: 150px; vertical-align: middle;">',
                         '<img src="images/battery_choices/Range_Degradation_320_4.png" style="width: 200px; height: 150px; vertical-align: middle;">')
      df_temp0$charge_freq = c('4 times', '4 times', '4 times')
      df_temp0$charge_DCFC = c('40%', '40%','40%')
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


  sd_store_value(respondentID)
  sd_store_value(battery_respondentID)

  df <- survey %>%
    filter(respID == respondentID)
  battery_df <- battery_survey %>%
    filter(respID == battery_respondentID)


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
