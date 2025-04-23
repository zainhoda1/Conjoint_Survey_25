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
respondentID <- sample(survey$respID, 1)


###################




###################




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


  observe(
    {

      # Create function to add image to the table
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
        # If neither electric nor gas, just return the original value
        #return(value)
      }

      # Run observer that updates the chosen_image when an image is chosen

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

 
      create_car_table1 <- function(car_data, chosen_src) {

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




        # car_data should be a data frame with columns: model, range, price


        # Create the header part of the table
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



          if (i == 1)
          {
            car_data$Row1[i] <- create_image_cell(car_data$Row1[i])
            car_data$Row2[i] <- create_image_cell(car_data$Row2[i])
            car_data$Row3[i] <- create_image_cell(car_data$Row3[i])

          }

          if (i ==3)
          {
            car_data$Row1[i] <- scales::dollar(ifelse(is.na(car_data$Row1[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row1[i])))
            car_data$Row2[i] <- scales::dollar(ifelse(is.na(car_data$Row2[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row2[i])))
            car_data$Row3[i] <- scales::dollar(ifelse(is.na(car_data$Row3[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row3[i])))

          }

          if (i ==5)
          {
            car_data$Row1[i] <- scales::comma(as.numeric(car_data$Row1[i]))
            car_data$Row2[i] <- scales::comma(as.numeric(car_data$Row2[i]))
            car_data$Row3[i] <- scales::comma(as.numeric(car_data$Row3[i]))

          }
          #temp <- as.numeric(input$budget) * as.numeric(car_data$Row1[i])
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




  sd_store_value(respondentID)

  df <- survey %>%
    filter(respID == respondentID)


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
    required_questions = c("images", "budget", "next_vehicle_purchase",
                           "which_market", "next_car_payment_source", "know_electric_vehicle",
                           "cbc_q1",  "cbc_q2" , "cbc_q3",  "cbc_q4" , "cbc_q5",  "cbc_q6" ),
    use_cookies = FALSE
  )

}




# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
