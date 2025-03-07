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

  observe(
    {

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
    select(powertrain, price, range, mileage, operating_cost)

    rownames(car_data) <- c("Row1","Row2","Row3")
    car_data <- t(car_data)
    car_data <- as.data.frame(car_data)
    row.names(car_data) <- NULL
    car_data$names <- c('<span title="Does the car run on gas or Electricity?" style="cursor: help; text-decoration: underline;">Powertrain</span>',
                        '<span title="What is the price of the vehicle" style="cursor: help; text-decoration: underline;">Price</span>',
                        '<span title="How many miles in a full tank/ fully charged battery" style="cursor: help; text-decoration: underline;">Range</span>',
                        '<span title="How many miles in a gallon of fuel" style="cursor: help; text-decoration: underline;">Mileage</span>',
                        '<span title="Operating cost per 100 miles" style="cursor: help; text-decoration: underline;">Operating Cost</span>'
                        )




    # car_data should be a data frame with columns: model, range, price

    # Create the header part of the table
    html_table <- sprintf('

    <link rel="stylesheet" href="css/testing_table.css">

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
      if (i ==2)
        {
          car_data$Row1[i] <- ifelse(is.na(car_data$Row1[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row1[i]))
          car_data$Row2[i] <- ifelse(is.na(car_data$Row2[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row2[i]))
          car_data$Row3[i] <- ifelse(is.na(car_data$Row3[i]), NA, as.numeric(input$budget) * as.numeric(car_data$Row3[i]))

        }
      temp <- as.numeric(input$budget) * as.numeric(car_data$Row1[i])
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
        **The average acceleration for cars in the U.S. is 0 to 60 mph in 7.4 seconds
      </div>
    </div>')



    function() { html_table }
  }

  df_temp <- df |> filter(qID == 1)
  df_temp2 <- df |> filter(qID == 2)
  df_temp3 <- df |> filter(qID == 3)
  df_temp4 <- df |> filter(qID == 4)
  df_temp5 <- df |> filter(qID == 5)
  df_temp6 <- df |> filter(qID == 6)


  output$make_table1 <-create_car_table1(df_temp, chosen_src)
  output$make_table2 <-create_car_table1(df_temp2, chosen_src)
  output$make_table3 <-create_car_table1(df_temp3, chosen_src)
  output$make_table4 <-create_car_table1(df_temp4, chosen_src)
  output$make_table5 <-create_car_table1(df_temp5, chosen_src)
  output$make_table6 <-create_car_table1(df_temp6, chosen_src)
    }
  )


  create_car_table2 <- function(car_data) {

    # car_data <- car_data %>%
    #   mutate(
    #     price = paste(scales::dollar(price), "/ lb")
    #     #, image = paste0('<img src="', image, '" width=100>')
    #   )

    # car_data should be a data frame with columns: model, range, price

    # Create the header part of the table
    html_table2 <-
    '
    <html>
<head>
<style>
  .container {
    font-family: Arial, sans-serif;
    max-width: 1200px;
    margin: 20px auto;
    padding: 20px;
  }

  .header {
    display: flex;
    gap: 20px;
    align-items: flex-start;
    margin-bottom: 20px;
  }

  .header-text {
    flex: 2;
    font-size: 14px;
    line-height: 1.4;
  }

  .header-image {
    flex: 1;
    max-width: 300px;
  }

  .header-image img {
    width: 100%;
    height: auto;
  }

  table {
    width: 100%;
    border-collapse: collapse;
    margin-bottom: 20px;
  }

  th, td {
    border: 1px solid #ddd;
    padding: 12px;
    text-align: center;
  }

  th {
    background-color: #f8f8f8;
    font-weight: bold;
  }

  td:first-child {
    text-align: left;
    background-color: #f8f8f8;
    font-weight: bold;
    width: 20%;
  }

  .footnote {
    font-size: 12px;
    color: #666;
    font-style: italic;
  }

  .vehicle-type {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 5px;
  }
</style>
</head>
<body>
<div class="container">
  <div class="header">
    <div class="header-text">
      Assuming that you were able to purchase a vehicle that looked like the one you selected (see photo), which of these versions of that vehicle would you be most likely to purchase Compare the key features below:
    </div>
    <div class="header-image">
      <img src="/api/placeholder/400/320" alt="SUV">
    </div>
  </div>

  <table>
    <tr>
      <th>Attribute</th>
      <th>Option 1</th>
      <th>Option 2</th>
      <th>Option 3</th>
    </tr>
    <tr>
      <td>Vehicle Type</td>
      <td>
        {car_data$range[1]}
      </td>
      <td>
        Plug-In Hybrid ⛽<br>
        300 mile range on 1 tank<br>
        (first 40 miles electric)
      </td>
      <td>
        Electric ⚡<br>
        75 mile range on full charge
      </td>
    </tr>
    <tr>
      <td>Range (in miles)</td>
      <td>`{r}car_data$range[1]`</td>
      <td>`car_data$range[1]`</td>
      <td>{car_data$range[1]}</td>
    </tr>
    <tr>
      <td>Purchase Price</td>
      <td>$28,000</td>
      <td>$32,000</td>
      <td>$35,000</td>
    </tr>
    <tr>
      <td>Operating Cost (Equivalent Gasoline Fuel Efficiency)</td>
      <td>21 cents per mile</td>
      <td>6 cents per mile<br>(20 MPG equivalent)</td>
      <td>12 cents per mile<br>(60 MPG equivalent)</td>
    </tr>
    <tr>
      <td>0 to 60 mph Acceleration Time</td>
      <td>8.1 seconds</td>
      <td>8.1 seconds</td>
      <td>8 seconds</td>
    </tr>
    <tr>
      <td>Mileage</td>
      <td>25</td>
      <td>25</td>
      <td>55</td>
    </tr>
  </table>

  <div class="footnote">
    *To view an attribute description, click on ℹ️<br>
    **The average acceleration for cars in the U.S. is 0 to 60 mph in 7.4 seconds
  </div>
</div>
</body>
</html>

    '
    function() { html_table2 }
  }




  # Database designation and other settings
  sd_server(
    db = db,
    required_questions = c("images", "budget"),
    use_cookies = FALSE
  )

}




# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
