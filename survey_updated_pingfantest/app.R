library(surveydown)
library(tidyverse)
library(here)
library(glue)
library(dplyr)
library(kableExtra)
library(digest)

# Database setup
db <- sd_db_connect()


demo_options <- tibble(
  profileID = c(1, 1, 1), # or c(1, 2, 3) if you want different IDs
  respID = c(1620, 1621, 1622), # Different response IDs
  qID = c(1, 1, 1),
  altID = c(1, 2, 3),
  obsID = c(1, 2, 3),
  powertrain = c('Battery electric', 'Battery electric', 'Battery electric'), # Different powertrain codes
  price = c(0.8, 1.2, 1.5), # Different prices
  range = c(
    "225 miles on full charge<br>",
    "225 miles on full charge<br>",
    "225 miles on full charge<br>"
  ),
  mileage = c(25000, 30000, 45000),
  make_year = c(2018, 2018, 2018),
  operating_cost = c(
    "7 cents per mile<br>( 46.7 MPG equivalent)",
    "8 cents per mile<br>( 42.3 MPG equivalent)",
    "9 cents per mile<br>( 55.2 MPG equivalent)"
  ),
  vehicle_type = c("car", "car", "car")
)

electric_icon <- '<img src="images/electric_plug.png" style="width: 20px; height: 20px; vertical-align: middle; margin-right: 5px;">'
gas_icon <- '<img src="images/gas_pump.png" style="width: 20px; height: 20px; vertical-align: middle; margin-right: 5px;">'

vehicle_cbc_options <- function(df, budget_select) {

  df <- df %>%
    mutate(
      powertrain = case_when(
        grepl("Battery electric", powertrain, ignore.case = TRUE) ~
          paste0(electric_icon, powertrain),
        grepl("Plug-in hybrid", powertrain, ignore.case = TRUE) ~
          paste0(gas_icon, electric_icon, powertrain),
        TRUE ~ paste0(gas_icon, powertrain)
      )
    )

  alt1 <- df |> filter(altID == 1)
  alt2 <- df |> filter(altID == 2)
  alt3 <- df |> filter(altID == 3)

  # Use only the first value of budget_select if it has multiple values
  budget_val <- budget_select[1]

  alt1$price <- alt1$price[1] * budget_val
  alt2$price <- alt2$price[1] * budget_val
  alt3$price <- alt3$price[1] * budget_val

  options <- c("option_1", "option_2", "option_3")

  names(options) <- c(
    HTML(glue(
      "
      <div style='text-align: left;'>
        <b>Option 1</b><br>
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt1$powertrain}</span><br>
        <b><span style='font-size: 13px;' title='The maximum distance a vehicle can travel in a full tank/ fully charged battery.' ><u>Range:</u></span></b><br> <span style='font-size: 13px;'>{alt1$range}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt1$price)}</span><br>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt1$make_year}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt1$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt1$operating_cost}</span><br>
      </div>
    "
    )),
    HTML(glue(
      "
      <div style='text-align: left;'>
        <b>Option 2</b><br>
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt2$powertrain}</span><br>
        <b><span style='font-size: 13px;' title='The maximum distance a vehicle can travel in a full tank/ fully charged battery.' ><u>Range:</u></span></b><br> <span style='font-size: 13px;'>{alt2$range}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt2$price)}</span><br>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt2$make_year}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt2$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt2$operating_cost}</span><br>
      </div>
    "
    )),
    HTML(glue(
      "
      <div style='text-align: left;'>
        <b>Option 3</b><br>
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt3$powertrain}</span><br>
        <b><span style='font-size: 13px;' title='The maximum distance a vehicle can travel in a full tank/ fully charged battery.' ><u>Range:</u></span></b><br> <span style='font-size: 13px;'>{alt3$range}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt3$price)}</span><br>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt3$make_year}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt3$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt3$operating_cost}</span><br>
      </div>
    "
    ))
  )
  return(options)
}

create_car_table_short <- function(chosen_vehicle_image) {
  html_table <- sprintf(
    '

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

      ',
    chosen_vehicle_image
  )
  function() {
    html_table
  }
}

# Server setup
server <- function(input, output, session) {
  survey <- read_csv(here('data', 'choice_questions.csv'))
  battery_survey <- read_csv(here('data', 'battery_choice_questions.csv'))
  respondentID <- sample(survey$respID, 1)
  battery_respondentID <- sample(battery_survey$respID, 1)

  sd_store_value(respondentID, "respID")
  sd_store_value(battery_respondentID, "respID")

  # Create random assignment when session starts
  prime_groups <- c('prime_short', 'prime_long')
  prime_group_label <- sample(c('prime_short', 'prime_long'), 1)
  names(prime_groups) <- prime_group_label

  sd_store_value(prime_group_label, id = "prime_group_label")

  ## data
  df <- survey %>%
    filter(respID == respondentID) |>
    mutate(
      `range` = case_when(
        str_detect(`range`, "\\(") ~ str_replace(`range`, "\\s*\\(", "<br>("),
        TRUE ~ paste0(`range`, "<br>")
      ),
      operating_cost = case_when(
        str_detect(operating_cost, "\\(") ~
          str_replace(operating_cost, "\\s*\\(", "<br>("),
        TRUE ~ paste0(operating_cost, "<br>")
      )
    )

  df_filtered_vehicle_type <- reactive({
    # Try to get vehicle style from input first
    vehicle_style <- input$next_veh_style

    # If not available, try from stored data
    if (is.null(vehicle_style)) {
      stored_data <- sd_get_data(db)
      if (!is.null(stored_data) && "next_veh_style" %in% names(stored_data)) {
        style_values <- stored_data$next_veh_style
        valid_styles <- style_values[!is.na(style_values)]
        if (length(valid_styles) > 0) {
          vehicle_style <- valid_styles[length(valid_styles)]
        }
      }
    }

    req(vehicle_style) # ensures we have a vehicle style

    df %>%
      {
        if (vehicle_style == "Car / sedan / hatchback") {
          filter(., vehicle_type == "car")
        } else {
          filter(., vehicle_type == "suv")
        }
      }
  })

  # pulling blocks outside of observe - start
  budget <- reactive({
    # First try to get from current input
    if (!is.null(input$next_veh_budget)) {
      value <- as.numeric(input$next_veh_budget)
      return(value)
    }

    # If not available, try to retrieve from stored data for current session only
    stored_data <- sd_get_data(db)
    if (!is.null(stored_data) && "next_veh_budget" %in% names(stored_data)) {
      # Try to get the actual session_id that surveydown is using (after cookie restoration)
      # Check if all_data has been initialized with the restored session_id
      actual_session_id <- NULL
      if (
        !is.null(session$userData$all_data) &&
          !is.null(session$userData$all_data$session_id)
      ) {
        actual_session_id <- session$userData$all_data$session_id
      } else {
        actual_session_id <- session$token
      }

      # Filter to get only current session data
      session_rows <- which(stored_data$session_id == actual_session_id)

      if (length(session_rows) > 0) {
        # Get only the current session's data
        session_data <- stored_data[session_rows, ]
        session_budget <- session_data$next_veh_budget[
          !is.na(session_data$next_veh_budget)
        ]

        if (length(session_budget) > 0) {
          # Get the most recent budget for this session
          value <- as.numeric(session_budget[length(session_budget)])
          return(value)
        }
      }

    }

    return(NULL)
  })

  # This updates whenever input$images changes
  chosen_input <- reactive({
    selected <- if (!is.null(input$next_veh_car_images)) {
      input$next_veh_car_images
    } else {
      input$next_veh_suv_images
    }

    # If no input available, try to get from stored data
    if (is.null(selected)) {
      stored_data <- sd_get_data(db)
      if (!is.null(stored_data)) {
        # Try car images first
        if ("next_veh_car_images" %in% names(stored_data)) {
          car_values <- stored_data$next_veh_car_images
          valid_cars <- car_values[!is.na(car_values) & car_values != ""]
          if (length(valid_cars) > 0) {
            selected <- valid_cars[length(valid_cars)]
          }
        }
        # Try SUV images if no car image found
        if (
          is.null(selected) && "next_veh_suv_images" %in% names(stored_data)
        ) {
          suv_values <- stored_data$next_veh_suv_images
          valid_suvs <- suv_values[!is.na(suv_values) & suv_values != ""]
          if (length(valid_suvs) > 0) {
            selected <- valid_suvs[length(valid_suvs)]
          }
        }
      }
    }

    if (!is.null(selected)) {
      chosen_src <- paste0(
        'images/car-images/',
        selected,
        '.png'
      )
      return(chosen_src)
    }

    # Return a default image if nothing found
    return('images/car-images/1_new_car_sedan_compact.png')
  })

  # pulling blocks outside of observe - end

  observe(
    {
      # Force reactivity on budget changes
      budget_val <- budget()
      req(budget_val) # Ensure budget is available

      df <- df_filtered_vehicle_type()

      output$make_table_short <- create_car_table_short(chosen_input())

      # Create the options for each choice question
      vehicle_cbc1_options <- vehicle_cbc_options(
        df |> filter(qID == 1),
        budget_val
      )
      vehicle_cbc0_options <- vehicle_cbc_options(demo_options, budget_val)


      sd_question(
        type = 'mc_buttons',
        id = 'vehicle_cbc_q0_button',
        label = "If these were your only options, which would you choose?",
        option = vehicle_cbc0_options,
        width = "70%",
        direction = "horizontal"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'vehicle_cbc_q1_button',
        label = "If these were your only options, which would you choose?",
        option = vehicle_cbc1_options,
        width = "70%",
        direction = "horizontal"
      )
    }
  )

  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_forward(
    # Screen out if the respondent doesn't have valid start
    # !is_valid_start() ~ "screenout",
  )

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if()

  # Database designation and other settings
  sd_server(
    db = db,
    #all_questions_required = TRUE,
    # required_questions = c("images", "budget", "next_vehicle_purchase",
    #                        "which_market", "next_car_payment_source", "know_electric_vehicle",
    #                        "cbc_q1",  "cbc_q2" , "cbc_q3",  "cbc_q4" , "cbc_q5",  "cbc_q6",
    #                        "battery_cbc_q1",  "battery_cbc_q2" , "battery_cbc_q3",  "battery_cbc_q4" , "battery_cbc_q5",  "battery_cbc_q6"),
    auto_scroll = FALSE,
    rate_survey = FALSE,
    language = "en",
    use_cookies = TRUE,
    highlight_unanswered = TRUE,
    highlight_color = "gray"
  )
}


# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
