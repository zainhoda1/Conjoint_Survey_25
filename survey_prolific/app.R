library(surveydown)
library(tidyverse)
library(here)
library(glue)
library(dplyr)
library(kableExtra)
library(digest)
library(arrow)

ui <- sd_ui()

# Database setup
db <- sd_db_connect()

survey <- read_parquet(here('data', 'design_vehicle.parquet'))
battery_survey <- read_parquet(here('data', 'design_battery.parquet'))

completion_code <- 'CWTFNPQX'
screenout_code <- 'C127J9EM'

demo_options <- tibble(
  profileID = c(1, 1, 1, 1), # or c(1, 2, 3) if you want different IDs
  respID = c(1620, 1621, 1622, 1623), # Different response IDs
  qID = c(1, 1, 1, 1),
  altID = c(1, 2, 3, 4),
  obsID = c(1, 2, 3, 4),
  powertrain = c('gas', 'gas', 'gas', 'gas'), # Different powertrain codes
  price = c(15000, 20000, 25000, 30000), # Different prices
  range = c(
    " ",
    " ",
    " ",
    " "
  ),
  mileage = c(25000, 30000, 45000, 60000),
  age = c(7, 7, 7, 7),
  operating_cost_text = c(
    "7 cents per mile<br>( 46.7 MPG equivalent)",
    "8 cents per mile<br>( 42.3 MPG equivalent)",
    "9 cents per mile<br>( 37.0 MPG equivalent)",
    "10 cents per mile<br>( 30.0 MPG equivalent)"
  ),
  vehicle_type = c("car", "car", "car", "car")
)


electric_icon <- '<img src="images/electric_plug.png" style="width: 20px; height: 20px; vertical-align: middle; margin-right: 5px;">'
gas_icon <- '<img src="images/gas_pump.png" style="width: 20px; height: 20px; vertical-align: middle; margin-right: 5px;">'

vehicle_cbc_options <- function(df) {
  df <- df %>%
    mutate(
      powertrain = case_when(
        powertrain == 'bev' ~ paste0(electric_icon, 'Battery electric'),
        powertrain == 'hev' ~ paste0(gas_icon, 'Gas hybrid'), # 'Gas hybrid electric'
        powertrain == 'gas' ~ paste0(gas_icon, 'Conventional')
      ),
      age = 2025 - age
    )

  alt1 <- df |> filter(altID == 1)
  alt2 <- df |> filter(altID == 2)
  alt3 <- df |> filter(altID == 3)
  alt4 <- df |> filter(altID == 4)

  options <- c("option_1", "option_2", "option_3", "option_4")

  names(options) <- c(
    HTML(glue(
      "
      <div style='text-align: left;'>
        <b style='position: absolute; top: 5px; left: 5px; margin: 0; padding: 0;'>Option 1</b><br>
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt1$powertrain}</span><br>
        <span style='font-size: 13px;'>{alt1$range}</span>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt1$age}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt1$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt1$operating_cost_text}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt1$price)}</span><br>
      </div>
    "
    )),
    HTML(glue(
      "
      <div style='text-align: left;'>
        <b style='position: absolute; top: 5px; left: 5px; margin: 0; padding: 0;'>Option 2</b><br>
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt2$powertrain}</span><br>
        <span style='font-size: 13px;'>{alt2$range}</span>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt2$age}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt2$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt2$operating_cost_text}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt2$price)}</span><br>
      </div>
    "
    )),
    HTML(glue(
      "
      <div style='text-align: left;'>
        <b style='position: absolute; top: 5px; left: 5px; margin: 0; padding: 0;'>Option 3</b><br>
        <b><span style='font-size: 13px;' title='Does the vehicle run on gas or electricity?' ><u>Powertrain:</u></span></b><br> <span style='font-size: 13px;'>{alt3$powertrain}</span><br>
        <span style='font-size: 13px;'>{alt3$range}</span>
        <b><span style='font-size: 13px;' title='Model/Manufacturing year is the actual year the vehicle was built.' ><u>Model year:</u></span></b><br> <span style='font-size: 13px;'>{alt3$age}</span><br>
        <b><span style='font-size: 13px;' title='The number of miles vehicle has travelled while in operation.' ><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt3$mileage)}</span><br>
        <b><span style='font-size: 13px;' title='Cost in cents per mile driven of fueling the vehicle.' ><u>Operating cost:</u></span></b><br> <span style='font-size: 13px;'>{alt3$operating_cost_text}</span><br>
        <b><span style='font-size: 13px;' title='The final price paid for the vehicle in dollars, including all taxes and fees.' ><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt3$price)}</span><br>
      </div>
    "
    )),
    HTML(glue(
      "
      <div style='text-align: left;'>
        <b style='position: absolute; top: 5px; left: 5px; margin: 0; padding: 0;'>Option 4</b>
        <b><span style='font-size: 13px;'>Even if these were </span></b><br>
        <b><span style='font-size: 13px;'>my best options,  </span></b><br>
        <b><span style='font-size: 13px;'>I would not choose any</span></b><br>
        <b><span style='font-size: 13px;'>of these vehicles</span></b><br>
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

battery_cbc_options <- function(df) {
  alt1 <- df |> filter(altID == 1)
  alt2 <- df |> filter(altID == 2)
  alt3 <- df |> filter(altID == 3)
  alt4 <- df |> filter(altID == 4) # Line added

  # Use only the first value of budget_select if it has multiple values
  #budget_val <- budget_select[1]

  options <- c("option_1", "option_2", "option_3", "option_4")

  #  <b style='position: absolute; top: 5px; left: 5px; margin: 0; padding: 0;'>Option 1</b><br>

  # <span style='font-size: 13px;'>{alt3$battery_refurbish}</span>
  names(options) <- c(
    HTML(glue(
      "
      <div style='text-align: left;'>
        <b style='position: absolute; top: 5px; left: 5px; margin: 0; padding: 0;'>Option 1</b><br>
        <b><span style='font-size: 13px;'><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt1$mileage)}</span><br>
        <b><span style='font-size: 13px;'><u>Battery condition:</u></span></b><br> <span style='font-size: 13px;'>{alt1$battery_condition} </span><br>
        <b><span style='font-size: 13px;'><u>Range on a full charge:</u></span></b><br>
        <b><span style='font-size: 13px;'>&nbsp; &nbsp; Current:</span></b><br> <span style='font-size: 13px;'>&nbsp; &nbsp; {alt1$battery_range_year3} miles<br>&nbsp; &nbsp; (Battery Health: {alt1$battery_health_year3}) </span><br>
        <b><span style='font-size: 13px;'>&nbsp; &nbsp; Expected in 5 years:</span></b><br> <span style='font-size: 13px;'>&nbsp; &nbsp; {alt1$battery_range_year8} miles<br>&nbsp; &nbsp; (Battery Health: {alt1$battery_health_year8}) </span><br>
        <b><span style='font-size: 13px;'><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt1$price)}</span><br>
      </div>
    "
    )),
    HTML(glue(
      "
      <div style='text-align: left;'>
        <b style='position: absolute; top: 5px; left: 5px; margin: 0; padding: 0;'>Option 2</b><br>
        <b><span style='font-size: 13px;'><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt2$mileage)}</span><br>
        <b><span style='font-size: 13px;'><u>Battery condition:</u></span></b><br> <span style='font-size: 13px;'>{alt2$battery_condition} </span><br>
        <b><span style='font-size: 13px;'><u>Range on a full charge:</u></span></b><br>
        <b><span style='font-size: 13px;'>&nbsp; &nbsp; Current:</span></b><br> <span style='font-size: 13px;'>&nbsp; &nbsp; {alt2$battery_range_year3} miles<br>&nbsp; &nbsp; (Battery Health: {alt2$battery_health_year3}) </span><br>
        <b><span style='font-size: 13px;'>&nbsp; &nbsp; Expected in 5 years:</span></b><br> <span style='font-size: 13px;'>&nbsp; &nbsp; {alt2$battery_range_year8} miles<br>&nbsp; &nbsp; (Battery Health: {alt2$battery_health_year8}) </span><br>
        <b><span style='font-size: 13px;'><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt2$price)}</span><br>
      </div>
    "
    )),
    HTML(glue(
      "
      <div style='text-align: left;'>
        <b style='position: absolute; top: 5px; left: 5px; margin: 0; padding: 0;'>Option 3</b><br>
        <b><span style='font-size: 13px;'><u>Mileage:</u></span></b><br> <span style='font-size: 13px;'>{scales::comma(alt3$mileage)}</span><br>
        <b><span style='font-size: 13px;'><u>Battery condition:</u></span></b><br> <span style='font-size: 13px;'>{alt3$battery_condition} </span><br>
        <b><span style='font-size: 13px;'><u>Range on a full charge:</u></span></b><br>
        <b><span style='font-size: 13px;'>&nbsp; &nbsp; Current:</span></b><br> <span style='font-size: 13px;'>&nbsp; &nbsp; {alt3$battery_range_year3} miles<br>&nbsp; &nbsp; (Battery Health: {alt3$battery_health_year3}) </span><br>
        <b><span style='font-size: 13px;'>&nbsp; &nbsp; Expected in 5 years:</span></b><br> <span style='font-size: 13px;'>&nbsp; &nbsp; {alt3$battery_range_year8} miles<br>&nbsp; &nbsp; (Battery Health: {alt3$battery_health_year8}) </span><br>
        <b><span style='font-size: 13px;'><u>Purchase price:</u></span></b><br> <span style='font-size: 13px;'>$ {scales::comma(alt3$price)}</span><br>
      </div>
    "
    )),
    HTML(glue(
      "
      <div style='text-align: left;'>
        <b style='position: absolute; top: 5px; left: 5px; margin: 0; padding: 0;'>Option 4</b><br>
        <b><span style='font-size: 13px;'>Even if these were </span></b><br>
        <b><span style='font-size: 13px;'>my best options,  </span></b><br>
        <b><span style='font-size: 13px;'>I would not choose any</span></b><br>
        <b><span style='font-size: 13px;'>of these vehicles</span></b><br>
      </div>
    "
    ))
  )
  return(options)
}


# Server setup
server <- function(input, output, session) {

  survey$range[is.na(survey$range)] <- ''
  respondentID <- sample(survey$respID, 1)
  battery_respondentID <- sample(battery_survey$respID, 1)

  ### Prolific Setup ###

  # Obtain parameters from the URL
  url_pars <- reactive({
    sd_get_url_pars()
  })

  # Get Prolific IDs from URL parameters
  prolific_pid <- reactive({
    url_pars()["PROLIFIC_PID"]  # Participant ID
  })

  study_id <- reactive({
    url_pars()["STUDY_ID"]  # Study ID (optional)
  })


  prolific_session_id <- reactive({
    url_pars()["SESSION_ID"]  # Session ID (optional)
  })

  # Store Prolific IDs in your database
  sd_store_value(prolific_pid(), "prolific_pid")
  sd_store_value(study_id(), "study_id")
  sd_store_value(prolific_session_id(), "prolific_session_id")

  # # Create completion redirect URL
  # completion_url <- reactive({
  #   paste0("https://app.prolific.com/submissions/complete?cc=",completion_code)
  # })


  # Create completion redirect URL - using Prolific code
  completion_url <- reactive({
    paste0("https://app.prolific.com/submissions/complete?cc=",completion_code)
  })



  # Create screenout redirect URL - using Prolific code
  screenout_url <- reactive({
    paste0("https://app.prolific.com/submissions/complete?cc=",screenout_code)
  })

  # Redirect buttons for Prolific
  sd_redirect(
    id = "redirect_complete",
    url = completion_url(),
    button = TRUE,
    label = "Complete and return to Prolific"
  )

  sd_redirect(
    id = "redirect_screenout",
    url = screenout_url(),
    button = TRUE,
    label = "Back to panel"
  )

  ### End of Prolific Setup ###
  # Make a 10-digit random number completion code
  completion_code <- sd_completion_code(10)

  # Create random assignment when session starts
  prime_groups <- c('prime_short', 'prime_long')
  prime_group_label <- sample(c('prime_short', 'prime_long'), 1)
  names(prime_groups) <- prime_group_label

  # Store necessary values in the survey data
  sd_store_value(completion_code)
  #sd_store_value(psid(), "psid")
  sd_store_value(respondentID, "respID")
  sd_store_value(battery_respondentID, "battery_respID")
  sd_store_value(prime_group_label, "prime_group_label")

  ## data

  df <- survey %>%
    filter(respID == respondentID)


  battery_df <- battery_survey |>
    filter(respID == battery_respondentID)

  df_filtered <- reactive({
    # Try to get vehicle style from input first
    vehicle_style <- input$next_veh_style
    vehicle_budget <- input$next_veh_budget

    #If not available, try from stored data
    if (is.null(vehicle_style)) {
      stored_data <- isolate(sd_get_data(db))
      if (!is.null(stored_data) && "next_veh_style" %in% names(stored_data)) {
        style_values <- stored_data$next_veh_style
        valid_styles <- style_values[!is.na(style_values)]
        if (length(valid_styles) > 0) {
          vehicle_style <- valid_styles[length(valid_styles)]
        }
      }
    }
    # If not available, try from stored data
    if (is.null(vehicle_budget)) {
      stored_data <- isolate(sd_get_data(db))
      if (!is.null(stored_data) && "next_veh_budget" %in% names(stored_data)) {
        budget_values <- stored_data$next_veh_budget
        valid_budgets <- budget_values[!is.na(budget_values)]
        if (length(valid_budgets) > 0) {
          vehicle_budget <- valid_budgets[length(valid_budgets)]
        }
      }
    }

    req(vehicle_style) # ensures we have a vehicle style
    req(vehicle_budget) # ensures we have a budget

df %>%
  {
    if (vehicle_style == "Car / sedan / hatchback") {
      filter(., vehicle_type == "car")
    } else {
      filter(., vehicle_type == "suv")
    }
  } %>%
  {
    if (vehicle_budget %in% c("5000", "10000", "15000", "20000")) {
      filter(., budget == "low")
    } else {
      filter(., budget == "high")
    }
  }

})

  df_battery_filtered <- reactive({
    # Try to get vehicle style from input first
    vehicle_style <- input$next_veh_style
    vehicle_budget <- input$next_veh_budget

    #If not available, try from stored data
    if (is.null(vehicle_style)) {
      stored_data <- isolate(sd_get_data(db))
      if (!is.null(stored_data) && "next_veh_style" %in% names(stored_data)) {
        style_values <- stored_data$next_veh_style
        valid_styles <- style_values[!is.na(style_values)]
        if (length(valid_styles) > 0) {
          vehicle_style <- valid_styles[length(valid_styles)]
        }
      }
    }
    # If not available, try from stored data
    if (is.null(vehicle_budget)) {
      stored_data <- isolate(sd_get_data(db))
      if (!is.null(stored_data) && "next_veh_budget" %in% names(stored_data)) {
        budget_values <- stored_data$next_veh_budget
        valid_budgets <- budget_values[!is.na(budget_values)]
        if (length(valid_budgets) > 0) {
          vehicle_budget <- valid_budgets[length(valid_budgets)]
        }
      }
    }

    req(vehicle_style) # ensures we have a vehicle style
    req(vehicle_budget) # ensures we have a budget

    battery_df %>%
      {
        # Determine vehicle type
        if (vehicle_style == "Car / sedan / hatchback") {
          temp <- filter(., vehicle_type == "car")
        } else {
          temp <- filter(., vehicle_type == "suv")
        }

        # Apply budget filter
        if (vehicle_budget %in% c("5000", "10000", "15000", "20000")) {
          filter(temp, budget == "low")
        } else {
          filter(temp, budget == "high")
        }
      }
  })

  chosen_input <- reactive({
    # First try current input
    selected <- input$next_veh_car_images %||% input$next_veh_suv_images

    if (!is.null(selected)) {
      return(paste0('images/car-images/', selected, '.png'))
    }

    # Fallback: get from current session data
    stored_data <- isolate(sd_get_data(db))
    session_id <- session$userData$all_data$session_id
    session_data <- stored_data[stored_data$session_id == session_id, ]

    # Try car images first, then SUV images
    car_image <- session_data$next_veh_car_images[
      !is.na(session_data$next_veh_car_images)
    ]
    suv_image <- session_data$next_veh_suv_images[
      !is.na(session_data$next_veh_suv_images)
    ]

    selected <- if (length(car_image) > 0) {
      car_image[length(car_image)]
    } else {
      suv_image[length(suv_image)]
    }

    paste0('images/car-images/', selected, '.png')
  })

  # Vehicle DCE -- Button Format
  observe(
    {


      df_vehicle <- df_filtered()

      # Run observer that updates the chosen_image when an image is chosen

      output$make_table_short <- create_car_table_short(chosen_input())

      # Create the options for each choice question
      vehicle_cbc0_options <- vehicle_cbc_options(demo_options)
      vehicle_cbc1_options <- vehicle_cbc_options(
        df_vehicle |> filter(qID == 1)
      )
      vehicle_cbc2_options <- vehicle_cbc_options(
        df_vehicle |> filter(qID == 2)
      )
      vehicle_cbc3_options <- vehicle_cbc_options(
        df_vehicle |> filter(qID == 3)
      )
      vehicle_cbc4_options <- vehicle_cbc_options(
        df_vehicle |> filter(qID == 4)
      )
      vehicle_cbc5_options <- vehicle_cbc_options(
        df_vehicle |> filter(qID == 5)
      )
      vehicle_cbc6_options <- vehicle_cbc_options(
        df_vehicle |> filter(qID == 6)
      )

      # Create each choice question - display these in your survey using sd_output()
      # Example: sd_output('cbc_q1', type = 'question')

      sd_question(
        type = 'mc_buttons',
        id = 'vehicle_cbc_q0_button',
        label = "If these were your only options, which would you choose?",
        option = vehicle_cbc0_options,
        width = "100%",
        direction = "horizontal"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'vehicle_cbc_q1_button',
        label = "If these were your only options, which would you choose?",
        option = vehicle_cbc1_options,
        width = "100%",
        direction = "horizontal"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'vehicle_cbc_q2_button',
        label = "If these were your only options, which would you choose?",
        option = vehicle_cbc2_options,
        width = "100%",
        direction = "horizontal"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'vehicle_cbc_q3_button',
        label = "If these were your only options, which would you choose?",
        option = vehicle_cbc3_options,
        width = "100%",
        direction = "horizontal"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'vehicle_cbc_q4_button',
        label = "If these were your only options, which would you choose?",
        option = vehicle_cbc4_options,
        width = "100%",
        direction = "horizontal"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'vehicle_cbc_q5_button',
        label = "If these were your only options, which would you choose?",
        option = vehicle_cbc5_options,
        width = "100%",
        direction = "horizontal"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'vehicle_cbc_q6_button',
        label = "If these were your only options, which would you choose?",
        option = vehicle_cbc6_options,
        width = "100%"
      )
    }
  )

  # Battery DCE -- Button Format
  observe(
    {


      battery_df <- df_battery_filtered()

      # Create the options for each choice question
      battery_cbc1_options <- battery_cbc_options(
        battery_df |> filter(qID == 1)
      )
      battery_cbc2_options <- battery_cbc_options(
        battery_df |> filter(qID == 2)
      )
      battery_cbc3_options <- battery_cbc_options(
        battery_df |> filter(qID == 3)
      )
      battery_cbc4_options <- battery_cbc_options(
        battery_df |> filter(qID == 4)
      )
      battery_cbc5_options <- battery_cbc_options(
        battery_df |> filter(qID == 5)
      )
      battery_cbc6_options <- battery_cbc_options(
        battery_df |> filter(qID == 6)
      )

      # Create each choice question - display these in your survey using sd_output()

      sd_question(
        type = 'mc_buttons',
        id = 'battery_cbc_q1_button',
        label = "If these were your only options, which would you choose?",
        option = battery_cbc1_options,
        width = "100%",
        direction = "horizontal"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'battery_cbc_q2_button',
        label = "If these were your only options, which would you choose?",
        option = battery_cbc2_options,
        width = "100%"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'battery_cbc_q3_button',
        label = "If these were your only options, which would you choose?",
        option = battery_cbc3_options,
        width = "100%"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'battery_cbc_q4_button',
        label = "If these were your only options, which would you choose?",
        option = battery_cbc4_options,
        width = "100%"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'battery_cbc_q5_button',
        label = "If these were your only options, which would you choose?",
        option = battery_cbc5_options,
        width = "100%"
      )

      sd_question(
        type = 'mc_buttons',
        id = 'battery_cbc_q6_button',
        label = "If these were your only options, which would you choose?",
        option = battery_cbc6_options,
        width = "100%"
      )
    }
  )

  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if(
    # Screen out if the respondent doesn't have valid start
    #!is_valid_start() ~ "screenout", # Fix it

    input$next_veh_when %in% c("24", "not_sure") ~ "screenout",
    input$next_veh_market %in% c("new") ~ "screenout",
    input$next_veh_style %in% c("van", "truck", "other") ~ "screenout",
    input$attention_check_toyota %in% c("yes_current", "yes_past", "no") ~
      "screenout",
    # input$attitudes_2_a_attention_check_agree %in%
    #   c("strongly_disagree", "somewhat_disagree", "neutral", "somewhat_agree") ~
    #   "screenout",

    input$household_veh_count == "0" ~ "next_veh_info",

    !is.null(input$next_veh_car_images) ~ "cbc_intro",

    input$next_veh_style == "SUV / crossover" &
      ((!is.null(input$next_veh_nobev)) |
        (input$next_veh_fuel_new_bev %in%
          c("neutral", "somewhat_likely", "very_likely") |
          input$next_veh_fuel_used_bev %in%
            c("neutral", "somewhat_likely", "very_likely"))) ~
      "next_veh_style_suv"
  )

  sd_show_if(
    !is.null(input$completion_code) ~ "attention_check_toyota",

    input$household_veh_count != "0" ~ "household_veh_fuel",

    !(input$household_veh_count == "1" &
      length(input$household_veh_fuel) == 1) ~
      "primary_veh_fuel",

    input$household_veh_count != "0"   ~ "primary_veh_mpg",

    input$primary_veh_obtain_how %in%
      c(
        "bought_dealership",
        "leased_dealership",
        "bought_private",
        "bought_online"
      ) ~ "primary_veh_cost",


    input$next_veh_fuel_new_bev %in%
      c("very_unlikely", "somewhat_unlikely") &
      input$next_veh_fuel_used_bev %in%
        c("very_unlikely", "somewhat_unlikely") ~
      "next_veh_info_nobev",

    input$know_electric_vehicle == "yes" ~ "write_electric_name",

    prime_group_label == "prime_short" ~ "battery_prime_short",
    prime_group_label == "prime_long" ~ "battery_prime_long",

    input$battery_cbc_q1_button == 'option_4' &
      input$battery_cbc_q2_button == 'option_4' &
      input$battery_cbc_q3_button == 'option_4' &
      input$battery_cbc_q4_button == 'option_4' &
      input$battery_cbc_q5_button == 'option_4' &
      input$battery_cbc_q6_button == 'option_4' ~
      'page2a'
  )

  # Database designation and other settings
  sd_server(
    db = db
  )
}


# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = ui, server = server)
