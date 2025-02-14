# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak(c(
#   'surveydown-dev/surveydown', # <- Development version from github
#   'here',
#   'glue',
#   'readr',
#   'dplyr',
#   'kableExtra'
# ))

# Load packages
library(surveydown)
library(dplyr)
library(readr)
library(glue)
library(here)
library(kableExtra)


# Database setup --------------------------------------------------------------
#
# Details at: https://surveydown.org/manuals/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# For this demo, we set ignore = TRUE in the following code, which will ignore
# the connection settings and won't attempt to connect to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.

db <- sd_db_connect(
  ignore = TRUE
)


# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {

  # Make a 10-digit random number completion code
  completion_code <- sd_completion_code(10)

  # Store the completion code in the survey data
  sd_store_value(completion_code)

  # Read in the full survey design file
  design <- read_csv(here("data", "choice_questions.csv"))

  # Sample a random respondentID and store it in your data
  respondentID <- sample(design$respID, 1)
  sd_store_value(respondentID, "respID")

  # Filter for the rows for the chosen respondentID
  df <- design %>%
    filter(respID == respondentID) %>%
    # Paste on the "images/" path (images are stored in the "images" folder)
    mutate(image = paste0("images/", image))

  # Function to create the options table for a given choice question
  make_cbc_table <- function(df) {
    alts <- df %>%
      mutate(
        price = paste(scales::dollar(price), "/ lb"),
        image = paste0('<img src="', image, '" width=100>')) %>%
      # Make nicer attribute labels
      select(
        `Option:` = altID,
        ` ` = image,
        `Price:` = price,
        `Type:` = type,
        `Freshness:` = freshness)
    row.names(alts) <- NULL # Drop row names

    table <- kbl(t(alts), escape = FALSE) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE,
        position = "center"
      )
    function() { table }
  }

  create_car_table <- function() {
    html_table <- '
      <style>
    table {
      border-collapse: collapse;
      width: 100%;
      max-width: 800px;
      margin: 20px 0;
      font-family: Arial, sans-serif;
    }

    th, td {
      padding: 12px;
      text-align: left;
      border: 1px solid #ddd;
    }

    th {
      background-color: #4CAF50;
      color: white;
    }

    tr:nth-child(even) {
      background-color: #f2f2f2;
    }

    tr:hover {
      background-color: #ddd;
    }
  </style>
  <table border="1">
    <thead>
      <tr>
        <th>Car Model</th>
        <th>Range (miles)</th>
        <th>Price ($)</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Tesla Model 3</td>
        <td>272</td>
        <td>38,990</td>
      </tr>
      <tr>
        <td>Chevrolet Bolt EV</td>
        <td>259</td>
        <td>26,500</td>
      </tr>
    </tbody>
  </table>
  '
    #return(html_table)
    function() { html_table }
  }

  create_car_table1 <- function(car_data) {

    car_data <- car_data %>%
      mutate(
        price = paste(scales::dollar(price), "/ lb"),
        image = paste0('<img src="', image, '" width=100>'))

    # car_data should be a data frame with columns: model, range, price

    # Create the header part of the table
    html_table <- '
    <style>
        table {
            border-collapse: collapse;
            width: 100%;
            max-width: 800px;
            margin: 20px 0;
            font-family: Arial, sans-serif;
        }

        th, td {
            padding: 12px;
            text-align: left;
            border: 1px solid #ddd;
        }

        th {
            background-color: #4CAF50;
            color: white;
        }

        tr:nth-child(even) {
            background-color: #f2f2f2;
        }

        tr:hover {
            background-color: #ddd;
        }
    </style>
    <table>
        <thead>
            <tr>
                <th>Car Image</th>
                <th>Car Model</th>
                <th>Range (miles)</th>
                <th>Price ($)</th>
            </tr>
        </thead>
        <tbody>'

    # Add rows for each car in the data frame
    for(i in 1:nrow(car_data)) {
      row <- sprintf('
            <tr>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>
            </tr>',
                     car_data$image[i],
                     car_data$type[i],
                     car_data$freshness[i],
                     formatC(car_data$price[i], format="f", big.mark=",", digits=0)
      )
      html_table <- paste0(html_table, row)
    }

    # Close the table
    html_table <- paste0(html_table, '
        </tbody>
    </table>')

    function() { html_table }
  }

  # Example usage:

  # table_function <- create_car_table(car_data)
  # table_function()
  df_temp <- df |> filter(qID == 1)

  output$make_table1 <-create_car_table()
  output$make_table2 <-create_car_table1(df_temp)

  # Create the options for each choice question
  output$cbc1_table <- make_cbc_table(df |> filter(qID == 1))
  output$cbc2_table <- make_cbc_table(df |> filter(qID == 2))
  output$cbc3_table <- make_cbc_table(df |> filter(qID == 3))
  output$cbc4_table <- make_cbc_table(df |> filter(qID == 4))
  output$cbc5_table <- make_cbc_table(df |> filter(qID == 5))
  output$cbc6_table <- make_cbc_table(df |> filter(qID == 6))

  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if(
    input$screenout == "blue" ~ "end_screenout",
    input$consent_age == "no" ~ "end_consent",
    input$consent_understand == "no" ~ "end_consent"
  )

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if(
    input$like_fruit %in% c("yes", "kind_of") ~ "fav_fruit"
  )

  # Database designation and other settings
  sd_server(
    db = db,
    all_questions_required = FALSE
  )

}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
