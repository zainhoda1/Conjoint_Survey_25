#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(surveydown)
library(DT)
library(tidyverse)
library(lubridate)

# Define server logic required to draw a histogram
function(input, output, session) {

  # Get the data
  # Get data reactively
  survey_data <- reactive({
    db <- sd_db_connect()
    sd_get_data(db)%>%
      #mutate(get_start_date =as.Date(as.POSIXct(time_start, tz = "UTC"))) %>%
      mutate(get_start_datetime = ymd_hms(time_start, tz = "UTC")) %>%
#<<<<<<< Updated upstream
      filter(get_start_datetime>"2025-10-14 00:00:00 UTC" )  #=======
      filter(get_start_datetime>"2025-10-10 21:00:00 UTC" )
#>>>>>>> Stashed changes
  })



  # Acceptance distribution
  output$completeTable <- renderDT({
    survey_data() %>%
      mutate(completion_status=case_when(current_page=="screenout" ~ "screenout",
                                            current_page=="end" ~ "complete",
                                            TRUE ~ "in progress"
                                            ))%>%
      mutate(completion_status = factor(completion_status, levels = c("complete", "in progress", "screenout"))) %>%

      count(completion_status, name = "sample_size") %>%
      complete(completion_status, fill = list(sample_size = 0)) %>%
      mutate(percentage=paste0(round(sample_size/sum(sample_size)*100,0),"%"))
  })


  # Age distribution (binned)
  output$ageTable <- renderDT({
    survey_data() %>%
      filter(current_page=="end") %>%
      mutate(
        birth_year = as.numeric(birth_year),
        age = 2025 - birth_year # yob changed to age
      ) %>%
      mutate(age_group = case_when(
        age <= 24 ~ "18 - 24",
        age <= 34 ~ "25 - 34",
        age <= 44 ~ "35 - 44",
        age <= 54 ~ "45 - 54",
        age <= 64 ~ "55 - 64",
        age >= 65 ~ "65+"
      )) %>%
      mutate(age_group = factor(age_group, levels = c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65+"))) %>%
      count(age_group, name = "sample_size") %>%
      complete(age_group, fill = list(sample_size = 0)) %>%
      mutate(percentage_num=round(sample_size/sum(sample_size)*100,0),
             percentage=paste0(round(sample_size/sum(sample_size)*100,0),"%")
      ) %>%
      mutate(population_target_num=c(18,17,17,18,14,16),
             population_target=paste0(population_target_num,"%")
      ) %>%
      mutate(discrepancy=paste0(percentage_num-population_target_num,"%")) %>%
      select(-c(percentage_num,population_target_num))
  })

  # Gender distribution
  output$genderTable <- renderDT({
    survey_data() %>%
      filter(current_page=="end") %>%
      mutate(gender=case_when(gender=="male" ~"male",
                              gender=="female" ~"female",
                              TRUE ~ "")) %>%
      filter(gender!="") %>%
      mutate(gender = factor(gender, levels = c("male", "female"))) %>%
      count(gender, name = "sample_size") %>%
      complete(gender, fill = list(sample_size = 0)) %>%
      mutate(percentage_num=round(sample_size/sum(sample_size)*100,0),
             percentage=paste0(round(sample_size/sum(sample_size)*100,0),"%")
      ) %>%
      mutate(population_target_num=c(49,51),
             population_target=paste0(population_target_num,"%")
      ) %>%
      mutate(discrepancy=paste0(percentage_num-population_target_num,"%")) %>%
      select(-c(percentage_num,population_target_num))
  })

  # Income distribution (binned)
  output$incomeTable <- renderDT({
    survey_data() %>%
      filter(current_page=="end") %>%
      mutate(hh_income=case_when(hh_income=="prefer_not_answer" ~ NA,
                                 TRUE ~ as.numeric(hh_income))) %>%
      filter(!is.na(hh_income)) %>%
      mutate(hh_income= case_when(hh_income<=15000 ~ "< $15,000",
                                  hh_income<=25000 ~ "$15,000 - $24,999",
                                  hh_income<=45000 ~ "$25,000 - $49,999",
                                  hh_income<=75000 ~ "$50,000 - $74,999",
                                  hh_income<=95000 ~ "$75,000 - $99,999",
                                  hh_income<=145000 ~ "$100,000 - $149,999",
                                  TRUE ~ "$150,000+"
                                  )) %>%
      mutate(hh_income = factor(hh_income, levels = c("< $15,000",
                                                   "$15,000 - $24,999",
                                                   "$25,000 - $49,999",
                                                   "$50,000 - $74,999",
                                                   "$75,000 - $99,999",
                                                   "$100,000 - $149,999",
                                                   "$150,000+"))) %>%
      count(hh_income, name = "sample_size") %>%
      complete(hh_income, fill = list(sample_size = 0)) %>%
      mutate(percentage_num=round(sample_size/sum(sample_size)*100,0),
             percentage=paste0(round(sample_size/sum(sample_size)*100,0),"%")
      ) %>%
      mutate(population_target_num=c(12,10,23,18,12,13,11),
             population_target=paste0(population_target_num,"%")
      ) %>%
      mutate(discrepancy=paste0(percentage_num-population_target_num,"%")) %>%
      select(-c(percentage_num,population_target_num))

  })

  # Disconnect when session ends
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })


}
