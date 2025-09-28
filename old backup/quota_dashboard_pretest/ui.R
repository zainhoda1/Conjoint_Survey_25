#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Survey Dashboard - Summary Tables"),
      tabsetPanel(
        tabPanel("Completion Distribution", DTOutput("completeTable")),
        tabPanel(HTML("Age Distribution<br><small>(upon completion)</small>"), DTOutput("ageTable")),
        tabPanel(HTML("Gender Distribution<br><small>(upon completion)</small>"), DTOutput("genderTable")),
        tabPanel(HTML("Income Distribution<br><small>(upon completion)</small>"), DTOutput("incomeTable"))
      )
    )
