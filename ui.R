#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinythemes)
library(shinyBS)
library(readr)
library(dplyr)
library(DT)

options(shiny.trace = TRUE)

sparsity_data <- read_csv("data/sparsity_data_2020.csv", 
                                                col_types = cols(ncesid = col_character()))

#### Define the UI ####
shinyUI(
    fluidPage(
        #theme = shinytheme("cosmo"),
        titlePanel("A Single Weight for Sparsity"),
        sidebarLayout(
            sidebarPanel(
              helpText("Choose the number of students per square mile that defines a sparsely populated district."),
              sliderInput("enroll_psm", "Enrollment per square mile:", 1, 100, 50),
              h5("New Aid Total ($):", textOutput("new_aid_total")),
              h5("weighted SR", textOutput("weighted_sr")),
              helpText("Choose a weight that will apply to each student attending a sparsely populated district."),
              numericInput("single_weight", label = "Single Weight for Sparsity", min = 0.05, max = 0.20, 
                             step = 0.02, value = 0.1),
              br(),
              h3("Impact to Overall Budget"),
              h5("Current Budget: ", textOutput("budget")),
              h5("Balance: ", textOutput("balance")),
            ),
            mainPanel(
                DT::dataTableOutput("prettyTable"),
            )
        )
    )
)