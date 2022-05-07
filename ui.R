library(shiny)
library(plotly)
library(tidyr)

ui = shinyUI(fluidPage(
  
  titlePanel("Bipllot"),
  
  sidebarPanel(
    numericInput("X.comp", "X component", value =1, min = 1, max = 5, step = 1),
    numericInput("Y.comp", "Y component",  value = 2, min = 1, max = 5, step = 1),
    selectInput("ColourBy", "Colour markers by", choices = ""),
    sliderInput("scsize", "Marker Size",min = 0, max = 24, step = 2, value = 10),
    width = 3
  ),
  mainPanel(
    plotlyOutput("sp", height = "600px")
  )
  
))