library(shiny)
library(plotly)
library(tidyr)

ui = shinyUI(fluidPage(

  sidebarPanel(
    numericInput("X.comp", "X component", value =1, min = 1, max = 5, step = 1),
    numericInput("Y.comp", "Y component",  value = 2, min = 1, max = 5, step = 1),
    wellPanel(
      selectInput("ColourBy", "Colour score markers by", choices = ""),
      sliderInput("scsize", "Marker Size",min = 0, max = 24, step = 2, value = 10),
    ),
    wellPanel(
      checkboxInput("showlines","Show loadings as lines", TRUE),
      sliderInput("lnsize", "Line Size", min = 0, max = 5, step =  .5, value = 1),
      checkboxInput("showlabels","Show loadings as labels", TRUE),
      sliderInput("lbsize", "Label Size", min = 0, max = 24, step=  2, value = 16),
    ),
    width = 3
  ),
  mainPanel(
    plotlyOutput("sp", height = "600px")
  )
  
))