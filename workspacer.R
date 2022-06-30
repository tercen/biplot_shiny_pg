library(shiny)
library(tercen)
library(plotly)
library(tidyverse)
library(reshape2)

source("ui.R")
source("server.R")

options("tercen.workflowId"= "0844af3c27bc4f1fd354e37fa800aa8e")
options("tercen.stepId"= "d61482e2-b205-4edd-9560-fc9688a94c1c")

runApp(shinyApp(ui, server))  
