library(shiny)
library(tercen)
library(plotly)
library(tidyverse)
library(reshape2)

source("ui.R")
source("server.R")

options("tercen.workflowId"= "88d0de31468a3f46080a3d475200b1fb")
options("tercen.stepId"= "662d213f-85c6-4d76-8e27-e60a51c61874")

runApp(shinyApp(ui, server))  
