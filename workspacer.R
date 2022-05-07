library(shiny)
library(tercen)
library(plotly)
library(tidyverse)
library(reshape2)

source("ui.R")
source("server.R")

options("tercen.workflowId"= "8f17d834dda49eba43ac822ed600aa7b")
options("tercen.stepId"= "99f3f30b-9f0e-4ea8-aba9-c611c2248aef")

runApp(shinyApp(ui, server))  
