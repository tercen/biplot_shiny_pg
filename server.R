library(shiny)
library(tercen)
library(plotly)
library(tidyverse)
library(reshape2)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

server <- shinyServer( 
  function(input, output, session) {
    
    dataInput <- reactive({
      getValues(session)
    })
    
    dataColors = reactive({
      getColors(session)
    })
    
    
    observe({
      if(input$ColourBy == "") {
        updateSelectInput(session, "ColourBy", choices = dataColors())
      }
      
      df = dataInput()
      
      plotColors = reactive({
        if(input$ColourBy ==""){
          clr = df %>%
            filter(.axisIndex == 0) %>%
            select(sclb) %>%
            mutate(clr = ".")
        } else {
          clr =df %>%
            filter(.axisIndex == 0) %>%
            select(sclb, clr = all_of(input$ColourBy))
        }
        clr %>%
          distinct(sclb,.keep_all = TRUE)
      })
      
      
      sx = reactive({
        df %>%
          filter(.axisIndex == 0) %>%
          filter(.ri == input$X.comp -1 & .ci ==0) %>%
          mutate(y = scale(.y)) %>%
          select(sclb, X = y)
      })
      
      sy = reactive({
        df %>%
          filter(.axisIndex == 0) %>%
          filter(.ri == input$Y.comp-1 & .ci == 0) %>%
          mutate(y = scale(.y))%>%
          select(sclb, Y = y)
      })
      
      lx = reactive({
        df %>%
          filter(.axisIndex == 1) %>%
          filter(.ci == input$X.comp-1 & .ri == 0) %>%
          mutate(y = scale(.y) %>% as.numeric()) %>%
          select(ldlb, X = y)
      })
      
      ly = reactive({
        df %>%
          filter(.axisIndex == 1) %>%
          filter(.ci == input$Y.comp-1 & .ri == 0) %>%
          mutate(y = scale(.y) %>%as.numeric) %>%
          select(ldlb, Y = y)
      })
      
      scores = reactive({
         sx() %>%
          left_join(sy(), by = "sclb") %>%
          left_join(plotColors(), by = "sclb")
      })
      
      loadings = reactive({
        lx() %>%
          left_join(ly(), by = "ldlb") %>%
          mutate(D = X^2 + Y^2) %>%
          arrange(-D) %>%
          mutate(nl = 1:n()) %>%
          filter(nl <= input$maxloadings) %>%
          select(ldlb, X, Y)
      })
      
  
      
      nComp = dim(scores())[2]
      updateNumericInput(session, "X.comp", max = nComp)
      updateNumericInput(session, "Y.comp", max = nComp)
      
      
      nVar = dim(lx())[1]

      updateSliderInput(session, "maxloadings", max = nVar, value = min(10, nVar))
      
      output$sp <- renderPlotly({
        if(TRUE){
          fig = scores() %>%
            plot_ly( x = ~X, y = ~Y, split = ~clr, text = ~ sclb) 
          ldf = loadings()
          if(input$showlines){
            fig = fig %>%
              add_segments(x = 0, xend = ldf$X, y = 0, yend = ldf$Y, line = list(color = 'gray', width = input$lnsize),inherit = FALSE, showlegend = FALSE)
          }
          if(input$showlabels){
            fig = fig %>%
              add_text(x=ldf$X, y = ldf$Y,text = ldf$ldlb, textfont = list(color = "gray", size = input$lbsize), inherit = FALSE, showlegend = FALSE)
          }
          fig %>%
            add_markers(marker = list(size = input$scsize, colorscale = "Accent"))
        }
      })
      
    })
  })

# L = df %>% filter(.axisIndex == 1) %>%
#   reshape2::acast(.ri ~ .ci, value.var = ".y")

getValues <- function(session){
  ctx <- getCtx(session)
  df <- ctx %>% 
    select(.y, .ri, .ci, .axisIndex)
  
  if (!(df %>% summarise(unique(.axisIndex)) %>% dim())[1] == 2){
    stop("Define exactly 2 layers for loadings and scores")
  }
  if (length(ctx$labels) != 2) stop("Add exactly 1 label for each layer")
  if(length(ctx$colors)) df = df %>% bind_cols(ctx$select(ctx$colors))
  basenames = colnames(df)
  if(length(ctx$labels)) df = df %>% bind_cols(ctx$select(ctx$labels))
  df %>%
    setNames(c(basenames, "sclb", "ldlb"))
}

getColors <- function(session){
  ctx <- getCtx(session)
  ctx$colors %>%
    unlist()
}







