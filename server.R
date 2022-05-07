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
      
      updateSelectInput(session, "ColourBy", choices = dataColors())
      df = dataInput()
      
      layer1 = reactive({
        labs = colnames(df)[dim(df)[2]-1]
        df %>% 
          filter(.axisIndex == 0) %>%
          dplyr::distinct(.ri, across(any_of(labs)), .keep_all = TRUE)
      })
      
      layer2 = reactive({
        labs = colnames(df)[dim(df)[2]]
        df %>%
          filter(.axisIndex == 1) %>%
          dplyr::distinct(.ci, across(any_of(labs)), .keep_all = TRUE)
      })
      
      scores = reactive({
        layer1() %>%
          reshape2::acast(.ci ~ .ri, value.var = ".y") %>%
          scale() %>%
          as.data.frame()
      })
      
      loadings = reactive({
        layer2() %>%
          reshape2::acast(.ri ~ .ci, value.var = ".y") %>%
          #t() %>%
          scale() %>%
          #t() %>%
          as.data.frame()
        
      })
      
      scores2plot = reactive({
        S = scores()
        cols = colnames(S)
        S %>%
          select( X = any_of(cols[input$X.comp]), Y = any_of(cols[input$Y.comp]))
      })
      
      loadings2plot = reactive({
        L = loadings()
        cols = colnames(L)
        L %>%
          select( X = any_of(cols[input$X.comp]), Y = any_of(cols[input$Y.comp]))
      })
      
      plotColors = reactive({
        layer1() %>%
          mutate(ri = as.factor(.ri)) %>%
          filter(ri == levels(ri)[1]) %>%
          select(clr = all_of(input$ColourBy))
      })
      
      scoreLabs = reactive({
        ldf = layer1()
        cols = colnames(ldf)
        ldf = ldf %>%
          mutate(ri = as.factor(.ri)) %>%
          filter(ri == levels(ri)[1]) %>%
          select(lbs = all_of(cols[length(cols)-1]))
        
      })
      
      nComp = dim(scores())[2]
      updateNumericInput(session, "X.comp", max = nComp)
      updateNumericInput(session, "Y.comp", max = nComp)
      
      output$sp <- renderPlotly({
        if(input$ColourBy != ""){
          fig = scores2plot() %>%
            bind_cols(plotColors()) %>%
            bind_cols(scoreLabs()) %>%
            plot_ly( x = ~X, y = ~Y, color = ~ clr, text = ~ lbs) %>%
            add_markers(marker = list(size = input$scsize))
          ldf = loadings2plot()
          fig %>%  
            add_markers(x=ldf$X, y = ldf$Y, inherit = FALSE, showlegend = FALSE, marker = list(size = 4, color = "gray"))
          
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
  if(length(ctx$labels)) df = df %>% bind_cols(ctx$select(ctx$labels))
  
  return(df)
}

getColors <- function(session){
  ctx <- getCtx(session)
  ctx$colors %>%
    unlist()
}







