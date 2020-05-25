library(shiny)
#library(reshape2)
#library(ggplot2)

ui <- fluidPage(
  
  # Application title
  #titlePanel("Distribution"),
  headerPanel('Distribution'),
  
  # INPUTS -------------------------------------------------------------------------
  sidebarPanel(
    fileInput(inputId = "mulColBedG", 
              label = "Select a multi-column bedGraph", 
              #multiple = FALSE, 
              #accept = NULL, 
              width = NULL),    
    numericInput(inputId = "num_signals", 
                 label = "Enter the number of signals contained", 
                 value = 0, 
                 min = 1, 
                 max = NA, 
                 step = NA, width = NULL),
    checkboxInput(inputId = "header", 
                  label = "Header?", 
                  value = FALSE, width = NULL),
    textInput(inputId = "prefix", 
              label = "Your preferred prefix for naming the signals", 
              value = 's', 
              width = NULL,
              placeholder = NULL),
    numericInput(inputId = "percentile", 
                 label = "Percentile", 
                 value = 100, min = NA, max = 100, 
                 step = 0.01,
                 width = NULL),
    checkboxInput(inputId = "enrichment", 
                  label = "Enrichment?", 
                  value = FALSE, width = NULL),
    checkboxInput(inputId = "nozero", 
                  label = "Remove zeros?", 
                  value = FALSE, width = NULL),
    selectInput(inputId = "plots", 
                label = "Plots", 
                choices = c("Distribution curve" = "curve",
                            "Empirical cumulative distribution" = "ecdf",
                            "Box plot" = "box"), 
                #selected = NULL, 
                #multiple = FALSE,
                selectize = TRUE, 
                width = NULL, size = NULL),
    actionButton(inputId = "go", label = "GO!")
  ),
  # END INPUTS ----------------------------------------------------------------------
  
  # OUTPUTS ------------------------------------------------------------------------
  mainPanel(
    plotOutput(outputId = 'curve', width = "100%", height = "400px", click = NULL,
               dblclick = NULL, hover = NULL, hoverDelay = NULL,
               hoverDelayType = NULL, brush = NULL, clickId = NULL,
               hoverId = NULL, inline = FALSE)
  )
)

# LOAD LIBRARIES HERE
library(ggplot2)
library(ggridges)
library(parallel)
options(shiny.maxRequestSize=100*1024^2)

wideToLong <- function(df_wide, percentile = 100, enrichment = FALSE, nozero = FALSE){
  
  #df_wide <- df_mergedBedGraph_wide
  
  suppressWarnings(df_long <- read.csv('', col.names = c('variable', 'value'),
                                       header = FALSE, sep = ' '))
  
  for(i in 1:ncol(df_wide)){
    temp <- df_wide[ ,i]
    if(percentile != 100){
      temp <- temp[temp < quantile(temp, c(percentile*0.01))]
    }
    if(enrichment){
      temp <- temp/mean(temp)
    }
    if(nozero){
      temp <- temp[which(temp!= 0)]
    }
    
    df_long <- rbind(df_long, data.frame('variable' = rep(colnames(df_wide)[i], length(temp)), 'value' = temp))
    rm(temp)
  }
  
  return(df_long)
}


server <- function(input, output) {
  
  #data <- reactiveValues(data = NULL)
  
  # output$
  mydata  <- eventReactive(input$go,{
    numCores <- detectCores()
    #df_signals <- input$path_mulColBedG
    print(input$mulColBedG)
    print(input$mulColBedG$datapath)
    df_signals <- read.csv(input$mulColBedG$datapath, header = input$header, sep = "\t", quote = "",
                           col.names = c('chr', 'start', 'end', paste(input$prefix, 1:input$num_signals, sep='')))[ ,(4:(3+input$num_signals))]
    print(head(df_signals))
    df_signals <- wideToLong(df_signals, percentile = input$percentile,
                            enrichment = input$enrichment, nozero = input$nozero)
    
#    list_signals <- mclapply(df_signals, FUN= function(col, per, en, nozero){
#      print("head(col):")
#      print(head(col))
#      col <- col[!is.na(col)]
#      print(head(col))
#      if(per != 100){
#        col <- col[col < quantile(col, c(per*0.01))]
#      }
#      if(en){
#        col <- col/mean(col, na.rm=TRUE)
#      }
#      if(nozero){
#        col <- col[which(col!= 0)]
#      }
#      return(col)
#    }, input$percentile, input$enrichment, input$nozero, mc.cores = numCores)
    
    print("*")
    print(head(df_signals))
    
    #df_signals  <- read.csv('', col.names = c('variable', 'value'),header = FALSE, sep = ' ')
    #for(i in 1:length(list_signals)){
    #  df_signals <- rbind(df_signals,
    #                      data.frame('variable' = rep(names(list_signals)[i], length(list_signals[[i]])),
    #                                 'value' = list_signals[[i]]))
    #}
    #head(df_signals)
    return(df_signals)
  })
  
#  observe({ print(input$go) })
  
  #output$curve <- renderPlot()
  output$curve <- renderPlot({
    if(!is.null(mydata())) {
      head(mydata())
      #ggplot(mydata(), aes(a,b))+geom_point()
      ggplot(mydata(), aes(x=value, y=variable, fill=variable)) +
        geom_density_ridges2(scale=0.9) + # rel_min_height = 0.01
        #labs(fill = "", x= 'mean enrichment' , y= 'feature' )+
        theme_classic(base_size = 21)
    }
  })
  
  
}

shinyApp(ui = ui, server = server)