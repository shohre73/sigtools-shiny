library(shiny)
#library(reshape2)
#library(ggplot2)

ui <- fluidPage(
  
  # Application title
  #titlePanel("Distribution"),
  headerPanel('Distribution'),
  
  # INPUTS -------------------------------------------------------------------------
  sidebarPanel(
    fileInput(inputId = "path_mulColBedG", 
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

server <- function(input, output) {
  
  # output$
  table <- renderDataTable({
    df_mergedBedGraph_wide <- read.csv(input$path_mulColBedG, 
                                       header = input$header, 
                                       sep = '\t',
                                       col.names = c('chr', 'start', 'end', paste(input$prefix, 1:input$num_signals, sep='')))[ ,(4:(3+input$num_signals))]
  })
  
  
  #output$curve <- renderPlot()
  renderPlot({
    title <- "100 random normal values"
    hist(rnorm(100), main = title)
  })
  
}

shinyApp(ui = ui, server = server)