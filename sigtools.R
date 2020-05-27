## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggridges)

options(shiny.maxRequestSize=100*1024^2)

ui <- dashboardPage(
  # START dashboardHeader -----------------------
  dashboardHeader(title = "Sigtools"), # END dashboardHeader -------------------------
  # START dashboardSidebar ----------------------
  dashboardSidebar(
    # START sidebarMenu ---------------
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("About", tabName = "about", icon = icon("info"))
    ) # END sidebarMenu -----------------
  ), # END dashboardSidebar-------------------------
  # START dashboardBody -------------------------
  dashboardBody(
    # START tabItems ------------------
    tabItems(
      # START tabItem -------
      tabItem(tabName = "home",
        # START fluidRow      
        fluidRow(
          box(title = "Input Data", width = 6, solidHeader = FALSE, status = "primary", 
              fluidRow(
                column(12,
                  fileInput(inputId = "mulColBedG", label = "Select a multi-column bedGraph")
                ) # END column
              ), # END fluidRow
              fluidRow(
                column(3, 
                  textInput(inputId = "prefix", label = "Prefix", value = 's', 
                    width = NULL, placeholder = NULL),
                ), # END column
                column(3,
                  checkboxInput(inputId = "header", label = "Header?", 
                    value = FALSE, width = NULL)
                ) # END column                
              ), # END fluidRow 
              fluidRow(
                column(4, 
                  actionButton(inputId = "btn_loadData", label = "Load Data")
                )
              ) # END fluidRow
          ) # END box
        ), # END fluidRow -
        fluidRow(
          box(title = "Distribution", width = 12, solidHeader = FALSE, status = "primary",
            sidebarPanel(
              actionButton(inputId = "go_dist", label = "GO!")
            ), # END sidebarPanel
            mainPanel(
              plotOutput(outputId = 'plot_dist', width = "100%", height = "400px", click = NULL,
                dblclick = NULL, hover = NULL, hoverDelay = NULL,
                hoverDelayType = NULL, brush = NULL, clickId = NULL,
                hoverId = NULL, inline = FALSE)
            ) # END mainPanel
          ) # END box
        ) # END fluidRow
      ), # END tabItem ---------
      # START tabItem -------
      tabItem(
        tabName = "about",
        # START fluidRow      
        fluidRow(
          box( title = "Sigtools", status = "info", 
          width = 12,
          "Sigtools is an R-based visualization package, 
          designed to enable the users with limited programming experience to produce statistical plots of continuous genomic data. 
          It consists of several statistical visualizations that provide insights regarding the behavior of a group of signals in large regions
          – such as a chromosome or the whole genome – as well as visualizing them around a specific point or short region."
          #p("This is content. The background color is set to light-blue")
          #includeMarkdown("www/README.md")
          )
        ) # END fluidRow -
      ) # END tabItem ---------
    ) # END tabItems --------------------
  ) # END dashboardBody ---------------------------
)

wideToLong <- function(df_wide, percentile = 100, enrichment = FALSE, nozero = FALSE){
  #df_wide <- df_mergedBedGraph_wide
  suppressWarnings(df_long <- read.csv('', col.names = c('variable', 'value'), header = FALSE, sep = ' '))
  for(i in 1:ncol(df_wide)){
    temp <- df_wide[ ,i]
    if(percentile != 100){temp <- temp[temp < quantile(temp, c(percentile*0.01))]}
    if(enrichment){temp <- temp/mean(temp)}
    if(nozero){temp <- temp[which(temp!= 0)]}
    df_long <- rbind(df_long, data.frame('variable' = rep(colnames(df_wide)[i], length(temp)), 'value' = temp))
    rm(temp)
  }
  return(df_long)
}

server <- function(input, output){ 
  
#  myData <- eventReactive(input$btn_loadData, {
#    print(":P")
#    rnorm(100)
#  })
#  output$plot_dist <- renderPlot({ hist(myData())})
 
  data_signals  <- eventReactive(input$btn_loadData,{
    print("Loading Data...")
    df_signals <- read.csv(input$mulColBedG$datapath, header = input$header, sep = "\t", quote = "")
    num_signals <- ncol(df_signals)-3
    colnames(df_signals) <- c('chr', 'start', 'end', paste(input$prefix, 1:num_signals, sep=''))
    df_signals <- df_signals[ ,(4:(3+num_signals))]
    
#    list_signals <- lapply(df_signals, FUN= function(col, per, en, nozero){
#      col <- col[!is.na(col)]
#      print(head(col))
#      if(per != 100){col <- col[col < quantile(col, c(per*0.01))]}
#      if(en){col <- col/mean(col, na.rm=TRUE)}
#      if(nozero){col <- col[which(col!= 0)]}
#      return(col)
#    }, input$percentile, input$enrichment, input$nozero)
    
    df_signals <- wideToLong(df_signals)
    return(df_signals)
  }) # END eventReactive
  
  output$plot_dist <- renderPlot({ 
    ggplot(data_signals(), aes(x=value, y=variable, fill=variable)) +
      geom_density_ridges2(scale=0.9) + # rel_min_height = 0.01
      #labs(fill = "", x= 'mean enrichment' , y= 'feature' )+
      theme_classic(base_size = 21)
  })
  
}

shinyApp(ui, server)