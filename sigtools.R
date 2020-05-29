## app.R ##
library(shiny)
library(shinydashboard)
library(shinycssloaders)

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
                column(9,
                  fileInput(inputId = "mulColBedG", label = "Select a multi-column bedGraph")
                ), # END column
                column(3)
              ), # END fluidRow
              fluidRow(
                column(3, 
                  textInput(inputId = "upload_prefix", label = "Prefix", value = 's', 
                    width = NULL, placeholder = NULL),
                ), # END column
                column(3,
                  checkboxInput(inputId = "upload_header", label = "Header?", 
                    value = FALSE, width = NULL)
                ) # END column                
              ), # END fluidRow 
              fluidRow(
                column(4, 
                  actionButton(inputId = "upload_btn", label = "Upload")
                )
              ) # END fluidRow
          ), # END box
          box(title = "Data", width = 6, solidHeader = FALSE, status = "primary",
            fluidRow(
              column(1), 
              column(10,
                withSpinner(
                  tableOutput(outputId = "dataTable") 
                )
              ),
              column(1)
            ) # END fluidRow
          ) # END box
        ), # END fluidRow -
        fluidRow(
          box(title = "Distribution", width = 12, solidHeader = FALSE, status = "primary",
            sidebarPanel(
              #textInput(inputId = "dist_title", label = "Plot Title:", value = "distribution", width = NULL, placeholder = NULL),
              #textInput(inputId = "dist_xAxis", label = "X-axis Title:", value = "x-axis", width = NULL, placeholder = NULL),
              #textInput(inputId = "dist_yAxis", label = "Y-axis Title:", value = "y-axis", width = NULL, placeholder = NULL),
              numericInput(inputId = "dist_percentile",  label = "Percentile", value = 100, min = NA, max = 100, step = 0.01, width = NULL),
              checkboxInput(inputId = "dist_enrichment", label = "Enrichment?", value = FALSE, width = NULL),
              checkboxInput(inputId = "dist_nozero", label = "No zeros?", value = FALSE, width = NULL),
              actionButton(inputId = "dist_go", label = "GO!"),
              hr(),
              uiOutput(outputId = "dist_controls")
            ), # END sidebarPanel
            mainPanel(
              withSpinner(
                plotOutput(outputId = 'dist_plot', width = "100%", height = "400px", click = NULL,
                  dblclick = NULL, hover = NULL, hoverDelay = NULL,
                  hoverDelayType = NULL, brush = NULL, clickId = NULL,
                  hoverId = NULL, inline = FALSE)
              )
            ) # END mainPanel
          ) # END box
        ), # END fluidRow
        fluidRow(
          box(title = "Autocorrelation", width = 12, solidHeader = FALSE, status = "primary"
          ) # END box
        ), # END fluidRow
        fluidRow(
          box(title = "Correlation", width = 12, solidHeader = FALSE, status = "primary"
          ) # END box
        ), # END fluidRow
        fluidRow(
          box(title = "Aggregation", width = 12, solidHeader = FALSE, status = "primary"
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

server <- function(input, output){ 

  # INPUT DATA --------------------------------------------
  reactiveExp_signals_wide  <- eventReactive(input$upload_btn,{
    print("Loading Data...")
    df_signals <- read.csv(input$mulColBedG$datapath, header = input$upload_header, sep = "\t", quote = "")
    num_signals <- ncol(df_signals)-3
    colnames(df_signals) <- c('chr', 'start', 'end', paste(input$upload_prefix, 1:num_signals, sep=''))
    #df_signals <- df_signals[ ,(4:(3+num_signals))]
    #df_signals <- wideToLong(df_signals)
    return(df_signals)
  }) # END eventReactive
  
  # SUMMARY TABLE 
  output$dataTable <- renderTable({
    print("Gen Data Summ")
    head(reactiveExp_signals_wide())
  })

  reactiveExp_dist_data <- eventReactive(input$dist_go, {
    print("Prep Dist Data:")
    num_signals <- ncol(reactiveExp_signals_wide())-3
    df_signals <- reactiveExp_signals_wide()[ ,(4:(3+num_signals))]
    list_signals <- lapply(df_signals, FUN= function(col, per, en, nozero){
      col <- col[!is.na(col)]
      #print(head(col))
      if(per != 100){ col <- col[col < quantile(col, c(per*0.01))]}
      if(en){ col <- col/mean(col, na.rm=TRUE)}
      if(nozero){col <- col[which(col!= 0)]}
      return(col)
    }, input$dist_percentile, input$dist_enrichment, input$dist_nozero)
    
    df_signals  <- read.csv('', col.names = c('variable', 'value'),header = FALSE, sep = ' ')
    for(i in 1:length(list_signals)){
      df_signals <- rbind(df_signals,
                          data.frame('variable' = rep(names(list_signals)[i], length(list_signals[[i]])),
                                     'value' = list_signals[[i]]))
    }
    return(df_signals)
  })
  
  # DISTRIBUTION RENDER UI --------------------------------
  output$dist_controls <- renderUI({
    print("Dist controls:")
    axisName <- colnames(reactiveExp_dist_data())
    tagList(
      textInput(inputId = "dist_title", label = "Plot Title:",   value = "Distribution Curve", width = NULL, placeholder = NULL),
      textInput(inputId = "dist_xAxis", label = "X-axis Title:", value = axisName[1], width = NULL, placeholder = NULL),
      textInput(inputId = "dist_yAxis", label = "Y-axis Title:", value = axisName[2], width = NULL, placeholder = NULL),
      actionButton(inputId = "dist_plotAes", label = "Update")
    )
  }) # END renderUI
  
  reactiveExp_dist_plot <- eventReactive(input$dist_go, {
    print("Event on dist_go")
    ggplot(reactiveExp_dist_data(), aes(x=value, y=variable, fill=variable)) +
      geom_density_ridges2(scale=0.9) + # rel_min_height = 0.01
      #labs(fill = "", x= 'mean enrichment' , y= 'feature' )+
#      ggtitle(input$dist_title) +  # for the main title
#      xlab(input$dist_xAxis) + # for the x axis label
#      ylab(input$dist_yAxis) + # for the y axis label
      #labs(fill = "abc") +
      theme_classic(base_size = 21)
    #print("finished ggplot")   
  })
  
#  reactiveExp_dist_plot <- eventReactive(output$dist_plotAes, {
#    reactiveExp_dist_plot() + 
#      ggtitle(input$dist_title) +  # for the main title
#      xlab(input$dist_xAxis) + # for the x axis label
#      ylab(input$dist_yAxis)  # for the y axis label
#  })
  
  output$dist_plot <- renderPlot({ 
    print("Dist plot:")
    reactiveExp_dist_plot()
#    ggplot(reactiveExp_dist_data(), aes(x=value, y=variable, fill=variable)) +
#      geom_density_ridges2(scale=0.9) + # rel_min_height = 0.01
#     #labs(fill = "", x= 'mean enrichment' , y= 'feature' )+
#      ggtitle(input$dist_title) +  # for the main title
#      xlab(input$dist_xAxis) + # for the x axis label
#      ylab(input$dist_yAxis) + # for the y axis label
#      #labs(fill = "abc") +
#      theme_classic(base_size = 21)
#    #print("finished ggplot")    
  })
  
  
  
}

shinyApp(ui, server)