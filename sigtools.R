## app.R ##
library(shiny)
library(shinydashboard)
library(shinycssloaders)

library(ggplot2)
library(ggridges)

library(plotly)
# If code didn't work:
# if (!require(remotes)) install.packages("remotes")
# remotes::install_github("cpsievert/plotly_book")


options(shiny.maxRequestSize=100*1024^2)

ui <- dashboardPage(
  dashboardHeader(title = "Sigtools"), 
  
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
          box(title = "Workstation", width = 6, solidHeader = FALSE, status = "primary",
            fluidRow(
              column(1), 
              column(10,
                withSpinner(
                  verbatimTextOutput(outputId = "dataTable")
                  #tableOutput(outputId = "dataTable") 
                )
              ), 
              column(1)
            ) # END fluidRow
          ) # END box
        ), # END fluidRow -
        fluidRow(
          box(title = "Canvas", width = 12, solidHeader = TRUE, status = "primary",
            tabsetPanel(
              tabPanel("Distribution",
                br(),
                #titlePanel("Controls"),
                #fluidRow(
                  sidebarLayout(
                    sidebarPanel(
                      numericInput(inputId = "dist_percentile",  label = "Percentile", value = 100, min = NA, max = 100, step = 0.01, width = NULL),
                      checkboxInput(inputId = "dist_enrichment", label = "Enrichment?", value = FALSE, width = NULL),
                      checkboxInput(inputId = "dist_nozero", label = "No zeros?", value = FALSE, width = NULL),
                      hr(),
#                      uiOutput(outputId = "dist_controls"),
                      textInput(inputId = "dist_title", label = "Plot Title:", value = "distribution", width = NULL, placeholder = NULL),
                      textInput(inputId = "dist_xAxis", label = "X-axis Title:", value = "values", width = NULL, placeholder = NULL),
                      textInput(inputId = "dist_yAxis", label = "Y-axis Title:", value = "frequency", width = NULL, placeholder = NULL),
                      hr(),
                      actionButton(inputId = "dist_go", label = "GO!")
                    ), # END sidebarPanel
                    mainPanel(
                      withSpinner(
                        #plotOutput(outputId = 'dist_plot', width = "100%", height = "400px", click = NULL,
                        #  dblclick = NULL, hover = NULL, hoverDelay = NULL,
                        #  hoverDelayType = NULL, brush = NULL, clickId = NULL,
                        #  hoverId = NULL, inline = FALSE)
                        plotlyOutput(outputId = 'dist_plot', width = "100%", height = "400px")
                        
                      )
                    ) # END mainPanel 
                  ) # END sidebarlayout  
                #) # END fluidRow  
              ), # END tabPanel
              tabPanel("Autocorrelation", 
                br(),
                sidebarLayout(
                sidebarPanel(
                  numericInput(inputId = "autoc_percentile",  label = "Percentile", value = 100, min = NA, max = 100, step = 0.01, width = NULL),
                  checkboxInput(inputId = "autoc_enrichment", label = "Enrichment?", value = FALSE, width = NULL),
                  checkboxInput(inputId = "autoc_nozero", label = "No zeros?", value = FALSE, width = NULL),
                  hr(),
                  textInput(inputId = "autoc_title", label = "Plot Title:", value = "", width = NULL, placeholder = NULL),
                  textInput(inputId = "autoc_xAxis", label = "X-axis Title:", value = "lag", width = NULL, placeholder = NULL),
                  textInput(inputId = "autoc_yAxis", label = "Y-axis Title:", value = "autocorrelation", width = NULL, placeholder = NULL),
                  hr(),
                  actionButton(inputId = "autoc_go", label = "GO!")
                ), # END sidebarPanel
                mainPanel(
                  withSpinner(
                    plotOutput(outputId = 'autoc_plot', width = "100%", height = "400px", click = NULL,
                      dblclick = NULL, hover = NULL, hoverDelay = NULL,
                      hoverDelayType = NULL, brush = NULL, clickId = NULL,
                      hoverId = NULL, inline = FALSE)
                    )
                  ) # END mainPanel 
                ) # END sidebarlayout  
              ), # END tabPanel
              tabPanel("Correlation", 
              ), # END tabPanel
              tabPanel("Aggregation",
              )  # END tabPanel
            ) # END tabsetPanel
          ) # END box
        ) # END fluidRow
      ), # END tabItem ---------
      # START tabItem -------
      tabItem(
        tabName = "about",
        # START fluidRow      
        fluidRow(
          box( title = "Info", status = "info", 
          width = 12,
          h1("Sigtools"),
          p("Sigtools is an R-based visualization package, 
            designed to enable the users with limited programming experience to produce statistical plots of continuous genomic data. 
            It consists of several statistical visualizations that provide insights regarding the behavior of a group of signals in large regions
            – such as a chromosome or the whole genome – as well as visualizing them around a specific point or short region."
          )  
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
    #print("Loading Data...")
    df_signals <- read.csv(input$mulColBedG$datapath, header = input$upload_header, sep = "\t", quote = "")
    num_signals <- ncol(df_signals)-3
    if(! input$upload_header){
      colnames(df_signals) <- c('chr', 'start', 'end', paste(input$upload_prefix, 1:num_signals, sep=''))
    }
    return(df_signals)
  }) # END eventReactive
  
  # SUMMARY TABLE 
#  output$dataTable <- renderTable({
#    print("Gen Data Summ")
#    summary()
#    head(reactiveExp_signals_wide(), 3)
#  })
  output$dataTable <- renderPrint({
    #print("Gen Data Summ")
    #summary(iris[ ,as.numeric(input$var)])
    num_signals <- ncol(reactiveExp_signals_wide())-3
    s <- summary(reactiveExp_signals_wide()[ ,4:(3+num_signals)])
    df_summary <- as.data.frame(matrix(as.numeric(sub('.*:', '', s)), nrow = num_signals, byrow = TRUE))
    if(ncol(df_summary) == 6){
      df_summary[ ,7] <- rep(0, num_signals)
    }
    colnames(df_summary) <- c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max", "NA's")
    df_summary <- cbind("signals" = colnames(reactiveExp_signals_wide())[4:(3+num_signals)] ,df_summary)
    
    return(df_summary)
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
    #print(head(df_signals))
    return(df_signals)
  })
  
  # DISTRIBUTION RENDER UI --------------------------------
#  output$dist_controls <- renderUI({
#    print("Dist controls:")
#    axisName <- colnames(reactiveExp_dist_data())
#    tagList(
#      textInput(inputId = "dist_title", label = "Plot Title:",   value = "Distribution Curve", width = NULL, placeholder = NULL),
#      textInput(inputId = "dist_xAxis", label = "X-axis Title:", value = axisName[1], width = NULL, placeholder = NULL),
#      textInput(inputId = "dist_yAxis", label = "Y-axis Title:", value = axisName[2], width = NULL, placeholder = NULL),
#      actionButton(inputId = "dist_plotAes", label = "Update")
#    )
#  }) # END renderUI
  
#  reactiveExp_dist_plot <- eventReactive(reactiveExp_dist_data(), {
#    print("Event on autoc")
#    
#    ggplotly(
#      #ggplot(df_signals, aes(x=value, y=signal, fill=signal)) +
#      ggplot(df_signals, aes(x=value, group=signal, fill=signal)) +
#        #geom_density_ridges2(scale=0.9) + # rel_min_height = 0.01
#        geom_density(adjust=1.5, alpha=.3) +
#        #labs(colour = "") +
#        #theme(legend.title=element_blank()) +
#        #labs(fill = "", x= 'mean enrichment' , y= 'feature' )+
#        theme_classic(base_size = 20) +
#        theme(legend.title=element_blank()) 
#      #+
#      #  theme(legend.position = "none")
#    )
#    
#    #ggplotly(
#    #ggplot(reactiveExp_dist_data(), aes(x=value, y=variable, fill=variable)) +
#    #  geom_density_ridges2(scale=0.9) + # rel_min_height = 0.01
#    #  theme(legend.title=element_blank()) +
#    #  #scale_color_manual(values=myColors) +
#    #  ggtitle(input$dist_title) +  # for the main title
#    #  xlab(input$dist_xAxis) + # for the x axis label
#    #  ylab(input$dist_yAxis) + # for the y axis label
#    #  #scale_color_discrete(name="") +
#    #  theme_classic(base_size = 21)
#    #)
#    
#  })
  
  output$dist_plot <- renderPlotly({
    ggplotly(
      #ggplot(df_signals, aes(x=value, y=signal, fill=signal)) +
      ggplot(reactiveExp_dist_data(), aes(x=value, group=variable, fill=variable)) +
        #geom_density_ridges2(scale=0.9) + # rel_min_height = 0.01
        geom_density(adjust=1.5, alpha=.3) +
        #labs(colour = "") +
        #theme(legend.title=element_blank()) +
        #labs(fill = "", x= 'mean enrichment' , y= 'feature' )+
        theme_classic(base_size = 20) +
        theme(legend.title=element_blank()) 
      #+
      #  theme(legend.position = "none")
    )
  })
}

shinyApp(ui, server)