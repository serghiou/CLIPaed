# Load packages
library(shiny)
library(shinydashboard) # great dashboard layout
library(plotly) # interactive graphs
library(rsconnect) # upload to shinyapps.io
library(DT) # great tables for shiny

# Define functions
convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    mi
}

# Create UI
ui <- dashboardPage(
    dashboardHeader(title = "CLIPaed"),
    dashboardSidebar(
        sidebarMenu(
            convertMenuItem(menuItem("Analysis", 
                                     tabName = "analysis", 
                                     icon = icon("cogs"),
                     # Input directly under menuItem
                     textInput("title", "Name of Graph", 
                               placeholder = "Control Graph", width = '100%'),
                     
                     # Input inside of menuSubItem
                     fileInput('file', 'Upload CSV File',
                               accept=c('text/csv', 
                                        'text/comma-separated-values,
                                        text/plain', 
                                        '.csv')),
                     actionButton(inputId = "go", label = "Plot", width = '88%'),
                     tags$style(type='text/css', 
                                "button#go { margin-left: 11px; }"),
                     tags$style(type='text/css', 
                                "button#go { margin-bottom: 12px; }")
                     ), "analysis"),
            menuItem("Help", tabName = "help", icon = icon("life-bouy"))
            
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "analysis",
                    fluidRow(
                        column(width = 8,
                               box(
                                   title = "Control Graph", 
                                   status = "primary",
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   width = NULL,
                                   plotlyOutput("plot",height = "auto"),
                                   verbatimTextOutput("event")
                               )
                        )
                    ),
                    fluidRow(
                        column(width = 8,
                               box(
                                   title = "Data",
                                   status = "success",
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   width = NULL,
                                   DT::dataTableOutput('tbl')
                               )
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "help",
                    fluidRow(
                        box(
                            title = "Help", 
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            "Thank you for using this website! For further information regarding this initiative and the statistical methods being used, have a look at ", HTML("<a href=\"CLIP and Control Charts.pptx\" download=\"CLIP\">this</a>"), " powerpoint presenation. For any other inquiries please send us an email at", HTML("<a href=\"mailto:sstelios@stanford.edu\">sstelios@stanford.edu</a>"), "."
                            )
                        )
                    )
            )
        )
    )

server <- function(input, output) {
    
    m = list(
        l = 50,
        r = 25,
        b = 100,
        t = 50,
        pad = 0
    ) 
    
    data <- eventReactive(input$go, { 
        
        inFile <- input$file
        
        validate(need(input$file != "", "Please upload a .csv file first."))
        validate(need(input$title != "", "Please give a title to your plot."))
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        
        df <- read.csv(inFile$datapath)
        
        dat1 <- df
        
        # Remove missing values
        dat1[, -5] <- na.omit(dat1[, -5])
        
        # Convert into date
        dttm <- gsub(" UTC", "", dat1$p_interval_start)
        dat1$p_interval_start <- as.POSIXct(dttm, 
                                            "%a %b %d %H:%M:%S %Y", tz = "UTC")
        
        # Prepare P-plot variables
        month <- format(dat1$p_interval_start, "%m-%Y")
        prptn <- with(dat1, p_observe / p_samplesize) * 100
        x.bar <- mean(prptn)
        stder <- with(dat1, (p_observe - (1 - p_observe))/p_samplesize)
        upper <- x.bar + (3*stder * 100)
        lower <- x.bar - (3*stder * 100)
        dtplt <- data.frame(month, upper, lower, prptn, x.bar)
        
        # The default order will be alphabetized unless specified as below:
        dtplt$month <- factor(dtplt$month, levels = dtplt[["month"]])
        
        # Return
        list(dat1, dtplt)
        })
    
    main <- eventReactive(input$go, { input$title })
    dtab <- eventReactive(input$go, {
        
        dd <- data()
        colnames(dd[[1]]) <- c("Interval start", "Interval end",
                               "No. of events", "Sample size", "Notes")
        dd[[1]]
        
    })
    
    output$plot <- renderPlotly({
        
        dd <- data()

        plot_ly(dd[[2]], x = ~ month, y = ~ upper, type = 'scatter',
                mode = 'lines', line = list(color = 'transparent'),
                width = 762, 
                showlegend = FALSE, name = 'Upper') %>%

            layout(autosize = F, margin = m) %>%

            add_trace(y = ~ lower, type = 'scatter', mode = 'lines',
                      fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
                      line = list(color = 'transparent'),
                      showlegend = FALSE, name = 'Lower') %>%

            add_trace(x = ~ month, y = ~ x.bar, type = 'scatter', mode = 'lines',
                      line = list(color='rgba(255, 0, 0, 0.5)'),
                      name = "Mean") %>%

            add_trace(x = ~ month, y = ~ prptn, type = 'scatter', mode = 'lines',
                      line = list(color='rgb(0,0.5,0.5)'), alpha = 0.5,
                      name = "Proportion") %>%

            layout(title = main(),
                   paper_bgcolor='rgb(255,255,255)',
                   plot_bgcolor='rgb(229,229,229)',

                   xaxis = list(title = "\nMonth",
                                gridcolor = 'rgb(255,255,255)',
                                showgrid = TRUE,
                                showline = FALSE,
                                showticklabels = TRUE,
                                tickcolor = 'rgb(127,127,127)',
                                ticks = 'outside',
                                zeroline = FALSE),

                   yaxis = list(title = "Proportion (%)",
                                gridcolor = 'rgb(255,255,255)',
                                showgrid = TRUE,
                                showline = FALSE,
                                showticklabels = TRUE,
                                tickcolor = 'rgb(127,127,127)',
                                ticks = 'outside',
                                zeroline = FALSE))
    })
    
    output$tbl <- DT::renderDataTable(dtab(), 
                                      options = list(lengthChange = FALSE)
    )
        
    # output$event <- renderPrint({
    #     d <- event_data("plotly_hover")
    #     if (is.null(d)) "Hover on a point!" else d
    # })
    
}


shinyApp(ui, server)
#runApp("/Users/Stelios/Desktop/TreeHacks/Shiny/App_1/", display.mode="showcase")

# Deploy app
# setAccountInfo(name='serghiou',
#               token='74F4B690E810520511FA12F9A14D7398',
#               secret='Fz7yQw2hT6cmPEmUleBN5Em76vvZxUhMrrOagXHx')

deployApp("/Users/Stelios/Desktop/TreeHacks/Shiny/App_1/")
