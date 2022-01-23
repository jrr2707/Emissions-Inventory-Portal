#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(RPostgres)
library(DBI)
library(RJSONIO)
library(DT)
library(ggplot2)
library(dplyr)
library(jsonlite)

my_secrets <- function() {
    path = "./secrets/secrets.json"
    if (!file.exists(path)) {
        stop("Can't find secret file: '", path, "'")
    }
    
    fromJSON(path)
}

secrets = my_secrets();

# Connect to a specific postgres database i.e. Heroku
con <- dbConnect(RPostgres::Postgres(),dbname = 'postgres', 
                 host = 'oasps.student.rit.edu', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = toString(secrets["username"]),
                 password = toString(secrets["password"]))

datasets <- list("1dig", "2dig", "3dig", "4dig")
gfl_1dig <- dbReadTable(con, 'gf1_1dig')
gfl_2dig <- dbReadTable(con, 'gf1_2dig')
gfl_3dig <- dbReadTable(con, 'gf1_3dig')
gfl_4dig <- dbReadTable(con, 'gf1_4dig')

ui <- fluidPage(
# Define UI for application that draws a histogram
    
    # Application title
    titlePanel(title = div(img(src="Logo_Flat.png", height = 100, width = 400), "Emissions Inventory Portal"),
               windowTitle = "Orebed Analytics - Emissions Inventory Portal"
    ),
    
    tagList(tags$head(tags$style(type = 'text/css', '.navbar-brand{display:none;}')),
            
        navbarPage("",
              
            tabPanel(icon("home"),
                tagList(tags$h1(
                    "Welcome to the Orebed Analytics Emissions Inventory Portal"
                )),
                
                tagList(tags$h3(
                    "Overview:"
                )),
                fluidRow(
                    column(6,
                        tagList(tags$h4(
                            "An emissions inventory is an estimate of the total quantity of specific substances of interest emitted ",
                            "by or due to human activities within a specified time period and geographic location. Emissions inventories ",
                            "are frequently prepared for toxic pollutants with direct adverse health effects (also known ",
                            "as \"criteria pollutants\" or \"schedule pollutants\"); the concern of the present paper is with greenhouse ",
                            "gas emissions inventories estimating the quantities of greenhouse gases emitted by or due to ",
                            "human activities.", br(), br(),
                            
                            "The global economy generates greenhouse gas emissions that are causing the global climate to ",
                            "warm and change at an alarming rate. However, we're looking at emissions only in the United ",
                            "States and at a smaller scale: state, region, county, or municipality, since that is often the scale at ",
                            "which policy interventions can be implemented most effectively."
                        ))
                    ),
                    
                    column(6,
                           div(img(src="HomePageData1.png", height = 600, width = 600))#,
                           #br(),
                           #div(img(src="HomePageData2.png", height = 200, width = 200))
                    )
                )
            ),
              
            tabPanel("NAICS",
                #Inputs to select which data set is displays
                fluidRow(
                  
                    column(6, selectInput("dataset", "Dataset", datasets, selected = "1dig"))),
                       
                    # Download Buttons
                    fluidRow(
                        column(6, downloadButton("downloadData", "Download Data")),
                        
                        column(6, downloadButton("downloadVis", "Download Visual"))
                ),
                       
                hr(),
                       
                #Where the table is displayed
                fluidRow(
                    column(width=6,
                        DT::dataTableOutput("table"), style = "border-right: 2px solid #F0F0F0"),
                        
                    column(width=6, plotOutput("myPlot"))
                )
            ),
            
            tabPanel("eGrid",
                "eGrid Data (blank for now)"
            ),
            
            tabPanel("...",
                     "(blank for now)"
            ),
            
            tabPanel("About",
                tagList(tags$h1(
                    "About Us"
                )),
                
                tagList(tags$h3(
                    "Team:"
                )),
                tagList(tags$h4(
                    "Olivia Dolan - Developer", br(),
                    "Eric Koski - Developer/Sponsor", br(),
                    "Keith Lynd - Developer", br(),
                    "EJ Martino - Developer", br(),
                    "Thomas Maszerowski - Coach", br(),
                    "Joey Rumfelt - Developer"
                )),
                
                br(),
                
                tagList(tags$h3(
                  "Special Thanks:"
                )),
                tagList(tags$h4(
                  "Rochester Institute of Technology", br(),
                  "Viewers Like You"
                ))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #When the user selected data set is changed, change the data set being viewed
    datasetInput <- reactive({
        switch(input$dataset,
               "1dig" = gfl_1dig,
               "2dig" = gfl_2dig,
               "3dig" = gfl_3dig,
               "4dig" = gfl_4dig)
    })

    refinedDataset <- reactive ({
        datasetInput()[,-1]
    })
    
    output$table <- DT::renderDataTable(refinedDataset(), filter = "top", rownames = FALSE, 
                                        options = list(scrollX = TRUE, dom = 'ltp')) # Show the (l)ength input, (t)able, and
                                                                                     # (p)agination

    output$downloadData <- downloadHandler(
        filename = function() {
            paste("GFLcountyEnergyPerFuelYear_", input$dataset, ".tsv", sep="")
        },
        content = function(file) {
            # Filtering the data allows us to download the currently shown data
            filtered = input$table_rows_all
            write.table(refinedDataset()[filtered, , drop = FALSE], file, row.names = FALSE, sep="\t")
        }
        
    )
    
    output$downloadVis <- downloadHandler(
      filename = function() {
        paste("GFLcountyEnergyPerFuelYear_", input$dataset, ".png", sep="")
      },
      content = function(file) {
        ggsave(file, plot = gflPlot(), device = "png")
      }
    )
    
    totalEmissions <- reactive({
      # Filtering the data allows the visualization to be adaptive to the currently shown data
      filtered = input$table_rows_all
      refinedDataset()[filtered, , drop = FALSE] %>% 
        group_by(YEAR) %>%
        summarize(sum = sum(Diesel, LPG_NGL, Net_electricity, Other, Residual_fuel_oil, 
                            Coal, Natural_gas, Coke_and_breeze), na.rm = TRUE)
    })
    
    gflPlot <- reactive({
      plot <- ggplot(totalEmissions(), aes(x=factor(YEAR), y=factor(as.integer(sum)))) +
        geom_bar(stat="identity", width=0.7, fill="red") +
        xlab("Year") + ylab("Total Emissions (UNITS)")
    })
    
    output$myPlot <- renderPlot({
        print(gflPlot())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)