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
library(plyr)
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

datasets <- list("1-digit", "2-digit", "3-digit", "4-digit")
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
              
            tabPanel(span(icon("home"), title="Home Page"),
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
                           div(img(src="HomePageData1.png", height = 600, width = 600))
                    )
                )
            ),
              
            tabPanel(span("Industry", title="NAICS Industry Emissions Data"),
                #Inputs to select which data set is displays, download buttons for plot and data, and a description of the sector
                
                fluidRow(
                    column(3, selectInput("dataset", "NAICS Code Length", datasets, selected = "1-digit"), 
                           downloadButton("downloadData", "Download Data"), 
                           downloadButton("DownloadVis", "Download Plot"),
                           style = "border-right: 2px solid #F0F0F0;"),
                    column(9, tagList(tags$h4("About These Data", br()),
                                       tags$h5("The North American Industry Classification System (NAICS) is the 
                                                     standard used by Federal statistical agencies in classifying business 
                                                     establishments for the purpose of collecting, analyzing, and publishing 
                                                     statistical data related to the U.S. business economy.")
                    )
                    ),
                    
                ),
                
                       
                hr(),
                
                #Display Plot
                fluidRow(
                  column(width=12, plotOutput("myPlot"))
                ),
                
                hr(),
                
                #Display Table
                fluidRow(
                  column(width=12, DT::dataTableOutput("table"), style = "border-right: 2px solid #F0F0F0;")
                ),
                
                hr()
            ),
            
            # TODO: This is the same as above, we need to change it
            tabPanel(span("Agriculture", title="NAICS Agriculture Emissions Data"),
                     "(blank for now)"
                     #Inputs to select which data set is displays
                     # fluidRow(
                     #   
                     #   column(6, selectInput("dataset", "NAICS Code Length", datasets, selected = "1-digit"))),
                     # 
                     # # Download Buttons
                     # fluidRow(
                     #   column(6, downloadButton("downloadData", "Download Data")),
                     #   
                     #   column(6, downloadButton("downloadVis", "Download Visual"))
                     # ),
                     # 
                     # hr(),
                     # 
                     # #Where the table is displayed
                     # fluidRow(
                     #   column(width=6,
                     #          DT::dataTableOutput("table"), style = "height: 500px; overflow-y: scroll; border-right: 2px solid #F0F0F0"),
                     #   
                     #   column(width=6, plotOutput("myPlot"))
                     # )
            ),
            
            tabPanel(span("Electricity Generation", title="Egrid Electricity Generation Emissions Data"),
                "eGrid Data (blank for now)"
            ),
            
            tabPanel(span("...",title="..."),
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
               "1-digit" = gfl_1dig,
               "2-digit" = gfl_2dig,
               "3-digit" = gfl_3dig,
               "4-digit" = gfl_4dig)
    })

    refinedDataset <- reactive ({
        datasetInput()[,-1] %>%
          mutate(across(is.numeric, round, digits=2))
    })
    
    output$table <- DT::renderDataTable(refinedDataset(), rownames = FALSE, filter = "top", 
                                        extensions = list("Scroller" = NULL),
                                        options = list(scrollX = TRUE, scrollY = '400px', scroller = TRUE, dom = "lt"), # Show the (l)ength input and (t)able
                                        colnames=c("Year", "County", "NAICS Code", "NAICS Category", "Diesel", "LPG NGL", 
                                                   "Net Electricity", "Other", "Residual Fuel Oil", "Coal", "Natural Gas", "Coke and Breeze"),
    )

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
      
      vals = totalEmissions()[2]
      if (!empty(vals)){
        min_val = min(vals)
        min_exponent = floor(log10(min_val)-1)
        final_min = round_any(min_val, 10^min_exponent, f = floor)
        
        max_val = max(vals)
        max_exponent = floor(log10(max_val)-1)
        final_max = round_any(max_val, 10^max_exponent, f = ceiling)
        
        sequence = (final_max - final_min) / 10
      } else {
        final_min = 0
        final_max = 0
        sequence = 0
      }
      
      plot <- ggplot(totalEmissions(), aes(x=YEAR, y=as.integer(sum))) +
        geom_col(width = 0.4, fill="red") +
        coord_cartesian(ylim = c(final_min, final_max + sequence)) +
        scale_y_continuous(breaks = seq(final_min, final_max + sequence, by = sequence)) + 
        scale_x_continuous(breaks = seq(2010, 2100, by = 1)) + 
        xlab("Year") + ylab("Total Emissions (UNITS)") + 
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
          )
    })
    
    output$myPlot <- renderPlot({
        print(gflPlot())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)