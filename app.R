#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

datasets <- list("1dig", "2dig", "3dig", "4dig")
gfl_1dig <- read.csv(file = "GFLcountyEnergyPerFuelYear_1dig.tsv", sep = "\t", header=TRUE)
gfl_2dig <- read.csv(file = "GFLcountyEnergyPerFuelYear_2dig.tsv", sep = "\t", header=TRUE)
gfl_3dig <- read.csv(file = "GFLcountyEnergyPerFuelYear_3dig.tsv", sep = "\t", header=TRUE)
gfl_4dig <- read.csv(file = "GFLcountyEnergyPerFuelYear_4dig.tsv", sep = "\t", header=TRUE)

ui <- fluidPage(
# Define UI for application that draws a histogram
    
    # Application title
    titlePanel("County / Sector (NAICS)"),
    
    hr(),
    
    #Inputs to select which data set is displays
    fluidRow(
        column(6,
               selectInput("dataset", "Dataset", datasets, selected = "1dig"))
    ),
    
    hr(),
    
    #Where the table is displayed
    fluidRow(
        column(12,
               dataTableOutput("table"))
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #When the user selected data set is changed, change the data set being viewed
    observe({
        currentDataset <<- input$dataset
        if (currentDataset == "1dig"){
            output$table <- renderDataTable(gfl_1dig)
        }
        else if (currentDataset == "2dig"){
            output$table <- renderDataTable(gfl_2dig)
        }
        else if (currentDataset == "3dig"){
            output$table <- renderDataTable(gfl_3dig)
        }
        else if (currentDataset == "4dig"){
            output$table <- renderDataTable(gfl_4dig)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)