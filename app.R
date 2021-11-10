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
    titlePanel("County / Sector (NAICS)"),
    
    hr(),
    
    #Inputs to select which data set is displays
    fluidRow(
        column(6,
               selectInput("dataset", "Dataset", datasets, selected = "1dig"),
               downloadButton("downloadData", "Download"))
    ),
    
    hr(),
    
    #Where the table is displayed
    fluidRow(
        column(12,
               dataTableOutput("table"))
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
    
    output$table <- renderDataTable({
        datasetInput()
    })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste("GFLcountyEnergyPerFuelYear_", input$dataset, ".tsv", sep="")
        },
        content = function(file) {
            #Is this acceptable?  Should it be like write.tsv?  Does that exist?
            write.table(datasetInput(), file, row.names = FALSE, sep="\t")
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)