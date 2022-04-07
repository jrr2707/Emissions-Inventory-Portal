library(shiny)
library(RPostgres)
library(DBI)
library(RJSONIO)
library(DT)
library(ggplot2)
library(plyr)
library(dplyr)
library(jsonlite)

source("homeModule.r")
source("aboutModule.r")
source("naicsModule.r")

# Define UI for application
ui <- fluidPage(
  
    # Application Title
    titlePanel(title = div(img(src="Logo_Flat.png", height = 100, width = 400), "Emissions Inventory Portal"),
               windowTitle = "Orebed Analytics - Emissions Inventory Portal"
    ),
    
    # Navbar
    tagList(tags$head(tags$style(type = 'text/css', '.navbar-brand{display:name;')),
        
        navbarPage("",
            # Home Page
            tabPanel(span(icon("home"), title="Home Page"),
                homeUI("homepage") 
            ),
        
            # Industry Page
            tabPanel(span("Industry", title="NAICS Industry Emissions Data"),
                naicsUI("ind")
            ),
            
            # Agriculture Page
            tabPanel(span("Agriculture", title="NAICS Agriculture Emissions Data"),
                naicsUI("agr")
            ),
            
            # About Page
            tabPanel(span("About", title="About Us"),
                aboutUI("about")
            )
        )
        
    )
)

# Define the server for application
server <- function(input, output, session) {
  naicsServer("ind")
  naicsServer("agr")
}
# Run the application 
shinyApp(ui = ui, server = server)