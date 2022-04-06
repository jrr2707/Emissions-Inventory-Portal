#Module for NAICS pages


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

#Get the data sets from dataingest.r
datasets <- list("1-digit", "2-digit", "3-digit", "4-digit")
gfl_1dig <- dbReadTable(con, 'gf1_1dig')
gfl_2dig <- dbReadTable(con, 'gf1_2dig')
gfl_3dig <- dbReadTable(con, 'gf1_3dig')
gfl_4dig <- dbReadTable(con, 'gf1_4dig')

#Unit options for NAICS plots
units <- list("MMbtu", "Therm", "Half", "Double")

#UI
naicsUI <- function(id){
  #Used so that we don't need to create unique ids for each UI element
  ns <- NS(id)  
  
  tagList(
      fluidRow(
          column(width=3, selectInput(ns("dataset"), "NAICS Code Length", datasets, selected = "1-digit", width = "200px"), 
                 selectInput(ns("unit"), "Unit", units, selected = "MMbtu", width = "200px"),
                 downloadButton(ns("downloadData"), "Download Data"),
                 downloadButton(ns("downloadVis"), "Download Plot"),
                 style = "border-right: 2px solid #F0F0F0;"),
          column(width=9, tagList(
              tags$h4("About These Data", br()),
              tags$h5("The North American Industry Classification System (NAICS) is the standard numeric coding system 
                      developed by the U.S. Office of Management and Budget, and used by Federal statistical agencies 
                      in classifying business establishments for the purpose of collecting, analyzing, 
                      and publishing statistical data related to the U.S. business economy. 
                      A handful of minor alterations 
                      have been made in the NAICS categories shown here, 
                      for clarity in presenting energy and emissions data..")
              )
          )
      ),
      
      hr(),                          
      
      navbarPage("",
                 tabPanel(span("Basic Plot", title="The Basic Plot"),
                          # Display basic plot
                          fluidRow(
                            column(width=12, plotOutput(ns("Plot")))
                          ),
                 ),
                 tabPanel(span("Stacked Plot", title="The Stacked Plot"),
                          # Display basic plot
                          fluidRow(
                            column(width=12, plotOutput(ns("Plot_Stack")))
                          ),
                 ),
      ),
      
      hr(),
      
      #Display Table
      fluidRow(
          column(width=12, DT::dataTableOutput(ns("table")), style=".dataTable({overflow-x:hidden})")
      )
  )
}

#NAICS Server
naicsServer <- function(id) {
    moduleServer(id, function(input, output, session){
      
        #Get the tooltip descriptions for the dataset. Used in output$table      
        fuelTypes <- read.csv("Standard fuel types.csv", header=FALSE)
        
        tooltips = paste("['", fuelTypes[2,2], "', '", fuelTypes[3,2], "', '", fuelTypes[4,2], "', '", fuelTypes[5,2], "', '",
                         fuelTypes[6,2], "', '", fuelTypes[7,2], "', '", fuelTypes[8,2], "', '", fuelTypes[9,2], "', '", fuelTypes[10,2], "', '",
                         fuelTypes[11,2], "', '", fuelTypes[12,2], "', '", fuelTypes[13,2], "', '", fuelTypes[14,2], "']", sep="")
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          paste("  var tooltips = ", tooltips, ";", sep=""),
          "  for(var i=0; i<13; i++){",
          "    $('th:eq('+i+')',thead).attr('title', tooltips[i]);",
          "  }",
          "}")
      
        #When the user selected data set is changed, change the data set being viewed
        datasetInput <- reactive({
            switch(input$dataset,
                "1-digit" = gfl_1dig,
                "2-digit" = gfl_2dig,
                "3-digit" = gfl_3dig,
                "4-digit" = gfl_4dig,
                "1-digit")
        })
        
        #When the user selects a different unit change the multiplier used to convert the data to the correct unit
        multiplier <- reactive({
            switch(input$unit,
                "MMbtu" = 1,
                "Therm" = .1,   #Place Holder Multipliers, can replace with Joules or kWh  
                "Half" = .5,      
                "Double" = 2)
        })
        
        unit_conversion <- function(x) {
            converted <- x * multiplier()
        }
        
        
        #Create the filter expression for the refined dataset
        naicsCode <-  reactive({
            if (id == "ind") {
              switch(input$dataset,
                  "1-digit" = "NAICS1dig > 1",
                  "2-digit" = "NAICS2dig >= 20",
                  "3-digit" = "NAICS3dig >= 200",
                  "4-digit" = "NAICS4dig >= 2000",
                  "NAICS1dig > 1")
            } else {
              switch(input$dataset,
                     "1-digit" = "NAICS1dig == 1",
                     "2-digit" = "NAICS2dig < 20",
                     "3-digit" = "NAICS3dig < 200",
                     "4-digit" = "NAICS4dig < 2000",
                     "NAICS1dig > 1")
            }
        })
        
        
        #Refine the dataset to do any unit conversion and rounding to the second decimal place, as well as turning year and county rows into factors.
        refinedDataset <- reactive ({
            datasetInput()[,-1] %>%
            mutate(across(c(5,13), unit_conversion)) %>%
            mutate(across(where(is.numeric), round, digits=2)) %>%
            mutate(across(.cols = 1:2, as.factor)) %>%
            filter_(naicsCode())
        })
        
        #Create the table to be displayed
        output$table <- DT::renderDataTable(refinedDataset(), rownames = FALSE, filter = "top",
                                                options = list(scrollX=FALSE, scrollY='400px', pageLength = 100, 
                                                               lengthMenu = c(50, 100, 150, 200, 250), dom = "ltp",
                                                               headerCallback=JS(headerCallback)), # Show the (l)ength input and (t)able
                                                colnames=c("Year", "County", "NAICS Code", "NAICS Category", "Diesel", "LPG NGL",
                                                           "Net Electricity", "Other", "Residual Fuel Oil", "Coal", "Natural Gas", "Coke and Breeze", "Total"),
        )
        
        
        #Download data from datatable
        output$downloadData <- downloadHandler(
          filename = function() {
            paste("IndustryNAICSHeatPerYear_", input$dataset, ".tsv", sep="")
          },
          content = function(file) {
            # Filtering the data allows us to download the currently shown data
            filtered = input$table_rows_all
            write.table(refinedDataset()[filtered, , drop = FALSE], file, row.names = FALSE, sep="\t")
          }
          
        )
        
        #Download visualization
        output$downloadVis <- downloadHandler(
          filename = function() {
            paste("IndustryNAICSHeatPerYear_", input$dataset, ".png", sep="")
          },
          content = function(file) {
            ggsave(file, plot = Plot(), device = "png")
          }
        )
        
        #TODO: Comments
        countiesSummary <- reactive({
            filtered = input$table_rows_all
            refinedDataset()[filtered, , drop = FALSE] %>%
              group_by(County) %>%
              summarize(emissions = sum(Diesel, LPG_NGL, Net_electricity, Other, Residual_fuel_oil,
                                        Coal, Natural_gas, Coke_and_breeze), na.rm = TRUE)
        })
        
        #TODO: Comments
        counties <- reactive({
          countiesSummary()$County
        })
        
        #TODO: Comments
        totalYearlyEmissions <- reactive({
          filtered = input$table_rows_all
          refinedDataset()[filtered, , drop = TRUE] %>%
            group_by(YEAR) %>%
            summarize(sum = sum(Diesel, LPG_NGL, Net_electricity, Other, Residual_fuel_oil,
                                Coal, Natural_gas, Coke_and_breeze), na.rm = TRUE, .groups = "drop")
        })
        
        #TODO: Comments
        totalCountyEmissions <- reactive({
          filtered = input$table_rows_all
          refinedDataset()[filtered, , drop = TRUE] %>%
            group_by(County, YEAR) %>%
            summarize(sum = sum(Diesel, LPG_NGL, Net_electricity, Other, Residual_fuel_oil,
                                Coal, Natural_gas, Coke_and_breeze), na.rm = TRUE, .groups = "drop") %>%
            arrange(sum)
        })
        
        #Generate Basic Plot
        Plot <- reactive({
          vals = totalYearlyEmissions()[2]
          if (!empty(vals)){
            min_val = min(vals)
            min_exponent = floor(log10(min_val)-1)
            final_min = round_any(min_val, 10^min_exponent, f = floor)
            
            max_val = max(vals)
            max_exponent = floor(log10(max_val)-1)
            final_max = round_any(max_val, 10^max_exponent, f = ceiling)
            
            sequence = (final_max - final_min) / 10
            
            plot <- ggplot(totalYearlyEmissions(), aes(x=YEAR, y=as.integer(sum))) +
              geom_col(width = 0.4, fill="red") +
              coord_cartesian(ylim = c(final_min, final_max)) +
              scale_y_continuous(breaks = seq(final_min, final_max, by = sequence)) + 
              scale_x_discrete(name = 'Year') +
              xlab("Year") + ylab(paste("Total Emissions (", input$unit, ")", sep="")) + 
              theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()
              )
          } else {
            # Before it would display an error on the UI.  This makes it so it displays 
            # a grey box where the plot should be.
            Plot <- ggplot()
          }
        })
        
        #Generate Stacked Plot
        Plot_Stack <- reactive({
          
          vals = totalYearlyEmissions()[2]
          if (!empty(vals)){
            min_val = min(vals)
            if (min_val > 0) {
              min_exponent = floor(log10(min_val)-1)
              final_min = round_any(min_val, 10^min_exponent, f = floor)
              
              max_val = max(vals)
              max_exponent = floor(log10(max_val)-1)
              final_max = round_any(max_val, 10^max_exponent, f = ceiling)
              
              sequence = (final_max - final_min) / 10
            }
            
            plot_stack <- ggplot(totalCountyEmissions()) +
              scale_fill_manual(values = c(
                "#e6194B", "#000075", "#3cb44b", "#9A6324",
                "#4363d8", "#f58231", "#43d4f4", "#f032e6",
                "#469990", "#800000", "#ffe119", "#aaffc3",
                "#e6194B", "#000075", "#3cb44b", "#9A6324",
                "#4363d8", "#f58231", "#43d4f4", "#f032e6",
                "#469990", "#800000", "#ffe119", "#aaffc3",
                "#e6194B", "#000075", "#3cb44b", "#9A6324",
                "#4363d8", "#f58231", "#43d4f4", "#f032e6",
                "#469990", "#800000", "#ffe119", "#aaffc3",
                "#e6194B", "#000075", "#3cb44b", "#9A6324",
                "#4363d8", "#f58231", "#43d4f4", "#f032e6",
                "#469990", "#800000", "#ffe119", "#aaffc3",
                "#e6194B", "#000075", "#3cb44b", "#9A6324",
                "#4363d8", "#f58231", "#43d4f4", "#f032e6",
                "#469990", "#800000", "#ffe119", "#aaffc3",
                "#e6194B", "#000075", "#3cb44b", "#9A6324",
                "#4363d8", "#f58231", "#43d4f4", "#f032e6",
                "#469990", "#800000", "#ffe119", "#aaffc3",
                "#e6194B", "#000075", "#3cb44b", "#9A6324",
                "#4363d8", "#f58231", "#43d4f4", "#f032e6",
                "#469990", "#800000", "#ffe119", "#aaffc3",
                "#e6194B", "#000075", "#3cb44b", "#9A6324",
                "#4363d8", "#f58231", "#43d4f4", "#f032e6",
                "#469990", "#800000", "#ffe119", "#aaffc3"),
                name = "County",
                breaks = counties()) +
              coord_cartesian(ylim = c(final_min, final_max)) +
              scale_y_continuous(breaks = seq(final_min, final_max, by = sequence)) +
              scale_x_discrete(name = 'Year') +
              xlab("Year") + ylab(paste("Total Emissions (", input$unit, ")", sep="")) +
              theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()
              ) +
              geom_col(width = 0.4, aes(x=YEAR, y=as.integer(sum), fill = County))
          } else {
            # Before it would display an error on the UI.  This makes it so it displays 
            # a grey box where the plot should be.
            Plot_Stack <- ggplot()
          }
        })
        
        output$Plot <- renderPlot({
            print(Plot())
        })
        
        output$Plot_Stack <- renderPlot({
          print(Plot_Stack())
        })
      
    })
}