#Module for Home Page

#UI
homeUI <- function(id){
    #Used so that we don't need to create unique ids for each UI element
    ns <- NS(id)  
  
    tagList(
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
    )
}

# homeServer <- function(id) {
#   moduleServer(id, function(input, output, session){
#     print("HOME")
#   })
# }

