# Module for About Page

# UI
aboutUI <- function(id){
    # Create a namespace so that we don't need to create unique ids for each UI element
    ns <- NS(id)  
  
    tagList(
        tags$h1(
            "About Us"
        ),
  
        tags$h3(
            "Team:"
        ),
  
        tags$h4(
            "Olivia Dolan - Developer", br(),
            "Eric Koski - Developer/Sponsor", br(),
            "Keith Lynd - Developer", br(),
            "EJ Martino - Developer", br(),
            "Prof. Thomas Maszerowski - Coach", br(),
            "Joey Rumfelt - Developer"
        ),
  
        br(),
  
        tags$h3(
            "Special Thanks:"
        ),
  
        tags$h4(
            "Rochester Institute of Technology", br(),
            "Prof. Sam Malachowsky"
        )
    )
}
