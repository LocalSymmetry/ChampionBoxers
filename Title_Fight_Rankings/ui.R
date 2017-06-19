# Server for the Boxer Ranking Shiny WebApp
library(shiny)

fluidPage(
  
  # Application title.
  titlePanel("Title Fight Boxer Rankings"),
  
  # Data selection sidebar.
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Weight Class:", 
                  choices = c("All Classes",
                              "Heavyweight",
                              "Cruiserwieght",
                              "Light heavyweight",
                              "Super middleweight",
                              "Middleweight",
                              "Super welterweight",
                              "Welterweight",
                              "Super lightweight",
                              "Lightweight",
                              "Super featherweight",
                              "Featherweight",
                              "Super bantamweight",
                              "Bantamweight",
                              "Super flyweight",
                              "Flyweight",
                              "Light flyweight",
                              "Minimumweight")),
      
      numericInput("obs", "Number of fighters to view:", 10),
      
      # Debut Time Inverval.
      sliderInput("debut.range", "Debut Year:", 
                  min = 1941, max = 2015, 
                  value = c(1941,2015), 
                  timeFormat = "%Y", dragRange = TRUE, sep=""),
      
      helpText("Note: While the rankings can be shown only for specific",
               "weight divisions, the ranking method uses all fighters.",
               "Data Source: BoxRec.com"),
      
      submitButton("Update View")
    ),
    
    # Main panel displays the rankings.
    mainPanel(
      h4("Rankings"),
      tableOutput("view")
    )
  )
)
