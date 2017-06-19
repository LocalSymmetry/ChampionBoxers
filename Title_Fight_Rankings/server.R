# Server for the Boxer Ranking Shiny WebApp

library(shiny)
library(dplyr)
library(readr)

# Seperate the data frame by weight class.
standard.ranked.boxers <- read_csv(file="StandardRanking.csv")
colnames(standard.ranked.boxers) <- c("boxerid", "Eigenvector Score", "Boxer", 
                                      "Alias", "Division", "Bouts",
                                      "KO Percentage", "Rounds Fought", 
                                      "Stance", "Debut")
standard.ranked.boxers$Division <- factor(standard.ranked.boxers$Division)
standard.ranked.boxers$Debut <- as.Date(standard.ranked.boxers$Debut)
weightclasses <- list()
for (wt.class in levels(standard.ranked.boxers$Division)) {
  weightclasses[[wt.class]] <- subset(standard.ranked.boxers, 
                                      Division == wt.class,
                                      select = -c(Division, boxerid, Stance))
}

# Define server logic required to summarize and view theselected dataset.
function(input, output) {
  
  # Return the requested dataset.
  datasetInput <- reactive({
    switch(input$dataset,
           "All Classes" = subset(standard.ranked.boxers, 
                                  select = -c(boxerid, Stance)),
           "Heavyweight" = weightclasses[["heavyweight"]],
           "Cruiserwieght" = weightclasses[["cruiserweight"]],
           "Light heavyweight" = weightclasses[["light heavyweight"]],
           "Super middleweight" = weightclasses[["super middleweight"]],
           "Middleweight" = weightclasses[["middleweight"]],
           "Super welterweight" = weightclasses[["super welterweight"]],
           "Welterweight" = weightclasses[["welterweight"]],
           "Super lightweight" = weightclasses[["super lightweight"]],
           "Lightweight" = weightclasses[["lightweight"]],
           "Super featherweight" = weightclasses[["super featherweight"]],
           "Featherweight" = weightclasses[["featherweight"]],
           "Super bantamweight" = weightclasses[["super bantamweight"]],
           "Bantamweight" = weightclasses[["bantamweight"]],
           "Super flyweight" = weightclasses[["super flyweight"]],
           "Flyweight" = weightclasses[["flyweight"]],
           "Light flyweight" = weightclasses[["light flyweight"]],
           "Minimumweight" = weightclasses[["minimumweight"]]
           )
  })
  
  # Show the first "n" observations in the year range given.
  output$view <- renderTable({
    head(subset(filter(datasetInput(), 
                Debut >= as.Date(paste(input$debut.range[1],"-01-01", 
                                       sep = "")),
                Debut <= as.Date(paste(input$debut.range[2],"-01-01", 
                                       sep = ""))),
                select = -Debut)
         , n = input$obs)
  })
}