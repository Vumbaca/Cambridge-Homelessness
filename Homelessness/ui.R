#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

shinyUI(fluidPage(
  
  titlePanel("Homelessness in Cambridge, MA"),
    
  sidebarPanel(
      selectInput(inputId = "year", label = "Year", choices = c(2014, 2015, 2016, 2017, 2018), selected = 2018),
      
      checkboxInput("age", "Age"),
      
      checkboxInput("gender", "Gender"),
      
      checkboxInput("race", "Race")
  ),
  
  mainPanel(
    textOutput(outputId = "total"),
    
    tabsetPanel(type = "tabs",
        tabPanel("Age", plotOutput(outputId = "ageplot")),
        
        tabPanel("Gender", plotOutput(outputId = "genderplot")),
        
        tabPanel("Race", plotOutput(outputId = "raceplot"))
    )
  )
))