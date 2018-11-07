#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

cambridge <- read_csv("Cambridge_Homeless_Point-in-Time_Count_data__2012-2018.csv", col_names =  FALSE, skip = 1)

cambridge_gender <- cambridge %>%
  select("year" = X1, "persons" = X5, "females" = X11, "males" = X12, "trans" = X13, "non_identifying" = X14) %>%
  group_by(year) %>%
  summarize(males = sum(males) / sum(persons), females = sum(females) / sum(persons), trans = sum(trans) / sum(persons), non_identifying = sum(non_identifying) / sum(persons)) %>%
  ungroup() %>%
  gather(key = gender, value = percentage, -year)

cambridge_race <- cambridge %>%
  select("year" = X1, "persons" = X5, "white" = X20, "black" = X21, "asian" = X22, "indian_alaskan" = X23, "hawaiian_islander" = X24, "multiple" = X25) %>%
  group_by(year) %>%
  summarize(white = sum(white) / sum(persons), black = sum(black) / sum(persons), asian = sum(asian) / sum(persons), indian_alaskan = sum(indian_alaskan) / sum(persons), hawaiian_islander = sum(hawaiian_islander) / sum(persons), multiple = sum(multiple) / sum(persons)) %>%
  ungroup() %>%
  gather(key = race, value = percentage, -year)

cambridge_age <- cambridge %>%
  select("year" = X1,  "persons" = X5, "under_eighteen" = X8, "eighteen_twentyfour" = X9, "over_twentyfour" = X10) %>%
  group_by(year) %>%
  summarize(under_eighteen = sum(under_eighteen) / sum(persons), eighteen_twentyfour = sum(eighteen_twentyfour) / sum(persons), over_twentyfour = sum(over_twentyfour) / sum(persons)) %>%
  gather(key = age, value = percentage, -year)

cambridge_01 <- cambridge %>%
  select("year" = X1, "persons" = X5) %>%
  group_by(year) %>%
  summarize(persons = sum(persons))

shinyServer(function(input, output) {
  
  output$total <- renderText({
    
    paste(
      "Individuals Experiencing Homelessness:", 
      filter(cambridge_01, year == input$year)$persons,
      sep = " ")
      
  })
  
  output$ageplot <- renderPlot({

    if (input$age)
      cambridge_age %>%
      filter(year == input$year) %>%
      ggplot(aes(x = "", y = percentage, fill = age)) +
      geom_bar(width = 1, stat = "identity")
    
  }, height = 400, width = 400)
  
  output$genderplot <- renderPlot({

    if (input$gender)
      cambridge_gender %>%
      filter(year == input$year) %>%
      ggplot(aes(x = "", y = percentage, fill = gender)) +
      geom_bar(width = 1, stat = "identity")
    
  }, height = 400, width = 400)
  
  output$raceplot <- renderPlot({
    
    if (input$race)
      cambridge_race %>%
      filter(year == input$year) %>%
      ggplot(aes(x = "", y = percentage, fill = race)) +
      geom_bar(width = 1, stat = "identity")
    
  }, height = 400, width = 400)
  
})