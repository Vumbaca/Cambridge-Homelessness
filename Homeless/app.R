library(tidyverse)
library(shiny)

cambridge_total <- read_rds("cambridge.rds")
homeless_total <- read_rds("homeless.rds")

ui <- fluidPage(
  
  titlePanel("Homelessness in Cambridge, MA"),
  
  sidebarPanel(
    selectInput(inputId = "year", label = "Year", choices = c(2014, 2015, 2016, 2017), selected = 2017)
  ),
  
  mainPanel(
    textOutput(outputId = "cambridge"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Age", plotOutput(outputId = "ageplot")),
                
                tabPanel("Gender", plotOutput(outputId = "genderplot")),
                
                tabPanel("Race", plotOutput(outputId = "raceplot"))
    ),
    
    textOutput(outputId = "total"),
    
    tableOutput(outputId = "totaltable"),
    
    textOutput(outputId = "description"),
    
    textOutput(outputId = "git")
  )
)

server <- function(input, output) {
  
  output$cambridge <- renderText({
    
    paste(
      "Individuals Experiencing Homelessness in Cambridge:", 
      filter(cambridge_total, year == input$year)$persons,
      sep = " ")
    
  })
  
  output$ageplot <- renderPlot({
    
      cambridge_total %>%
      select(year,
             p_under_eighteen,
             p_eighteen_twentyfour,
             p_over_twentyfour) %>%
      filter(year == input$year) %>%
      gather(key = age, value = percentage, -year) %>%
      ggplot(aes(x = "", y = percentage, fill = age)) +
      geom_bar(width = 1, stat = "identity")
    
  }, height = 400, width = 400)
  
  output$genderplot <- renderPlot({
    
      cambridge_total %>%
      select(year,
             p_males,
             p_females,
             p_trans,
             p_non_identifying) %>%
      gather(key = gender, value = percentage, -year) %>%
      filter(year == input$year) %>%
      ggplot(aes(x = "", y = percentage, fill = gender)) +
      geom_bar(width = 1, stat = "identity")
    
  }, height = 400, width = 400)
  
  output$raceplot <- renderPlot({
    
      cambridge_total %>%
      select(year,
             p_white,
             p_black,
             p_asian,
             p_indian_alaskan,
             p_hawaiian_islander,
             p_multiple) %>%
      gather(key = race, value = percentage, -year) %>%
      filter(year == input$year) %>%
      ggplot(aes(x = "", y = percentage, fill = race)) +
      geom_bar(width = 1, stat = "identity")
    
  }, height = 400, width = 400)
  
  output$total <- renderText({
    
    paste(
      "Individuals Experiencing Homelessness in the United States:", 
      filter(homeless_total, coc_name == "Total" & year == input$year)$count,
      sep = " ")
    
  })
  
  output$totaltable <- renderTable({
    
    homeless_total %>%
      filter(year == input$year & coc_name != "Total") %>%
      select(coc_name, count) %>%
      arrange(desc(count)) %>%
      head(10)
    
  })
  
  output$description <- renderText({
    
    "This app draws on data collected by Cambridge government regarding homelessness in the city from 2012-2017. Every year a count of individuals experiencing homelessness is taken on a single night in January. Demographic information about the individuals counted is also collected but is recorded in aggregate for the given year. Cambridge sends its annual homelessness data to the U.S. Department of Housing and Urban Development. This app also uses data from all cities that submitted to HUD from 2013 to 2017."
    
  })
  
  output$git <- renderText({
    
    "The code for this app may be found at: https://github.com/Vumbaca/Final-Project-Vumbaca"
    
  })
  
}

shinyApp(ui = ui, server = server)