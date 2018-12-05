library(tidyverse)
library(shiny)

cambridge_total <- read_rds("cambridge_total.rds")
homeless_total <- read_rds("homeless.rds")

ui <- fluidPage(theme = "united",
  
  titlePanel("Homelessness in Cambridge, MA"),
  
  headerPanel("Test"),
  
  sidebarPanel(
    selectInput(inputId = "year", label = "Year", choices = c(2014, 2015, 2016, 2017), selected = 2017)
  ),
  
  mainPanel(
    textOutput(outputId = "cambridge"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Time",
                  tabsetPanel(
                    tabPanel("Age", plotOutput(outputId = "agep"), plotOutput(outputId = "aget")),
                    tabPanel("Gender", plotOutput(outputId = "genderp"), plotOutput(outputId = "gendert")),
                    tabPanel("Race", plotOutput(outputId = "racep"), plotOutput(outputId = "racet"))
                  )
                ),
                tabPanel("Place"),
                tabPanel("Situation")
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
  
  output$agep <- renderPlot({
    
      cambridge_total %>%
      select(year,
             p_under_eighteen,
             p_eighteen_twentyfour,
             p_over_twentyfour) %>%
      filter(p_over_twentyfour != 0) %>%
      gather(key = age, value = percentage, -year) %>%
      ggplot(aes(x = year, y = percentage, fill = age)) +
      geom_bar(stat = "identity")
    
  }, height = 400, width = 400)
  
  output$aget <- renderPlot({
    
    cambridge_total %>%
      select(year,
             t_under_eighteen,
             t_eighteen_twentyfour,
             t_over_twentyfour) %>%
      filter(t_over_twentyfour != 0) %>%
      gather(key = age, value = total, -year) %>%
      ggplot(aes(x = year, y = total, color = age)) +
      geom_line(size = 1)
    
  }, height = 400, width = 400)
  
  output$genderp <- renderPlot({
    
      cambridge_total %>%
      select(year,
             p_males,
             p_females,
             p_trans,
             p_non_identifying) %>%
      filter(p_males != 0) %>%
      gather(key = gender, value = percentage, -year) %>%
      ggplot(aes(x = year, y = percentage, fill = gender)) +
      geom_bar(stat = "identity")
    
  }, height = 400, width = 400)
  
  output$gendert <- renderPlot({
    
      cambridge_total %>%
      select(year,
             t_males,
             t_females,
             t_trans,
             t_non_identifying) %>%
      filter(t_males != 0) %>%
      gather(key = gender, value = total, -year) %>%
      ggplot(aes(x = year, y = total, color = gender)) +
      geom_line(size = 1)
    
  })
  
  output$racep <- renderPlot({
    
      cambridge_total %>%
      select(year,
             p_white,
             p_black,
             p_asian,
             p_indian_alaskan,
             p_hawaiian_islander,
             p_multiple) %>%
      filter(p_white != 0) %>%
      gather(key = race, value = percentage, -year) %>%
      ggplot(aes(x = year, y = percentage, fill = race)) +
      geom_bar(stat = "identity")
    
  }, height = 400, width = 400)
  
  output$racet <- renderPlot({
    
    cambridge_total %>%
      select(year,
             t_white,
             t_black,
             t_asian,
             t_indian_alaskan,
             t_hawaiian_islander,
             t_multiple) %>%
      filter(t_white != 0) %>%
      gather(key = race, value = total, -year) %>%
      ggplot(aes(x = year, y = total, color = race)) +
      geom_line(size = 1)
    
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