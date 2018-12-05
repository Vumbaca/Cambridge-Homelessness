# call library for working with data
library(tidyverse)
# call library for creating shiny app
library(shiny)
# call library for stylizing shiny app
library(shinythemes)

# save data from file into dataframe
cambridge_total <- read_rds("cambridge_total.rds")
# save data from file into dataframe
cambridge_type <- read_rds("cambridge_type.rds")
# save data from file into dataframe
homeless_total <- read_rds("homeless.rds")

# create shiny app ui and theme
ui <- fluidPage(theme = shinytheme("sandstone"),
  
  # create shiny app title
  titlePanel("Homelessness in Cambridge, MA"),

  navlistPanel(
    
      tabPanel("Time",
               
        tabsetPanel(
          
          tabPanel("Age", 
                   
                   fluidRow(
                     
                     column(width = 6, plotOutput(outputId = "agep")),
                     
                     column(width = 6, plotOutput(outputId = "aget")))),
          
          tabPanel("Gender",
                   
                   fluidRow(
                     
                     column(width = 6, plotOutput(outputId = "genderp")),
                     
                     column(width = 6, plotOutput(outputId = "gendert")))),
          
          tabPanel("Race",
                   
                   fluidRow(
                     
                     column(width = 6, plotOutput(outputId = "racep")),
                     
                     column(width = 6, plotOutput(outputId = "racet")))))),
      
      tabPanel("Place",
               
               sidebarPanel(
                  selectInput(inputId = "year", label = "Year", choices = c(2014, 2015, 2016, 2017), selected = 2017)),
               
               mainPanel(
                  textOutput(outputId = "cambridge"))),
      
      tabPanel("Situation",
               
               tabsetPanel(
                 
                 tabPanel("Unsheltered"),
                 
                 tabPanel("Emergency"),
                 
                 tabPanel("Transitional")
               )
               
               )
  )
)

server <- function(input, output) {
  
  # create age percentage plot
  output$agep <- renderPlot({
    
      cambridge_total %>%
      filter(p_over_twentyfour != 0) %>%
      select(year,
             "< 18" = p_under_eighteen,
             "18 - 24" = p_eighteen_twentyfour,
             "> 24" = p_over_twentyfour) %>%
      gather(key = age, value = percentage, -year) %>%
      mutate(age = factor(age, c("< 18", "18 - 24", "> 24"))) %>%
      ggplot(aes(x = year, y = percentage, fill = age)) +
      geom_bar(stat = "identity") +
      labs(fill = "Age", x = "Year", y = "Percentage of Homeless Community")
    
  }, height = 500, width = 500)
  
  # create age total plot
  output$aget <- renderPlot({
    
    cambridge_total %>%
      filter(t_over_twentyfour != 0) %>%
      select(year,
             "< 18" = t_under_eighteen,
             "18 - 24" = t_eighteen_twentyfour,
             "> 24" = t_over_twentyfour) %>%
      gather(key = age, value = total, -year) %>%
      mutate(age = factor(age, c("< 18", "18 - 24", "> 24"))) %>%
      ggplot(aes(x = year, y = total, color = age)) +
      geom_line(size = 1, alpha = 0.5) +
      labs(color = "Age", x = "Year", y = "Total Individuals Experiencing Homelessness")
    
  }, height = 500, width = 500)
  
  # create gender percentage plot
  output$genderp <- renderPlot({
    
      cambridge_total %>%
      filter(p_males != 0) %>%
      select(year,
             "Male" = p_males,
             "Female" = p_females,
             "Trans" = p_trans,
             "Non-binary" = p_non_identifying) %>%
      gather(key = gender, value = percentage, -year) %>%
      ggplot(aes(x = year, y = percentage, fill = gender)) +
      geom_bar(stat = "identity") +
      labs(fill = "Gender", x = "Year", y = "Percentage of Homeless Community")
    
  }, height = 500, width = 500)
  
  # create gender total plot
  output$gendert <- renderPlot({
    
      cambridge_total %>%
      filter(t_males != 0) %>%
      select(year,
             "Male" = t_males,
             "Female" = t_females,
             "Trans" = t_trans,
             "Non-binary" = t_non_identifying) %>%
      gather(key = gender, value = total, -year) %>%
      ggplot(aes(x = year, y = total, color = gender)) +
      geom_line(size = 1, alpha = 0.5) +
      labs(color = "Gender", x = "Year", y = "Total Individuals Experiencing Homelessness")
    
  }, height = 500, width = 500)
  
  # create race percentage plot
  output$racep <- renderPlot({
    
      cambridge_total %>%
      filter(p_white != 0) %>%
      select(year,
             "White" = p_white,
             "Black" = p_black,
             "Asian" = p_asian,
             "Native American" = p_indian_alaskan,
             "Pacific Islander" = p_hawaiian_islander,
             "Multiple" = p_multiple) %>%
      gather(key = race, value = percentage, -year) %>%
      ggplot(aes(x = year, y = percentage, fill = race)) +
      geom_bar(stat = "identity") +
      labs(fill = "Race", x = "Year", y = "Percentage of Homeless Community")
    
  }, height = 500, width = 500)
  
  # create race total plot
  output$racet <- renderPlot({
    
    cambridge_total %>%
      filter(t_white != 0) %>%
      select(year,
             "White" = t_white,
             "Black" = t_black,
             "Asian" = t_asian,
             "Native American" = t_indian_alaskan,
             "Pacific Islander" = t_hawaiian_islander,
             "Multiple" = t_multiple) %>%
      gather(key = race, value = total, -year) %>%
      ggplot(aes(x = year, y = total, color = race)) +
      geom_line(size = 1, alpha = 0.5) +
      labs(color = "Race", x = "Year", y = "Total Individuals Experiencing Homelessness")
    
  }, height = 500, width = 500)
  
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
  
  output$cambridge <- renderText({
    
    paste(
      "Individuals Experiencing Homelessness in Cambridge:", 
      filter(cambridge_total, year == input$year)$persons,
      sep = " ")
    
  })
  
  output$description <- renderText({
    
    "This app draws on data collected by Cambridge government regarding homelessness in the city from 2012-2017. Every year a count of individuals experiencing homelessness is taken on a single night in January. Demographic information about the individuals counted is also collected but is recorded in aggregate for the given year. Cambridge sends its annual homelessness data to the U.S. Department of Housing and Urban Development. This app also uses data from all cities that submitted to HUD from 2013 to 2017."
    
  })
  
  output$git <- renderText({
    
    "The code for this app may be found at: https://github.com/Vumbaca/Final-Project-Vumbaca"
    
  })
  
}

shinyApp(ui = ui, server = server)