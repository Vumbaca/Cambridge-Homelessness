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
      
      tabPanel("Situation",
               
               tabsetPanel(
                 
                 tabPanel("Age",
                          
                          fluidRow(
                            
                            column(width = 6, plotOutput(outputId = "ageplot")),
                            
                            column(width = 6, tableOutput(outputId = "agetable")))),
                 
                 tabPanel("Gender",
                          
                          fluidRow(
                            
                            column(width = 6, plotOutput(outputId = "genderplot")),
                            
                            column(width = 6, tableOutput(outputId = "gendertable")))),
                 
                 tabPanel("Race",
                          
                          fluidRow(
                            
                            column(width = 6, plotOutput(outputId = "raceplot")),
                            
                            column(width = 6, tableOutput(outputId = "racetable")))))),
      
      tabPanel("Place",
               
               sidebarPanel(
                 
                 selectInput(inputId = "year", label = "Year", choices = c(2014, 2015, 2016, 2017), selected = 2017)),
               
               mainPanel(
                 
                 tabsetPanel(
                   
                   tabPanel("General",
                 
                     tableOutput(outputId = "totaltable"),
                     
                     textOutput(outputId = "cambridge"),
                     
                     textOutput(outputId = "nationwide")),
                   
                   tabPanel("Veterans",
                            
                     tableOutput(outputId = "vettable"),
                     
                     textOutput(outputId = "vetcambridge"),
                     
                     textOutput(outputId = "vetnationwide")),
                   
                   tabPanel("Chronic",
                            
                     tableOutput(output = "chrtable"),
                     
                     textOutput(outputId = "chrcambridge"),
                     
                     textOutput(outputId = "chrnationwide")))))))

# create server for app
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
  
  # create age average plot
  output$ageplot <- renderPlot({
    
    cambridge_type %>%
      select(type,
             "< 18" = a_under_eighteen,
             "18 - 24" = a_eighteen_twentyfour,
             "> 24" = a_over_twentyfour) %>%
      gather(key = age, value = average, -type) %>%
      mutate(age = factor(age, c("< 18", "18 - 24", "> 24"))) %>%
      mutate(type = factor(type, c("Unsheltered", "Emergency Shelter", "Transitional Housing"))) %>%
      ggplot(aes(x = type, y = average, fill = age)) +
      geom_bar(stat = "identity") +
      labs(fill = "Age", x = "Living Situation", y = "Average Individuals Experiencing Homelessness 2012-2017")
    
  }, height = 500, width = 500)
  
  # create age average table
  output$agetable <- renderTable({
    
    cambridge_type %>%
      select("Living Situation" = type,
             "< 18" = a_under_eighteen,
             "18 - 24" = a_eighteen_twentyfour,
             "> 24" = a_over_twentyfour)
    
  }, height = 500, width = 500)
  
  # create gender average plot
  output$genderplot <- renderPlot({
    
    cambridge_type %>%
      select(type,
             "Male" = a_males,
             "Female" = a_females,
             "Trans" = a_trans,
             "Non-binary" = a_non_identifying) %>%
      gather(key = gender, value = average, -type) %>%
      mutate(type = factor(type, c("Unsheltered", "Emergency Shelter", "Transitional Housing"))) %>%
      ggplot(aes(x = type, y = average, fill = gender)) +
      geom_bar(stat = "identity") +
      labs(fill = "Gender", x = "Living Situation", y = "Average Individuals Experiencing Homelessness 2012-2017")
    
  }, height = 500, width = 500)
  
  # create gender average table
  output$gendertable <- renderTable({
    
    cambridge_type %>%
      select("Living Situation" = type,
             "Male" = a_males,
             "Female" = a_females,
             "Trans" = a_trans,
             "Non-binary" = a_non_identifying)
    
  }, height = 500, width = 500)
  
  # create race average plot
  output$raceplot <- renderPlot({
    
    cambridge_type %>%
      select(type,
             "White" = a_white,
             "Black" = a_black,
             "Asian" = a_asian,
             "Native American" = a_indian_alaskan,
             "Pacific Islander" = a_hawaiian_islander,
             "Multiple" = a_multiple) %>%
      gather(key = race, value = average, -type) %>%
      mutate(type = factor(type, c("Unsheltered", "Emergency Shelter", "Transitional Housing"))) %>%
      ggplot(aes(x = type, y = average, fill = race)) +
      geom_bar(stat = "identity") +
      labs(fill = "Race", x = "Living Situation", y = "Average Individuals Experiencing Homelessness 2012-2017")
    
  }, height = 500, width = 500)
  
  # create race average table
  output$racetable <- renderTable({
    
    cambridge_type %>%
      select("Living Situation" = type,
             "White" = a_white,
             "Black" = a_black,
             "Asian" = a_asian,
             "Native American" = a_indian_alaskan,
             "Pacific Islander" = a_hawaiian_islander,
             "Multiple" = a_multiple)
    
  }, height = 500, width = 500)
  
  output$totaltable <- renderTable({
    
    homeless_total %>%
      filter(year == input$year & coc_name != "Total") %>%
      arrange(desc(total)) %>%
      select("Continuum of Care" = coc_name,
             "Individuals Experiencing Homelessness" = total) %>%
      head(10)
    
  })
  
  output$cambridge <- renderText({
    
    paste(
      "Individuals Experiencing Homelessness in Cambridge:", 
      filter(cambridge_total, year == input$year)$persons,
      sep = " ")
    
  })
  
  output$nationwide <- renderText({
    
    paste(
      "Individuals Experiencing Homelessness in the United States:", 
      filter(homeless_total, coc_name == "Total" & year == input$year)$total,
      sep = " ")
    
  })
  
  output$vettable <- renderTable({
    
    homeless_total %>%
      filter(year == input$year & coc_name != "Total") %>%
      arrange(desc(veterans)) %>%
      select("Continuum of Care" = coc_name,
             "Veterans Experiencing Homelessness" = veterans) %>%
      head(10)
    
  })
  
  output$vetcambridge <- renderText({
    
    paste(
      "Veterans Experiencing Homelessness in Cambridge:", 
      filter(cambridge_total, year == input$year)$veteran,
      sep = " ")
    
  })
  
  output$vetnationwide <- renderText({
    
    paste(
      "Veterans Experiencing Homelessness in the United States:", 
      filter(homeless_total, coc_name == "Total" & year == input$year)$veterans,
      sep = " ")
    
  })
  
  output$chrtable <- renderTable({
    
    homeless_total %>%
      filter(year == input$year & coc_name != "Total") %>%
      arrange(desc(chronic)) %>%
      select("Continuum of Care" = coc_name,
             "Individuals Experiencing Chronic Homelessness" = chronic) %>%
      head(10)
    
  })
  
  output$chrcambridge <- renderText({
    
    paste(
      "Individuals Experiencing Chronic Homelessness in Cambridge:", 
      filter(cambridge_total, year == input$year)$chronic,
      sep = " ")
    
  })
  
  output$chrnationwide <- renderText({
    
    paste(
      "Individuals Experiencing Chronic Homelessness in the United States:", 
      filter(homeless_total, coc_name == "Total" & year == input$year)$chronic,
      sep = " ")
    
  })
  
}

shinyApp(ui = ui, server = server)