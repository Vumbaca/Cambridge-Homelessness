# call library for working with data
library(tidyverse)

# call library for creating shiny app
library(shiny)

# call library for stylizing shiny app
library(shinythemes)

# save data on total Cambridge homeless from file into dataframe
cambridge_total <- read_rds("cambridge_total.rds")

# save data on type of homelessness in Cambridge from file into dataframe
cambridge_type <- read_rds("cambridge_type.rds")

# save national homelessness data from file into dataframe
homeless_total <- read_rds("homeless.rds")

# create shiny app ui and theme
ui <- fluidPage(theme = shinytheme("sandstone"),

  # create navigation bar with app title
  navbarPage("Homelessness in Cambridge, MA",
    
      # create first navigation tab called time
      tabPanel("Time",
               
        # create set of tabs within tab called time
        tabsetPanel(
          
          # create tab called age
          tabPanel("Age", 
                   
                   # create row within tab called age with two columns inside
                   fluidRow(
                     
                     # create half page column with plot of age
                     column(width = 6, plotOutput(outputId = "time_age_plot")),
                     
                     # create half page column with table of age
                     column(width = 6, plotOutput(outputId = "time_age_table")))),
          
          # create tab called gender
          tabPanel("Gender",
                   
                   # create row within tab called gender with two columns inside
                   fluidRow(
                     
                     # create half page column with plot of gender
                     column(width = 6, plotOutput(outputId = "time_gender_plot")),
                     
                     # create half page column with table of gender
                     column(width = 6, plotOutput(outputId = "time_gender_table")))),
          
          # create tab called race
          tabPanel("Race",
                   
                   # create row within tab called race with two columns inside
                   fluidRow(
                     
                     # create half page column with plot of race
                     column(width = 6, plotOutput(outputId = "time_race_plot")),
                     
                     # create half page column with table of race
                     column(width = 6, plotOutput(outputId = "time_race_table")))))),
      
      # create second navigation tab called situation
      tabPanel("Situation",
               
               # create set of tabs within tab called situation
               tabsetPanel(
                 
                 # create another tab called age nested under situation
                 tabPanel("Age",
                          
                          # create row under age tab containing two columns
                          fluidRow(
                            
                            # create half page column with different age plot
                            column(width = 6, plotOutput(outputId = "sit_age_plot")),
                            
                            # create half page column with different age table
                            column(width = 6, tableOutput(outputId = "sit_age_table")))),
                 
                 # create another tab called gender nested under situation
                 tabPanel("Gender",
                          
                          # create row under gender tab containing two columns
                          fluidRow(
                            
                            # create half page column with different gender plot
                            column(width = 6, plotOutput(outputId = "sit_gender_plot")),
                            
                            # create half page column with different gender table
                            column(width = 6, tableOutput(outputId = "sit_gender_table")))),
                 
                 # create another tab called race nested under situation
                 tabPanel("Race",
                          
                          # create row under race tab containing two columns
                          fluidRow(
                            
                            # create half page column with different race plot
                            column(width = 6, plotOutput(outputId = "sit_race_plot")),
                            
                            # create half page column with different race table
                            column(width = 6, tableOutput(outputId = "sit_race_table")))))),
      
      # create third and final navigation tab called place
      tabPanel("Place",
               
               # create one third left side panel for input
               sidebarPanel(
                 
                 # prompt user to select a year
                 selectInput(inputId = "year", label = "Select Year",
                             # choose from years with national and local data
                             choices = c(2014, 2015, 2016, 2017), selected = 2017),
               
                 # prompt user to select a living situation type
                 selectInput(inputId = "sit", label = "Select Living Situation Type",
                             # choose from types of living situation as categorized by data
                             choices = c("Unsheltered", "Emergency", "Transitional", "All"), selected = "All")),
               
               # create two thirds right side panel for output
               mainPanel(
                 
                 # create set of tabs located in main panel
                 tabsetPanel(
                   
                   # create tab called total
                   tabPanel("Total",
                            
                     # create total homeless map
                     leafletOutput(outputId = "total_map"),
                 
                     # create table within tab called total
                     tableOutput(outputId = "total_table"),
                     
                     # create text on total in Cambridge
                     textOutput(outputId = "total_cambridge"),
                     
                     # create nationwide total text
                     textOutput(outputId = "total_nation")),
                   
                   # create tab called veterans
                   tabPanel("Veterans",
                            
                     # create homeless veterans map
                     leafletOutput(outputId = "vet_map"),
                            
                     # create table within tab called veterans
                     tableOutput(outputId = "vet_table"),
                     
                     # create text on veterans in Cambridge
                     textOutput(outputId = "vet_cambridge"),
                     
                     # create nationwide veterans text
                     textOutput(outputId = "vet_nation")),
                   
                   # create tab called chronic
                   tabPanel("Chronically Homeless",
                            
                     # create chronically homeless map
                     leafletOutput(outputId = "chron_map"),
                     
                     # create table within chronically homeless tab
                     tableOutput(output = "chron_table"),
                     
                     # create chronically homeless in Cambridge text
                     textOutput(outputId = "chron_cambridge"),
                     
                     # create text on chronically homeless nationwide
                     textOutput(outputId = "chron_nation")))))))

# create server for app
server <- function(input, output) {
  
  ##################
  
  # create age percentage plot
  output$time_age_plot <- renderPlot({
    
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
  output$time_age_table <- renderPlot({
    
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
  output$time_gender_plot <- renderPlot({
    
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
  output$time_gender_table <- renderPlot({
    
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
  output$time_race_plot <- renderPlot({
    
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
  output$time_race_table <- renderPlot({
    
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
  output$sit_age_plot <- renderPlot({
    
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
  output$sit_age_table <- renderTable({
    
    cambridge_type %>%
      select("Living Situation" = type,
             "< 18" = a_under_eighteen,
             "18 - 24" = a_eighteen_twentyfour,
             "> 24" = a_over_twentyfour)
    
  }, height = 500, width = 500)
  
  # create gender average plot
  output$sit_gender_plot <- renderPlot({
    
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
  output$sit_gender_table <- renderTable({
    
    cambridge_type %>%
      select("Living Situation" = type,
             "Male" = a_males,
             "Female" = a_females,
             "Trans" = a_trans,
             "Non-binary" = a_non_identifying)
    
  }, height = 500, width = 500)
  
  # create race average plot
  output$sit_race_plot <- renderPlot({
    
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
  output$sit_race_table <- renderTable({
    
    cambridge_type %>%
      select("Living Situation" = type,
             "White" = a_white,
             "Black" = a_black,
             "Asian" = a_asian,
             "Native American" = a_indian_alaskan,
             "Pacific Islander" = a_hawaiian_islander,
             "Multiple" = a_multiple)
    
  }, height = 500, width = 500)
  
  output$total_table <- renderTable({
    
    homeless_total %>%
      filter(year == input$year & coc_name != "Total") %>%
      arrange(desc(total)) %>%
      select("Continuum of Care" = coc_name,
             "Individuals Experiencing Homelessness" = total) %>%
      head(10)
    
  })
  
  output$total_cambridge <- renderText({
    
    paste(
      "Individuals Experiencing Homelessness in Cambridge:", 
      filter(cambridge_total, year == input$year)$persons,
      sep = " ")
    
  })
  
  output$total_nation <- renderText({
    
    paste(
      "Individuals Experiencing Homelessness in the United States:", 
      filter(homeless_total, coc_name == "Total" & year == input$year)$total,
      sep = " ")
    
  })
  
  output$vet_table <- renderTable({
    
    homeless_total %>%
      filter(year == input$year & coc_name != "Total") %>%
      arrange(desc(veterans)) %>%
      select("Continuum of Care" = coc_name,
             "Veterans Experiencing Homelessness" = veterans) %>%
      head(10)
    
  })
  
  output$vet_cambridge <- renderText({
    
    paste(
      "Veterans Experiencing Homelessness in Cambridge:", 
      filter(cambridge_total, year == input$year)$veteran,
      sep = " ")
    
  })
  
  output$vet_nation <- renderText({
    
    paste(
      "Veterans Experiencing Homelessness in the United States:", 
      filter(homeless_total, coc_name == "Total" & year == input$year)$veterans,
      sep = " ")
    
  })
  
  output$chron_table <- renderTable({
    
    homeless_total %>%
      filter(year == input$year & coc_name != "Total") %>%
      arrange(desc(chronic)) %>%
      select("Continuum of Care" = coc_name,
             "Individuals Experiencing Chronic Homelessness" = chronic) %>%
      head(10)
    
  })
  
  output$chron_cambridge <- renderText({
    
    paste(
      "Individuals Experiencing Chronic Homelessness in Cambridge:", 
      filter(cambridge_total, year == input$year)$chronic,
      sep = " ")
    
  })
  
  output$chron_nation <- renderText({
    
    paste(
      "Individuals Experiencing Chronic Homelessness in the United States:", 
      filter(homeless_total, coc_name == "Total" & year == input$year)$chronic,
      sep = " ")
    
  })
  
}

shinyApp(ui = ui, server = server)