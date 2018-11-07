library(shiny)
library(ggplot2)

function(input, output) {
  
  # dataset <- reactive({
  #   cambridge_01 %>%
  #   slice(((input$start_year - 2011) * 3): (((input$end_year - 2011) * 3) + 2)) %>%
  #   filter(housing == case_when(
  #     input$emergency & !input$transitonal ~ "emergency",
  #     !input$emergency & input$transitonal ~ "transitional",
  #     TRUE ~ housing))
  # })
  
  output$plot <- renderPlot({
    
    cambridge_01 %>%
      filter(year == input$year) %>%
      ggplot(aes(x = "", y = input$demo2, fill = input$demo)) + geom_bar(width = 1, stat = "identity")

  }, height=700)
  
}