library(shiny)
library(ggplot2)

cambridge <- read_csv("Cambridge_Homeless_Point-in-Time_Count_data__2012-2018.csv", col_names =  FALSE, skip = 1)

cambridge_gender <- cambridge %>%
  select("year" = X1, "persons" = X5, "females" = X11, "males" = X12, "trans" = X13, "non_identifying" = X14) %>%
  group_by(year) %>%
  summarize(males = sum(males) / sum(persons), females = sum(females) / sum(persons), trans = sum(trans) / sum(persons), non_identifying = sum(non_identifying) / sum(persons)) %>%
  ungroup() %>%
  gather(key = gender, value = gender_percentage, -year)

cambridge_race <- cambridge %>%
  select("year" = X1, "persons" = X5, "white" = X20, "black" = X21, "asian" = X22, "indian_alaskan" = X23, "hawaiian_islander" = X24, "multiple" = X25) %>%
  group_by(year) %>%
  summarize(white = sum(white) / sum(persons), black = sum(black) / sum(persons), asian = sum(asian) / sum(persons), indian_alaskan = sum(indian_alaskan) / sum(persons), hawaiian_islander = sum(hawaiian_islander) / sum(persons), multiple = sum(multiple) / sum(persons)) %>%
  ungroup() %>%
  gather(key = race, value = race_percentage, -year)

cambridge_age <- cambridge %>%
  select("year" = X1,  "persons" = X5, "under_eighteen" = X8, "eighteen_twentyfour" = X9, "over_twentyfour" = X10) %>%
  group_by(year) %>%
  summarize(under_eighteen = sum(under_eighteen) / sum(persons), eighteen_twentyfour = sum(eighteen_twentyfour) / sum(persons), over_twentyfour = sum(over_twentyfour) / sum(persons)) %>%
  gather(key = age, value = age_percentage, -year)

cambridge_01 <- bind_rows(bind_rows(cambridge_age, cambridge_gender), cambridge_race)

fluidPage(
  
  titlePanel("Homelessness in Cambridge, MA"),
  
  sidebarPanel(
    selectInput(inputId = "demo", label = "Demographic", choices = c("age", "gender", "race"), selected = "gender"),
    
    selectInput(inputId = "demo2", label = "Demographic2", choices = c("age_percentage", "gender_percentage", "race_percentage"), selected = "gender_percentage"),
      
    selectInput(inputId = "year", label = "Year", choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018"), selected = "2018")
      
      # sliderInput("start_year", "Start Year", min = min(dataset$year), max = max(dataset$year),
      #           value = min(dataset$year), step = 1, round = 0),
      # 
      # sliderInput("end_year", "End Year", min = min(dataset$year), max=max(dataset$year),
      #           value = max(dataset$year), step = 1, round = 0),
      #
      # checkboxInput("emergency", "Emergency"),
      # 
      # checkboxInput("transitonal", "Transitional"),
    
    ),
  
  mainPanel(
    plotOutput(outputId = "plot")
  )
)