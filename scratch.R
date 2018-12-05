library(tidyverse)
library(readxl)

cambridge <- read_csv("Data/Cambridge_Homeless_Point-in-Time_Count_data__2012-2018.csv", col_names =  FALSE, skip = 1)

cambridge[is.na(cambridge)] <- 0

cambridge_total <- cambridge %>%
  select("year" = X1, "persons" = X5) %>%
  group_by(year) %>%
  summarize(persons = sum(persons))

cambridge_gender <- cambridge %>%
  select("year" = X1,
         "persons" = X5,
         "females" = X11,
         "males" = X12,
         "trans" = X13,
         "non_identifying" = X14) %>%
  group_by(year) %>%
  summarize(t_males = sum(males),
            t_females = sum(females),
            t_trans = sum(trans),
            t_non_identifying = sum(non_identifying),
            p_males = sum(males) / sum(persons),
            p_females = sum(females) / sum(persons),
            p_trans = sum(trans) / sum(persons),
            p_non_identifying = sum(non_identifying) / sum(persons))

cambridge_total <- left_join(cambridge_total, cambridge_gender, by = "year")

cambridge_race <- cambridge %>%
  select("year" = X1,
         "persons" = X5,
         "white" = X20,
         "black" = X21,
         "asian" = X22,
         "indian_alaskan" = X23,
         "hawaiian_islander" = X24,
         "multiple" = X25) %>%
  group_by(year) %>%
  summarize(t_white = sum(white),
            t_black = sum(black),
            t_asian = sum(asian),
            t_indian_alaskan = sum(indian_alaskan),
            t_hawaiian_islander = sum(hawaiian_islander),
            t_multiple = sum(multiple),
            p_white = sum(white) / sum(persons),
            p_black = sum(black) / sum(persons),
            p_asian = sum(asian) / sum(persons),
            p_indian_alaskan = sum(indian_alaskan) / sum(persons),
            p_hawaiian_islander = sum(hawaiian_islander) / sum(persons),
            p_multiple = sum(multiple) / sum(persons))

cambridge_total <- left_join(cambridge_total, cambridge_race, by = "year")

cambridge_age <- cambridge %>%
  select("year" = X1, 
         "persons" = X5,
         "under_eighteen" = X8,
         "eighteen_twentyfour" = X9,
         "over_twentyfour" = X10) %>%
  group_by(year) %>%
  summarize(t_under_eighteen = sum(under_eighteen),
            t_eighteen_twentyfour = sum(eighteen_twentyfour),
            t_over_twentyfour = sum(over_twentyfour),
            p_under_eighteen = sum(under_eighteen) / sum(persons),
            p_eighteen_twentyfour = sum(eighteen_twentyfour) / sum(persons),
            p_over_twentyfour = sum(over_twentyfour) / sum(persons))

cambridge_total <- left_join(cambridge_total, cambridge_age, by = "year")

cambridge_child <- cambridge %>%
  select("year" = X1,
         "persons" = X5,
         "children" = X6,
         "single" = X7) %>%
  group_by(year) %>%
  summarize(t_children = sum(children),
            t_single = sum(single),
            p_children = sum(children) / sum(persons),
            p_single = sum(single) / sum(persons))

cambridge_other <- cambridge %>%
  select("year" = X5,
         "veteran" = X15,
         "mental" = X26,
         "substance" = X27,
         "hiv" = X28,
         "domestic" = X29,
         "chronic" = X30) %>%
  group_by(year) %>%
  summarize(veteran = sum(veteran),
            mental = sum(mental),
            substance = sum(substance),
            hiv = sum(hiv),
            domestic = sum(domestic),
            chronic = sum(chronic))

cambridge_total <- left_join(cambridge_total, cambridge_other, by = "year")

write_rds(cambridge_total, "Homeless/cambridge_total.rds", compress = "gz")

cambridge_type <- cambridge %>%
  select("type" = X3, "persons" = X5) %>%
  group_by(type) %>%
  summarize(persons = sum(persons))

cambridge_gendert <- cambridge %>%
  select("type" = X3,
         "persons" = X5,
         "females" = X11,
         "males" = X12,
         "trans" = X13,
         "non_identifying" = X14) %>%
  group_by(type) %>%
  summarize(t_males = sum(males),
            t_females = sum(females),
            t_trans = sum(trans),
            t_non_identifying = sum(non_identifying),
            p_males = sum(males) / sum(persons),
            p_females = sum(females) / sum(persons),
            p_trans = sum(trans) / sum(persons),
            p_non_identifying = sum(non_identifying) / sum(persons))

cambridge_type <- left_join(cambridge_type, cambridge_gendert, by = "type")

cambridge_racet <- cambridge %>%
  select("type" = X3,
         "persons" = X5,
         "white" = X20,
         "black" = X21,
         "asian" = X22,
         "indian_alaskan" = X23,
         "hawaiian_islander" = X24,
         "multiple" = X25) %>%
  group_by(type) %>%
  summarize(t_white = sum(white),
            t_black = sum(black),
            t_asian = sum(asian),
            t_indian_alaskan = sum(indian_alaskan),
            t_hawaiian_islander = sum(hawaiian_islander),
            t_multiple = sum(multiple),
            p_white = sum(white) / sum(persons),
            p_black = sum(black) / sum(persons),
            p_asian = sum(asian) / sum(persons),
            p_indian_alaskan = sum(indian_alaskan) / sum(persons),
            p_hawaiian_islander = sum(hawaiian_islander) / sum(persons),
            p_multiple = sum(multiple) / sum(persons))

cambridge_type <- left_join(cambridge_type, cambridge_racet, by = "type")

cambridge_aget <- cambridge %>%
  select("type" = X3, 
         "persons" = X5,
         "under_eighteen" = X8,
         "eighteen_twentyfour" = X9,
         "over_twentyfour" = X10) %>%
  group_by(type) %>%
  summarize(t_under_eighteen = sum(under_eighteen),
            t_eighteen_twentyfour = sum(eighteen_twentyfour),
            t_over_twentyfour = sum(over_twentyfour),
            p_under_eighteen = sum(under_eighteen) / sum(persons),
            p_eighteen_twentyfour = sum(eighteen_twentyfour) / sum(persons),
            p_over_twentyfour = sum(over_twentyfour) / sum(persons))

cambridge_type <- left_join(cambridge_type, cambridge_aget, by = "type")

write_rds(cambridge_type, "Homeless/cambridge_type.rds", compress = "gz")

homeless_17 <- read_excel("Data/2007-2017-PIT-Counts-by-CoC.XLSX")
homeless_16 <- read_excel("Data/2007-2016-PIT-Counts-by-CoC.XLSX")
homeless_15 <- read_excel("Data/2007-2015-PIT-Counts-by-CoC.XLSX")
homeless_14 <- read_excel("Data/2007-2014-PIT-Counts-by-CoC.XLSX")
homeless_13 <- read_excel("Data/2007-2013-PIT-Counts-by-CoC.XLSX")
homeless_total <- left_join(homeless_17, homeless_16, by = c("CoC Number", "CoC Name"))
homeless_total <- left_join(homeless_total, homeless_15, by = c("CoC Number", "CoC Name"))
homeless_total <- left_join(homeless_total, homeless_14, by = c("CoC Number", "CoC Name"))
homeless_total <- left_join(homeless_total, homeless_13, by = c("CoC Number", "CoC Name"))

homeless_total <- homeless_total %>%
  select(coc_num = `CoC Number`,
         coc_name = `CoC Name`,
         "2017" = `Total Homeless, 2017`,
         "2016" = `Total Homeless, 2016`,
         "2015" = `Total Homeless, 2015`,
         "2014" = `Total Homeless, 2014`,
         "2013" = `Total Homeless 2013`) %>%
  gather(key = year, value = count, -coc_name, -coc_num)
  
write_rds(homeless_total, "Homeless/homeless.rds", compress = "gz")
