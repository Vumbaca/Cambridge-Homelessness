# call library for tidying of data
library(tidyverse)

# call library for reading in data
library(readxl)

# save data from csv into dataframe
cambridge <- read_csv("Data/Cambridge_Homeless_Point-in-Time_Count_data__2012-2018.csv", col_names = FALSE, skip = 1)

# replace all na values with zeroes
cambridge[is.na(cambridge)] <- 0

# create base dataframe with yearly count of persons experiencing homelessness
cambridge_total <- cambridge %>%
  # keep only year and persons variables
  select("year" = X1,
         "persons" = X5) %>%
  # sort observations by year
  group_by(year) %>%
  # calculate total number of persons for all observations in each given year
  summarize(persons = sum(persons))

# save gender information as separate dataframe
cambridge_gender <- cambridge %>%
  # keep variables of year, persons, and counts of various gender identity groups
  select("year" = X1,
         "persons" = X5,
         "females" = X11,
         "males" = X12,
         "trans" = X13,
         "non_identifying" = X14) %>%
  # sort observations by year
  group_by(year) %>%
  # calculate annual totals and percentages for each gender identity group
  summarize(t_males = sum(males),
            t_females = sum(females),
            t_trans = sum(trans),
            t_non_identifying = sum(non_identifying),
            p_males = sum(males) / sum(persons),
            p_females = sum(females) / sum(persons),
            p_trans = sum(trans) / sum(persons),
            p_non_identifying = sum(non_identifying) / sum(persons))

# supplement base dataframe by adding gender information for each year
cambridge_total <- left_join(cambridge_total, cambridge_gender, by = "year")

# save race information as a separate dataframe
cambridge_race <- cambridge %>%
  # keep variables of year, persons, and counts of various racial identity groups
  select("year" = X1,
         "persons" = X5,
         "white" = X20,
         "black" = X21,
         "asian" = X22,
         "indian_alaskan" = X23,
         "hawaiian_islander" = X24,
         "multiple" = X25) %>%
  # sort observations by year
  group_by(year) %>%
  # calculate annual totals and percentages for each gender identity group
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

# supplement base dataframe by adding race information for each year
cambridge_total <- left_join(cambridge_total, cambridge_race, by = "year")

# save age information as separate dataframe
cambridge_age <- cambridge %>%
  # keep variables of year, persons, and counts of various age range groups
  select("year" = X1, 
         "persons" = X5,
         "under_eighteen" = X8,
         "eighteen_twentyfour" = X9,
         "over_twentyfour" = X10) %>%
  # sort observations by year
  group_by(year) %>%
  # calculate annual totals and percentages for each gender identity group 
  summarize(t_under_eighteen = sum(under_eighteen),
            t_eighteen_twentyfour = sum(eighteen_twentyfour),
            t_over_twentyfour = sum(over_twentyfour),
            p_under_eighteen = sum(under_eighteen) / sum(persons),
            p_eighteen_twentyfour = sum(eighteen_twentyfour) / sum(persons),
            p_over_twentyfour = sum(over_twentyfour) / sum(persons))

# supplement base dataframe by adding age information for each year
cambridge_total <- left_join(cambridge_total, cambridge_age, by = "year")

# save other information as separate dataframe
cambridge_other <- cambridge %>%
  # keep variables of year, persons, and counts of various other notable groups
  select("year" = X1,
         "persons" = X5,
         "veteran" = X15,
         "mental" = X26,
         "substance" = X27,
         "hiv" = X28,
         "domestic" = X29,
         "chronic" = X30) %>%
  # sort observations by year
  group_by(year) %>%
  # calculate totals for each other notable group within each year
  summarize(veteran = sum(veteran),
            mental = sum(mental),
            substance = sum(substance),
            hiv = sum(hiv),
            domestic = sum(domestic),
            chronic = sum(chronic))

# supplement base dataframe by adding age information for each year
cambridge_total <- left_join(cambridge_total, cambridge_other, by = "year")

# save base dataframe to file which can be read by shiny app
write_rds(cambridge_total, "Homeless/cambridge_total.rds", compress = "gz")

# create base dataframe with average number of persons in each type of living situation over past seven years
cambridge_type <- cambridge %>%
  # keep only type and persons variables
  select("type" = X3,
         "persons" = X5) %>%
  # sort observations by type
  group_by(type) %>%
  # calculate average number of persons for all observations of each given type
  summarize(persons = mean(persons))

# save gender information as separate dataframe
cambridge_g <- cambridge %>%
  # keep variables of type, persons, and counts of various gender identity groups
  select("type" = X3,
         "persons" = X5,
         "females" = X11,
         "males" = X12,
         "trans" = X13,
         "non_identifying" = X14) %>%
  # sort observations by type
  group_by(type) %>%
  # calculate average totals and average percentages of each gender identity group within each type
  summarize(a_males = round(mean(males), digits = 2),
            a_females = round(mean(females), digits = 2),
            a_trans = round(mean(trans), digits = 2),
            a_non_identifying = round(mean(non_identifying), digits = 2),
            p_males = mean(males) / mean(persons),
            p_females = mean(females) / mean(persons),
            p_trans = mean(trans) / mean(persons),
            p_non_identifying = mean(non_identifying) / mean(persons))

# supplement base dataframe by adding gender information for each type
cambridge_type <- left_join(cambridge_type, cambridge_g, by = "type")

# save race information as separate dataframe
cambridge_r <- cambridge %>%
  # keep variables of type, persons, and counts of various racial identity groups
  select("type" = X3,
         "persons" = X5,
         "white" = X20,
         "black" = X21,
         "asian" = X22,
         "indian_alaskan" = X23,
         "hawaiian_islander" = X24,
         "multiple" = X25) %>%
  # sort observations by type
  group_by(type) %>%
  # calculate average totals and average percentages of each racial identity group within each type
  summarize(a_white = round(mean(white), digits = 2),
            a_black = round(mean(black), digits = 2),
            a_asian = round(mean(asian), digits = 2),
            a_indian_alaskan = round(mean(indian_alaskan), digits = 2),
            a_hawaiian_islander = round(mean(hawaiian_islander), digits = 2),
            a_multiple = round(mean(multiple), digits = 2),
            p_white = mean(white) / mean(persons),
            p_black = mean(black) / mean(persons),
            p_asian = mean(asian) / mean(persons),
            p_indian_alaskan = mean(indian_alaskan) / mean(persons),
            p_hawaiian_islander = mean(hawaiian_islander) / mean(persons),
            p_multiple = mean(multiple) / mean(persons))

# supplement base dataframe by adding race information for each type
cambridge_type <- left_join(cambridge_type, cambridge_r, by = "type")

# save age information as separate dataframe
cambridge_a <- cambridge %>%
  # keep variables of type, persons, and counts of various age range groups
  select("type" = X3, 
         "persons" = X5,
         "under_eighteen" = X8,
         "eighteen_twentyfour" = X9,
         "over_twentyfour" = X10) %>%
  # sort observations by type
  group_by(type) %>%
  # calculate average totals and average percentages of each age range group within each type
  summarize(a_under_eighteen = round(mean(under_eighteen, digits = 2)),
            a_eighteen_twentyfour = round(mean(eighteen_twentyfour, digits = 2)),
            a_over_twentyfour = round(mean(over_twentyfour), digits = 2),
            p_under_eighteen = mean(under_eighteen) / mean(persons),
            p_eighteen_twentyfour = mean(eighteen_twentyfour) / mean(persons),
            p_over_twentyfour = mean(over_twentyfour) / mean(persons))

# supplement base dataframe by adding age information for each type
cambridge_type <- left_join(cambridge_type, cambridge_a, by = "type")

# save other information as separate dataframe
cambridge_o <- cambridge %>%
  # keep variables of type, persons, and counts of various other notable groups
  select("type" = X3,
         "persons" = X5,
         "veteran" = X15,
         "mental" = X26,
         "substance" = X27,
         "hiv" = X28,
         "domestic" = X29,
         "chronic" = X30) %>%
  # sort observations by type
  group_by(type) %>%
  # calculate averages of each other notable group within each type
  summarize(a_veteran = mean(veteran),
            a_mental = mean(mental),
            a_substance = mean(substance),
            a_hiv = mean(hiv),
            a_domestic = mean(domestic),
            a_chronic = mean(chronic))

# supplement base dataframe by adding other information for each type
cambridge_type <- left_join(cambridge_type, cambridge_o, by = "type")

# save base dataframe to file which can be read by shiny app
write_rds(cambridge_type, "Homeless/cambridge_type.rds", compress = "gz")

# read data from 2017 into dataframe
homeless_17 <- read_excel("Data/2007-2017-PIT-Counts-by-CoC.XLSX")

# read data from 2016 into dataframe
homeless_16 <- read_excel("Data/2007-2016-PIT-Counts-by-CoC.XLSX")

# read data from 2015 into dataframe
homeless_15 <- read_excel("Data/2007-2015-PIT-Counts-by-CoC.XLSX")

# read data from 2014 into dataframe
homeless_14 <- read_excel("Data/2007-2014-PIT-Counts-by-CoC.XLSX")

# read data from 2013 into dataframe
homeless_13 <- read_excel("Data/2007-2013-PIT-Counts-by-CoC.XLSX")

# add 2016 data to 2017 data linked according to continuum of care to create base dataframe
homeless <- left_join(homeless_17, homeless_16, by = c("CoC Number", "CoC Name"))

# add 2015 data to base dataframe
homeless <- left_join(homeless, homeless_15, by = c("CoC Number", "CoC Name"))

# add 2014 data to base dataframe
homeless <- left_join(homeless, homeless_14, by = c("CoC Number", "CoC Name"))

# add 2014 data to base dataframe
homeless <- left_join(homeless, homeless_13, by = c("CoC Number", "CoC Name"))

# save reorganized data to base dataframe
homeless_total <- homeless %>%
  # keep key variables only
  select(coc_num = `CoC Number`,
         coc_name = `CoC Name`,
         "2017" = `Total Homeless, 2017`,
         "2016" = `Total Homeless, 2016`,
         "2015" = `Total Homeless, 2015`,
         "2014" = `Total Homeless, 2014`,
         "2013" = `Total Homeless 2013`) %>%
  # reorganize data
  gather(key = year, value = total, -coc_name, -coc_num)

# save reorganized chronic homelessness information to separate dataframe
homeless_chronic <- homeless %>%
  # keep key variables only
  select(coc_num = `CoC Number`,
         coc_name = `CoC Name`,
         "2017" = `Chronically Homeless, 2017`,
         "2016" = `Chronically Homeless, 2016`,
         "2015" = `Chronically Homeless, 2015`,
         "2014" = `Chronically Homeless, 2014`,
         "2013" = `Total Chronically Homeless 2013`) %>%
  # reorganize data
  gather(key = year, value = chronic, -coc_name, -coc_num)

# add data on chronic homelessness to base dataframe
homeless_total <- left_join(homeless_total, homeless_chronic, by = c("year", "coc_num", "coc_name"))

# save reorganized veteran homelessness information to separate dataframe
homeless_veteran <- homeless %>%
  # keep key variables only
  select(coc_num = `CoC Number`,
         coc_name = `CoC Name`,
         "2017" = `Homeless Veterans, 2017`,
         "2016" = `Homeless Veterans, 2016`,
         "2015" = `Homeless Veterans, 2015`,
         "2014" = `Homeless Veterans, 2014`,
         "2013" = `Total Veterans 2013`) %>%
  # reorganize data
  gather(key = year, value = veterans, -coc_name, -coc_num)

# add data on veteran homelessness to base dataframe
homeless_total <- left_join(homeless_total, homeless_veteran, by = c("year", "coc_num", "coc_name"))
  
# save cleaned coc_name data
homeless_total$coc_name <- homeless_total$coc_name %>%
  # remove phrase continuum of care from all coc_names
  str_remove(" Continuum of Care") %>%
  # remove its abbreviation too
  str_remove(" CoC") %>%
  # remove phrase balance of state from all coc_names
  str_remove(" Balance of State") %>%
  # remove its abbreviation too
  str_remove(" \\(BoS\\)") %>%
  # remove commonwealth version also
  str_remove(" Balance of Commonwealth") %>%
  # remove word statewide from all coc_names
  str_remove(" Statewide") %>%
  # remove word city from all coc_names
  str_remove(" City") %>%
  # remove word county from all coc_names
  str_remove(" County") %>%
  # remove word and from all coc_names
  str_remove(" and") %>%
  # remove ampersand symbol from all coc_names
  str_remove(" &") %>%
  # remove any content after slashes from all coc_names
  str_remove("/.{1,}") %>%
  # remove any content after commas from all coc_names
  str_remove(",.{1,}") %>%
  # remove any content after dashes from all coc_names
  str_remove("-.{1,}") %>%
  # remove word metropolitan from all coc_names
  str_remove("Metropolitan ") %>%
  # remove phrase homeless initiative from all coc_names also
  str_remove(" Homeless Initiative")

# create new variable state from part of coc_num
homeless_total$state <- homeless_total$coc_num %>%
  # keep only parts of coc_num other than dash and next three characters after it
  str_remove("-.{3}")

# save data with location variable to base dataframe
homeless_total <- homeless_total %>%
  # create new location variable from coc_name and state
  mutate(location = paste(coc_name, state, sep = ", "))

# save base dataframe to file which can be read by shiny app
write_rds(homeless_total, "Homeless/homeless.rds", compress = "gz")
