
#load required packages
library(shiny)
library(tidycensus)
library(tidyverse)
library(readr)

#install census key
census_api_key("4e9d7fd959555208210856aaa5061b593c3722af", install = T, overwrite = T)

#create label for the year of the update
label <- load_variables(2016, "acs5", cache = TRUE)
label <- label %>%
  select(-concept) %>% 
  mutate(name = sub('E$', '',name), # get rid of E at the end of each name
         label = sub('Estimate!!', '',label)) %>%
  rename(variable = name)


source("edu_help_function.R")

edu_state <-edu_help_function(geo="state", year=2016)
