library(tidyverse)
criminal <- read_csv("https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-raw.csv")

two_year <- read_csv("https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-two-years.csv")

criminal %>% 
  filter(DisplayText == "Risk of Recidivism")