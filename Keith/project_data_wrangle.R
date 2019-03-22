library(tidyverse)
library(readr)

dat <- read_csv('https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-two-years.csv')


dat_n <- dat %>% 
  select()