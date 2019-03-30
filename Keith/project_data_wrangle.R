
################################library##############################################
library(tidyverse)
library(readr)

################################Read File############################################
dat <- read_csv('https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-two-years.csv')


################################data wrangle#########################################
dat_n <- dat %>% 
  select(sex,  
         age, 
         age_cat, 
         race, 
         juv_fel_count,
         juv_misd_count, 
         juv_other_count, 
         priors_count, 
         c_charge_degree,
         c_charge_desc,
         is_recid, 
         two_year_recid)

c_cu <- dat_n$c_charge_desc %>% 
  unique()

c_charge <- tibble(c_charge_desc = c_cu) %>% 
  mutate(index = row_number())

dat_n %>% 
  left_join(c_charge) %>% 
  mutate(c_charge_desc = )




