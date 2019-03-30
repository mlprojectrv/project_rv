library(tidyverse)
library(readr)


################################Read File############################################
dat <- read_csv('https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-two-years.csv')


################################data wrangle#########################################
dat_n <- dat %>% 
  select(sex,  age, age_cat, race, juv_fel_count, juv_misd_count, juv_other_count, priors_count, c_charge_degree,
         c_charge_desc, is_recid, two_year_recid)

c_cu <- dat_n$c_charge_desc %>% 
  unique()

c_charge <- tibble(c_charge_desc = c_cu) %>% 
  mutate(index = row_number())

dat_n <- dat_n %>% 
  left_join(c_charge) %>% 
  mutate(c_charge_desc = index)

race <- dat_n %>% 
  select(-age_cat) %>% 
  mutate(sex = case_when(
    sex == "Male" ~ 1,
    sex == "Female" ~ 0
  ), c_charge_degree = case_when(
    c_charge_degree == "M" ~ "1",
    c_charge_degree == "F" ~ "0"
  ), race = case_when(
    race == "Caucasian" ~ "1",
    race == "African-American" ~ "0",
    race == "Hispanic" ~ "2",
    race == "Other" ~ "3"))

no_race <- race %>% 
  select(-race)


