library(tidyverse)
library(readr)


################################Read File############################################
dat <- read_csv('https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-two-years.csv')


################################data wrangle#########################################
dat_n <- dat %>% 
  select(sex,  age, age_cat, race, juv_fel_count, juv_misd_count, juv_other_count, priors_count, c_charge_degree,
         c_charge_desc, is_recid, two_year_recid) %>% 
  drop_na()


c_cu <- dat_n$c_charge_desc %>% 
  unique()

c_charge <- tibble(c_charge_desc = c_cu) %>% 
  mutate(index = row_number())

dat_n <- dat_n %>% 
  left_join(c_charge) %>% 
  mutate(c_charge_desc = index) 
  

race <- dat_n %>% 
  select(-age_cat) %>% 
  select(-is_recid) %>% 
  select(-index) %>% 
  mutate(sex = case_when(
    sex == "Male" ~ 1,
    sex == "Female" ~ 0
  ), c_charge_degree = case_when(
    c_charge_degree == "M" ~ "1",
    c_charge_degree == "F" ~ "0"
  ), race = case_when(
    race == "Caucasian" ~ "0",
    race == "African-American" ~ "1",
    race == "Hispanic" ~ "2",
    race == "Native American" ~ "3",
    race == "Asian" ~ "4",
    race == "Other" ~ "5"))

race_data <- race %>%
  select(-two_year_recid)
  

race_target <- race %>% 
  select(two_year_recid)
  

no_race <- race %>% 
  select(-race)

no_race_data <- no_race %>% 
  select(-two_year_recid) 
  

no_race_target <- no_race %>% 
  select(two_year_recid)
  

write_csv(race_data, "C:/Users/james/Desktop/Professional/current classes/CS450/final_project/race_data.csv")
write_csv(race_target, "C:/Users/james/Desktop/Professional/current classes/CS450/final_project/race_target.csv")
write_csv(no_race_data, "C:/Users/james/Desktop/Professional/current classes/CS450/final_project/no_race_data.csv")
write_csv(no_race_target, "C:/Users/james/Desktop/Professional/current classes/CS450/final_project/no_race_target.csv")
