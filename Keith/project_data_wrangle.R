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
         two_year_recid) %>% 
  drop_na()

dat_out <- dat_n %>%  
  select(-age) %>% 
  mutate(juv_fel_count = case_when(juv_fel_count == 0     ~ 'None',
                                   juv_fel_count  < 4      ~ '1 to 4',
                                   T                       ~ '4+'),
         juv_misd_count = case_when(juv_misd_count == 0   ~ 'None',
                                    juv_misd_count  < 2   ~ '1 to 2',
                                    T                     ~ '2+'),
         juv_other_count = case_when(juv_other_count == 0 ~ 'None',
                                     juv_other_count  < 4 ~ '1 to 2',
                                     T                    ~ '2+'),
         priors_count    = case_when(priors_count == 0    ~ 'None',
                                     priors_count  < 5    ~ '1 to 5',
                                     priors_count  < 15   ~ '5+ to 15',
                                     priors_count  < 25   ~ '15+ to 25',
                                     T                    ~ '25+'),
         race            = case_when(race == "African-American" ~ T,
                                     T                          ~ F)) 

write_csv(dat_out,'dt_dat.csv')


###################################convert numeric##################################
c_cu <- dat_n$c_charge_desc %>% 
  unique()

c_charge <- tibble(c_charge_desc = c_cu) %>% 
  mutate(index = row_number())

dat_n %>% 
  left_join(c_charge) %>% 
  mutate(c_charge_desc = )