################################library##############################################
library(tidyverse)
library(readr)
library(ggplot2)
library(ggridges)
library(pander)
library(caret)
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
                                     T                    ~ '25+')) 

write_csv(dat_out,'dt_dat.csv')


###################################convert numeric##################################
c_cu <- dat_n$c_charge_desc %>% 
  unique()

c_charge <- tibble(c_charge_desc = c_cu) %>% 
  mutate(index = row_number())

dat_n %>% 
  left_join(c_charge) %>% 
  mutate(c_charge_desc = )






dat_o %>% 
  mutate(age_cat = fct_relevel(age_cat, 
                               c('Less than 25',
                                 '25 - 45',
                                 'Greater than 45'))) %>% 
  ggplot() +
  aes(y = age_cat, 
      x = dat_o$decile_score.1, 
      fill = (race == 'African-American')) +
  geom_density_ridges(panel_scaling = F,
                       alpha = 0.35) +
  coord_cartesian(xlim = c(0,10)) +
  scale_fill_brewer(palette = 'Dark2') +
  ggthemes::theme_pander() +
  labs(x = 'predicted score',
       y = 'age group') +
  guides(fill = guide_legend(title = 'African-American', label.position = 'right')) +
  theme(axis.line.x = element_line(linetype = 'dashed', colour = 'Gray'),
        legend.position = c(0.9,0.9))



dat_o %>% 
  ggplot() +
  aes(x = dat_o$decile_score.1, fill = dat_o$two_year_recid== 1) +
  geom_histogram(position = 'dodge_2') +
  facet_grid(race == 'African-American'~.) +
  ggthemes::theme_pander() +
  theme(strip.text.y = element_text(size = 8, angle = 0),
        strip.background = element_blank()) +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = guide_legend(title = 'recidivist', label.position = 'right')) +
  labs(x = 'predicted score', y = '') +
  theme(axis.line.x = element_line(linetype = 'dashed', colour = 'Gray'),
        legend.position = c(0.9,0.9))
  


rm <- glm(dat_o$two_year_recid ~dat_o$decile_score.1, family = binomial ,data = mpg)
pander::pander(summary(rm))


dat_o %>% 
  select(two_year_recid, decile_score.1)
  





x <- dat_o %>% 
  select(race, two_year_recid, decile_score.1) %>% 
  mutate(highlow = case_when(decile_score.1 > 5 ~ 1,
                             T                  ~ 0)) 
p_class <- factor(x$highlow)
class_levels <- factor(x$two_year_recid)

confusionMatrix(p_class, class_levels)


y <- dat_o %>% 
  filter(race == 'African-American') %>% 
  select(race, two_year_recid, decile_score.1) %>% 
  mutate(highlow = case_when(decile_score.1 > 5 ~ 1,
                             T                  ~ 0)) 

p_class <- factor(y$highlow)
class_levels <- factor(y$two_year_recid)
confusionMatrix(p_class, class_levels)


no_cores <- detectCores() - 1
c1 <- parallel::makeCluster(no_cores)
library(tidyverse)

clusterEvalQ(c1, library(tidyverse))

