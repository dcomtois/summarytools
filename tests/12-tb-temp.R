# fails for unknown reason, only when calling from 00-Main.R
suppressPackageStartupMessages(library(summarytools))
(ff4 <- with(tobacco, stby(smoker, list(gender, age.gr), freq)))
class(ff4)
ff4 %>% tb()
ff4 %>% tb(2)



with(tobacco, stby(smoker, list(gender, age.gr), freq)) 
tobacco %>% group_by(gender, age.gr) %>% select(smoker) %>% freq()
