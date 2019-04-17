# -------------------------------- dplyr.R -------------------------------------
library(dplyr)
library(summarytools)
library(magrittr)
library(dplyr)
data(tobacco)
tobacco %>% select(age.gr, smoker) %>% freq() %>% view()

tobacco$age.gr %<>% forcats::fct_explicit_na()
(tmp <- tobacco %>% group_by(age.gr) %>% select(age.gr, smoker) %>% freq())
print(tmp, file = "01-group_by_freq.html")
tmp %>% tb()

tobacco %>% descr()
tobacco %>% select(age, BMI, smoker) %>% descr(stats = "fivenum")
tobacco %>% filter(smoker == "Yes") %>% descr(stats = "common")
tobacco %>% group_by(smoker) %>% descr(stats = "common") %>% view()
tobacco %$% descr(age)


tobacco %>% dfSummary() %>% view()
tobacco %>% select(gender, age, BMI, smoker) %>% dfSummary()

tobacco$gender %<>% forcats::fct_explicit_na()
tobacco %>% group_by(gender) %>% dfSummary() %>% view()

