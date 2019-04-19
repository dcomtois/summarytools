# -------------------------------- dplyr.R -------------------------------------
library(dplyr)
library(summarytools)
library(magrittr)
library(dplyr)
data(tobacco)
tobacco %>% select(age.gr, smoker) %>% freq() %>% view()
tobacco %>% group_by(age.gr = fct_explicit_na(age.gr), gender = fct_explicit_na(gender)) %>% freq(smoker) %>% view()
tobacco %>% group_by(age.gr = fct_explicit_na(age.gr)) %>% freq(smoker) %>% tb()
tobacco %>% group_by(age.gr = fct_explicit_na(age.gr)) %>% freq(.fct_explicit_na(smoker)) %>% tb()

tobacco %>% freq(smoker, plain.ascii = FALSE)
tobacco %$% freq(smoker, plain.ascii = FALSE)

freq(tobacco$smoker)



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
(dfgr <- tobacco %>% group_by(gender) %>% dfSummary())
dfgr %>% view(file = "02-group_by_dfs.html")

