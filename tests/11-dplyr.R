# -------------------------------- dplyr.R -------------------------------------
library(dplyr)
library(summarytools)
library(magrittr)
library(dplyr)
library(forcats)
data(tobacco)
(fr1 <- tobacco %>% select(age.gr, gender, smoker) %>% freq())
# fr1 %>% view()
fr1 %>% print("01-freqs-df.html")

(fr2 <- tobacco %>% group_by(age.gr = fct_explicit_na(age.gr)) %>% freq(smoker, report.nas = FALSE))
fr2 %>% tb()
fr2 %>% print("03-freqs-group_by-2.html")

(fr3 <- tobacco %>% group_by(age.gr = fct_explicit_na(age.gr), gender = fct_explicit_na(gender)) %>% freq(smoker))
fr3 %>% tb()
fr3 %>% tb(order = 2, na.rm = TRUE)
fr3 %>% print("02-freqs-group_by-1.html")

tobacco %>% group_by(age.gr = fct_explicit_na(age.gr)) %>% freq(.fct_explicit_na(smoker)) %>% tb()

tobacco %>% freq(smoker, plain.ascii = FALSE)
tobacco %$% freq(smoker, plain.ascii = FALSE)

freq(tobacco$smoker)



tobacco$age.gr %<>% fct_explicit_na()
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
tobacco$gender %<>% fct_explicit_na()
(dfgr <- tobacco %>% group_by(gender) %>% dfSummary())
dfgr %>% view(file = "02-group_by_dfs.html")

