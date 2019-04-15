# -------------------------------- dplyr.R -------------------------------------
library(dplyr)
library(summarytools)
data(tobacco)
tobacco %>% dfSummary() %>% view()
tobacco %>% select(gender, age, BMI, smoker) %>% dfSummary()
tobacco %>% group_by(gender) %>% dfSummary() %>% view()
