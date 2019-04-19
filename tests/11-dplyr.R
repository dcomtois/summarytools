# -------------------------------- dplyr.R -------------------------------------
library(dplyr)
library(summarytools)
library(magrittr)
library(forcats)
data(tobacco)
(fr1 <- tobacco %>% select(age.gr, gender, smoker) %>% freq())
# fr1 %>% view()
fr1 %>% print(file = "01-freqs-df.html")

(fr2 <- tobacco %>% group_by(age.gr = fct_explicit_na(age.gr)) %>% freq(smoker, report.nas = FALSE))
fr2 %>% tb()
fr2 %>% print(file = "02-freqs-group_by-1.html")

(fr3 <- tobacco %>% group_by(age.gr = fct_explicit_na(age.gr), gender = fct_explicit_na(gender)) %>% freq(smoker))
fr3 %>% tb()
fr3 %>% tb(order = 2, na.rm = TRUE)
fr3 %>% print(file = "03-freqs-group_by-2.html")

tobacco$age.gr %<>% fct_explicit_na()
(fr4 <- tobacco %>% group_by(age.gr) %>% select(age.gr, smoker) %>% freq())
fr4 %>% tb()
fr4 %>% print(file = "04-freqs-group_by-3.html")

(fr5 <- tobacco %>% freq(fct_explicit_na(smoker)))
fr5 %>% tb()
fr5 %>% tb(2, TRUE)
fr5 %>% print(file = "05-freqs-fn-1.html")

(fr6 <- tobacco %>% group_by(age.gr = fct_explicit_na(age.gr)) %>% freq(na.omit(smoker)))
fr6 %>% tb()
fr6 %>% tb(2, TRUE)
fr6 %>% print(file = "06-freqs-fn-2.html")

# descr
(d1 <- tobacco %>% descr(stats = "common"))
d1 %>% tb()
d1 %>% print(headings = FALSE)
d1 %>% print(headings = FALSE, footnote = "test footnote", file = "07-descr1-footnote.html")

(d2 <- tobacco %>% select(age, BMI, smoker) %>% descr(stats = "fivenum"))
d2 %>% tb()
d2 %>% print(file = "08-descr-select.html")

(d3 <- tobacco %>% filter(smoker == "Yes") %>% descr(stats = "common"))
d3 %>% tb()
d3 %>% print(file = "09-descr-filter.html")

(d4 <- tobacco %$% descr(age))
d4 %>% tb()
d4 %>% print(file = "10-descr-dollarpipe.html")

(d5 <- tobacco %>% group_by(smoker) %>% descr(stats = "common"))
#d5 %>% view(method = "browser")
d5 %>% tb()
d5 %>% tb(2)
d5 %>% print(file = "11-descr-group_by-1.html")

(d6 <- tobacco %>% group_by(smoker, gender = fct_explicit_na(gender)) %>% descr(stats = "common"))
#d5 %>% view(method = "browser")
d6 %>% tb()
d6 %>% tb(2)
d6 %>% print(file = "12-descr-group_by-2.html")

(d7 <- tobacco %>% group_by(smoker) %>% descr(age, stats = "common"))
d7 %>% tb()
d7 %>% tb(2)
d7 %>% print(file = "13-descr-group_by-special.html")

view(d7)

d7_2 <- stby(tobacco$age, tobacco$smoker, descr)
d7_2 %>% tb()
view(d7_2)

# dfSummary
(dfs1 <- tobacco %>% dfSummary(varnumbers = FALSE, valid.col = FALSE))
dfs1 %>% print(file = "13-dfSummary.html")


(dfs2 <- tobacco %>% select(gender, age, BMI, smoker) %>% dfSummary(valid.col = FALSE))
dfs2 %>% print(file = "11-dfSummary-select.html")
dfs2 %>% view()


tobacco$gender %<>% fct_explicit_na()
(dfs3 <- tobacco %>% group_by(gender) %>% dfSummary(valid.col = F))
dfs3 %>% view(file = "12-dfSummary-group_by.html")

