# ---------------------------- tb.R ------------------------------------
library(summarytools)
library(magrittr)
library(dplyr)
set.seed(765543543)
tobacco$smoker[sample(1:1000, size = 25, replace = FALSE)] <- NA_character_

# Normal freq table
(fr1 <-  tobacco %$% freq(gender))
fr1 %>% tb()
fr1 %>% tb(2) # aucun effet attendu
fr1 %>% tb() %>% select(2,3,5) %>% colSums(na.rm = T)

# No NA's
(fr2 <- tobacco %$% freq(gender, report.nas = FALSE))
fr2 %>% tb()
fr2 %>% tb() %>% select(2,3) %>% colSums(na.rm = T)

# with sampling weights
(fr3 <- tobacco %$% freq(gender, weights = samp.wgts))
fr3 %>% tb()
fr3 %>% tb() %>% select(2,3,5) %>% colSums(na.rm = T)

# with sampling weights, no cumul
(fr4 <- tobacco %$% freq(gender, weights = samp.wgts, cumul = FALSE))
fr4 %>% tb()
fr4 %>% tb() %>% select(2,3,4) %>% colSums(na.rm = T)

# with sampling weights, no cumul, no missing
(fr5 <- tobacco %$% freq(gender, weights = samp.wgts, cumul = FALSE, report.nas = FALSE))
fr5 %>% tb()
fr5 %>% tb() %>% select(2,3) %>% colSums(na.rm = T)

# Freq with grouped results
(fgr1 <- stby(tobacco$smoker, INDICES = tobacco$gender, FUN = freq))
fgr1 %>% tb()
fgr1 %>% tb(2)
fgr1 %>% tb() %>% select(3,4,6) %>% colSums(na.rm = T)
fgr1 %>% tb(2) %>% select(3,4,6) %>% colSums(na.rm = T)

# Freq with grouped results, no cumul
(fgr2 <- stby(tobacco$smoker, INDICES = tobacco$gender, FUN = freq, cumul = FALSE))
fgr2 %>% tb()
fgr2 %>% tb(2)
fgr2 %>% tb() %>% select(3,4,5) %>% colSums(na.rm = T)
fgr2 %>% tb(2) %>% select(3,4,5) %>% colSums(na.rm = T)

# Freq with grouped results, no cumul, no NA's
(fgr3 <- stby(tobacco$smoker, INDICES = tobacco$gender, FUN = freq, cumul = FALSE, report.nas = FALSE))
fgr3 %>% tb()
fgr3 %>% tb(2)
fgr3 %>% tb() %>% select(3,4) %>% colSums(na.rm = T)
fgr3 %>% tb(2) %>% select(3,4) %>% colSums(na.rm = T)

# Freq with grouped results
(fgr4 <- stby(tobacco$smoker, INDICES = tobacco$age.gr, FUN = freq))
fgr4 %>% tb()
fgr4 %>% tb(2)
fgr4 %>% tb() %>% select(3,4,6) %>% colSums(na.rm = T)
fgr4 %>% tb(2) %>% select(3,4,6) %>% colSums(na.rm = T)




# Normal descr() results
(de1 <- descr(examens))
de1 %>% tb()
de1 %>% tb(2) # no difference expected

(de2 <- examens$francais %>% descr())
de2 %>% tb()

# Descr with grouped results, one variable only
(dgr1 <- examens %$% stby(maths, sexe, descr, stats = "common"))
dgr1 %>% tb()
identical(dgr1 %>% tb(2), dgr1 %>% tb())

# Descr with grouped results, complete df
(dgr2 <- stby(examens, examens$sexe, descr))
dgr2 %>% tb()
dgr2 %>% tb(2)

