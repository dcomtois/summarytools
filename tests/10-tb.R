# tb.R ------------------------------------
suppressPackageStartupMessages(library(summarytools))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))
options(tibble.print_max = Inf)
set.seed(765543543)
na_pos <- sample(1:1000, size = 25, replace = FALSE)
dput(na_pos)
na_pos <- c(804L, 800L, 290L, 532L, 243L, 336L, 207L, 805L, 318L, 78L, 
            285L, 931L, 298L, 204L, 113L, 323L, 671L, 2L, 179L, 508L, 72L, 
            755L, 341L, 930L, 569L)
data(tobacco)
tobacco$smoker[na_pos] <- NA_character_

# Normal freq table
(fr1 <-  tobacco %$% freq(gender))
fr1 %>% tb()
fr1 %>% tb(2) # aucun effet attendu
fr1 %>% tb(3) # aucun effet attendu
fr1 %>% tb(na.rm = TRUE)
fr1 %>% tb() %>% select(2,3,5) %>% colSums(na.rm = T)

# No NA's
(fr2 <- tobacco %$% freq(gender, report.nas = FALSE))
fr2 %>% tb(na.rm = TRUE)
fr2 %>% tb() %>% select(2,3) %>% colSums(na.rm = T)

# with sampling weights
(fr3 <- tobacco %$% freq(gender, weights = samp.wgts))
fr3 %>% tb()
fr3 %>% tb(na.rm = TRUE)
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
fgr1 %>% tb(3)
fgr1 %>% tb() %>% select(3,4,6)
fgr1 %>% tb() %>% select(3,4,6) %>% colSums(na.rm = T)

# Freq with grouped results, no cumul
(fgr2 <- stby(tobacco$smoker, INDICES = tobacco$gender, FUN = freq, cumul = FALSE))
fgr2 %>% tb()
fgr2 %>% tb(2)
fgr2 %>% tb(3)
fgr2 %>% tb() %>% select(3,4,5) %>% colSums(na.rm = T)
fgr2 %>% tb(2) %>% select(3,4,5) %>% colSums(na.rm = T)

# Freq with grouped results, no cumul, no NA's
(fgr3 <- stby(tobacco$smoker, INDICES = tobacco$gender, FUN = freq, cumul = FALSE, report.nas = FALSE))
fgr3 %>% tb()
fgr3 %>% tb(2)
fgr3 %>% tb(3)
fgr3 %>% tb() %>% select(3,4) %>% colSums(na.rm = T)

# Freq with grouped results
(fgr4 <- stby(tobacco$smoker, INDICES = tobacco$age.gr, FUN = freq))
fgr4 %>% tb()
fgr4 %>% tb(2)
fgr4 %>% tb() %>% select(3,4,6) %>% colSums(na.rm = T)
fgr4 %>% tb(2) %>% select(3,4,6) %>% colSums(na.rm = T)

# Freq with grouped results, freq sorting (+)
(fgr5 <- stby(tobacco$smoker, INDICES = tobacco$age.gr, FUN = freq, order = "freq"))
fgr5 %>% tb()
fgr5 %>% tb(2)
fgr5 %>% tb(3)
fgr5 %>% tb() %>% select(3,4,6) %>% colSums(na.rm = T)
fgr5 %>% tb(2) %>% select(3,4,6) %>% colSums(na.rm = T)

# Freq with grouped results, freq sorting (-)
(fgr6 <- stby(tobacco$smoker, INDICES = tobacco$age.gr, FUN = freq, order = "freq-"))
fgr6 %>% tb()
fgr6 %>% tb(2)
fgr6 %>% tb() %>% select(3,4,6) %>% colSums(na.rm = T)
fgr6 %>% tb(2) %>% select(3,4,6) %>% colSums(na.rm = T)

# Freq with grouped results, 2 grouping vars
(fgr7 <- stby(tobacco$smoker, INDICES = list(tobacco$gender, tobacco$age.gr), FUN = freq))
fgr7 %>% tb()
fgr7 %>% tb(2)
fgr7 %>% tb() %>% select(4,5,7) %>% colSums(na.rm = T)

# Normal descr() results
(de1 <- descr(examens))
de1 %>% tb()
de1 %>% tb(2) # no difference expected

(de2 <- examens$francais %>% descr())
de2 %>% tb()
de2 %>% tb(drop.var.col = T)

# Descr with grouped results, one variable only
(dgr1 <- examens %$% stby(maths, sexe, descr, stats = "common"))
dgr1 %>% tb()
identical(dgr1 %>% tb(2), dgr1 %>% tb())

# Descr with grouped results, complete df
(dgr2 <- stby(examens, examens$sexe, descr))
dgr2 %>% tb()
dgr2 %>% tb(2)
dgr2 %>% tb(3)

(dd1 <- stby(tobacco, tobacco$gender, descr))
dd1 %>% tb()
dd1 %>% tb(2)
dd1 %>% tb(3)
view(dd1, file = "dd1.html")

tobacco$gender %<>% forcats::fct_explicit_na()
(dd2 <- tobacco %>% group_by(gender) %>% descr(stats = "common"))
dd2 %>% tb()
dd2 %>% tb(2)
view(dd2, file = "dd2.html")

(dd3 <- with(tobacco, stby(tobacco, list(gender, age.gr), descr, stats = "common")))
dd3 %>% tb()
dd3 %>% tb(2)
view(dd3, file = "dd3.html")

tobacco$age.gr %<>% forcats::fct_explicit_na()
(dd4 <- tobacco %>% group_by(gender, age.gr) %>% descr(stats = "common"))
dd4 %>% tb()
dd4 %>% tb(na.rm = TRUE) # no effect expected
dd4 %>% tb(order = 2)
view(dd4, file = "dd4.html")


# freq
data(tobacco)

(ff1 <- stby(tobacco$smoker, tobacco$gender, freq))
ff1 %>% tb()
ff1 %>% tb(2)
ff1 %>% tb(2, TRUE)
view(ff1, file = "ff1.html")

(ff2 <- stby(tobacco$smoker, tobacco$gender, freq, report.nas = F))
ff2 %>% tb()
ff2 %>% tb(2)
ff2 %>% tb(3)

tobacco$gender %<>% forcats::fct_explicit_na()
(ff3 <- tobacco %>% group_by(gender) %>% select(gender, smoker) %>% freq())
ff3 %>% tb()
ff3 %>% tb(2)
ff3 %>% tb(2, TRUE)
view(ff3, file = "ff3.html")

# fails for unknown reason, only when sourcing from main script (00-Main.R)
(ff4 <- with(tobacco, stby(smoker, list(gender, age.gr), freq)))
# ff4 %>% tb()
# ff4 %>% tb(2)

tobacco$age.gr %<>% forcats::fct_explicit_na()
(ff5 <- tobacco %>% group_by(gender, age.gr) %>% select(gender, age.gr, smoker) %>% freq())
view(ff5, file = "ff5.html")
ff5 %>% tb()
ff5 %>% tb(2)
ff5 %>% tb(2, na.rm = TRUE)

# drop.var.col argument
tb(descr(iris$Sepal.Length), drop.var.col = TRUE)

# drop.var.col should be ignored here
tb(stby(tobacco, tobacco$gender, descr), drop.var.col = TRUE)

# order & swap combination
tb(stby(tobacco, tobacco$gender, descr), order = 3)
tb(stby(tobacco, list(tobacco$gender, tobacco$smoker), descr), order = 3)
