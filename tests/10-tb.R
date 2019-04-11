# ---------------------------- tb.R ------------------------------------
library(magrittr)
(ff <- stby(tobacco$smoker, INDICES = tobacco$gender, FUN = freq))
(res1 <- ff %>% tb())
colSums(res1[,3:5])
(res2 <- ff %>% tb(2))
colSums(res2[,3:5])

(dd <- stby(examens, examens$sexe, descr))
(res1 <- dd %>% tb())
(res2 <- dd %>% tb(2))
