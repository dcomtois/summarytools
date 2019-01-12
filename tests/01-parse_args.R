library(summarytools)
data(tobacco)
label(tobacco) <- "Tobacco Study"
label(tobacco$smoker) <- "Smoking Status"

# Basic cases
freq(tobacco$smoker)
freq(tobacco["smoker"])
freq(tobacco[["smoker"]])
freq(tobacco[,5])
freq(tobacco[[5]])

# Some row indexing
freq(tobacco$smoker[tobacco$smoker=="Yes"]) 

# Standalone variable
smoker <- tobacco$smoker 
freq(smoker) 

# with
with(tobacco, freq(smoker))

# pipe
library(magrittr)
tobacco %>% freq(smoker)
tobacco$smoker %>% freq()
tobacco["smoker"] %>% freq()
tobacco[["smoker"]] %>% freq()
tobacco[,5] %>% freq()
tobacco[[5]] %>% freq()

# by
by(tobacco$smoker, tobacco$gender, freq)
by(tobacco["smoker"], tobacco["gender"], freq)
by(tobacco[["smoker"]], tobacco[["gender"]], freq)
by(tobacco[,5], tobacco[,1], freq)
by(tobacco[[5]], tobacco[[1]], freq)
by(list(x = tobacco$smoker, y = tobacco$diseased), tobacco$gender, ctable)

# with + by
with(tobacco, by(smoker, gender, freq))
with(tobacco, by(list(x = smoker, y = diseased), gender, ctable))




