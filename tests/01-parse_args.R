library(summarytools)
data(tobacco)
label(tobacco) <- "Tobacco Study"
label(tobacco$smoker) <- "Smoking Status"
label(tobacco$age) <- "Age of the Subject"

# Basic cases
freq(tobacco$smoker)
freq(tobacco["smoker"])
freq(tobacco[["smoker"]])
freq(tobacco[,5])
freq(tobacco[[5]])

# With row subsets
freq(tobacco$smoker[1:100])
freq(tobacco[1:100, "smoker"])
freq(tobacco[1:100, 5])
freq(tobacco[[5]][1:100])
freq(tobacco$smoker[tobacco$smoker=="Yes"]) 

# Standalone variable
smoker <- tobacco$smoker 
freq(smoker) 

# with
with(tobacco, freq(smoker))               # ok
with(tobacco, descr(age))                 # ok
with(tobacco, ctable(smoker, diseased))   # pas ok

# pipe
library(magrittr)
tobacco$smoker %>% freq()      # ok
tobacco["smoker"] %>% freq()   # ok
tobacco[["smoker"]] %>% freq() # ok
tobacco[,5] %>% freq()         # ok
tobacco[[5]] %>% freq()        # ok

# by
by(tobacco$smoker, tobacco$gender, freq)           # ok
by(tobacco["smoker"], tobacco["gender"], freq)     # ok
by(tobacco[,"smoker"], tobacco[,"gender"], freq)   # ok
by(tobacco[["smoker"]], tobacco[["gender"]], freq) # ok
by(tobacco[[5]], tobacco[[1]], freq)               # ok
by(tobacco[,5], tobacco[,1], freq)                 # ok
by(list(x = tobacco$smoker, y = tobacco$diseased), tobacco$gender, ctable) # ok

# with + by
with(tobacco, by(smoker, gender, freq))                                  # ok
with(tobacco, by(list(x = smoker, y = diseased), gender, ctable))        # ok
with(tobacco[1:7], by(list(x = smoker, y = diseased), gender, ctable))   # ok

# lapply
print(lapply(tobacco[c(1,3,5)], freq))
lap <- lapply(tobacco[c(1,3,5)], freq)
print(lap)

