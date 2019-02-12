# ------------------------------ parse_args.R ----------------------------------
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
with(tobacco, freq(smoker))           
with(tobacco, descr(age))             
with(tobacco, ctable(smoker, diseased))

# pipe
library(magrittr)
tobacco$smoker %>% freq()     
tobacco["smoker"] %>% freq()  
tobacco[["smoker"]] %>% freq()
tobacco[,5] %>% freq()        
tobacco[[5]] %>% freq()       

# by
stby(tobacco$smoker, tobacco$gender, freq)          
stby(tobacco["smoker"], tobacco["gender"], freq)    
stby(tobacco[,"smoker"], tobacco[,"gender"], freq)  
stby(tobacco[["smoker"]], tobacco[["gender"]], freq)
# by, numeric column indexing
stby(tobacco[[5]], tobacco[[1]], freq)
stby(tobacco[,5], tobacco[,1], freq)  
# by with ctable
stby(list(x = tobacco$smoker, y = tobacco$diseased), tobacco$gender, ctable) # ok

# with + by
with(tobacco, stby(smoker, gender, freq))                               
with(tobacco, stby(list(x = smoker, y = diseased), gender, ctable))     
with(tobacco[1:7], stby(list(x = smoker, y = diseased), gender, ctable))

# lapply
print(lapply(tobacco[c(1,3,5)], freq))
lap <- lapply(tobacco[c(1,3,5)], freq)
print(lap)

