# ctable.R ----------------------------------------
suppressPackageStartupMessages(library(dplyr))
options(tibble.print_max = Inf)
suppressPackageStartupMessages(library(summarytools))
data(tobacco)
tobacco <- tibble::as_tibble(tobacco)

ctable(tobacco$smoker, tobacco$diseased, caption = "test encod: éêàáûï")

label(tobacco) <- "A Study on Tobacco Use and Health"

# always show NAs
(ct1 <- ctable(tobacco$smoker, tobacco$diseased, useNA = 'always'))

st_options(display.labels = F)
print(ct1, file = "01.html")
print(ct1, totals=FALSE, file = "02.md", style = "grid", plain.ascii = FALSE)
view(ct1, totals=FALSE, file = "03.md", style = "grid", plain.ascii = FALSE)

# Minimal
with(tobacco, ctable(gender, smoker, headings = FALSE, prop = "n", totals = FALSE))

# Minimal with totals
with(tobacco, ctable(gender, smoker, headings = FALSE, prop = "n"))

# Non-default proportions
ctable(tobacco$smoker, tobacco$diseased, useNA = 'no', prop = 'c')
ctable(tobacco$smoker, tobacco$diseased, prop = 'n')
ctable(tobacco$smoker, tobacco$diseased, prop = 't')

# Calculated y variable
data(exams)
ctable(exams$gender, exams$french < 60)
ctable(exams$gender, exams$french < 60, dnn = c("Gender", "Failed French Class"), caption = "dnn in use")

# Subsetting
ctable(tobacco$smoker[1:250], tobacco$diseased[1:250], headings = FALSE)
ctable(tobacco[[1]], tobacco[,3])

# with()
with(tobacco, ctable(smoker,  diseased))

# stby() 
stby(list(x = tobacco$smoker, y = tobacco$diseased), tobacco$gender, ctable)

# with() + stby()
with(tobacco, stby(list(x = smoker, y = diseased), gender, ctable)) # problématique!
wbc <- with(tobacco, stby(list(x = smoker, y = diseased), gender, ctable, headings = F))
print(wbc, headings = T)
print(wbc, headings = T, file = "04.md")
print(wbc, headings = T, file = "04.html")
view(wbc, headings = T, caption = "with head", file = "05.md")
view(wbc, headings = T, footnote = "with head", file = "05.html")

# Weights
(wc1 <- with(tobacco, ctable(gender, smoker, weights = samp.wgts)))
print(wc1, totals = FALSE)
print(wc1, round.digits = 2)

(wc2 <- tobacco %$% stby(list(x = smoker, y = diseased), gender, ctable))

tobacco %$% stby(list(smoker, diseased), gender, ctable)
with(tobacco, stby(list(smoker, diseased), gender, ctable))
stby(list(tobacco$smoker, tobacco$diseased), tobacco$gender, ctable)

# Statistics
with(tobacco, ctable(smoker, diseased, chisq = TRUE, OR = TRUE, RR = TRUE))
view(with(tobacco, ctable(smoker, diseased, chisq = TRUE, OR = .9, RR = .9)), file = "06.html")

# Global Options
st_options(ctable.prop = "t", ctable.totals = FALSE)
with(tobacco, ctable(smoker, diseased))

st_options(ctable.prop = "n")
with(tobacco, ctable(smoker, diseased))
