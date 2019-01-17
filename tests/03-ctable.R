# ---------------------------- ctable.R ----------------------------------------
data(tobacco)
tobacco <- tibble::as_tibble(tobacco)

ctable(tobacco$smoker, tobacco$diseased, caption = "test encod: éêàáûï")

label(tobacco) <- "A Study on Tobacco Use and Health"

# always show NAs
(ct1 <- ctable(tobacco$smoker, tobacco$diseased, useNA = 'always'))

st_options(display.labels = F)
print(ct1, file = "ctable-ct1-1.html")
print(ct1, totals=FALSE, file = "ctable-ct1-no-totals-print.md", style = "grid", plain.ascii = FALSE)
view(ct1, totals=FALSE, file = "ctable-ct1-no-totals-view.md", style = "grid", plain.ascii = FALSE)

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

# by() 
by(list(x = tobacco$smoker, y = tobacco$diseased), tobacco$gender, ctable)

# with() + by()
with(tobacco, by(list(x = smoker, y = diseased), gender, ctable)) # problématique!
wbc <- with(tobacco, by(list(x = smoker, y = diseased), gender, ctable, headings = F))
print(wbc, headings = T)
print(wbc, headings = T, file = "ctable-wbc-print.md")
print(wbc, headings = T, file = "ctable-wbc-print.html")
view(wbc, headings = T, file = "ctable-wbc-view-with-head.md")
view(wbc, headings = T, file = "ctable-wbc-view-with-head.html")
