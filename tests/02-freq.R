# ---------------------------- freq.R ------------------------------------------
library(magrittr)
data(tobacco)
tobacco <- tibble::as_tibble(tobacco)
label(tobacco$gender) <- "Subject's Gender"

# Basic tables + general formatting (style / plain.ascii)
(f1 <- freq(tobacco$gender))
print(f1, plain.ascii = F)
print(f1, plain.ascii = F, style = "rmarkdown")
print(f1, plain.ascii = F, style = "grid", headings = FALSE)

# Variable label
data(tabagisme)
label(tabagisme$age.gr) <- "Groupe d'âge"

# Round to 1 decimal
(f2 <- freq(tabagisme$age.gr, round.digits = 1))

# Mask label, then type
print(f2, display.labels = FALSE)
print(f2, display.type = F)

# Mask total / NAs / cumul
print(f2, totals = FALSE)          # OK
print(f2, report.nas = FALSE)      # OK
print(f2, cumul = FALSE)                              # FIXED: MISSING <NA> % IN TOTAL %
print(f2, totals = FALSE, cumul = FALSE)              # FIXED: MISSING <NA> % IN TOTAL %
print(f2, report.nas = FALSE, totals = FALSE)  # OK
print(f2, report.nas = FALSE, cumul = FALSE)   # OK
print(f2, report.nas = FALSE, cumul = FALSE, totals = FALSE)

# Replace the missing symbol
print(f2, missing = "---")

# 1) Original order - factor
freq(tobacco$diseased)

# 2) Modified order
freq(tobacco$diseased, order = "freq")
freq(tobacco$diseased, order = "freq", report.nas = FALSE)
freq(tobacco$diseased, order = "freq-")
freq(tobacco$diseased, order = "freq-", report.nas = FALSE)

freq(tobacco$diseased, order = "names")
freq(tobacco$diseased, order = "names", report.nas = FALSE)
freq(tobacco$diseased, order = "names-")
freq(tobacco$diseased, order = "names-", report.nas = FALSE)

freq(tobacco$diseased, order = "levels")
freq(tobacco$diseased, order = "levels", report.nas = FALSE)
freq(tobacco$diseased, order = "levels-")
freq(tobacco$diseased, order = "levels-", report.nas = FALSE)

# 1) Original order - factor
freq(tobacco$age.gr)

# 2) Modified order
freq(tobacco$age.gr, order = "freq")
freq(tobacco$age.gr, order = "freq", report.nas = FALSE)
freq(tobacco$age.gr, order = "freq-")
freq(tobacco$age.gr, order = "freq-", report.nas = FALSE)

freq(tobacco$age.gr, order = "names")
freq(tobacco$age.gr, order = "names", report.nas = FALSE)
freq(tobacco$age.gr, order = "names-")
freq(tobacco$age.gr, order = "names-", report.nas = FALSE)

freq(tobacco$age.gr, order = "levels")
freq(tobacco$age.gr, order = "levels", report.nas = FALSE)
freq(tobacco$age.gr, order = "levels-")
freq(tobacco$age.gr, order = "levels-", report.nas = FALSE)



# 1) Original order - character
freq(tobacco$disease)

# 2) Modified order
freq(tobacco$disease, order = "freq")
freq(tobacco$disease, order = "freq", report.nas = FALSE)
freq(tobacco$disease, order = "freq-")
freq(tobacco$disease, order = "-freq", report.nas = FALSE)

freq(tobacco$disease, order = "names")
freq(tobacco$disease, order = "names", report.nas = FALSE)
freq(tobacco$disease, order = "names-")
freq(tobacco$disease, order = "names-", report.nas = FALSE)


# Order and subset
freq(tobacco$disease)
(f3 <- freq(tobacco$disease, order = "freq", rows = 1:5))
print(f3, cumul = FALSE)        # OK
print(f3, report.nas = FALSE)   # OK
print(f3, totals = FALSE)       # OK
print(f3, cumul = FALSE, report.nas = FALSE)  # OK
print(f3, cumul = FALSE, totals = FALSE)      # OK
print(f3, report.nas = FALSE, totals = FALSE) # OK
print(f3, report.nas = FALSE, totals = FALSE, cumul = FALSE)   # OK 

freq(tobacco$disease, rows = -1)
freq(tobacco$disease, rows = "H")
freq(tobacco$disease, rows = -1:-10)
freq(tobacco$disease, rows = 1:13, report.nas = FALSE, totals = FALSE)


# Override the rounding
print(f3, round.digits = 2)


# "Free" (not in a df) variables
Gender <- tobacco$gender
Gender[1] <- "F"
freq(Gender)
view(freq(Gender))

# subsetting
# On factor - headings not affected
freq(tobacco$gender[tobacco$smoker=="Yes"])
freq(tobacco$gender[1:500])

# on dataframe - variable name should be set correctly
freq(tobacco[,1]) # label doesn't show (ok)
freq(tobacco[[3]])

# Print to files (Style and/or method should be modified with message)
print(f1, file = "01.md")
print(f1, file = "01.html")
print(f1, bootstrap.css = FALSE, footnote = "no bootstrap", file = "02-no-bootstrap.html", report.title = "Freq without bootstrap")

# Omit cumulative and na reporting
(f4 <- freq(tobacco$age.gr))
print(f4, cumul = FALSE)
print(f4, cumul = FALSE, report.nas = FALSE)

(f5 <- freq(tobacco$age.gr, cumul = FALSE, report.nas = FALSE))
print(f5, cumul = TRUE, report.nas = TRUE)

# Weights
(wf1 <- freq(tabagisme$maladie, weights = tabagisme$ponderation))
print(wf1, file = "03-weights.html")
print(wf1, report.nas = FALSE)

(wf2 <- freq(tabagisme$maladie, weights = tabagisme$ponderation, report.nas = FALSE, order = "freq"))
print(wf2, report.nas = TRUE)

mala.f <- factor(tabagisme$maladie, levels = sort(names(table(tabagisme$maladie)), decreasing = TRUE))
(wf3 <- freq(mala.f, weights = tabagisme$ponderation))
print(wf3, report.nas = FALSE, totals = FALSE)

# with()
label(tobacco) <- "Study on Tobacco and Health"
label(tobacco$gender) <- "Subject's Gender"
(wf1 <- with(tobacco, freq(gender, plain.ascii = F, weights = samp.wgts)))

# stby()
label(tobacco$diseased) <- "Subject has an illness"
(bf1 <- stby(data = tobacco$diseased, INDICES = tobacco$smoker, FUN = freq))
view(bf1, 'pander')
print(bf1, headings = FALSE, plain.ascii = FALSE)
print(bf1, footnote = "by", file = "04-by.html", missing = "xxxx")
view(bf1, headings = FALSE, footnote = "no headings", file = "05-by.html")

(bf2 <- stby(data = tobacco$diseased, INDICES = tobacco$smoker, FUN = freq))
print(bf2, file = "06-by.html")

(bf3 <- stby(iris$Species, iris$Sepal.Length > mean(iris$Sepal.Length), freq))
print(bf3, file = "07-by.html")

# Using stby() + with() --- retrieving labels works only when executed "not in batch"
label(tobacco$diseased) <- "Subject has an illness"
(bwf1 <- with(tobacco, stby(data = diseased, INDICES = smoker, FUN = freq)))
view(bwf1)

# Labels, global options
st_options(plain.ascii = TRUE, style = "grid")
label(tobacco) <- "A Study On Tobacco and Health"
label(tobacco$gender) <- "Subject's Gender"
label(tobacco$age.gr) <- "Age Group"
label(tobacco$smoker) <- "Subject Smokes"
(ft <- freq(tobacco))
print(ft, display.labels = F, display.type = F)


# lapply()
st_options(style = "simple")
tobacco_subset <- tobacco[,c(3,5,6)]
lapply(tobacco_subset, freq, style = "rmarkdown")

# Other global options
st_options(freq.totals = FALSE, freq.report.nas = FALSE)
freq(tobacco)

st_options(freq.totals = TRUE, freq.report.nas = TRUE)

# HTML - omitting NA reporting and/or Cumulative proportions
view(freq(tobacco$age.gr), file = "08-omissions.html", footnote = "No omissions")
view(freq(tobacco$age.gr, report.nas = FALSE), file = "08-omissions.html", append = TRUE, footnote = "report.nas = FALSE")
view(freq(tobacco$age.gr, cumul = FALSE), file = "08-omissions.html", append = TRUE, footnote = "cumul = FALSE")
view(freq(tobacco$age.gr, report.nas = FALSE, cumul = FALSE), file = "08-omissions.html", append = TRUE, footnote = "report.nas = FALSE & cumul = FALSE")

# tb()
library(magrittr)

iris %$% freq(Species) %>% tb()
iris %$% freq(Species, report.nas = FALSE) %>% tb()
iris %$% freq(Species, cumul = FALSE) %>% tb()
iris %$% freq(Species, cumul = FALSE, report.nas = FALSE) %>% tb()

library(dplyr)
tobacco %>% select(disease) %>% arrange() %>% freq(rows = 1:10)

# Deal with explicited NA's
tobacco$age.gr %<>% forcats::fct_explicit_na()
freq(tobacco$age.gr)

# Full dataframes
(ftob1 <- freq(tobacco))
(ftob2 <- freq(tobacco, display.type = FALSE))

print(ftob1, file = "09-full-dataset.html", footnote = "With type")
print(ftob2, file = "10-full-dataset-notype.html", footnote = "Without type")

# Collapsible outputs
print(ftob2, collapse = 1, file = "11-full-dataset-collapse.html", footnote = "collapsible sections")
