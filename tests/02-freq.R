# ---------------------------- freq.R ------------------------------------------
data(tobacco)
tobacco <- tibble::as_tibble(tobacco)
label(tobacco$gender) <- "Subject's Gender"

# Basic tables + general formatting (style / plain.ascii)
(freq1 <- freq(tobacco$gender))
print(freq1, plain.ascii = F)
print(freq1, plain.ascii = F, style = "rmarkdown")
print(freq1, plain.ascii = F, style = "grid", headings = FALSE)

# Variable label
data(tabagisme)
label(tabagisme$age.gr) <- "Groupe d'âge"

# Round to 1 decimal
(freq2 <- freq(tabagisme$age.gr, round.digits = 1))

# Mask label, then type
print(freq2, display.labels = FALSE)
print(freq2, display.type = F)

# Mask total and/or NAs
print(freq2, totals = FALSE)
print(freq2, report.nas = FALSE)
print(freq2, report.nas = FALSE, totals = FALSE)

# Replace the missing symbol
print(freq2, missing = "---")

# Row order - levels by default for factors
freq(tobacco$diseased)
# 2) modified order
freq(tobacco$diseased, order = "freq")
freq(tobacco$diseased, order = "names")

# Override the rounding
print(freq2, round.digits = 2)

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
print(freq1, file = "01.md")
print(freq1, file = "01.html")
print(freq1, bootstrap.css = FALSE, footnote = "no bootstrap", file = "02 - no bootstrap.html", report.title = "Freq without bootstrap")

# Weights
(wf1 <- freq(tabagisme$maladie, weights = tabagisme$ponderation))
print(wf1, file = "03 - weights.html")
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
print(bf1, footnote = "by", file = "04 - by.html", missing = "xxxx")
view(bf1, headings = FALSE, footnote = "no headings", file = "05 - by.html")

(bf2 <- stby(data = tobacco$diseased, INDICES = tobacco$smoker, FUN = freq))
print(bf2, file = "06 - by.html")

(bf3 <- stby(iris$Species, iris$Sepal.Length > mean(iris$Sepal.Length), freq))
print(bf3, file = "07 - by.html")

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
tobacco_subset <- tobacco[,c(3,5,6)]
lapply(tobacco_subset, freq, style = "rmarkdown")

# Other global options
st_options(freq.totals = FALSE, freq.report.nas = FALSE)
freq(tobacco)

st_options(freq.totals = TRUE, freq.report.nas = TRUE)
