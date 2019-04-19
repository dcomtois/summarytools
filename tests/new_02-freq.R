prout <- function(x, ext = "html", ...) {
  if (!exists("pr_number", envir = parent.frame()))
    pr_number <<- 1
  fname <- paste(paste0(sprintf("%02d", pr_number), "-"),
                 deparse(substitute(x)), ext, sep = ".")
  print(x, file = fname, ...)
  pr_number <<- pr_number + 1
}



# ---------------------------- freq.R ------------------------------------------
library(magrittr)
data(tobacco)
tobacco <- tibble::as_tibble(tobacco)
label(tobacco$gender) <- "Subject's Gender"

# Basic tables + general formatting (style / plain.ascii)
(freq_gender <- freq(tobacco$gender))
print(freq_gender, plain.ascii = F)
print(freq_gender, plain.ascii = F, style = "rmarkdown")
print(freq_gender, plain.ascii = F, style = "grid", headings = FALSE)

# Print to files (Style and/or method should be modified with message)
prout(freq_gender, ext = "md", caption = "markdown table")
prout(freq_gender, ext = "html", footnote = "html table")
prout(freq_gender, ext = "txt", caption = "txt table")
prout(freq_gender, ext = "html", footnote = "no bootstrap, with report title", report.title = "Freq without bootstrap",  bootstrap.css = FALSE)

# Variable label
data(tabagisme)
label(tabagisme$age.gr) <- "Groupe d'âge"

# Round to 1 decimal
(freq_age_gr <- freq(tabagisme$age.gr, round.digits = 1))

# Mask label, then type
print(freq_age_gr, display.labels = FALSE)
print(freq_age_gr, display.type = F)

# Mask total / NAs / cumul
print(freq_age_gr, totals = FALSE)          # OK
print(freq_age_gr, report.nas = FALSE)      # OK
print(freq_age_gr, cumul = FALSE)                              # FIXED: MISSING <NA> % IN TOTAL %
print(freq_age_gr, totals = FALSE, cumul = FALSE)              # FIXED: MISSING <NA> % IN TOTAL %
print(freq_age_gr, report.nas = FALSE, totals = FALSE)  # OK
print(freq_age_gr, report.nas = FALSE, cumul = FALSE)   # OK
print(freq_age_gr, report.nas = FALSE, cumul = FALSE, totals = FALSE)

# Replace the missing symbol
prout(freq_age_gr, missing = "---", footnote = "missing: ---")

# Omit cumulative and na reporting
print(freq_age_gr, cumul = FALSE)
print(freq_age_gr, cumul = FALSE, report.nas = FALSE)
prout(freq_age_gr, cumul = FALSE, report.nas = FALSE, footnote = "No cumul & no NA's")

(freq_age_gr_minimal <- freq(tabagisme$age.gr, cumul = FALSE, totals = FALSE, report.nas = FALSE))

print(freq_age_gr_minimal, footnote = "Minimal table")
print(freq_age_gr_minimal, cumul = TRUE, report.nas = TRUE, totals = TRUE)

prout(freq_age_gr_minimal, footnote = "Minimal orverrided to full-flavored", cumul = TRUE, report.nas = TRUE, totals = TRUE)

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
(freq_disease <- freq(tobacco$disease, order = "freq", rows = 1:5))
print(freq_disease, cumul = FALSE)        # OK
print(freq_disease, report.nas = FALSE)   # OK
print(freq_disease, totals = FALSE)       # OK
print(freq_disease, cumul = FALSE, report.nas = FALSE)  # OK
print(freq_disease, cumul = FALSE, totals = FALSE)      # OK
print(freq_disease, report.nas = FALSE, totals = FALSE) # OK
print(freq_disease, report.nas = FALSE, totals = FALSE, cumul = FALSE)   # OK 

freq(tobacco$disease, rows = -1)
freq(tobacco$disease, rows = "H")
freq(tobacco$disease, rows = -1:-10)
freq(tobacco$disease, rows = 1:13, report.nas = FALSE, totals = FALSE)


# Override the rounding
print(freq_disease, round.digits = 2)

# "Free" (not in a df) variables
Gender <- tobacco$gender
Gender[1] <- "F"
gender_standalone <- freq(Gender)
prout(gender_standalone)

# subsetting
# On factor - headings not affected
freq(tobacco$gender[tobacco$smoker=="Yes"])
freq(tobacco$gender[1:500])

# on dataframe - variable name should be set correctly
freq(tobacco[,1]) # label doesn't show (ok)
freq(tobacco[[3]])

# Weights
(weighted_maladie <- freq(tabagisme$maladie, weights = tabagisme$ponderation))
print(weighted_maladie, report.nas = FALSE)
prout(weighted_maladie)

(weighted_maladie_fr_ordered <- freq(tabagisme$maladie, weights = tabagisme$ponderation, report.nas = FALSE, order = "freq"))
prout(weighted_maladie_fr_ordered, footnote = "no nas overrided to yes nas", report.nas = TRUE)

mala.f <- factor(tabagisme$maladie, levels = sort(names(table(tabagisme$maladie)), decreasing = TRUE))
(weighted_maladie_fct <- freq(mala.f, weights = tabagisme$ponderation))
prout(weighted_maladie_fct, footnote = "No NAs & No totals (overrides)", report.nas = FALSE, totals = FALSE)

# with()
label(tobacco) <- "Study on Tobacco and Health"
label(tobacco$gender) <- "Subject's Gender"
(weighted_gender <- with(tobacco, freq(gender, plain.ascii = F, weights = samp.wgts)))
prout(weighted_gender, footnote = "With labels")

# stby()
label(tobacco$diseased) <- "Subject has an illness"
(dis_by_smoker <- stby(data = tobacco$diseased, INDICES = tobacco$smoker, FUN = freq))
view(dis_by_smoker, 'pander')
print(dis_by_smoker, headings = FALSE, style = "rmarkdown", plain.ascii = FALSE)
prout(dis_by_smoker, ext = "md", caption = "missing: xxxx, no headings, rmarkdown", missing = "xxxx")
prout(dis_by_smoker, headings = FALSE, footnote = "no headings")

(species_by_boolean_cond <- stby(iris$Species, iris$Sepal.Length > mean(iris$Sepal.Length), freq))
prout(species_by_boolean_cond)

# Using stby() + with() --- retrieving labels works only when executed "not in batch"
label(tobacco$diseased) <- "Subject has an illness"
(dis_by_smoker_with <- with(tobacco, stby(data = diseased, INDICES = smoker, FUN = freq)))
prout(dis_by_smoker_with)

# Labels, global options
st_options(plain.ascii = TRUE, style = "grid")
label(tobacco) <- "A Study On Tobacco and Health"
label(tobacco$gender) <- "Subject's Gender"
label(tobacco$age.gr) <- "Age Group"
label(tobacco$smoker) <- "Subject Smokes"
(all_tobacco <- freq(tobacco))
prout(all_tobacco)
prout(all_tobacco, footnote = "No labels, No type (overrides)", display.labels = F, display.type = F)

# lapply()
st_options(style = "simple")
tobacco_subset <- tobacco[,c(3,5,6)]
lapply(tobacco_subset, freq, style = "rmarkdown")

# Other global options
st_options(freq.totals = FALSE, freq.report.nas = FALSE, freq.cumul = FALSE)
print(all_tobacco, caption = "No totals, No NA's, No cumul (global options)")

st_options("reset")

# HTML - omitting NA reporting and/or Cumulative proportions
print(freq_age_gr, file = "99-Append.html", footnote = "After options reset - to be appended")
print(freq_age_gr, file = "99-Append.html", footnote = "No NAs (override), append", report.nas = FALSE, append = TRUE)
print(freq_age_gr, file = "99-Append.html", footnote = "No cumul, append", cumul = FALSE, append = TRUE)
print(freq_age_gr, file = "99-Append.html", footnote = "No NAs, No cumul, No total (override)", report.nas = FALSE, cumul = FALSE, totals = FALSE, append = TRUE)

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
