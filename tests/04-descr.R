#-------------------------------- 4-descr.R ------------------------------------
# library(summarytools)

tobacco <- tibble::as_tibble(tobacco)
(d1 <- descr(tobacco))

print(d1, plain.ascii = F)
print(d1, style = 'rmarkdown')
print(d1, style = 'grid', plain.ascii = FALSE)

# Global options
st_options(style = 'rmarkdown', descr.stats = "common", descr.transpose = TRUE)
(d2 <- descr(tobacco))
print(d2, headings = FALSE)

# labels
label(tobacco) <- "A study on Tobacco and Health"
descr(tobacco, style = "simple")

label(tobacco$BMI) <- "Body Mass Index"
(d2 <- descr(tobacco$BMI))

# print to files
print(d2, file = "descr-d2-1.html")
view(d2, footnote = "test: àéïôù", file = "descr-d2-2.html")

# long var names
st_options(descr.transpose = FALSE)
some.long.variable.name <- tobacco$age
some.long.variabl.name <- tobacco$age
print(descr(some.long.variable.name), file = "descr-long-name-1.html")
view(descr(some.long.variabl.name), file = "descr-long-name-2.html")

# Round digits
(d3 <- descr(tobacco$age, round.digits = 1))
view(d3,  file = "descr-d3.md")

# Fivenum
(d4 <- descr(tobacco$BMI, stats = 'fivenum', caption = "fivenum"))

# Subsetting
descr(tobacco[[2]], stats = c("min", "med", "max"))
descr(tobacco[,2], stats = c("min", "med", "max"))

# Weights (from inside data frame)
descr(tobacco, weights = tobacco$samp.wgts)

# Weights (outside data frame)
wgts <- tobacco$samp.wgts
tobacco_subset <- tobacco[,-9]
(d5 <- descr(tobacco_subset, weights = wgts))
print(d5, file = "descr-d5.html", footnote = "Weights")

# split tables at 60 char
print(d5, transpose = TRUE, split.tables = 60)

# by(), special case
(d7 <- by(data = tobacco$BMI, INDICES = tobacco$gender, FUN = descr))
print(d7, file = "descr-d7-by-special-print.html")

# by() - whole data frame
(d8 <- by(data = tobacco, INDICES = tobacco$gender, FUN = descr))
view(d8, file = "descr-d8-by-view.md")

# with() + by()
label(tobacco$BMI) <- "Body Mass Index"
(d9 <- with(tobacco, by(BMI, gender, descr)))
view(d9, file = "descr-d9-by-special-view.html")
