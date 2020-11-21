# descr.R ---------------------------------------
suppressPackageStartupMessages(library(summarytools))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

options(tibble.print_max = Inf)

tobacco <- tibble::as_tibble(tobacco)
(d1 <- descr(tobacco))

print(d1, plain.ascii = F)
print(d1, style = 'rmarkdown')
print(d1, style = 'grid', plain.ascii = FALSE)

# Global options
st_options(style = 'rmarkdown', descr.stats = "common", descr.transpose = TRUE)
(d2 <- descr(tobacco))
print(d2, headings = FALSE)

# Silent Option
print(descr(tobacco), silent = TRUE)
st_options(descr.silent = TRUE)
descr(tobacco)
print(descr(tobacco), silent = FALSE)
st_options(descr.silent = FALSE)

# labels
label(tobacco) <- "A study on Tobacco and Health"
descr(tobacco, style = "simple")

label(tobacco$BMI) <- "Body Mass Index"
(d2 <- descr(tobacco$BMI))

# print to files
print(d2, file = "01.html")
view(d2, footnote = "test: àéïôù", file = "02.html")
print(d2, caption = "test: àéïôù", file = "02.md")

# long var names
st_options(descr.transpose = FALSE)
some.long.variable.name <- tobacco$age
some.long.variabl.name <- tobacco$age
print(descr(some.long.variable.name), file = "03.html")
view(descr(some.long.variabl.name), file = "04.html")

# Round digits
(d3 <- descr(tobacco$age, round.digits = 1))
view(d3,  file = "05.md")

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
print(d5, file = "06.html", footnote = "Weights")

# split tables at 40 char
print(d5, transpose = TRUE, split.tables = 40)

# stby(), special case
(d7 <- stby(data = tobacco$BMI, INDICES = tobacco$gender, FUN = descr))
print(d7, file = "07.html")

# stby() - whole data frame
(d8 <- stby(data = tobacco, INDICES = tobacco$gender, FUN = descr))
view(d8, file = "08.md")

# with() + stby()
label(tobacco$BMI) <- "Body Mass Index"
(d9 <- with(tobacco, stby(BMI, gender, descr)))
view(d9, file = "09.html")

# by with weights
(d10 <- stby(tobacco, tobacco$smoker, descr, weights = tobacco$samp.wgts))
view(d10)

# tb()
descr(tobacco) %>% tb()
descr(tobacco, stats = "common", transpose = TRUE) %>% tb()

tobacco %>% select(age) %>% arrange() %>% descr(stats = "common")

# Column order
descr(exams, order = "sort")
descr(exams, order = c("french", "english", "geography", "history", "math", "economics"))
(d11 <- descr(exams, order = "preserve"))
view(d11, file = "11.html")

# Large numbers
bignum <- rep(tobacco$cigs.per.day, 500) * 999.25

(dbig1 <- descr(bignum, big.mark = ","))
view(dbig1, file = "dbig1.html")
(dbig2 <- descr(bignum, big.mark = " ", decimal.mark = ","))
view(dbig2, file = "dbig2.html")
(dbig3 <- descr(bignum, big.mark = "'", decimal.mark = "."))
view(dbig3, file = "dbig3.html")
(dbig4 <- descr(bignum, big.mark = ".", decimal.mark = ","))
view(dbig4, file = "dbig4.html")


st_options("reset")
detach("package:summarytools")
detach("package:dplyr")
detach("package:magrittr")
