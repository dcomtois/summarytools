#-------------------------------- with-by.R ------------------------------------
# with
w1 <- with(tobacco, freq(gender, plain.ascii = F, weights = samp.wgts))
view(w1, file = "01-with.md")
view(w1, file = "01-with.html")

(w2 <- with(tobacco, descr(BMI, transpose = TRUE)))
view(w2, footnote = "with, transpose", file = "02-with-transposed.html")

view(with(tobacco, ctable(gender, diseased, style = "grid")), method = "pander")

(w3 <- with(tobacco, ctable(gender, diseased, style = "grid", prop = "C")))
view(w3, method = "browser", footnote = "with, prop = c", file = "03-with-col-props.html")

# by
bf1 <- stby(data = tobacco$diseased, INDICES = tobacco$smoker, FUN = freq)
view(bf1, 'pander')
view(bf1, 'pander', headings = FALSE, plain.ascii = FALSE)
print(bf1)

view(bf1, file = "04-by-freq.html", missing = "xxxx")

# with + by
label(tobacco$BMI) <- "Body Mass Index"
label(tobacco$diseased) <- "Subject has illness"
view(stby(data = tobacco$diseased, INDICES = tobacco$smoker, FUN = freq), file = "05-with-and-by-freq.html")
view(stby(data = tobacco$BMI, INDICES = tobacco$gender, FUN = descr), file = "06-with-and-by-descr.md")

bf2 <- stby(iris$Species, iris$Sepal.Length > mean(iris$Sepal.Length), freq)
view(bf2, 'pander')
view(bf2, method = "browser", footnote = "condition for by", file = "07-by-calculated-freq.html")

bd1 <- stby(data = tobacco$BMI, INDICES = tobacco$gender, FUN = descr, style = "grid")
view(bd1, headings = FALSE, method = "browser", footnote = "by, no headings", file = "08-by-descr-no-head.html")
view(bd1, method = 'pander')

bd2 <- stby(data = tobacco, INDICES = tobacco$gender, FUN = descr)
view(bd2, method = "browser", file = "09-by-descr.html")
view(bd2, method = "pander", file = "09-by-descr-view.md")

label(tobacco) <- "Blabla"
bd3 <- stby(data = tobacco, INDICES = tobacco$gender, FUN = descr)
st_options(display.labels = FALSE)
view(bd3, display.labels = T, method = "browser", footnote = "override option disp.labels = F", file = "10-by-descr.html")
print(bd3, file = "10-by-descr.md")

attach(tobacco)
bd4 <- stby(data = BMI, INDICES = gender, FUN = descr)
view(bd4, method = "browser", footnote = "stand-alone variable", file = "11-by-descr-standalone-var.html")
print(bd4, file = "11-by-descr-standalone-var.md")
detach(tobacco)

# cas particulier - by, 1 groupe seul.
(b8 <- stby(iris, !is.na(iris$Species), descr))
view(with(tobacco, descr(BMI, transpose = TRUE)), method = "browser", footnote = "with, transpose", file = "12.html")

attach(tobacco)
view(stby(data = BMI, INDICES = gender, FUN = descr), method = "browser", file = "13.html") 
view(stby(data = BMI, INDICES = gender, FUN = descr), method = "pander", style = "rmarkdown") 
view(stby(data = BMI, INDICES = gender, FUN = descr), method = "render") 
detach(tobacco)

stby(tobacco, tobacco$gender, dfSummary)
stby(tobacco, tobacco$gender, descr)
stby(tobacco$disease, list(tobacco$smoker, tobacco$gender), freq)


# with + by
wb1 <- with(tobacco, stby(data = diseased, INDICES = smoker, FUN = freq)) 
view(wb1, method = "pander", caption = "with + by")
view(wb1, method = "browser", footnote = "with + by", file = "14.html")
view(wb1, headings = FALSE, footnote = "no headings", file = "15.html")
view(wb1, headings = FALSE, footnote = "no headings", method = "pander") 

wb2 <- with(tobacco, stby(data = BMI, INDICES = gender, FUN = descr)) 
view(wb2, method = "pander")
view(wb2, method = "browser", footnote = "with + by", file = "16.html")

view(with(tobacco, stby(data = BMI, INDICES = gender, FUN = descr, stats = "fivenum")), method = "browser", footnote = "with + by fivenum", file = "17.html")

(b5 <- with(tobacco, stby(BMI, age.gr, descr, stats = c("mean", "sd", "min", "med", "max"))))
print(b5, "pander", style = "rmarkdown") 

b6 <- with(tobacco, stby(BMI, age.gr, descr, stats = c("mean", "sd", "min", "max"), transpose = TRUE))
view(b6, "pander") 

# TODO: Mettre nom de variable de groupe dans la cellule en haut à gauche (age.gr)
view(b6, method = "browser", footnote = "4 stats", file = "18.html")
with(tobacco, view(stby(data = diseased, INDICES = smoker, FUN = freq, totals = F), method = "browser", footnote = "with + by", file = "19.html"))
view(with(tobacco, stby(data = BMI, INDICES = gender, FUN = descr)), method = "browser", file = "20.html")
with(tobacco, view(stby(data = BMI, INDICES = gender, FUN = descr), method = "pander"))

# Ouverture dans viewer
view(b8, method = "viewer")

# by + weights
with(tobacco, stby(smoker, gender, freq, weights = samp.wgts))
with(tobacco, stby(list(x=gender, y=smoker), age.gr, ctable, weights = samp.wgts))

library(dplyr)
tobacco %>% group_by(gender) %>% freq(smoker, weights = samp.wgts)
