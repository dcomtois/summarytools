#-------------------------------- with-by.R ------------------------------------
# with
w1 <- with(tobacco, freq(gender, plain.ascii = F, weights = samp.wgts))
view(w1, file = "01 - with.md")
view(w1, file = "01 - with.html")

(w2 <- with(tobacco, descr(BMI, transpose = TRUE)))
view(w2, footnote = "with, transpose", file = "02 - with-transposed.html")

view(with(tobacco, ctable(gender, diseased, style = "grid")), method = "pander")

(w3 <- with(tobacco, ctable(gender, diseased, style = "grid", prop = "C")))
view(w3, method = "browser", footnote = "with, prop = c", file = "03 - with-col props.html")

# by
bf1 <- by(data = tobacco$diseased, INDICES = tobacco$smoker, FUN = freq)
view(bf1, 'pander')
view(bf1, 'pander', headings = FALSE, plain.ascii = FALSE)
print(bf1)

view(bf1, file = "04 - by.html", missing = "xxxx")

# with + by
label(tobacco$BMI) <- "Body Mass Index"
label(tobacco$diseased) <- "Subject has illness"
view(by(data = tobacco$diseased, INDICES = tobacco$smoker, FUN = freq), file = "04 - with and by.html")
view(by(data = tobacco$BMI, INDICES = tobacco$gender, FUN = descr), file = "05 - with and by.md")

bf2 <- by(iris$Species, iris$Sepal.Length > mean(iris$Sepal.Length), freq)
view(bf2, 'pander') # 7-23
view(bf2, method = "browser", footnote = "condition pour by", file = "7-24.html")

bd1 <- by(data = tobacco$BMI, INDICES = tobacco$gender, FUN = descr, style = "grid") # 7-11
view(bd1, headings = FALSE, method = "browser", footnote = "by, no headings", file = "7-12.html")
view(bd1, method = 'pander') # 7-13

bd2 <- by(data = tobacco, INDICES = tobacco$gender, FUN = descr) # 7-14
view(bd2, method = "browser", file = "7-15.html")
view(bd2, method = "pander")
label(tobacco) <- "Blabla"

bd3 <- by(data = tobacco, INDICES = tobacco$gender, FUN = descr) # 7-16
st_options(display.labels = FALSE)
view(bd3, display.labels = T, method = "browser", footnote = "override option disp.labels = F", file = "7-17.html")
view(bd3, method = "pander") # 7-18

attach(tobacco)
bd4 <- by(data = BMI, INDICES = gender, FUN = descr) # 7-18
view(bd4, method = "browser", footnote = "floating variable", file = "7-19.html")
view(bd4, method = 'pander') # 7-20

detach(tobacco)

# cas particulier - by, 1 groupe seul.
(b8 <- by(iris, !is.na(iris$Species), descr)) # 7-25
view(with(tobacco, descr(BMI, transpose = TRUE)), method = "browser", footnote = "with, transpose", file = "7-27.html")

attach(tobacco)
view(by(data = BMI, INDICES = gender, FUN = descr), method = "browser", file = "7-29.html") # 7-29
view(by(data = BMI, INDICES = gender, FUN = descr), method = "pander", style = "rmarkdown") # 7-30
view(by(data = BMI, INDICES = gender, FUN = descr), method = "render") # 7-31
detach(tobacco)

b9 <- by(tobacco, tobacco$gender, dfSummary)

# with + by
wb1 <- with(tobacco, by(data = diseased, INDICES = smoker, FUN = freq)) # 7-32
view(wb1, method = "pander", caption = "with + by, # 7-32") # 7-32
view(wb1, method = "browser", footnote = "with + by", file = "7-33.html")
view(wb1, headings = FALSE, footnote = "no headings", file = "7-34.html")
view(wb1, headings = FALSE, footnote = "no headings", method = "pander") # 7-34b

wb2 <- with(tobacco, by(data = BMI, INDICES = gender, FUN = descr)) # 7-35
view(wb2, method = "pander")
view(wb2, method = "browser", footnote = "with + by", file = "7-36.html")

view(with(tobacco, by(data = BMI, INDICES = gender, FUN = descr, stats = "fivenum")), method = "browser", footnote = "with + by fivenum", file = "7-36.html")

(b5 <- with(tobacco, by(BMI, age.gr, descr, stats = c("mean", "sd", "min", "med", "max"))))
print(b5, "pander", style = "rmarkdown") # 7-38

b6 <- with(tobacco, by(BMI, age.gr, descr, stats = c("mean", "sd", "min", "max"), transpose = TRUE))
view(b6, "pander") # 7-39

# TODO: Mettre nom de variable de groupe dans la cellule en haut à gauche (age.gr)
view(b6, method = "browser", footnote = "4 stats", file = "7-40.html")
with(tobacco, view(by(data = diseased, INDICES = smoker, FUN = freq, totals = F), method = "browser", footnote = "with + by", file = "7-41.html"))
view(with(tobacco, by(data = BMI, INDICES = gender, FUN = descr)), method = "browser", file = "7-42.html")
with(tobacco, view(by(data = BMI, INDICES = gender, FUN = descr), method = "pander")) # 7-43

# Ouverture dans viewer
view(b8, footnote = "7-26", method = "viewer")
