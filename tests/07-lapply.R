# lapply.R -------------------------------------
suppressPackageStartupMessages(library(summarytools))
options(tibble.print_max = Inf)

data("tobacco")
tobacco_subset <- tibble::as_tibble(tobacco[ ,c("gender", "age.gr", "smoker")])
label(tobacco_subset) <- "subset of tobacco"
label(tobacco_subset$gender) <- "The Gender"
label(tobacco_subset$age.gr) <- "The Age Group"
label(tobacco_subset$smoker) <- "Subject smokes"

l1 <- lapply(tobacco_subset, freq) 
print(l1)
view(l1, footnote = "freq -- gender/age.gr/smoker", file = "01.html")

label(tabagisme) <- "Label tabagisme"
label(tabagisme$sexe) <- "Le sexe"
label(tabagisme$age.gr) <- "Groupe d'age"

l1 <- lapply(X = tabagisme[,-c(2,4,9)], FUN = freq) 
print(l1)
print(l1, footnote = "freq -- negative indexing on columns", file = "02.html")
view(l1, headings = FALSE, footnote = "headings = FALSE", file = "03.html") 

l2 <- lapply(X = tabagisme[1:20, c(1,3)], FUN = freq)
view(l2, footnote = "2 columns with row indexing", file = "04.html")

l3 <- lapply(X = tabagisme[1:20, c(1,3)], FUN = freq, totals = FALSE)
print(l3, style = "rmarkdown", caption = "2 columns with row indexing, no totals")
view(l3, method = "pander", style = "rmarkdown", display.type = FALSE, caption = "display.type = FALSE")
print(l3, headings = FALSE, footnote = "no headings", file = "05.html")
