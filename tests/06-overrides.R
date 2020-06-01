# overrides.R -------------------------------------
suppressPackageStartupMessages(library(summarytools))
options(tibble.print_max = Inf)

data(tobacco)
label(tobacco$age.gr) <- "Groupe d'âge"

f1 <- freq(tobacco$age.gr)
print(f1, plain.ascii = F, round.digits = 3, display.labels = FALSE, display.type = F, missing = '------', report.nas = T, totals = F, Variable = "fake var name") # 6-1
view(f1, round.digits = 1, display.labels = F, display.type = F, missing = '---', report.nas = F, totals = F, Variable = "fake var name", footnote = 'no totals no nas no type no label', method="browser", file = "01.html")
print(f1, method = 'render', footnote = 'f1')

(f2 <- freq(tabagisme$maladie, order = "freq"))
print(f2, justify = "left", file = "02-justify-left.md")
view(f2, justify = "right", footnote = 'justify right', file = '03-justify-right.html')

st_options("reset")
detach("package:summarytools")
