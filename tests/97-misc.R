
s <- "N.Valid 975.00 974.00 938.00 812.11"
summarytools:::trs("n.valid")

library(summarytools)
library(devtools)
load_all()
cpd <- rep(tobacco$cigs.per.day * 947535, 1000)
freq(cpd, big.mark = " ")
freq(cpd, big.mark = " ", big.mark = ",", justify = "c", missing="?", keep.trailing.zeros=F, round.digits = 2)
freq(cpd, big.mark = " ", big.mark = ",", justify = "l", missing=" ", keep.trailing.zeros=F, round.digits = 2)
freq(cpd, big.mark = " ", big.mark = ",", justify = "r", missing="<NA>", keep.trailing.zeros=F, round.digits = 2)
freq(cpd, big.mark = ",", big.mark = " ", justify = "c", missing="?", keep.trailing.zeros=F, round.digits = 2)
view(freq(cpd, big.mark = ","))


pattn <- paste0("(", trs("n.valid"), ".+?\\d+)\\.". strrep("0", format_info$round.digits))
repl  <- paste0("\\1", strrep(" ", format_info$round.digits))            
while(grepl(re, s)) {
  s <- sub(re, "\\1", s)
}
s
sub("(N.Valid.+?\\d+)\\.00", "\\1",s)

s <- "N.Valid 975.00 974.00 938.11"
re <- "((?:\\G)N.Valid )|(?&rec)(\\d+\\.00\\s*)(?'rec')"
re <- "(?&rec)(975.00)+(?'rec')"
sub(re, "\\1", s)
"((?:\\A|\\G) )|(?&rec)( )+(?'rec')$"

# - 03-ctable lignes 60+ noms de vars ctable + stby
# - ajout or et rr ctable

load("tests/data/special_vars.RData")
sv <- special_vars["some_empty_str"]
dfSummary(sv)
view(dfSummary(sv))
table(special_vars$some_empty_str)

# Pander // digits conundrum
panderOptions('keep.trailing.zeros', T)
pander(mtcars[1:3, 1:5], nsmall=2, digits=4, keep.trailing.zeros=T)

# ?
con <- file("test.log")

sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

# This will echo all input and not truncate 150+ character lines...
source("script.R", echo=TRUE, max.deparse.length=10000)

# Restore output to console
sink() 
sink(type="message")
close(con)

library(magrittr)
grouped_freqs <- stby(data = tobacco$smoker,
                      INDICES = tobacco$gender,
                      FUN = freq, cumul = FALSE, report.nas = FALSE)
grouped_freqs %>% tb()
dfSummary(tobacco, valid.col = FALSE, graph.magnif = .75, plain.ascii = FALSE, tmp.img.dir = "/tmp", style = "grid")

library(summarytools)
library(knitr)
library(kableExtra)
library(formattable)
library(magrittr)

library(kableExtra)
library(magrittr)
library(dplyr)
stby(iris, iris$Specie, descr, stats = "common") %>%
  tb(order=2) %>%
  select(variable, everything()) %>%
  arrange() %>%
  kable(format = "html", digits = 2) %>%
  collapse_rows(1)

%>%
  collapse_rows(columns = 2, valign = "top")


grf <- tobacco %$% stby(diseased, INDICES = list(gender, smoker),
                        FUN = freq)

ttt <- grf %>% tb()
grf %>% tb(order = 2)

grd <- tobacco %$% stby(tobacco[c(2,4,6)], 
                        INDICES = list(gender, smoker, age.gr),
                        FUN = descr, stats = "fivenum")
grd %>% tb()
grd %>% tb(2, swap = TRUE)
colnames(tobacco)
grouped_descr %>% tb(order = 2)

grouped_descr <- stby(data = exams, INDICES = exams$gender, 
                      FUN = descr, stats = "common")
grouped_descr %>% tb(2)
grouped_descr %>% tb(2, swap = T)

library(kableExtra)
library(magrittr)
stby(iris, iris$Species, descr, stats = "common") %>%
  tb(order = 2, swap = TRUE) %>%
  kable(format = "html", digits = 2) %>%
  collapse_rows(columns = 1, valign = "top")

stby(tobacco$BMI, INDICES = list(tobacco$gender, tobacco$age.gr),
     descr, stats = "common") %>%
  tb() %>%
  kable(digits = 2)


stby(iris, iris$Species, descr, stats = "fivenum") %>%
  tb(order = 1, swap = FALSE)

stby(iris, iris$Species, descr, stats = "fivenum") %>%
  tb(order = 2, swap = TRUE) %>%
  kable(format = "html", digits = 2) %>%
  collapse_rows(columns = 1, valign = "top")  

print(dfSummary(iris, varnumbers = FALSE, valid.col = FALSE, 
                graph.magnif = 0.8), 
      method = 'render',
      headings = FALSE,
      bootstrap.css = FALSE)
data <- iris
print(dfSummary(data), method = 'render')
