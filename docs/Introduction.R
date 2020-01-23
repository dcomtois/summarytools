## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(comment = NA, prompt = FALSE, cache = FALSE, results = 'asis')
library(summarytools)
st_options(plain.ascii = FALSE,
           style = "rmarkdown",
           footnote = NA,
           subtitle.emphasis = FALSE)

## ---- echo=FALSE--------------------------------------------------------------
st_css()

## -----------------------------------------------------------------------------
library(summarytools)
freq(iris$Species, plain.ascii = FALSE, style = "rmarkdown")

## -----------------------------------------------------------------------------
freq(iris$Species, report.nas = FALSE, headings = FALSE)

## -----------------------------------------------------------------------------
freq(iris$Species, report.nas = FALSE, totals = FALSE, 
     cumul = FALSE, headings = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  freq(tobacco)

## -----------------------------------------------------------------------------
freq(tobacco$disease, order = "freq", rows = 1:5)

## -----------------------------------------------------------------------------
print(ctable(x = tobacco$smoker, y = tobacco$diseased, prop = "r"),
      method = "render")

## -----------------------------------------------------------------------------
with(tobacco, 
     print(ctable(x = smoker, y = diseased, prop = 'n', 
                  totals = FALSE, headings = FALSE),
           method = "render"))

## -----------------------------------------------------------------------------
library(magrittr)
tobacco %$% 
  ctable(gender, smoker, chisq = TRUE, headings = FALSE) %>%
  print(method = "render")

## -----------------------------------------------------------------------------
descr(iris)

## -----------------------------------------------------------------------------
descr(iris, stats = c("mean", "sd"), transpose = TRUE, headings = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  library(summarytools)
#  view(dfSummary(iris))

## ---- eval=FALSE--------------------------------------------------------------
#  dfSummary(tobacco, style = "grid", graph.magnif = 0.75,
#            valid.col = FALSE, tmp.img.dir = "/tmp")

## -----------------------------------------------------------------------------
(iris_stats_by_species <- stby(data = iris, 
                               INDICES = iris$Species, 
                               FUN = descr, stats = c("common"), transpose = TRUE))

## -----------------------------------------------------------------------------
with(tobacco, stby(data = BMI, INDICES = age.gr, 
                   FUN = descr, stats = c("mean", "sd", "min", "med", "max")))

## ---- eval=FALSE--------------------------------------------------------------
#  stby(list(x = tobacco$smoker, y = tobacco$diseased), tobacco$gender, ctable)
#  # or equivalently
#  with(tobacco, stby(list(x = smoker, y = diseased), gender, ctable))

## ---- eval=FALSE--------------------------------------------------------------
#  library(dplyr)
#  tobacco$gender <- forcats::fct_explicit_na(tobacco$gender)
#  tobacco %>% group_by(gender) %>% descr(stats = "fivenum")

## ---- echo=FALSE--------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
tobacco$gender <- forcats::fct_explicit_na(tobacco$gender)
tobacco %>% group_by(gender) %>% descr(stats = "fivenum")

## ---- results='markup'--------------------------------------------------------
library(magrittr)
iris %>% descr(stats = "common") %>% tb()
iris$Species %>% freq(cumul = FALSE, report.nas = FALSE) %>% tb()

## ---- results='markup'--------------------------------------------------------
grouped_freqs <- stby(data = tobacco$smoker, INDICES = tobacco$gender,
                      FUN = freq, cumul = FALSE, report.nas = FALSE)
grouped_freqs %>% tb()
grouped_freqs %>% tb(order = 2)

grouped_descr <- stby(data = exams, INDICES = exams$gender, 
                      FUN = descr, stats = "common")
grouped_descr %>% tb()
grouped_descr %>% tb(order = 2)

## ---- eval=FALSE--------------------------------------------------------------
#  view(iris_stats_by_species, file = "~/iris_stats_by_species.html")

## ---- eval=FALSE--------------------------------------------------------------
#  st_options()                      # display all global options values
#  st_options('round.digits')        # display the value of a specific option
#  st_options(style = 'rmarkdown')   # change one or several options' values
#  st_options(footnote = NA)         # Turn off the footnote on all outputs.
#                                    # This option was used prior to generating
#                                    # the present document.

## -----------------------------------------------------------------------------
(age_stats <- freq(tobacco$age.gr)) 
print(age_stats, report.nas = FALSE, totals = FALSE, display.type = FALSE,
      Variable.label = "Age Group")

## ---- eval=FALSE--------------------------------------------------------------
#  print(dfSummary(tobacco), custom.css = 'path/to/custom.css',
#        table.classes = 'tiny-text', file = "tiny-tobacco-dfSummary.html")

## ---- eval=FALSE--------------------------------------------------------------
#  print(dfSummary(somedata, graph.magnif = 0.8),
#        method = 'render',
#        headings = FALSE,
#        bootstrap.css = FALSE)

## -----------------------------------------------------------------------------
st_options(lang = "fr")

## -----------------------------------------------------------------------------
freq(iris$Species)

## ---- eval = FALSE------------------------------------------------------------
#  Sys.setlocale("LC_CTYPE", "russian")
#  st_options(lang = 'ru')

## ---- results='hide'----------------------------------------------------------
Sys.setlocale("LC_CTYPE", "")
st_options(lang = "en")

## ---- eval=FALSE--------------------------------------------------------------
#  define_keywords(freq = "N")

