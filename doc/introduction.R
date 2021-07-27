## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(comment = NA,
               prompt  = FALSE,
               cache   = FALSE,
               results = 'asis')
library(kableExtra)
library(summarytools)
library(magrittr)
st_options(plain.ascii = FALSE,
           style       = "rmarkdown",
           footnote    = NA,
           subtitle.emphasis = FALSE,
           lang = "en")

## ---- echo=FALSE--------------------------------------------------------------
st_css(main = TRUE, global = TRUE, bootstrap = FALSE)

## ---- results='asis', echo=FALSE----------------------------------------------
txt <- data.frame(
  Function = 
    c('<a href="#freq"><strong><code>freq()</strong></code></a>',
      '<a href="#ctable"><strong><code>ctable()</strong></code></a>',
      '<a href="#descr"><strong><code>descr()</strong></code></a>',
      '<a href="#dfsummary"><strong><code>dfSummary()</strong></code></a>'),
  Description = 
    c(paste("**Frequency Tables** featuring counts, proportions, cumulative",
             "statistics as well as missing data reporting"),
      paste("**Cross-Tabulations** (joint frequencies) between pairs",
            "of discrete/categorical variables, featuring marginal sums",
            "as well as row, column or total proportions"),
      paste("**Descriptive (Univariate) Statistics** for numerical data, featuring",
            "common measures of central tendency and dispersion"),
      paste("**Data Frame Summaries** featuring type-specific",
            "information for all variables: univariate",
            "statistics and/or frequency distributions, bar charts or",
            "histograms, as well as missing data counts and proportions.",
            "Very useful to quickly, detect anomalies and identify trends",
            "at a glance"))
)

kable(txt, format = "html", escape = FALSE, align = c('l', 'l')) %>%
  kable_paper(full_width = FALSE, position = "left") %>%
  column_spec(1, extra_css = "vertical-align:top") %>%
  column_spec(2, extra_css = "vertical-align:top")

## -----------------------------------------------------------------------------
freq(iris$Species, plain.ascii = FALSE, style = "rmarkdown")

## -----------------------------------------------------------------------------
freq(iris$Species, report.nas = FALSE, headings = FALSE)

## -----------------------------------------------------------------------------
freq(iris$Species, 
     report.nas = FALSE, 
     totals     = FALSE, 
     cumul      = FALSE, 
     headings   = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  freq(tobacco)

## -----------------------------------------------------------------------------
freq(tobacco$disease, 
     order    = "freq",
     rows     = 1:5,
     headings = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  view(freq(tobacco), collapse = TRUE)

## -----------------------------------------------------------------------------
ctable(x = tobacco$smoker, 
       y = tobacco$diseased, 
       prop = "r")   # Show row proportions

## -----------------------------------------------------------------------------
with(tobacco, 
     print(ctable(x = smoker, 
                  y = diseased, 
                  prop     = 'n',
                  totals   = FALSE, 
                  headings = FALSE),
           method = "render")
)

## -----------------------------------------------------------------------------
library(magrittr)
tobacco %$%  # Acts like with(tobacco, ...)
  ctable(x = smoker, y = diseased,
         chisq = TRUE,
         OR    = TRUE,
         RR    = TRUE,
         headings = FALSE) %>%
  print(method = "render")

## -----------------------------------------------------------------------------
descr(iris)

## -----------------------------------------------------------------------------
st_options(descr.silent = TRUE)

## -----------------------------------------------------------------------------
descr(iris,
      stats     = c("mean", "sd"),
      transpose = TRUE,
      headings  = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  st_options(descr.stats = "common")

## ---- eval=FALSE--------------------------------------------------------------
#  view(dfSummary(iris))

## ---- eval=FALSE--------------------------------------------------------------
#  dfSummary(tobacco,
#            plain.ascii  = FALSE,
#            style        = "grid",
#            graph.magnif = 0.75,
#            valid.col    = FALSE,
#            tmp.img.dir  = "/tmp")

## -----------------------------------------------------------------------------
st_options(
  dfSummary.custom.1 = 
    expression(
      paste(
        "Q1 - Q3 :",
        round(
          quantile(column_data, probs = .25, type = 2, 
                   names = FALSE, na.rm = TRUE), digits = 1
        ), " - ",
        round(
          quantile(column_data, probs = .75, type = 2, 
                   names = FALSE, na.rm = TRUE), digits = 1
        )
      )
    )
)

print(
  dfSummary(iris, 
            varnumbers   = FALSE,
            na.col       = FALSE,
            style        = "multiline",
            plain.ascii  = FALSE,
            headings     = FALSE,
            graph.magnif = .8),
  method = "render"
)

## ---- results='markup'--------------------------------------------------------
library(formatR)
st_options(dfSummary.custom.1 = "default")
formatR::tidy_source(
  text   = deparse(st_options("dfSummary.custom.1")),
  indent = 2,
  args.newline = TRUE
)

## ---- eval=FALSE--------------------------------------------------------------
#  dfs <- dfSummary(iris)
#  dfs$Variable <- NULL # This deletes the "Variable" column

## -----------------------------------------------------------------------------
(iris_stats_by_species <- stby(data      = iris, 
                               INDICES   = iris$Species, 
                               FUN       = descr, 
                               stats     = "common", 
                               transpose = TRUE))

## -----------------------------------------------------------------------------
with(tobacco, 
     stby(data    = BMI, 
          INDICES = age.gr, 
          FUN     = descr,
          stats   = c("mean", "sd", "min", "med", "max"))
)

## ---- eval=FALSE--------------------------------------------------------------
#  stby(data    = list(x = tobacco$smoker, y = tobacco$diseased),
#       INDICES = tobacco$gender,
#       FUN     = ctable)
#  
#  # or equivalently
#  with(tobacco,
#       stby(data    = list(x = smoker, y = diseased),
#            INDICES = gender,
#            FUN     = ctable))

## ---- eval=FALSE--------------------------------------------------------------
#  library(dplyr)
#  tobacco$gender %<>% forcats::fct_explicit_na()
#  tobacco %>%
#    group_by(gender) %>%
#    descr(stats = "fivenum")

## ---- echo=FALSE--------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
library(magrittr)
tobacco$gender %<>% forcats::fct_explicit_na()
tobacco %>% group_by(gender) %>% descr(stats = "fivenum")

## ---- results='markup'--------------------------------------------------------
library(magrittr)
iris %>%
  descr(stats = "common") %>%
  tb()

iris$Species %>% 
  freq(cumul = FALSE, report.nas = FALSE) %>% 
  tb()

## ---- results='markup'--------------------------------------------------------
grouped_descr <- stby(data    = exams,
                      INDICES = exams$gender, 
                      FUN     = descr,
                      stats   = "common")

grouped_descr %>% tb()

## ---- results='markup'--------------------------------------------------------
grouped_descr %>% tb(order = 2)

## ---- results='markup'--------------------------------------------------------
grouped_descr %>% tb(order = 3)

## ---- results='asis'----------------------------------------------------------
library(kableExtra)
library(magrittr)
stby(data    = iris, 
     INDICES = iris$Species, 
     FUN     = descr, 
     stats   = "fivenum") %>%
  tb(order = 3) %>%
  kable(format = "html", digits = 2) %>%
  collapse_rows(columns = 1, valign = "top")

## ---- eval=FALSE--------------------------------------------------------------
#  view(iris_stats_by_species, file = "~/iris_stats_by_species.html")
#  view(iris_stats_by_species, file = "~/iris_stats_by_species.md")

## ---- eval=FALSE--------------------------------------------------------------
#  st_options()                      # Display all global options values
#  st_options('round.digits')        # Display the value of a specific option
#  st_options(style = 'rmarkdown',   # Set the value of one or several options
#             footnote = NA)         # Turn off the footnote for all html output

## -----------------------------------------------------------------------------
(age_stats <- freq(tobacco$age.gr)) 
print(age_stats,
      report.nas     = FALSE, 
      totals         = FALSE, 
      display.type   = FALSE,
      Variable.label = "Age Group")

## ---- eval=FALSE--------------------------------------------------------------
#  print(dfSummary(tobacco),
#        custom.css    = 'path/to/custom.css',
#        table.classes = 'tiny-text',
#        file          = "tiny-tobacco-dfSummary.html")

## ---- eval=FALSE--------------------------------------------------------------
#  print(dfSummary(somedata,
#                  varnumbers   = FALSE,
#                  valid.col    = FALSE,
#                  graph.magnif = 0.8),
#        method   = 'render',
#        headings = FALSE,
#        bootstrap.css = FALSE)

## -----------------------------------------------------------------------------
st_options(lang = "fr")

## -----------------------------------------------------------------------------
freq(iris$Species)

## ---- eval = FALSE------------------------------------------------------------
#  Sys.setlocale("LC_CTYPE", "russian")
#  st_options(lang = 'ru')

## ---- eval=FALSE--------------------------------------------------------------
#  Sys.setlocale("LC_CTYPE", "")
#  st_options(lang = "en")

## -----------------------------------------------------------------------------
section_title <- "**Species of Iris**"
define_keywords(title.freq = section_title,
                freq = "N")
freq(iris$Species)

## -----------------------------------------------------------------------------
define_keywords(title.freq = "Types and Counts, Iris Flowers")
print(
  freq(iris$Species,
       display.type = FALSE), # Variable type won't be displayed...
  Variable = ""               # and neither will the variable name
  ) 

