## ----setup, include=FALSE------------------------------------------------
library(summarytools)
library(knitr)
opts_chunk$set(comment=NA, prompt=FALSE, cache=FALSE)

## ----barebones-----------------------------------------------------------
freq(iris$Species)

## ---- results='asis', echo=FALSE-----------------------------------------
library(summarytools)  
freq(tobacco$smoker, style='rmarkdown')

## ----descr_md, results='asis'--------------------------------------------
data(exams)
descr(exams[ ,3:5], style='rmarkdown')

## ----descr_md2, eval=FALSE-----------------------------------------------
#  descr(exams, style = 'rmarkdown', transpose = TRUE)

## ----ctable2-------------------------------------------------------------
with(tobacco, ctable(smoker, diseased, prop = 'n', totals = FALSE))

## ----ctable1-------------------------------------------------------------
with(tobacco, ctable(smoker, diseased, prop = 'r'))

## ----render_html, results='asis'-----------------------------------------
crosstable <- with(tobacco, ctable(smoker, diseased))
print(crosstable, method='render', footnote = NA)

## ----dfSummary1, results='asis'------------------------------------------
dfSummary(tobacco, style='grid', plain.ascii = FALSE)

## ----redir, eval=FALSE---------------------------------------------------
#  my_summary <- dfSummary(tobacco)
#  print(my_summary, file = "tobacco.txt", style = "grid")  # Creates tobacco.txt
#  my_stats <- descr(tobacco)
#  print(my_stats, file = "tobacco.txt", append = TRUE) # Appends results to tobacco.txt

## ----view_html, eval=FALSE-----------------------------------------------
#  print(dfSummary(tobacco), method = 'browser')  # Displays results in default Web Browser
#  print(dfSummary(tobacco), method = 'viewer')   # Displays results in RStudio's Viewer
#  view(dfSummary(tobacco))                       # Same as line above -- view() is a wrapper function

## ----create_html, eval=FALSE---------------------------------------------
#  print(dfSummary(tobacco), file = '~/Documents/tobacco_summary.html')

## ----glob_opts, eval=FALSE-----------------------------------------------
#  st_options() # display all global options' values
#  st_options('round.digits') # display only one option
#  st_options('round.digits', 1) # change an option's value

## ----by_stats1, results='asis'-------------------------------------------
stats <- by(data = exams$geography, INDICES = exams$gender, FUN = descr, style = 'rmarkdown')
view(stats, method = 'pander')

## ----what_is, warning=FALSE----------------------------------------------
what.is(iris)

