## ----setup, include=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(comment=NA, prompt=FALSE, cache=FALSE, echo=TRUE, results='asis')
library(summarytools)

## ----setup_st------------------------------------------------------------
st_options('omit.headings', TRUE)
st_options('bootstrap.css', FALSE)
st_options('footnote', NA)

## ----freq_rm-------------------------------------------------------------
freq(tobacco$gender, style = 'rmarkdown')

## ----freq_html-----------------------------------------------------------
print(freq(tobacco$gender), method = 'render')

## ----descr_rm------------------------------------------------------------
descr(tobacco, style = 'rmarkdown')

## ----descr_html----------------------------------------------------------
print(descr(tobacco), method = 'render', table.classes = 'st-small')

## ----ctable_rm, results='asis'-------------------------------------------
ctable(tobacco$gender, tobacco$smoker, style = 'rmarkdown')

## ----ctable_html---------------------------------------------------------
print(ctable(tobacco$gender, tobacco$smoker), method = 'render')

## ----dfs_grid, results='asis'--------------------------------------------
dfSummary(tobacco, style = 'grid', plain.ascii = FALSE)

## ----dfs_html, results='asis'--------------------------------------------
print(dfSummary(tobacco, graph.magnif = 0.75), method = 'render')

