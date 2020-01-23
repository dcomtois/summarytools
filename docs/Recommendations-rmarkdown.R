## ---- include=FALSE-----------------------------------------------------------
library(knitr)
opts_chunk$set(comment=NA, prompt=FALSE, cache=FALSE, echo=TRUE, results='asis')

## ---- echo=FALSE--------------------------------------------------------------
library(summarytools)
st_css()

## -----------------------------------------------------------------------------
st_options(bootstrap.css     = FALSE,       # Already part of the theme so no need for it
           plain.ascii       = FALSE,       # One of the essential settings
           style             = "rmarkdown", # Idem.
           dfSummary.silent  = TRUE,        # Suppresses messages about temporary files
           footnote          = NA,          # Keeping the results minimalistic
           subtitle.emphasis = FALSE)       # For the vignette theme, this gives
                                            # much better results. Your mileage may vary.

## ---- eval=FALSE--------------------------------------------------------------
#  library(knitr)
#  opts_chunk$set(comment=NA, prompt=FALSE, cache=FALSE, echo=TRUE, results='asis')

## ---- eval=FALSE--------------------------------------------------------------
#  st_css()

## -----------------------------------------------------------------------------
freq(tobacco$gender, style = 'rmarkdown')

## -----------------------------------------------------------------------------
print(freq(tobacco$gender), method = 'render')

## -----------------------------------------------------------------------------
ctable(tobacco$gender, tobacco$smoker, style = 'rmarkdown')

## ----ctable_html--------------------------------------------------------------
print(ctable(tobacco$gender, tobacco$smoker), method = 'render')

## -----------------------------------------------------------------------------
descr(tobacco, style = 'rmarkdown')

## -----------------------------------------------------------------------------
print(descr(tobacco), method = 'render', table.classes = 'st-small')

## ----dfs_grid, eval=FALSE-----------------------------------------------------
#  dfSummary(tobacco, style = 'grid', graph.magnif = 0.75, tmp.img.dir = "/tmp")

## -----------------------------------------------------------------------------
print(dfSummary(tobacco, graph.magnif = 0.75), method = 'render')

