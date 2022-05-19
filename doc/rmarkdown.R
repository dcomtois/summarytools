## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(comment = NA, 
               prompt  = FALSE,
               cache   = FALSE,
               echo    = TRUE,
               results = 'asis')
library(summarytools)
st_options(bootstrap.css     = FALSE,       # Already part of the theme so no need for it
           plain.ascii       = FALSE,       # One of the essential settings
           style             = "rmarkdown", # Idem.
           dfSummary.silent  = TRUE,        # Suppresses messages about temporary files
           footnote          = NA,          # Keeping the results minimalist
           descr.silent      = TRUE,        # To avoid messages when building / checking
           subtitle.emphasis = FALSE)       # For the vignette theme, this gives better results.
                                            # For other themes, using TRUE might be preferable.

## ---- echo=FALSE--------------------------------------------------------------
st_css(main = TRUE, global = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  st_options(plain.ascii = FALSE, style = "rmarkdown")

## -----------------------------------------------------------------------------
freq(tobacco$gender, plain.ascii = FALSE, style = 'rmarkdown')

## -----------------------------------------------------------------------------
print(freq(tobacco$gender), method = 'render')

## ---- message=FALSE-----------------------------------------------------------
print(descr(tobacco), method = 'render', table.classes = 'st-small')

## -----------------------------------------------------------------------------
ctable(tobacco$gender, 
       tobacco$smoker,
       plain.ascii = FALSE, 
       style = 'rmarkdown')

## ----ctable_html--------------------------------------------------------------
print(ctable(tobacco$gender, tobacco$smoker), method = 'render')

## -----------------------------------------------------------------------------
descr(tobacco, plain.ascii = FALSE, style = 'rmarkdown')

## ---- message=FALSE-----------------------------------------------------------
print(descr(tobacco), method = 'render', table.classes = 'st-small')

## ----dfs_grid, eval=FALSE-----------------------------------------------------
#  dfSummary(tobacco,
#            plain.ascii  = FALSE,
#            style        = 'grid',
#            graph.magnif = 0.85,
#            varnumbers = FALSE,
#            valid.col    = FALSE,
#            tmp.img.dir  = "/tmp")

## -----------------------------------------------------------------------------
print(dfSummary(tobacco, 
                varnumbers   = FALSE, 
                valid.col    = FALSE, 
                graph.magnif = 0.76),
      method = 'render')

## -----------------------------------------------------------------------------
print(dfSummary(tobacco, 
                varnumbers   = FALSE,
                valid.col    = FALSE,
                graph.magnif = 0.76), 
      max.tbl.height = 300,
      method = "render")

## -----------------------------------------------------------------------------
library(kableExtra)
library(magrittr)
stby(iris, iris$Species, descr, stats = "fivenum") %>%
  tb() %>%
  kable(format = "html", digits = 2) %>%
  collapse_rows(columns = 1, valign = "top")

## -----------------------------------------------------------------------------
stby(iris, iris$Species, descr, stats = "fivenum") %>%
  tb(order = 3) %>%
  kable(format = "html", digits = 2) %>%
  collapse_rows(columns = 1, valign = "top")

