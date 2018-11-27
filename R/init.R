# Initialize vector containing paths to temporary html files generated when viewing
# in browser or in RStudio visualization pane. Will be updated whenever
# print.summarytools() / cleartmp() are called.
.st_env <- new.env(parent = emptyenv())
.st_env$tmpfiles <- c()

# Initialize list used by view() when printing an object of class "by"
.st_env$byInfo <- list()

# summarytools global options
.onLoad <- function(libname, pkgname) {
  summarytools.options <- list('style'                  = 'simple',
                               'round.digits'           = 2,
                               'plain.ascii'            = TRUE,
                               'omit.headings'          = FALSE,
                               'footnote'               = 'default',
                               'display.labels'         = TRUE,
                               'bootstrap.css'          = TRUE,
                               'custom.css'             = NA,
                               'escape.pipe'            = FALSE,
                               'freq.totals'            = TRUE,
                               'freq.report.nas'        = TRUE,
                               'ctable.prop'            = 'r',
                               'ctable.totals'          = TRUE,
                               'descr.stats'            = 'all',
                               'descr.transpose'        = FALSE,
                               'dfSummary.varnumbers'   = TRUE,
                               'dfSummary.valid.col'    = TRUE,
                               'dfSummary.na.col'       = TRUE,
                               'dfSummary.graph.col'    = TRUE,
                               'dfSummary.graph.magnif' = 1)
  
  toset <- !(names(summarytools.options) %in% names(options()))
  
  if(any(toset)) options(summarytools.options[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  pander::panderOptions("knitr.auto.asis", FALSE)
  if (packageDate('pander',date.fields = 'Packaged') <= "2018-11-06")
    message("For best results, consider updating pander to its most recent version ",
            "with devtools::install_github('rapporter/pander')")
}
