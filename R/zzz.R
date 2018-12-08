# Initialize vector containing paths to temporary html files generated when 
# viewing in browser or in RStudio visualization pane. Will be updated whenever
# print.summarytools() / cleartmp() are called.
.st_env <- new.env(parent = emptyenv())
.st_env$tmpfiles <- c()

# Initialize list used by view() when printing an object of class "by"
.st_env$byInfo <- list()

# Initialise message to be displayed in some specific circumstances
.st_env$last.message <- list(msg = "", time = 0)

# summarytools global options
.onLoad <- function(libname, pkgname) {
  st.options <- list('style'                  = 'simple',
                     'round.digits'           = 2,
                     'plain.ascii'            = TRUE,
                     'headings'               = TRUE,
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
                     'dfSummary.labels.col'   = TRUE,
                     'dfSummary.graph.col'    = TRUE,
                     'dfSummary.valid.col'    = TRUE,
                     'dfSummary.na.col'       = TRUE,
                     'dfSummary.graph.magnif' = 1)

  if (!'summarytools' %in% names(options())) {
    options(summarytools = st.options)
  } else {
    toadd <- st.options[which(!names(st.options) %in% 
                                names(options()$summarytools))]
    options(summarytools = append(options()$summarytools, toadd))
  }
  return(invisible())
}

#' @importFrom utils packageDate
#' @importFrom pander panderOptions
.onAttach <- function(libname, pkgname) {
  #panderOptions("knitr.auto.asis", FALSE)
  if (packageDate('pander',date.fields = 'Packaged') <= "2018-11-06")
    packageStartupMessage("For best results, consider updating pander to its ",
                          "most recent version with ",
                          "devtools::install_github('rapporter/pander')")
}
