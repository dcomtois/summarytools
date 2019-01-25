# Initialize vector containing paths to temporary html files generated when 
# viewing in browser or in RStudio visualization pane. Will be updated whenever
# print.summarytools() / cleartmp() are called.
.st_env <- new.env(parent = emptyenv())
.st_env$tmpfiles <- c()

# Initialize list used by view() when printing an object of class "by"
.st_env$byInfo <- list()

# Initialise list of displayed messages, to avoid repeating the same
# message several times in a short period of time
.st_env$messages <- data.frame(msg = character(), time = numeric(),
                               stringsAsFactors = FALSE)
class(.st_env$messages$time) <- c("POSIXct", "POSIXt")

# Placeholder for customized translations
.st_env$custom_lang <- list()

# Hideous hack to avoid warning on check
utils::globalVariables(c("."))

# summarytools global options
#' @importFrom utils data
.onLoad <- function(libname, pkgname) {
  options(summarytools =
            list("style"                  = "simple",
                 "plain.ascii"            = TRUE,
                 "round.digits"           = 2,
                 "headings"               = TRUE,
                 "footnote"               = "default",
                 "display.labels"         = TRUE,
                 "bootstrap.css"          = TRUE,
                 "custom.css"             = NA,
                 "escape.pipe"            = FALSE,
                 "freq.totals"            = TRUE,
                 "freq.report.nas"        = TRUE,
                 "ctable.prop"            = "r",
                 "ctable.totals"          = TRUE,
                 "descr.stats"            = "all",
                 "descr.transpose"        = FALSE,
                 "descr.silent"           = FALSE,
                 "dfSummary.style"        = "multiline",
                 "dfSummary.varnumbers"   = TRUE,
                 "dfSummary.labels.col"   = TRUE,
                 "dfSummary.graph.col"    = TRUE,
                 "dfSummary.valid.col"    = TRUE,
                 "dfSummary.na.col"       = TRUE,
                 "dfSummary.graph.magnif" = 1,
                 "dfSummary.silent"       = FALSE,
                 "subtitle.emphasis"      = TRUE,
                 "lang"                   = "en"))

  return(invisible())
}

#' @importFrom utils packageDescription
#' @importFrom pander panderOptions
.onAttach <- function(libname, pkgname) {
  pander_built_dt <- packageDescription("pander")$Built
  pander_built_dt <- sub(".+?(\\d+\\-\\d+\\-\\d+).+", "\\1", pander_built_dt)
  should_update <- try(pander_built_dt <= "2018-11-06", silent = TRUE)
  if(isTRUE(should_update))
    packageStartupMessage("For best results, consider updating pander to its ",
                          "most recent version. You can do so by using \n",
                          "devtools::install_github('rapporter/pander')")
}
