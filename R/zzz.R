# Initialize vector containing paths to temporary html files generated when 
# viewing in browser or in RStudio visualization pane. Will be updated whenever
# print.summarytools() / cleartmp() are called.
.st_env <- new.env(parent = emptyenv())

# Determine OS : Windows | Linux | Darwin
.st_env$sysname <- Sys.info()[["sysname"]]

# Initialize vector for tempfiles -- useful for cleartmp()
.st_env$tmpfiles <- c()

# Initialize list used by view() when printing an object of class "by"
.st_env$byInfo <- list()

# Placeholder for customized translations
.st_env$custom_lang <- list()

# "Hideous hack" to avoid warning on check
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
                 "char.split"             = 12,
                 "freq.cumul"             = TRUE,
                 "freq.totals"            = TRUE,
                 "freq.report.nas"        = TRUE,
                 "freq.ignore.threshold"  = 25,
                 "freq.silent"            = FALSE,
                 "ctable.prop"            = "r",
                 "ctable.totals"          = TRUE,
                 "ctable.round.digits"    = 1,
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
                 "tmp.img.dir"            = NA_character_,
                 "subtitle.emphasis"      = TRUE,
                 "lang"                   = "en",
                 "use.x11"                = TRUE))

  return(invisible())
}

#' @importFrom utils packageDescription
#' @importFrom pander panderOptions
.onAttach <- function(libname, pkgname) {
  
  if (Sys.info()[["sysname"]] != "Windows" && !isTRUE(capabilities("X11"))) {
    packageStartupMessage("system might not have X11 capabilities; in case of ",
                          "errors when using dfSummary(), set ",
                          "st_options(use.x11 = FALSE)")
  }
  
  # Check if the latest (github) pander is installed;
  # We can't use version number since the relevant fix is not in an incremented
  # package version number (still 0.6.3); we use the *Packaged* attribute as a proxy.
  pander_pkg_dt <- substr(packageDescription("pander")$Packaged, 1, 10)
  should_update <- try(pander_pkg_dt <= "2018-11-06", silent = TRUE)
  
  if (isTRUE(should_update))
    packageStartupMessage("For best results, restart R session and update pander using devtools:: or remotes::",
                          "install_github('rapporter/pander')")
}
