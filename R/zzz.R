# Initialize vector containing paths to temporary html files generated when 
# viewing in browser or in RStudio visualization pane. Will be updated whenever
# print.summarytools() / cleartmp() are called.
.st_env <- new.env()

# Initialize environment used by parse_call()
.p <- new.env()

# Determine OS : Windows | Linux | Darwin
.st_env$sysname <- Sys.info()[["sysname"]]

# Initialize vector for tempfiles -- useful for cleartmp()
.st_env$tmpfiles <- c()

# Initialise list used to keep track of current process
.st_env$ps <- list()

# Placeholder for customized translations
.st_env$custom_lang <- list()

# Predefined stats values for descr
.st_env$descr.stats <- list(
  all     = c("mean", "sd", "min", "q1", "med", "q3","max", "mad",
              "iqr", "cv", "skewness", "se.skewness", "kurtosis",
              "n.valid", "pct.valid", "n"),
  common  = c("mean", "sd", "min", "med", "max",
              "n.valid", "pct.valid", "n"),
  fivenum = c("min", "q1", "med", "q3", "max")
)

.st_env$descr.stats.valid <- list(
  no_wgts = c("mean", "sd", "min", "q1", "med", "q3","max", "mad", 
              "iqr", "cv", "skewness", "se.skewness", "kurtosis", 
              "n.valid", "pct.valid", "n"),
  wgts = c("mean", "sd", "min", "med", "max", "mad", "cv", 
           "n.valid", "pct.valid", "n")
)

# most common operators -- used by parse_call()
.st_env$oper <- c("$", "[", "[[", "<", ">", "<=", ">=", "==", ":", "|>", 
                  "%!>%", "%$%", "%<>%", "%>%", "%T>%", "%>>%")

# regex used in parse_call
.st_env$re <-list(
  # 1) both names are there (df$name, df['name']), etc.
  two_names = paste0("^([\\w.]+)(\\$|\\[{1,2})(.*\\,\\s*)?['\"]?",
                     "([a-zA-Z._][\\w._]+)['\"]?\\]{0,2}(\\[.+)?$"),
  
  # 2) there is numeric indexing (df[[2]], df[ ,2], df[2])
  num_index = "^([\\w.]+)(\\$|\\[{1,2})(.*\\,\\s*)?(\\d+)\\]{1,2}(\\[.+)?$",
  
  
  # 3) fallback solution when only 1 name can be found / second group
  #    can also be further decomposed if needed
  fallback_1name = "^([a-zA-Z._][\\w.]*)[$[]*(.*?)$",


  # 4) Like re #1 but doesn't match "df$name"
  two_names_no_doll = paste0("^([\\w.]+)(\\[{1,2})(.*\\,\\s*)?['\"]?",
                              "([a-zA-Z._][\\w._]+)['\"]?\\]{0,2}(\\[.+)?$"),

  # 5) Negative indexing
  neg_num_index = paste0("^([\\w.]+)(\\$|\\[{1,2})(.*\\,\\s*)?",
                          "(-\\d+)\\]{1,2}(\\[.+)?$")
)



# "Hideous hack" to avoid warning on check
utils::globalVariables(c("."))

# Declare global flag_by variable, (can be declared in check_args)
# utils::globalVariables(c("flag_by"))

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
                 "ctable.silent"          = FALSE,
                 "descr.stats"            = "all",
                 "descr.transpose"        = FALSE,
                 "descr.silent"           = FALSE,
                 "dfSummary.style"        = "multiline",
                 "dfSummary.varnumbers"   = TRUE,
                 "dfSummary.class"        = TRUE,
                 "dfSummary.labels.col"   = TRUE,
                 "dfSummary.graph.col"    = TRUE,
                 "dfSummary.valid.col"    = TRUE,
                 "dfSummary.na.col"       = TRUE,
                 "dfSummary.graph.magnif" = 1,
                 "dfSummary.silent"       = FALSE,
                 "dfSummary.custom.1"     = 
                   expression(
                     paste(
                       paste0(
                         trs("iqr"), " (", trs("cv"), ") : "
                       ),
                       format_number(
                         IQR(column_data, na.rm = TRUE), round.digits
                       ),
                       " (",
                       format_number(
                         sd(column_data, na.rm = TRUE) /
                           mean(column_data, na.rm = TRUE), round.digits
                       ),
                       ")",
                       collapse = "", sep = ""
                     )
                   ),
                 "dfSummary.custom.2"     = NA,
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
}

# Define a null coalescing operator if not available
if (getRversion() < "4.1.0") {
  if (!requireNamespace("backports", quietly = TRUE) || 
      !"%||%" %in% getNamespaceExports("backports")) {
    `%||%` <- function(x, y) {
      if (is.null(x)) y else x
    }
  } else {
    `%||%` <- backports::`%||%`
  }
}

if (getRversion() < "4.4.0") {
  if (!requireNamespace("backports", quietly = TRUE) || 
      !"%||%" %in% getNamespaceExports("backports")) {
    `%||%` <- function(x, y) {
      if (is.null(x)) y else x
    }
  } else {
    `%||%` <- backports::`%||%`
  }
}
