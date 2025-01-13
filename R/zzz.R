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
  
  st_version <- utils::packageDescription(pkgname)$Version
  st_opts <- getOption("summarytools")
  
  if (is.null(st_opts)) {
    st_options("reset")
    st_version <- st_options("version")
  } else {
    # If options exist, compare versions
    if (is.null(st_opts$version) || st_opts$version != st_version) {
      
      message("summarytools has been updated; see news(package = ",
              "'summarytools') to learn what has changed")
      message("summarytools options now persist across sessions; to disable ",
              "this feature, use st_options(persist = FALSE)")

    }
  }
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
