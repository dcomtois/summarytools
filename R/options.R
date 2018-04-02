# Initialize vector containing paths to temporary html files generated when viewing
# in browser or in RStudio visualization pane. Will be updated whenever
# print.summarytools() / cleartmp() are called.
.st_env <- new.env(parent = emptyenv())
.st_env$tmpfiles <- c()

# Initialize list used by view() when printing an object of class "by"
.st_env$byInfo <- list()

# summarytools global options
.onLoad <- function(libname, pkgname) {
  options('summarytools' = list(
    'round.digits'     = 2,
    'plain.ascii'      = TRUE,
    'omit.headings'    = FALSE,
    'footnote'         = 'default',
    'display.labels'   = TRUE,
    'freq.totals'      = TRUE,
    'freq.display.nas' = TRUE,
    'ctable.totals'    = TRUE,
    'ctable.prop'      = 'r',
    'descr.stats'      = 'all',
    'descr.transpose'  = FALSE,
    'bootstrap.css'    = TRUE,
    'custom.css'       = NA,
    'escape.pipe'      = FALSE
  ))
}

#' Displaying and setting summarytools global options
#'
#' To list all \code{summarytools} global options, run this function without any parameters. 
#' To display the value of an option, use the first parameter only. To modify it, add the new
#' value as a second parameter.
#'
#' The following options are available:
#'
#' \itemize{
#'   \item \code{round.digits}  Numeric. Default: \code{2}.
#'   \item \code{plain.ascii}  Logical. \code{TRUE} by default. Set to \code{FALSE} when using
#'     summarytools with a rendering tool such as \code{knitr} or when creating output files
#'     to be converted with Pandoc.
#'   \item \code{omit.headings}  Logical. \code{FALSE} by default. Set to \code{TRUE} to remove all
#'     headings from the outputs (only the tables will be printed out).
#'   \item \code{footnote}  Character. When the default value 'default' is used, the package name,
#'     version, and R + version are displayed below html outputs. Set no \code{NA} to omit
#'     the footnote, or provide a string to personalize it.
#'   \item \code{display.labels}  Logical. \code{TRUE} by default. Set to \code{FALSE} to omit data 
#'     frame and variable labels in the headings section.
#'   \item \code{freq.totals}  Logical. Corresponds to the \code{totals} parameter of \code{\link{freq}}.
#'     \code{TRUE} by default.
#'   \item \code{freq.display.nas}  Logical. Corresponds to the \code{display.nas} parameter of 
#'     \code{freq()}. \code{TRUE} by default.
#'   \item \code{ctable.totals}  Logical. Corresponds to the \code{totals} parameter of 
#'     \code{\link{ctable}}. \code{TRUE} by default.
#'   \item \code{ctable.prop}  Character. Corresponds to the \code{prop} parameter of 
#'     \code{\link{ctable}}. Defaults to \dQuote{r} (row).
#'   \item \code{descr.stats}  Character. Corresponds to the \code{stats} parameter of 
#'     \code{\link{descr}}. Defaults to \dQuote{all}.
#'   \item \code{descr.transpose}  Logical. Corresponds to the \code{transpose} parameter of 
#'     \code{\link{descr}}. \code{FALSE} by default.
#'   \item \code{bootstrap.css}  Logical.  Include Bootstrap CSS in html outputs. Defaults to 
#'     \code{TRUE}. When using the \dQuote{render} method, it may be a good idea is to set this
#'     to \code{FALSE}.
#'   \item \code{custom.css}  Character. Path to an additional, user-provided CSS file. \code{NA} 
#'     by default.
#'   \item \code{escape.pipe}  Logical. \code{FALSE} by default. Set to \code{TRUE} if Pandoc conversion
#'     is your goal and you have unsatisfying results with grid or multiline tables.
#'     
#' }
#' @param option option name (string).
#' @param value value to assign (optional)
#' @examples \dontrun{
#' st_options()
#' st_options('round.digits')
#' st_options('round.digits', 1)
#' }
#' @export
st_options <- function(option, value) {
  
  allOpts <- getOption('summarytools')
  
  # Querying
  if (missing(value)) {
    
    if (missing(option)) {
      return(allOpts)
    }
    
    if (option %in% names(allOpts)) {
      return(allOpts[[option]])
    } 
    
    message('Available options:', paste(names(allOpts), collapse = ", "))
    stop('Option not recognized / not available')
    
  } else {
    
    if (!option %in% names(allOpts)) {
      message('Available options: ', paste(names(allOpts), collapse = ", "))
      stop('Invalid option name: ', option)
    }
    
    ## fix assigning NULL to a list element
    if (is.null(value)) {
      allOpts[option] <- list(NULL)
    } else {
      allOpts[[option]] <- value
    }
    
    options('summarytools' = allOpts)
    
  }
}

.onAttach <- function(libname, pkgname) {
  pander::panderOptions("knitr.auto.asis", FALSE)
}
