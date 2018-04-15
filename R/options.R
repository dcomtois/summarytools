# Initialize vector containing paths to temporary html files generated when viewing
# in browser or in RStudio visualization pane. Will be updated whenever
# print.summarytools() / cleartmp() are called.
.st_env <- new.env(parent = emptyenv())
.st_env$tmpfiles <- c()

# Initialize list used by view() when printing an object of class "by"
.st_env$byInfo <- list()

#' Displaying and setting summarytools global options
#'
#' To list all \code{summarytools} global options, run this function without any parameters. 
#' To display the value of an option, use the first parameter only. To modify it, add the new
#' value as a second parameter.
#'
#' The following options are available:
#'
#' \itemize{
#'   \item \code{style}  Character. One of \dQuote{simple} (default), \dQuote{rmarkdown}, or
#'     \dQuote{grid}.
#'   \item \code{plain.ascii}  Logical. \code{TRUE} by default. Set to \code{FALSE} when using
#'     summarytools with a rendering tool such as \code{knitr} or when creating rmarkdown output
#'     files to be converted with Pandoc (although note that its value will automatically be set to
#'     \code{FALSE} whenever \code{style} = \dQuote{rmarkdown}).
#'   \item \code{round.digits}  Numeric. Defaults to \code{2}.
#'   \item \code{omit.headings}  Logical. Set to \code{TRUE} to remove all headings from outputs
#'     (only the tables will be printed out). \code{FALSE} by default.
#'   \item \code{footnote}  Character. When the default value \dQuote{default} is used, the 
#'     package name, version, and R version are displayed below html outputs. Set no \code{NA} to omit
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
#'     \code{\link{ctable}}. Defaults to \dQuote{r} (\emph{r}ow).
#'   \item \code{descr.stats}  Character. Corresponds to the \code{stats} parameter of 
#'     \code{\link{descr}}. Defaults to \dQuote{all}.
#'   \item \code{descr.transpose}  Logical. Corresponds to the \code{transpose} parameter of 
#'     \code{\link{descr}}. \code{FALSE} by default.
#'   \item \code{bootstrap.css}  Logical.  Include Bootstrap CSS in html outputs. Defaults to 
#'     \code{TRUE}. Set to \code{FALSE} When using the \dQuote{render} method inside a 
#'     \code{shiny} app.  
#'   \item \code{custom.css}  Character. Path to an additional, user-provided, CSS file. \code{NA} 
#'     by default.
#'   \item \code{escape.pipe}  Logical. Set to \code{TRUE} if Pandoc conversion
#'     is your goal and you have unsatisfying results with grid or multiline tables.
#'     \code{FALSE} by default.
#'     
#' }
#' @param option option name (string).
#' @param value value to assign (optional)
#' @author
#' Dominic Comtois, \email{dominic.comtois@@gmail.com},
#' @note Loosely based on Gergely Daróczi's \code{\link[pander]{panderOptions}} function.
#' 
#' @examples \dontrun{
#' st_options()                   # show all summarytools global options
#' st_options('round.digits')     # show a specific global option 
#' st_options('round.digits', 1)  # set an option
#' st_options('reset')            # reset all summarytools global options
#' }
#' @export
st_options <- function(option, value) {
  
  allOpts <- getOption('summarytools')
  
  # Querying
  if (missing(value)) {
    
    if (missing(option)) {
      return(allOpts)
    } else if (option == 'reset') {
        
        options('summarytools' = list(
          'style'              = 'simple',
          'round.digits'       = 2,
          'plain.ascii'        = TRUE,
          'omit.headings'      = FALSE,
          'footnote'           = 'default',
          'display.labels'     = TRUE,
          'bootstrap.css'      = TRUE,
          'custom.css'         = NA,
          'escape.pipe'        = FALSE,
          'freq.totals'        = TRUE,
          'freq.report.nas'    = TRUE,
          'ctable.prop'        = 'r',
          'ctable.totals'      = TRUE,
          'descr.stats'        = 'all',
          'descr.transpose'    = FALSE,
          'dfSummary.varnumbers'   = TRUE,
          'dfSummary.valid.col'    = TRUE,
          'dfSummary.na.col'       = TRUE,
          'dfSummary.graph.col'    = TRUE,
          'dfSummary.graph.magnif' = 1
        ))
        
        message('summarytools options have been reset')
        return(invisible())
        
    } else if (option %in% names(allOpts)) {
      return(allOpts[[option]])
    } 
    
    message('Available options:', paste(names(allOpts), collapse = ", "))
    stop('Option not recognized / not available')
    
  }  else {
    
    if (!option %in% names(allOpts)) {
      message('Available options: ', paste(names(allOpts), collapse = ", "))
      stop('Invalid option name: ', option)
    }
    
    allOpts[[option]] <- value
    
    options('summarytools' = allOpts)
    
  }
}

# summarytools global options
.onLoad <- function(libname, pkgname) {
  options('summarytools' = list(
    'style'              = 'simple',
    'round.digits'       = 2,
    'plain.ascii'        = TRUE,
    'omit.headings'      = FALSE,
    'footnote'           = 'default',
    'display.labels'     = TRUE,
    'bootstrap.css'      = TRUE,
    'custom.css'         = NA,
    'escape.pipe'        = FALSE,
    'freq.totals'        = TRUE,
    'freq.report.nas'    = TRUE,
    'ctable.prop'        = 'r',
    'ctable.totals'      = TRUE,
    'descr.stats'        = 'all',
    'descr.transpose'    = FALSE,
    'dfSummary.varnumbers'   = TRUE,
    'dfSummary.valid.col'    = TRUE,
    'dfSummary.na.col'       = TRUE,
    'dfSummary.graph.col'    = TRUE,
    'dfSummary.graph.magnif' = 1
  ))
}


.onAttach <- function(libname, pkgname) {
  pander::panderOptions("knitr.auto.asis", FALSE)
}
