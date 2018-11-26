# Initialize vector containing paths to temporary html files generated when viewing
# in browser or in RStudio visualization pane. Will be updated whenever
# print.summarytools() / cleartmp() are called.
.st_env <- new.env(parent = emptyenv())
.st_env$tmpfiles <- c()

# Initialize list used by view() when printing an object of class "by"
.st_env$byInfo <- list()

#' Displaying and setting summarytools global options
#'
#' To list all \code{summarytools} global options, run this function without any arguments. 
#' To display the value of one or several options, use the first parameter only.
#'
#' @param option option(s) name(s) to query (optional). When provided alone, can be a
#' character vector to display several option values at once.
#' @param value value to assign (optional).
#' @param style Character. One of \dQuote{simple} (default), \dQuote{rmarkdown}, or \dQuote{grid}. 
#' Does not apply to \code{\link{dfSummary}}.
#' @param plain.ascii Logical. \code{TRUE} by default. Set to \code{FALSE} when using
#' summarytools with a rendering tool such as \code{knitr} or when creating rmarkdown output
#' files to be converted with Pandoc (although note that its value will automatically be set
#' to \code{FALSE} whenever \code{style} = \dQuote{rmarkdown}).
#' @param round.digits Numeric. Defaults to \code{2}.
#' @param omit.headings Logical. Set to \code{TRUE} to remove all headings from outputs
#' (only the tables will be printed out). \code{FALSE} by default.
#' @param footnote Character. When the default value \dQuote{default} is used, the 
#' package name, version, and R version are displayed below html outputs. Set no \code{NA} to omit
#' the footnote, or provide a string to personalize it.
#' @param display.labels  Logical. \code{TRUE} by default. Set to \code{FALSE} to omit data 
#' frame and variable labels in the headings section.
#' @param bootstrap.css Logical.  Include Bootstrap CSS in html outputs. Defaults to 
#' \code{TRUE}. Set to \code{FALSE} When using the \dQuote{render} method inside a 
#' \code{shiny} app.
#' @param custom.css Character. Path to an additional, user-provided, CSS file. \code{NA} 
#' by default.
#' @param escape.pipe Logical. Set to \code{TRUE} if Pandoc conversion
#' is your goal and you have unsatisfying results with grid or multiline tables.
#' \code{FALSE} by default.
#' @param freq.totals Logical. Corresponds to the \code{totals} parameter of \code{\link{freq}}.
#' \code{TRUE} by default.         
#' @param freq.report.nas Logical. Corresponds to the \code{display.nas} parameter of 
#' \code{freq()}. \code{TRUE} by default.
#' @param ctable.prop Character. Corresponds to the \code{prop} parameter of 
#' \code{\link{ctable}}. Defaults to \dQuote{r} (\emph{r}ow).
#' @param ctable.totals Logical. Corresponds to the \code{totals} parameter of 
#' \code{\link{ctable}}. \code{TRUE} by default.
#' @param descr.stats Character. Corresponds to the \code{stats} parameter of 
#' \code{\link{descr}}. Defaults to \dQuote{all}.
#' @param descr.transpose Logical. Corresponds to the \code{transpose} parameter of 
#' \code{\link{descr}}. \code{FALSE} by default.
#' @param dfSummary.varnumbers Logical. In \code{\link{dfSummary}}, should the first column 
#' contain variable number? Defaults to \code{TRUE}.
#' @param dfSummary.valid.col Logical. In \code{\link{dfSummary}}, include column indicating
#' count and proportion of valid (non-missing). \code{TRUE} by default.
#' @param dfSummary.na.col Logical. In \code{\link{dfSummary}}, include column indicating count
#' and proportion of missing (NA) values. \code{TRUE} by default.
#' @param dfSummary.graph.col Logical. Display barplots / histograms column in 
#' \code{\link{dfSummary}} \emph{html} reports. \code{TRUE} by default.
#' @param dfSummary.graph.magnif Numeric. Magnification factor, useful if 
#' \code{\link{dfSummary}} graphs show up too large (then use a value between 0 and 1) or 
#' too small (use a value > 1). Must be positive. Default to \code{1}.
#' 
#' @author
#' Dominic Comtois, \email{dominic.comtois@@gmail.com},
#' @note Loosely based on Gergely Daróczi's \code{\link[pander]{panderOptions}} function.
#' 
#' @examples \dontrun{
#' st_options()                   # show all summarytools global options
#' st_options('round.digits')     # show a specific global option 
#' st_options(round.digits = 1)   # set an option
#' st_options('round.digits', 1)  # set an option (legacy way)
#' st_options('reset')            # reset all summarytools global options
#' }
#' @export
st_options <- function(option = NULL, value = NULL, style = 'simple', round.digits = 2,
                       plain.ascii = TRUE, omit.headings = FALSE,
                       footnote = 'default', display.labels = TRUE,
                       bootstrap.css = TRUE, custom.css = NA,
                       escape.pipe = FALSE, freq.totals = TRUE,
                       freq.report.nas = TRUE, ctable.prop = 'r',
                       ctable.totals = TRUE, descr.stats = 'all',
                       descr.transpose = FALSE, dfSummary.varnumbers = TRUE,
                       dfSummary.valid.col = TRUE, dfSummary.na.col = TRUE,
                       dfSummary.graph.col = TRUE, dfSummary.graph.magnif = 1) {
  
  allOpts <- getOption('summarytools')
  
  mc <- match.call()
  
  # Querying all
  if (is.null(names(mc))) {
    return(allOpts)
  }
  
  # Querying one or several
  if (length(mc) == 2 && "option" %in% names(mc) && option != "reset") {
    for (o in option) {
      if (!o %in% names(allOpts)) {
        message('Available options:', paste(names(allOpts), collapse = ", "))
        stop('Option ', o, 'not recognized / not available')
      }
    }
    if (length(option) == 1) {
      return(allOpts[[option]])
    } else {
      return(allOpts[option])
    }
  }

  if (isTRUE(option == 'reset')) {
    if (length(mc) > 2)
      stop('Cannot reset options and set them at the same time')
    
    options('summarytools' = 
              list('style'                  = 'simple',
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
                   'dfSummary.graph.magnif' = 1))
    
    message('summarytools options have been reset')
    return(invisible())
  }

  # Legacy way of setting of options
  if (length(names(mc)) == 3 && all.equal(sort(names(mc)), c("", "option", "value"))) {
    if (length(option) > 1)
      stop("Cannot set more than one option at a time in the legacy way.",
           "Use separate arguments for each option instead")
    if (!option %in% names(allOpts)) {
      message('Available options:', paste(names(allOpts), collapse = ", "))
      stop('Option ', o, 'not recognized / not available')
    }
    allOpts[[option]] <- value
    options('summarytools' = allOpts)
    return(invisible())
  }
  
  # Regular way of setting options    
  for (o in setdiff(names(mc), c("", "option", "value"))) {
    allOpts[[o]] <- get(o)
  }
  options('summarytools' = allOpts)
  return(invisible())
}

# summarytools global options
.onLoad <- function(libname, pkgname) {
  options('summarytools' = 
            list('style'                  = 'simple',
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
                 'dfSummary.graph.magnif' = 1))
          
}

.onAttach <- function(libname, pkgname) {
  pander::panderOptions("knitr.auto.asis", FALSE)
}
