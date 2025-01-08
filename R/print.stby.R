#' Print Method for Objects of Class \dQuote{stby}
#' 
#' Displays a list comprised of summarytools objects created with \code{stby}. 
#' 
#' @usage
#'  \method{print}{stby}(x, method = "pander", file = "", 
#'   append = FALSE, report.title = NA, table.classes = NA, 
#'   bootstrap.css = st_options('bootstrap.css'), 
#'   custom.css = st_options('custom.css'), silent = FALSE, 
#'   footnote = st_options('footnote'), 
#'   escape.pipe = st_options('escape.pipe'), \dots)
#' 
#' @inheritParams print.summarytools
#' @method print stby
#' @export
print.stby <- function(x, 
                       method        = "pander", 
                       file          = "", 
                       append        = FALSE, 
                       report.title  = NA, 
                       table.classes = NA, 
                       bootstrap.css = st_options("bootstrap.css"), 
                       custom.css    = st_options("custom.css"),
                       silent        = FALSE,
                       footnote      = st_options("footnote"), 
                       escape.pipe   = st_options("escape.pipe"),
                       ...) {
  
  # Special case of nested lists (by() / stby() used with freq(), 2+ vars)
  if (!"silent" %in% names(match.call())) {
    if (!"st_type" %in% names(attributes(x[[1]])) && is.list(x[[1]]))
      silent <- st_options(paste0(attr(x[[1]][[1]], "st_type"), ".silent"))
    else
      silent <- st_options(paste0(attr(x[[1]], "st_type"), ".silent"))
  }
  
  view(x,
       method        = method,
       file          = file,
       append        = append,
       report.title  = report.title,
       table.classes = table.classes,
       bootstrap.css = bootstrap.css,
       custom.css    = custom.css,
       silent        = silent,
       footnote      = footnote,
       escape.pipe   = escape.pipe,
       ...)
}
