#' Print Method for Objects of Class \dQuote{list}
#'
#' Displays a list comprised of summarytools objects created with
#'   \code{\link{lapply}}. 
#'
#' @usage
#'  \method{print}{list}(x, method = "pander", file = "", 
#'   append = FALSE, report.title = NA, table.classes = NA, 
#'   bootstrap.css = st_options('bootstrap.css'), 
#'   custom.css = st_options('custom.css'), silent = FALSE, 
#'   footnote = st_options('footnote'), collapse = 0,
#'   escape.pipe = st_options('escape.pipe'), \dots)
#' 
#' @details This function is there only for cases where the object to be printed
#'   was created with \code{\link{lapply}}, as opposed to the recommended
#'   functions for creating grouped results (\code{\link{stby}} and
#'   \code{\link[dplyr]{group_by}}).
#' 
#' @inheritParams print.summarytools
#' @method print list
#' @export
print.list <- function(x,
                       method        = "pander",
                       file          = "",
                       append        = FALSE, 
                       report.title  = NA,
                       table.classes = NA, 
                       bootstrap.css = st_options("bootstrap.css"), 
                       custom.css    = st_options("custom.css"),
                       silent        = FALSE,
                       footnote      = st_options("footnote"), 
                       collapse      = 0,
                       escape.pipe   = st_options("escape.pipe"),
                       ...) {

  if (inherits(x[[1]], "summarytools")) {
    
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
         collapse      = collapse,
         escape.pipe   = escape.pipe,
         ...)
    
  } else {
    base::print.default(x, ...)
  }
}
