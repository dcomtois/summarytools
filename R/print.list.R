#' Print Method for Objects of Class \dQuote{list}
#'
#' Displays a list comprised of summarytools objects created with \code{lapply}. 
#'
#' @usage
#'  \method{print}{list}(x, method = "pander", file = "", 
#'   append = FALSE, report.title = NA, table.classes = NA, 
#'   bootstrap.css = st_options('bootstrap.css'), 
#'   custom.css = st_options('custom.css'), silent = FALSE, 
#'   footnote = st_options('footnote'), 
#'   escape.pipe = st_options('escape.pipe'), \dots)
#' 
#' @inheritParams print.summarytools
#' @method print list
#' @export
print.list <- function(x,
                       method = "pander",
                       file = "",
                       append = FALSE, 
                       report.title = NA,
                       table.classes = NA, 
                       bootstrap.css = st_options("bootstrap.css"), 
                       custom.css = st_options("custom.css"),
                       silent = FALSE, footnote = st_options("footnote"), 
                       escape.pipe = st_options("escape.pipe"),
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
         escape.pipe   = escape.pipe,
         ...)
    
  } else {
    
    base::print.default(x, ...)
  
  }
}
