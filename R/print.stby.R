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
print.stby <- function(x, method = "pander", file = "", append = FALSE, 
                     report.title = NA, table.classes = NA, 
                     bootstrap.css = st_options("bootstrap.css"), 
                     custom.css = st_options("custom.css"),
                     silent = FALSE, footnote = st_options("footnote"), 
                     escape.pipe = st_options("escape.pipe"), ...) {
    view(x, method = method, file = file, append = append, 
         report.title = report.title, table.classes = table.classes, 
         bootstrap.css = bootstrap.css, custom.css = custom.css,
         silent = silent, footnote = footnote, escape.pipe = escape.pipe,
         ...)
}