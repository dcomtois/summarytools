#' @export
view <- function(x, method = "viewer", file = "", append = FALSE, report.title = NA,
                 group.only = FALSE, escape.pipe = FALSE, html.table.class = NA,
                 custom.css = NA, silent = FALSE, footnote = "default", ...) {

  if ("summarytools" %in% class(x))
    print.summarytools(x, method = method, silent = silent, footnote = footnote,
                       file = file, append = append, report.title = report.title, ...)

  else if ("by" %in% class(x) && attr(x[[1]], "st_type") %in% c("freq", "descr")) {
    len <- length(x)
    if (method %in% c("viewer", "browser")) {
      file <- ifelse(file == "", paste0(tempfile(),".html"), file)
      print.summarytools(x[[1]], method = method, silent = FALSE,
                         footnote = FALSE, file = file, append = FALSE,
                         report.title = report.title, group.only = FALSE, ...)
      for (i in 2:len) {
        print.summarytools(x[[i]], method = method, silent = silent,
                           footnote = ifelse(i == len, footnote, FALSE),
                           file = file, append = TRUE, group.only = TRUE,
                           open = TRUE, ...)
      }
    }

    else if (method == "pander") {
      print.summarytools(x[[1]], method = "pander", silent = silent,
                         report.title = report.title,
                         file = file, append = FALSE, group.only = FALSE,
                         escape.pipe = escape.pipe, ...)
      for (i in 2:len) {
        print.summarytools(x[[i]], method = "pander", silent = silent,
                           file = file, append = ifelse(file == "", FALSE, TRUE),
                           report.title = NA, group.only = TRUE,
                           escape.pipe = escape.pipe, ...)
      }
    }
  }

  else if ("list" %in% class(x) && attr(x[[1]], "st_type") == "freq") {
    len <- length(x)
    if (method %in% c("viewer", "browser")) {
      file <- ifelse(file == "", paste0(tempfile(),".html"), file)
      print.summarytools(x[[1]], method = method, silent = FALSE,
                         footnote = FALSE, file = file, append = FALSE,
                         report.title = report.title, #group.only = FALSE,
                         ...)
      for (i in 2:(len-1)) {
        print.summarytools(x[[i]], method = method, silent = silent,
                           footnote = ifelse(i == len, footnote, FALSE),
                           file = file, append = TRUE, #group.only = TRUE,
                           open = TRUE, ...)
      }
    }

    else if (method == "pander") {
      print.summarytools(x[[1]], method = "pander", silent = silent,
                         report.title = report.title,
                         file = file, append = FALSE, group.only = FALSE,
                         escape.pipe = escape.pipe, ...)
      for (i in 2:len) {
        print.summarytools(x[[i]], method = "pander", silent = silent,
                           file = file, append = ifelse(file == "", FALSE, TRUE),
                           report.title = NA, group.only = TRUE,
                           escape.pipe = escape.pipe, ...)
      }
    }
  }

  else message(paste("x must either be a summarytools object created with freq(), descr(),",
                     "or a list of freq() / descr() objects created using by(),",
                     "or a list of freq() objects created using lapply().",
                     "Support for by() used with ctable() may be available in future realeases."))
}
