#' @export
view <- function(x, method = "viewer", file = "", append = FALSE, report.title = NA,
                 table.classes = NA, bootstrap.css = st_options('bootstrap.css'), 
                 custom.css = st_options('custom.css'), silent = FALSE, 
                 footnote = st_options('footnote'), 
                 escape.pipe = st_options('escape.pipe'),
                 ...) {

  # Objects not created via by() or lapply() ----------------------------------
  if ("summarytools" %in% class(x)) {
    print.summarytools(x,
                       method = method,
                       file = file,
                       append = append,
                       report.title = report.title,
                       escape.pipe = escape.pipe,
                       table.classes = table.classes,
                       bootstrap.css = bootstrap.css,
                       custom.css = custom.css,
                       silent = silent,
                       footnote = footnote,
                       ...)

  } else if ("by" %in% class(x) &&
             attr(x[[1]], "st_type") %in% c("freq", "descr")) {

    # Objects created via by() ------------------------------------------------
    
    if (method %in% c("viewer", "browser")) {
      
      file <- ifelse(file == "", paste0(tempfile(),".html"), file)
      
      for (i in seq_along(x)) {
        
        if (i == 1) {
          if (isTRUE(append) && !is.na(custom.css)) {
            stop("Can't append existing html file with new custom.css")
          }
          if (isTRUE(append) && !is.na(report.title)) {
            stop("Can't append existing html file with new report.title")
          }

          print.summarytools(x[[i]],
                             method = method,
                             file = file,
                             append = append,
                             report.title = report.title,
                             escape.pipe = escape.pipe,
                             table.classes = table.classes,
                             bootstrap.css = bootstrap.css,
                             custom.css = custom.css,
                             silent = silent,
                             footnote = NA,
                             ...)

        } else if (i < length(x)) {
          print.summarytools(x[[i]],
                             method = method,
                             file = file,
                             append = TRUE,
                             escape.pipe = escape.pipe,
                             table.classes = table.classes,
                             silent = silent,
                             footnote = NA,
                             group.only = TRUE,
                             ...)
        } else {
          print.summarytools(x[[i]],
                             method = method,
                             file = file,
                             append = TRUE,
                             escape.pipe = escape.pipe,
                             table.classes = table.classes,
                             silent = silent,
                             footnote = footnote,
                             group.only = TRUE,
                             open.doc = TRUE,
                             ...)
        }
      }
      
    } else if (method == "render") {
      
      for (i in seq_along(x)) {

        if (i == 1) {
          html_content <- list(print.summarytools(x[[i]],
                                                  method = method,
                                                  table.classes = table.classes,
                                                  bootstrap.css = bootstrap.css,
                                                  custom.css = custom.css,
                                                  silent = silent,
                                                  footnote = NA,
                                                  ...))
          
        } else if (i < length(x)) {
          html_content[[i]] <- print.summarytools(x[[i]],
                                                  method = method,
                                                  table.classes = table.classes,
                                                  silent = silent,
                                                  footnote = NA,
                                                  group.only = TRUE,
                                                  ...)
          
          
        } else {
          html_content[[i]] <- print.summarytools(x[[i]],
                                                  method = method,
                                                  table.classes = table.classes,
                                                  silent = silent,
                                                  footnote = footnote,
                                                  group.only = TRUE,
                                                  ...)
          
        }
      }
      
      return(tagList(html_content))
      
    } else if (method == "pander") {
      
      for (i in seq_along(x)) {
        if (i == 1) {
          print.summarytools(x[[1]],
                             method = "pander",
                             silent = silent,
                             file = file,
                             append = append,
                             group.only = FALSE,
                             escape.pipe = escape.pipe,
                             ...)
        } else {
          print.summarytools(x[[i]],
                             method = "pander",
                             silent = silent,
                             file = file,
                             append = ifelse(file == "", FALSE, TRUE),
                             group.only = TRUE,
                             escape.pipe = escape.pipe,
                             ...)
        }
      }
    }
    
  } else if ("list" %in% class(x) && attr(x[[1]], "st_type") == "freq") {

    # Objects created via lapply() --------------------------------------------
    
    if (method %in% c("viewer", "browser")) {
      file <- ifelse(file == "", paste0(tempfile(),".html"), file)

      for (i in seq_along(x)) {
        if (i == 1) {
          print.summarytools(x[[i]],
                             method = method,
                             file = file,
                             silent = FALSE,
                             footnote = NA,
                             append = FALSE,
                             report.title = report.title,
                             ...)
        } else if (i < length(x)) {
          print.summarytools(x[[i]],
                             method = method,
                             file = file,
                             append = TRUE,
                             var.only = TRUE,
                             silent = TRUE,
                             footnote = NA,
                             ...)
        } else {
          print.summarytools(x[[i]],
                             method = method,
                             file = file,
                             append = TRUE,
                             var.only = TRUE,
                             silent = FALSE,
                             footnote = footnote,
                             open.doc = TRUE,
                             ...)
        }
      }
    } else if (method == "pander") {
      for (i in seq_along(x)) {
        if (i == 1) {
          print.summarytools(x[[1]],
                             method = "pander",
                             file = file,
                             silent = silent,
                             append = append,
                             escape.pipe = escape.pipe,
                             ...)
        } else {
          print.summarytools(x[[i]],
                             method = "pander",
                             file = file,
                             silent = silent,
                             append = ifelse(file == "", FALSE, TRUE),
                             var.only = TRUE,
                             escape.pipe = escape.pipe,
                             ...)
        }
      }
    }
  } else {
    message(paste("x must either be a summarytools object created with freq(), descr(),",
                  "or a list of freq() / descr() objects created using by(),",
                  "or a list of freq() objects created using lapply().",
                  "Support for by() used with ctable() may be available in future realeases."))
  }
}
