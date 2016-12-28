# view is a wrapper function for print(x, "view"). Allows alternate "browser" or "pander" methods as well.
view <- function(x, method = "viewer", silent = FALSE, report.title = NA, html.header = TRUE, 
                 html.footer = TRUE, footer.note = TRUE, file = "", append = FALSE, ...) {
  
  if ("summarytools" %in% class(x))
    print.summarytools(x, method = method, silent = silent, report.title = report.title, 
                       html.header = html.header, html.footer = html.footer, 
                       footer.note = footer.note, file = file, append = append, ...)
  
  else if ("by" %in% class(x)) {
    len <- length(x)
    if (method %in% c("viewer", "browser")) {
      file <- ifelse(file == "", paste0(tempfile(),".html"), file)
      print.summarytools(x[[1]], method = method, silent = silent, html.header = TRUE, 
                         html.footer = FALSE, footer.note = FALSE, 
                         report.title = report.title, file = file, append = FALSE)
      for (i in 2:len) {
        print.summarytools(x[[i]], method = method, silent = silent, html.header = FALSE, 
                           html.footer = ifelse(i == len, TRUE, FALSE), 
                           footer.note = ifelse(i == len, footer.note, FALSE),
                           section.title = FALSE, group.only = TRUE,
                           file = file, append = TRUE)
      }
      
    } else if (method == "pander") {
      if (file != "") {
        print.summarytools(x[[1]], method = "pander", silent = silent, 
                           report.title = report.title,
                           file = file, append = FALSE)
        for (i in 2:len) {
          print.summarytools(x[[i]], method = "pander", silent = silent,
                             section.title = FALSE, group.only = TRUE,
                             file = file, append = TRUE)
        }
      } else {
        print.summarytools(x[[1]], method = "pander", silent = silent, 
                           report.title = report.title,
                           file = file, append = FALSE)
        for (i in 2:len) {
          print.summarytools(x[[i]], method = "pander", silent = silent,
                             section.title = FALSE, group.only = TRUE)
        }
      }
    }
  }
}
