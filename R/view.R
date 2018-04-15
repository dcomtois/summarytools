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
                       method        = method,
                       file          = file,
                       append        = append,
                       report.title  = report.title,
                       escape.pipe   = escape.pipe,
                       table.classes = table.classes,
                       bootstrap.css = bootstrap.css,
                       custom.css    = custom.css,
                       silent        = silent,
                       footnote      = footnote,
                       ...)

    
  } else if ("by" %in% class(x) &&
             attr(x[[1]], "st_type") == "descr" &&
             ((!attr(x[[1]], 'data_info')$transposed && dim(x[[1]])[2] == 1) || 
              ( attr(x[[1]], 'data_info')$transposed && dim(x[[1]])[1] == 1))) {
    
    # Special case: descr by() objects with 1 variable ------------------------------
    
    byvar <- sub("^.*\\$(.+)", "\\1", names(attributes(x)$dimnames))
    
    if (attr(x[[1]], 'data_info')$transposed) {
      xx <- do.call(rbind, x)
    } else {
      # 1 Column several times - use cbind
      xx <- do.call(cbind, x)
      class(xx)     <- class(x[[1]])
      colnames(xx)  <- names(x)
    }
    
    attr(xx, 'st_type')   <- "descr"
    attr(xx, 'date')      <- attr(x[[1]], 'date')
    attr(xx, 'data_info') <- attr(x[[1]], 'data_info')
    
    attr(xx, 'data_info')$byvar    <- byvar
    attr(xx, 'data_info')$Group    <- NULL
    attr(xx, 'data_info')$by.first <- NULL
    attr(xx, 'data_info')$by.last  <- NULL
    attr(xx, 'data_info')$N.Obs    <- NULL
    
    attr(xx, "data_info") <- attr(xx, "data_info")[!is.na(attr(xx, "data_info"))]
    
    attr(xx, 'formatting') <- attr(x[[1]], 'formatting')
    attr(xx, 'user_fmt')   <- attr(x, 'user_fmt')
    
    print.summarytools(xx,
                       method        = method,
                       file          = file,
                       append        = append,
                       report.title  = report.title,
                       escape.pipe   = escape.pipe,
                       table.classes = table.classes,
                       bootstrap.css = bootstrap.css,
                       custom.css    = custom.css,
                       silent        = silent,
                       footnote      = footnote,
                       ...)
    
  } else if ("by" %in% class(x) &&
             attr(x[[1]], "st_type") %in% c("freq", "descr")) {

    if (method %in% c("viewer", "browser")) {

      # by object, viewer / browser -------------------------------------------------
            
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
                             method        = method,
                             file          = file,
                             append        = append,
                             report.title  = report.title,
                             escape.pipe   = escape.pipe,
                             table.classes = table.classes,
                             bootstrap.css = bootstrap.css,
                             custom.css    = custom.css,
                             silent        = silent,
                             footnote      = NA,
                             ...)

        } else if (i < length(x)) {
          print.summarytools(x[[i]],
                             method = method,
                             file = file,
                             append = TRUE,
                             escape.pipe = escape.pipe,
                             table.classes = table.classes,
                             silent = TRUE,
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
      
      # by object, render ----------------------------------------------------
      
      for (i in seq_along(x)) {

        if (i == 1) {
          html_content <- 
            list(print.summarytools(x[[i]],
                                    method = method,
                                    table.classes = table.classes,
                                    bootstrap.css = bootstrap.css,
                                    custom.css = custom.css,
                                    silent = silent,
                                    footnote = NA,
                                    ...))
          
        } else if (i < length(x)) {
          html_content[[i]] <- 
            print.summarytools(x[[i]],
                               method = method,
                               table.classes = table.classes,
                               silent = silent,
                               footnote = NA,
                               group.only = TRUE,
                               ...)
          
          
        } else {
          html_content[[i]] <- 
            print.summarytools(x[[i]],
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
      
      # by object, pander ----------------------------------------------------
      
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
    
  } else if ("list" %in% class(x) && "summarytools" %in% class(x[[1]]) &&
             attr(x[[1]], "st_type") == "freq") {

    if (method %in% c("viewer", "browser")) {
      
      # list (lapply) object, viewer / browser ------------------------------
      
      file <- ifelse(file == "", paste0(tempfile(),".html"), file)

      for (i in seq_along(x)) {
        if (i == 1) {
          print.summarytools(x[[i]],
                             method = method,
                             file = file,
                             silent = silent,
                             footnote = NA,
                             append = FALSE,
                             report.title = report.title,
                             escape.pipe = escape.pipe,
                             table.classes = table.classes,
                             bootstrap.css = bootstrap.css,
                             custom.css = custom.css,
                             ...)
          
        } else if (i < length(x)) {
          print.summarytools(x[[i]],
                             method = method,
                             file = file,
                             append = TRUE,
                             var.only = TRUE,
                             silent = TRUE,
                             footnote = NA,
                             escape.pipe = escape.pipe,
                             table.classes = table.classes,
                             ...)
        } else {
          print.summarytools(x[[i]],
                             method = method,
                             file = file,
                             append = TRUE,
                             var.only = TRUE,
                             silent = silent,
                             footnote = footnote,
                             escape.pipe = escape.pipe,
                             table.classes = table.classes,
                             open.doc = TRUE,
                             ...)
        }
      }
    } else if (method == "render") {
      
      # list (lapply) object, render ----------------------------------------
      
      for (i in seq_along(x)) {
        if (i == 1) {
          html_content <- 
            list(print.summarytools(x[[i]],
                                    method = method,
                                    silent = TRUE,
                                    footnote = NA,
                                    table.classes = table.classes,
                                    bootstrap.css = bootstrap.css,
                                    custom.css = custom.css,
                                    ...))
          
        } else if (i < length(x)) {
          html_content[[i]] <-
            print.summarytools(x[[i]],
                             method = method,
                             var.only = TRUE,
                             silent = TRUE,
                             footnote = NA,
                             table.classes = table.classes,
                             bootstrap.css = FALSE,
                             ...)
        } else {
          html_content[[i]] <- 
            print.summarytools(x[[i]],
                               method = method,
                               var.only = TRUE,
                               silent = silent,
                               footnote = footnote,
                               table.classes = table.classes,
                               bootstrap.css = FALSE,
                               ...)
        }
      }
      
      return(tagList(html_content))
      
    } else if (method == "pander") {
      
      # list (lapply) object, pander ------------------------------------------
      
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
