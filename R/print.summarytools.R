#' print.summarytools
#' 
#' Display \code{summarytools} objects in the console, in Web Browser or in
#'  \emph{RStudio}'s Viewer, or write content to file.
#'
#' @usage
#'  \method{print}{summarytools}(x, method = "pander", file = "",
#'    append = FALSE, report.title = NA, table.classes = NA,
#'    bootstrap.css = st_options('bootstrap.css'), 
#'    custom.css = st_options('custom.css'), silent = FALSE, 
#'    footnote = st_options('footnote'), max.tbl.height = Inf,
#'    escape.pipe = st_options('escape.pipe'), \dots)
#'
#' @param x A summarytools object that was generated with \code{\link{freq}},
#'   \code{\link{descr}}, \code{\link{ctable}} or \code{\link{dfSummary}}.
#' @param method One of \dQuote{pander}, \dQuote{viewer}, \dQuote{browser}, or
#'   \dQuote{render}. For \code{print()}, default is \dQuote{pander}; for
#'   \code{view()}, default is \dQuote{viewer}. If \dQuote{viewer} is used
#'   outside \emph{RStudio}, \dQuote{browser} will be used instead. Use
#'   \dQuote{render} if function is called from an Rmd document.
#' @param file File name to write output to. Defaults to \dQuote{}.
#' @param append Logical. When \code{file} argument is supplied, this indicates
#'   whether to append output to existing file. \code{FALSE} by default.
#' @param report.title For \emph{html} reports, this goes into the
#'   \code{<title>} tag. Defaults to \code{NA}, in which case \code{<title>}
#'   will be generic.
#' @param table.classes Character.  Additional classes to assign to output
#'   tables. All \emph{Bootstrap css} classes can be used. It also allows
#'   user-defined classes (see custom.css parameter). See \emph{details}
#'   section. \code{NA} by default.
#' @param bootstrap.css Logical. Set to \code{FALSE} to omit 
#'   \emph{Bootstrap css}. \code{TRUE} by default. To change this default value
#'   globally, see \code{\link{st_options}}.
#' @param custom.css Path to a user-defined \emph{.css} file. Classes defined in
#'   this file can be used in the \code{table.classes} parameter. \code{NA} by
#'   default. To change this default value globally, see
#'   \code{\link{st_options}}.
#' @param silent Hide console messages (such as ignored variables or \code{NaN}
#'   to \code{NA} transformations).
#' @param footnote footnote in \emph{html} output. When set to \dQuote{default},
#'   this is the package name and version, R version, and current date). Has no
#'   effect when \code{method} is \dQuote{pander}. Set to \dQuote{default},
#'   provide your own text, or set to \code{NA} to omit. To change this default
#'   value globally, see \code{\link{st_options}}.
#' @param max.tbl.height Maximum table height (in pixels) allowed in rendered
#'  \code{dfSummary()} tables. When this argument is used, results will show up
#'  in a \code{<div>} with the specified height and a scroll bar. Intended
#'  to be used in \emph{Rmd} documents. Has no effect when \code{method} is 
#'  \dQuote{pander}. \code{Inf} by default.
#' @param escape.pipe Logical. Set to \code{TRUE} when using \code{style='grid'}
#'   and \code{file} argument is supplied if the intent is to generate a text
#'   file that can be converted to other formats using \emph{Pandoc}. To change
#'   this default value globally, see \code{\link{st_options}}.
#' @param \dots Additional arguments can be used to override parameters stored
#'   as attributes in the object being printed. See \emph{Details} section.
#'
#' @return \code{NULL} when \code{method="pander"}; a file path (returned
#'   invisibly) when \code{method="viewer"} or \code{method="browser"}. In the
#'   latter case, the file path is also passed to \code{shell.exec} so the
#'   document is opened in default Web Browser.
#'
#' @details
#'   Plain ascii and \emph{rmarkdown} tables are generated via
#'   \code{\link[pander]{pander}}. See \emph{References} section
#'   for a list of all available \emph{pander} options.
#'
#' The following additional arguments can be used to override
#'   formatting attributes stored in the object to be printed. Refer to the 
#'   function's documentation for details on these arguments.
#'    \itemize{
#'      \item \code{style}
#'      \item \code{round.digits} (except for \code{\link{dfSummary}} objects)
#'      \item \code{plain.ascii}
#'      \item \code{justify}
#'      \item \code{headings}
#'      \item \code{display.labels}
#'      \item \code{varnumbers}    (\code{\link{dfSummary}} objects)
#'      \item \code{labels.col}    (\code{\link{dfSummary}} objects)
#'      \item \code{graph.col}     (\code{\link{dfSummary}} objects)
#'      \item \code{valid.col}     (\code{\link{dfSummary}} objects)
#'      \item \code{na.col}         (\code{\link{dfSummary}} objects)
#'      \item \code{col.widths}     (\code{\link{dfSummary}} objects)
#'      \item \code{split.tables}
#'      \item \code{report.nas}    (\code{\link{freq}} objects)
#'      \item \code{display.type}  (\code{\link{freq}} objects)
#'      \item \code{missing}       (\code{\link{freq}} objects)
#'      \item \code{totals}        (\code{\link{freq}} and \code{\link{ctable}}
#'      objects)
#'      \item \code{caption}       (\code{\link{freq}} and \code{\link{ctable}}
#'      objects)
#'    }
#'
#' The following additional arguments can be used to override
#'   heading elements to be printed:
#'    \itemize{
#'      \item \code{Data.frame}
#'      \item \code{Data.frame.label}
#'      \item \code{Variable}
#'      \item \code{Variable.label}
#'      \item \code{Group}
#'      \item \code{date}
#'      \item \code{Weights} (\code{\link{freq}} & \code{\link{descr}} objects)
#'      \item \code{Data.type} (\code{\link{freq}} objects)
#'      \item \code{Row.variable} (\code{\link{ctable}} objects)
#'      \item \code{Col.variable} (\code{\link{ctable}} objects)
#'    }
#'
#' @method print summarytools
#'
#' @references
#' \href{http://rstudio.com}{RStudio}
#' \href{https://github.com/dcomtois/summarytools}{Summarytools on GitHub}
#' \href{http://rapporter.github.io/pander/#general-options}{List of pander options on GitHub}
#' \href{http://getbootstrap.com/css/#tables}{Bootstrap Cascading Stylesheets}
#'
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#'
#' @seealso
#' \code{\link[pander]{pander}}
#'
#' @examples
#' \dontrun{
#' data(tobacco)
#' view(dfSummary(tobacco), footnote = NA)
#' }
#' data(exams)
#' print(freq(exams$gender), style = 'rmarkdown')
#' print(descr(exams), headings = FALSE)
#'
#' @keywords print methods
#' @import htmltools
#' @importFrom pander pander panderOptions
#' @importFrom utils capture.output packageVersion head
#' @importFrom checkmate test_logical test_path_for_output test_choice
#'             test_string check_file_exists
#' @export
print.summarytools <- function(x,
                               method         = "pander", 
                               file           = "", 
                               append         = FALSE,
                               report.title   = NA, 
                               table.classes  = NA, 
                               bootstrap.css  = st_options("bootstrap.css"),
                               custom.css     = st_options("custom.css"), 
                               silent         = FALSE, 
                               footnote       = st_options("footnote"),
                               max.tbl.height = Inf,
                               escape.pipe    = st_options("escape.pipe"), 
                               ...) {

  # object is a list, either created
  # - using lapply() 
  # - using freq with a dataframe as x
  if (is.list(x) && 
      !attr(x, "st_type") %in% c("ctable", "descr", "dfSummary")) {
    view(x, method = method, file = file, append = append,
         report.title = report.title, table.classes = table.classes, 
         bootstrap.css = bootstrap.css, custom.css = custom.css, 
         silent = silent, footnote = footnote, 
         escape.pipe = escape.pipe, ...)
    return(invisible())
  }
  
  knitr.auto.asis.value <- panderOptions("knitr.auto.asis")
  panderOptions("knitr.auto.asis", FALSE)
  
  on.exit(panderOptions("knitr.auto.asis", knitr.auto.asis.value))
  
  dotArgs <- list(...)
  mc <- match.call()
  
  # Recup arguments from view() if present -------------------------------------
  if ("open.doc" %in% names(dotArgs)) {
    open.doc <- eval(dotArgs[["open.doc"]])
  } else {
    open.doc <- FALSE
  }

  if ("group.only" %in% names(dotArgs)) {
    attr(x, "format_info")$group.only <- eval(dotArgs[["group.only"]])
  }

  if ("var.only" %in% names(dotArgs)) {
    attr(x, "format_info")$var.only <- eval(dotArgs[["var.only"]])
  }
  
  # Parameter validation -------------------------------------------------------
  errmsg <- character()
  
  method <- switch(tolower(substring(method, 1, 1)),
                   p = "pander",
                   b = "browser",
                   v = "viewer",
                   r = "render")

  if (attr(x, "lang") != st_options("lang")) {
    op <- st_options("lang")
    st_options(lang = attr(x, "lang"))
    on.exit(st_options(lang = op), add = TRUE)
  }

  if (!isTRUE(test_choice(method, 
                          c("pander", "browser", "viewer", "render")))) {
    errmsg %+=% paste("'method' must be one of 'pander', 'browser', 'viewer',",
                      "or 'render'")
  }

  if (!isTRUE(test_int(max.tbl.height, lower = 100, na.ok = FALSE)) &&
      !is.infinite(max.tbl.height)) {
    errmsg %+=% "'max.tbl.height' must be an integer between 100 and Inf"
  } else {
    attr(x, "format_info")$max.tbl.height <- max.tbl.height
  }
  
  if (file == "" && isTRUE(append)) {
    errmsg %+=% "'append' is set to TRUE but no file name has been specified"
  }

  if (file != "" && isTRUE(append) && !file.exists(file)) {
    errmsg %+=% "'append' is set to TRUE but specified file does not exist"
  }

  if (file != "" && isTRUE(append) && !is.na(report.title)) {
    errmsg %+=% "Appending existing file -- 'report.title' arg. will be ignored"
  }

  if (!isTRUE(test_string(report.title, na.ok = TRUE))) {
    errmsg %+=% "'report.title' must either be NA or a character string"
  }

  if (!isTRUE(test_logical(escape.pipe, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'escape.pipe' must be either TRUE or FALSE"
  }

  if (!is.na(custom.css) && 
      !isTRUE(check_file_exists(custom.css, access = "r"))) {
    errmsg %+=% "'custom.css' must point to an existing file."
  }

  if (!isTRUE(test_logical(silent, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'silent' must be either TRUE or FALSE"
  }

  if (file != "" && !isTRUE(test_path_for_output(file, overwrite = TRUE))) {
     errmsg %+=% "'file' path is not valid - check that directory exists"
  }
  
  # Change method to browser when file name was (most likely) provided by user
  if (grepl("\\.html$", file, ignore.case = TRUE, perl = TRUE) &&
      !grepl(pattern = tempdir(), x = file, fixed = TRUE) && 
      method == "pander") {
    method <- "browser"
    message("Switching method to 'browser'")
  }
  
  if (method == "pander" && !is.na(table.classes)) {
    errmsg %+=% "'table.classes' option does not apply to method 'pander'"
  }
  
  if (method == "pander" && !is.na(custom.css)) {
    errmsg %+=% "'custom.css' option does not apply to method 'pander'"
  }
  
  # Set plain.ascii to false and adjust style when file name ends with .md
  if (grepl("\\.md$", file, ignore.case = TRUE, perl = TRUE) 
      && !"style" %in% names(dotArgs)) {
    if (isTRUE(attr(x, "format_info")$plain.ascii) && 
        !"plain.ascii" %in% names(dotArgs)) {
      tmp_msg_flag <- TRUE
      dotArgs %+=% list(plain.ascii = FALSE)
    } else {
      tmp_msg_flag <- FALSE
    }

    newstyle = switch(attr(x, "st_type"),
                      freq      = "rmarkdown",
                      ctable    = "grid",
                      descr     = "rmarkdown",
                      dfSummary = "grid")
    
    if (attr(x, "format_info")$style %in% c("simple", "multiline")) {
      dotArgs %+=% list(style = newstyle)
      if (isTRUE(tmp_msg_flag)) {
        message("Setting 'plain.ascii' to FALSE and Changing style to '",
                newstyle, "' for improved markdown compatibility")
      } else {
        message("Changing style to '", newstyle, 
                "' for improved markdown compatibility")
      }
    } else if (isTRUE(tmp_msg_flag)) {
      message("Setting 'plain.ascii' to FALSE for improved markdown ",
              "compatibility")
    }
  }
  
  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "))
  }
  
  if (is.na(footnote)) {
    footnote <- ""
  }

  if (!"silent" %in% names(mc)) {
    if (attr(x, "st_type") == "descr") {
      silent <- st_options("descr.silent")
    } else if (attr(x, "st_type") == "dfSummary") {
      silent <- st_options("dfSummary.silent")
    }
  }
  
  # Display message if list object printed with base print() method with pander
  if (method == "pander" && 
      (identical(deparse(sys.calls()[[sys.nframe()-1]][2]), "x[[i]]()") ||
       any(grepl(pattern = "fn_call = FUN(x = X[[i]]", 
                 x = deparse(sys.calls()[[sys.nframe()-1]]), fixed = TRUE)))) {
    message("For best results printing list objects with summarytools, ",
            "use print(x); if by() was used, use stby() instead")
  }
  
  # Override x's attributes (format_info and heading info) ---------------------
  if ("date" %in% names(dotArgs)) {
    attr(x, "date") <- dotArgs[["date"]]
  }
  
  # Override of formatting elements, first by looking at '...' (var dotArgs),
  # then by looking at the match.call() from x to set global parameters that 
  # were not explicit in the latter.
  # Here we check for arguments that can be specified at the function level for
  # freq, descr, ctable and dfSummary (we don't include print/view args)
  overrided_args <- character()
  # Todo: remove "omit.headings" in next release
  for (format_element in c("style", "plain.ascii", "round.digits",
                           "justify", "cumul", "totals", "report.nas",
                           "missing", "headings", "display.labels",
                           "display.type", "varnumbers", "labels.col", 
                           "graph.col", "col.widths", "na.col", "valid.col", 
                           "split.tables", "omit.headings")) {
    if (format_element %in% names(dotArgs)) {
      if (format_element == "omit.headings") {
        message("'omit.headings' will disappear in future releases; ",
                "use 'headings' instead")
        attr(x, "format_info")[["headings"]] <- 
          !isTRUE(eval(dotArgs[["omit.headings"]]))
        overrided_args <- append(overrided_args, "headings")
      } else {
        attr(x, "format_info")[[format_element]] <- dotArgs[[format_element]]
        overrided_args <- append(overrided_args, format_element)
      }
    }
  }
  
  # Global options that apply to all types of summarytools objects
  # This is useful in a case where some global option was changed after the
  # object was created.
  for (format_element in c("style", "plain.ascii", "round.digits", 
                           "headings", "display.labels")) {
    if (!format_element %in% c(overrided_args, names(attr(x, "fn_call")))) {
      # Todo: following condition is to be removed in further releases
      if (!(format_element == "headings" &&
            "omit.headings" %in% names(attr(x, "fn_call")))) {
        if (!(format_element == "style" &&
              attr(x, "st_type") == "dfSummary") &&
            !(format_element == "round.digits" && 
              attr(x, "st_type") == "ctable")) {
          attr(x, "format_info")[[format_element]] <- st_options(format_element)
        }
      }
    }
  }
  
  # Global options specific to one type of summarytools object
  prefix <- paste0(attr(x, "st_type"), ".")
  for (format_element in sub(prefix, "", 
                             grep(prefix, names(st_options()), value = TRUE, 
                                  fixed = TRUE),
                             fixed = TRUE)) {
    if (!format_element %in% c(overrided_args, names(attr(x, "fn_call")))) {
      attr(x, "format_info")[[format_element]] <- 
        st_options(paste0(prefix, format_element))
    }
  }

  # Override of data info attributes
  if ("dataframe" %in% tolower(names(dotArgs))) {
    dotArgs$Data.frame <- dotArgs$Dataframe
    message("Attribute 'Dataframe' has been renamed to 'Data.frame'; ",
            "please use the latter in the future")
  }
  
  if ("dataframe.label" %in% tolower(names(dotArgs))) {
    dotArgs$Data.frame.label <- dotArgs$Dataframe.label
    message("Attribute 'Dataframe.label' has been renamed to ",
            "'Data.frame.label'; please use the latter in the future")
  }

  data_info_elements <- c("Data.frame", "Data.frame.label", "Variable", 
                          "Variable.label", "Data.type", "Group", "Weights",
                          "Row.variable", "Col.variable")
  for (data_info_element in data_info_elements) {
    if (tolower(data_info_element) %in% tolower(names(dotArgs))) {
      attr(x, "data_info")[[data_info_element]] <- 
        dotArgs[[grep(paste0("^", data_info_element, "$"), names(dotArgs), 
                      ignore.case = TRUE)]]
      overrided_args <- append(overrided_args, data_info_element)
    }
  }
  
  # Add caption if present in dotArgs
  if ("caption" %in% names(dotArgs)) {
    attr(x, "user_fmt")$caption <- dotArgs$caption
  }

  # When style == 'rmarkdown', set plain.ascii to FALSE unless 
  # explicitly specified otherwise
  if (method == "pander" && attr(x, "format_info")$style == "rmarkdown" &&
      isTRUE(attr(x, "format_info")$plain.ascii) &&
      (!"plain.ascii" %in% (names(dotArgs)))) {
    attr(x, "format_info")$plain.ascii <- FALSE
  }
  
  # Evaluate formatting attributes that are symbols at this stage (F, T)
  for (i in seq_along(attr(x, "format_info"))) {
    if (is.symbol(attr(x, "format_info")[[i]])) {
      attr(x, "format_info")[[i]] <- eval(attr(x, "format_info")[[i]])
    }
  }
  
  # Build default footnote
  if (method %in% c("browser", "viewer", "render") && footnote == "default") {
    footnote <- 
      paste0(
        "<p>", conv_non_ascii(trs("generated.by")),
        " <a href='https://github.com/dcomtois/summarytools'>",
        "summarytools</a> ", packageVersion(pkg = "summarytools"),
        " (<a href='https://www.r-project.org/'>R</a> ", trs("version"), " ", 
        getRversion(), ")", "<br/>", strftime(attr(x, "date"), trs("date.fmt")),
        "</p>"
      )
  }

  # Concatenate data frame + $ + variable name where appropriate
  if (!("Variable" %in% overrided_args) && 
      length(attr(x, "data_info")$Variable) == 1 && 
      length(attr(x, "data_info")$Data.frame) == 1 &&
      !("by_var_special" %in% names(attr(x, "data_info")))) {
    attr(x, "data_info")$Variable <- paste(attr(x, "data_info")$Data.frame,
                                           attr(x, "data_info")$Variable, 
                                           sep = "$")
  }

  # Dispatch to the right function for preparing output ------------------------
  if (attr(x, "st_type") == "freq") {
    res <- print_freq(x, method)
    if (is.na(report.title)) {
      if (!("Weights" %in% names(attr(x, "data_info")))) {
        report.title <- trs("title.freq")
      } else {
        report.title <- trs("title.freq.weighted")
      }
    }
  } else if (attr(x, "st_type") == "ctable") {
    res <- print_ctable(x, method)
    if (is.na(report.title)) {
      report.title <- trs("title.ctable")
    }
  } else if (attr(x, "st_type") == "descr") {
    res <- print_descr(x, method)
    if (is.na(report.title)) {
      if (!("Weights" %in% names(attr(x, "data_info")))) {
        report.title <- trs("title.descr")
      } else {
        report.title <- trs("title.descr.weighted")
      }
    }
  } else if (attr(x, "st_type") == "dfSummary") {
    res <- print_dfs(x, method)
    if (is.na(report.title)) {
      report.title <- trs("title.dfSummary")
    }
  }

  # Print or write to file - pander --------------------------------------------
  if (method == "pander") {

    # Remove doubled linefeed
    res[[length(res)]] <- 
      sub("^\\n\\n", "\n", res[[length(res)]])
    
    file <- normalizePath(file, mustWork = FALSE)
    cat(do.call(paste0, res), file = file, append = append)
    
    if (file != "" && !isTRUE(silent)) {
      if (isTRUE(append))
        message("Output file appended: ", file)
      else
        message("Output file written: ", file)
      return(invisible())
    }
    
  } else {
    
    # Print or write to file - html --------------------------------------------
    
    if (isTRUE(append)) {
      f <- file(file, open = "r", encoding = "utf-8")
      html_content_in <- paste(readLines(f, warn = FALSE, encoding = "utf-8"), 
                               collapse="\n")
      close(f)
      top_part    <- sub("(^.+)(</body>.+)", "\\1", html_content_in)
      bottom_part <- sub("(^.+)(</body>.+)", "\\2", html_content_in)
      insert_part <- 
        iconv(paste(capture.output(tags$div(class="container st-container", 
                                            res)), 
                    collapse="\n"), to = "utf-8")
      html_content <- paste(capture.output(cat(top_part, insert_part, 
                                               bottom_part)), collapse="\n")
      
    } else {

      if (method %in% c("browser", "viewer")) {
        html_content <-
          tags$div(
            class="container st-container",
            tags$head(
              tags$title(HTML(conv_non_ascii(report.title))),
              if (isTRUE(bootstrap.css))
                includeCss(system.file(package="summarytools", 
                                       "includes/stylesheets/bootstrap.min.css")),
              includeCss(system.file(package="summarytools", 
                                     "includes/stylesheets/summarytools.css")),
              if (!is.na(custom.css)) 
                includeCss(path = custom.css)
            ),
            res)
        
      } else {
        # method == "render"
        html_content <-
          tags$div(
            class="container st-container",
            tags$head(
              includeCss(system.file(package="summarytools", 
                                     "includes/stylesheets/summarytools.css")),
              if (!is.na(custom.css))
                includeCss(path = custom.css)
            ),
            res)
      }
    }

    if (method == "render") {
      return(html_content)
    }

    outfile_path <- ifelse(file == "", paste0(tempfile(),".html"), file)
    outfile_path <- normalizePath(outfile_path, mustWork = FALSE)
    
    if (isTRUE(append)) {
      capture.output(cat(html_content, "\n"), file = outfile_path)
    } else {
      save_html(html = html_content, file = outfile_path)
    }

    if (method == "viewer") {
      if (file == "" || isTRUE(open.doc)) {
        if (.Platform$GUI == "RStudio") {
          viewer <- getOption("viewer")
          if (!is.null(viewer)) {
            viewer(outfile_path)
          } else {
            message("To view html content in RStudio, please install ",
                    "the 'rstudioapi' package")
            message("Switching method to 'browser'")
            method <- "browser"
          }
        } else {
          message("Switching method to 'browser'")
          method <- "browser"
        }
      }
    }

    # For method "browser", we don't use utils::browseURL() because of 
    # compatibility issues with RStudio
    if (method == "browser") {
      if (file == "" || isTRUE(open.doc)) {
        switch(Sys.info()[["sysname"]],
               Windows = {shell.exec(file = paste0("file:///", outfile_path))},
               Linux   = {system(paste("/usr/bin/xdg-open", outfile_path), 
                                 wait = FALSE, ignore.stdout = TRUE)},
               Darwin  = {system(paste("open", outfile_path), wait = FALSE, 
                                 ignore.stderr = TRUE)})
      }
    }

    # return file path and update tmpfiles vector when method = browser / viewer
    if (file == "" && method %in% c("browser", "viewer")) {
      .st_env$tmpfiles <- c(.st_env$tmpfiles, outfile_path)
      if (!silent) {
        message("Output file written: ", outfile_path)
      }
      return(invisible(outfile_path))
    } else if (file != "") {
      if (!silent) {
        if (isTRUE(append)) {
          message("Output file appended: ", outfile_path)
        } else {
          message("Output file written: ", outfile_path)
        }
      }
      return(invisible())
    }
  }
}

# Prepare freq objects for printing --------------------------------------------
#' @import htmltools
print_freq <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "format_info")
  user_fmt    <- attr(x, "user_fmt")
  
  if (!isTRUE(parent.frame()$silent) && !isTRUE(format_info$group.only) && 
     (!"by_first" %in% names(data_info) || 
      isTRUE(as.logical(data_info$by_first))) &&
     "ignored" %in% names(attributes(x))) {
    message("Non-categorical variable(s) ignored: ",
            paste(attr(x, "ignored"), collapse = ", "))
  }

  if (!isTRUE(format_info$report.nas) && !isTRUE(format_info$cumul)) {
    # Subtract NA counts from total
    x[nrow(x), 1] <- x[nrow(x), 1] - x[nrow(x) -1, 1]
    # Remove NA row and keep only desired columns
    x <- x[-(nrow(x)-1), 1:2]
    colnames(x) <- c(trs("freq"), trs("pct"))
    
  } else if (!isTRUE(format_info$report.nas) && isTRUE(format_info$cumul)) {
    # Substract NA counts from total
    x[nrow(x), 1] <- x[nrow(x), 1] - x[nrow(x) -1, 1]
    # Remove NA row and keep only desired columns
    x <- x[-(nrow(x)-1), 1:3]
    colnames(x) <- c(trs("freq"), trs("pct"), trs("pct.cum"))
    
  } else if (isTRUE(format_info$report.nas) && !isTRUE(format_info$cumul)) {
    x <- x[ ,-c(3,5)]
    colnames(x) <- c(trs("freq"), trs("pct.valid.f"), trs("pct.total"))
  }

  if (!isTRUE(format_info$totals)) {
    x <- x[-nrow(x),]
  }
  
  if (method=="pander") {
    
    # print_freq -- pander method ---------------------------------------------
    justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                      l = "left",
                      c = "centre",
                      d = "right",
                      r = "right")
    
    freq_table <- format(x = round(x, format_info$round.digits),
                         trim = FALSE,
                         nsmall = format_info$round.digits,
                         justify = justify)
    
    if (isTRUE(format_info$report.nas)) {
      # Put NA in relevant cells so that pander recognizes them as such
      freq_table[nrow(freq_table) -
                   as.numeric(isTRUE(format_info$totals)), 2] <- NA
      
      # This not good for case with cumul = FALSE
      if (isTRUE(format_info$cumul)) {
        freq_table[nrow(freq_table) -
                     as.numeric(isTRUE(format_info$totals)), 3] <- NA
      }
    }
    
    # Remove .00 digits in Freq column when weights are not used
    if (!"Weights" %in% names(data_info)) {
      freq_table[ ,1] <- sub("\\.0+", "", freq_table[ ,1])
    }
    
    # Escape "<" and ">" when used in pairs in rownames
    if (!isTRUE(format_info$plain.ascii)) {
      row.names(freq_table) <- gsub(pattern = "\\<(.*)\\>", 
                                    replacement = "\\\\<\\1\\\\>",
                                    x = row.names(freq_table), perl = TRUE)
    }
    
    # set encoding to native to allow proper display of accentuated characters
    if (parent.frame()$file == "") {
      row.names(freq_table) <- enc2native(row.names(freq_table))
      colnames(freq_table)  <- enc2native(colnames(freq_table))
    }
    
    pander_args <- append(list(style        = format_info$style,
                               plain.ascii  = format_info$plain.ascii,
                               justify      = justify,
                               missing      = format_info$missing,
                               split.tables = Inf),
                          user_fmt)
    
    main_sect <- build_heading_pander(format_info, data_info)
    
    main_sect %+=%
      paste(
        capture.output(
          do.call(pander, append(pander_args, list(x = quote(freq_table))))
        ),
        collapse = "\n")
    
    if (isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- gsub("\\|","\\\\|", 
                                             main_sect[[length(main_sect)]])
    }
    
    return(main_sect)
    
  } else {
    
    # print_freq -- html method ------------------------------------------------
    justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                      l = "left",
                      c = "center",
                      d = "center",
                      r = "right")
    
    table_head <- list()
    table_rows <- list()
    
    for (ro in seq_len(nrow(x))) {
      table_row <- list()
      for (co in seq_len(ncol(x))) {
        if (co == 1) {
          table_row %+=% list(tags$th(row.names(x)[ro],
                                      align = "center",
                                      class = "st-protect-top-border"))
          if (!"Weights" %in% names(data_info)) {
            cell <- sub(pattern = "\\.0+", replacement = "", x[ro,co], 
                        perl = TRUE)
            table_row %+=% list(tags$td(cell, align = justify))
            next
          }
        }
        
        if (is.na(x[ro,co])) {
          table_row %+=% list(tags$td(format_info$missing, align = justify))
        } else {
          cell <- sprintf(paste0("%.", format_info$round.digits, "f"), x[ro,co])
          table_row %+=% list(tags$td(cell, align = justify))
        }
        
        if (co == ncol(x)) {
          table_rows %+=% list(tags$tr(table_row))
        }
      }
    }
    
    if (isTRUE(format_info$report.nas) && isTRUE(format_info$cumul)) {
      table_head[[1]] <- list(tags$th("", colspan = 2),
                              tags$th(HTML(conv_non_ascii(trs("valid"))),
                                      colspan=2, align="center",
                                      class = "st-protect-top-border"),
                              tags$th(HTML(conv_non_ascii(trs("total"))),
                                      colspan=2, align="center",
                                      class = "st-protect-top-border"))
      table_head[[2]] <- list(tags$th(HTML(conv_non_ascii(
                                            sub("^.*\\$(.+)$", "\\1", 
                                            data_info$Variable))),
                                      align="center"),
                              tags$th(HTML(conv_non_ascii(trs("freq"))),
                                      align="center"),
                              tags$th(HTML(conv_non_ascii(trs("pct"))),
                                      align="center"),
                              tags$th(HTML(conv_non_ascii(trs("pct.cum"))),
                                      align="center"),
                              tags$th(HTML(conv_non_ascii(trs("pct"))),
                                      align="center"),
                              tags$th(HTML(conv_non_ascii(trs("pct.cum"))),
                                      align="center"))
      
      freq_table_html <-
        tags$table(
          tags$thead(tags$tr(table_head[[1]]),
                     tags$tr(table_head[[2]])),
          tags$tbody(table_rows),
          class = paste(
            "table table-striped table-bordered",
            "st-table st-table-striped st-table-bordered st-freq-table",
            ifelse(is.na(parent.frame()$table.classes), 
                   "", parent.frame()$table.classes)
          )
        )
      
    } else {
      if (isTRUE(format_info$cumul) && !isTRUE(format_info$report.nas)) {
        
        # No NA reporting
        table_head <- 
          list(tags$th(HTML(conv_non_ascii(sub("^.*\\$(.+)$", "\\1", 
                                               data_info$Variable))),
                       align = "center",
                       class = "st-protect-top-border"),
               tags$th(HTML(conv_non_ascii(trs("freq"))),
                       align = "center",
                       class = "st-protect-top-border"),
               tags$th(HTML(conv_non_ascii(trs("pct"))),
                       align = "center",
                       class = "st-protect-top-border"),
               tags$th(HTML(conv_non_ascii(trs("pct.cum"))),
                       align = "center",
                       class = "st-protect-top-border"))
      } else if (isTRUE(format_info$report.nas) && !isTRUE(format_info$cumul)) {
      
        # No cumulative proportions
        table_head <- 
          list(tags$th(HTML(conv_non_ascii(sub("^.*\\$(.+)$", "\\1", 
                                               data_info$Variable))),
                       align = "center",
                       class = "st-protect-top-border"),
               tags$th(HTML(conv_non_ascii(trs("freq"))),
                       align = "center",
                       class = "st-protect-top-border"),
               tags$th(HTML(conv_non_ascii(trs("pct.valid.f"))),
                       align = "center",
                       class = "st-protect-top-border"),
               tags$th(HTML(conv_non_ascii(trs("pct.total"))),
                       align = "center",
                       class = "st-protect-top-border"))
        
      } else {
        
        # No cumulative proportions, no NA reporting
        table_head <- 
          list(tags$th(HTML(conv_non_ascii(sub("^.*\\$(.+)$", "\\1", 
                                               data_info$Variable))),
                       align = "center",
                       class = "st-protect-top-border"),
               tags$th(HTML(conv_non_ascii(trs("freq"))),
                       align = "center",
                       class = "st-protect-top-border"),
               tags$th(HTML(conv_non_ascii(trs("pct")))))
      }
      
      freq_table_html <-
        tags$table(
          tags$thead(tags$tr(table_head)),
          tags$tbody(table_rows),
          class = paste(
            "table table-striped table-bordered",
            "st-table st-table-striped st-table-bordered st-freq-table-nomiss",
            ifelse(is.na(parent.frame()$table.classes),
                   "", parent.frame()$table.classes)
          )
        )
    }
    
    # Cleanup extra spacing and linefeeds in html to correct layout issues
    freq_table_html <- gsub(pattern = "\\s*(\\d*)\\s*(<span|</td>)",
                            replacement = "\\1\\2", 
                            x = as.character(freq_table_html),
                            perl = TRUE)
    freq_table_html <- gsub(pattern = "</span>\\s*</span>",
                            replacement = "</span></span>",
                            x = freq_table_html,
                            perl = TRUE)
    
    # Prepare the main "div" for the html report
    div_list <- build_heading_html(format_info, data_info, method)
    
    if (length(div_list) > 0 &&
        !("shiny.tag" %in% class(div_list[[length(div_list)]]))) {
      div_list %+=% list(HTML(text = "<br/>"))
    }
    
    div_list %+=% list(HTML(text = conv_non_ascii(freq_table_html)))
    
    if (parent.frame()$footnote != "") {
      footn <- conv_non_ascii(parent.frame()[["footnote"]])
      div_list %+=% list(HTML(text = paste0("<p>", footn, "</p>")))
    }
  }
  
  return(div_list)
}

# Prepare ctable objects for printing ------------------------------------------
#' @import htmltools
#' @keywords internal
print_ctable <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "format_info")
  user_fmt    <- attr(x, "user_fmt")
  
  # align_numbers --------------------------------------------------------------
  align_numbers <- function(counts, props) {
    res <- sapply(seq_len(ncol(counts)), function(colnum) {
      
      if ("Weights" %in% names(data_info)) {
        maxchar_cnt <- 
          nchar(as.character(round(max(counts[ ,colnum]), 
                                   digits = format_info$round.digits)))
        maxchar_pct <- 
          nchar(sprintf(paste0("%.", format_info$round.digits, "f"),
                        max(props[ ,colnum]*100)))
        
        return(paste(sprintf(paste0("%", maxchar_cnt, ".", 
                                    format_info$round.digits, "f"),
                             counts[ ,colnum]),
                     sprintf(paste0("(%", maxchar_pct, ".", 
                                    format_info$round.digits, "f%%)"),
                             props[ ,colnum]*100)))
      } else {
        maxchar_cnt <- nchar(as.character(max(counts[ ,colnum])))
        maxchar_pct <- nchar(sprintf(paste0("%.", format_info$round.digits,"f"), 
                                     max(props[ ,colnum]*100)))
        return(paste(sprintf(paste0("%", maxchar_cnt, "i"),
                             counts[ ,colnum]),
                     sprintf(paste0("(%", maxchar_pct, ".", 
                                    format_info$round.digits, "f%%)"),
                             props[ ,colnum]*100)))
      }
    })
    
    dim(res) <- dim(counts)
    dimnames(res) <- dimnames(counts)
    
    return(res)
  }
  
  if (!isTRUE(format_info$totals)) {
    x$cross_table <-
      x$cross_table[which(rownames(x$cross_table) != trs("total")), 
                    which(colnames(x$cross_table) != trs("total"))]
    if (data_info$Proportions != "None") {
      x$proportions <- 
        x$proportions[which(rownames(x$proportions) != trs("total")), 
                      which(colnames(x$proportions) != trs("total"))]
    }
  }
  
  if (data_info$Proportions %in% c("Row", "Column", "Total")) {
    cross_table <- align_numbers(x$cross_table, x$proportions)
  } else {
    cross_table <- x$cross_table
  }
  
  format_info$justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                    l = "left",
                    c = "center",
                    r = "right")
  
  # print_ctable -- pander method ----------------------------------------------
  if (method == "pander") {
    
    # Escape "<" and ">" when used in pairs in rownames or colnames
    if (!isTRUE(format_info$plain.ascii)) {
      row.names(cross_table) <-
        gsub(pattern = "\\<(.*)\\>", replacement = "\\\\<\\1\\\\>",
             x = row.names(cross_table), perl = TRUE)
      colnames(cross_table) <- 
        gsub(pattern = "\\<(.*)\\>", replacement = "\\\\<\\1\\\\>",
             x = colnames(cross_table), perl = TRUE)
    }
    
    pander_args <- append(list(style        = format_info$style,
                               plain.ascii  = format_info$plain.ascii,
                               justify      = format_info$justify,
                               split.tables = format_info$split.tables),
                          user_fmt)
    
    main_sect <- build_heading_pander(format_info, data_info)
    
    main_sect %+=%
      paste(
        capture.output(
          do.call(pander, append(pander_args, 
                                 list(x = quote(ftable(cross_table)))))
        ),
        collapse = "\n")
    
    if (isTRUE(format_info$headings) && format_info$style != "grid") {
      main_sect[[length(main_sect)]] <- sub("^\n", "\n\n", 
                                            main_sect[[length(main_sect)]])
    }
    
    if (isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect)]])
    }
    
    return(main_sect)
    
  } else {
    
    # print_ctable -- html method ----------------------------------------------
    dnn <- names(dimnames(cross_table))
    
    table_head <- list()
    table_rows <- list()
    
    has_prop <- length(x$proportions) > 0
    
    table_head[[1]] <- 
      list(tags$th(""), 
           tags$th(
             dnn[2], 
             colspan = (1 + has_prop*3) * 
               (ncol(cross_table) - as.numeric(isTRUE(format_info$totals))),
             align = "center", class = "st-protect-top-border"  
             )
           )
    
    if (isTRUE(format_info$totals)) {
      table_head[[1]][[3]] <- tags$th("", colspan = (1 + has_prop*3))
    }
    
    table_head[[2]] <-list(tags$td(tags$strong(dnn[1]), align = "center"))
    
    for(cn in colnames(cross_table)) {
      if (nchar(cn) > 12) {
        cn <- smart_split(cn, 12)
      }
      cn <- sub("<", "&lt;", cn, fixed = TRUE)
      cn <- sub(">", "&gt;", cn, fixed = TRUE)
      table_head[[2]][[length(table_head[[2]]) + 1]] <- 
        tags$th(HTML(conv_non_ascii(cn)), 
                colspan = (1 + has_prop*3), align = "center")
    }
    
    table_rows <- list()
    for (ro in seq_len(nrow(cross_table))) {
      table_row <- list()
      for (co in seq_len(ncol(cross_table))) {
        if (co == 1) {
          
          rn <- row.names(cross_table)[ro]
          rn <- sub("<", "&lt;", rn, fixed = TRUE)
          rn <- sub(">", "&gt;", rn, fixed = TRUE)
          
          table_row %+=%
            list(
              tags$td(
                tags$strong(
                  HTML(conv_non_ascii(rn)), 
                  align = "center"
                  )
                )
              )
        }
        
        # No proportions
        if (!isTRUE(has_prop)) {
          cell <- cross_table[ro,co]
          table_row %+=% list(tags$td(tags$span(cell)))
        } else {
          cell <- gsub(" ", "", cross_table[ro,co])
          cell <- sub(")$", "", cell)
          cell <- strsplit(cell, "\\(")[[1]]
          
          table_row %+=% list(
            tags$td(
              cell[1],
              align = "right",
              style = "padding:0 0 0 15px;border-right:0;text-align:right"
            )
          )
          
          table_row %+=% list(
            tags$td(
              "(", align = "left",
              style = paste0("padding:0 1px 0 4px;border-left:0;",
                             "border-right:0;text-align:left")
              )
            )

          table_row %+=% list(
            tags$td(
              HTML(cell[2]),
              align = "left",
              style = "padding:0;border-left:0;border-right:0;text-align:right"
            )
          )
          
          table_row %+=% list(
            tags$td(")",
                    align = "left",
                    style = "padding:0 15px 0 1px;border-left:0;text-align:right"
            )
          )
        }
        
        # On last col, insert row into list
        if (co == ncol(cross_table)) {
          table_rows %+=% list(tags$tr(table_row))
        }
      }
    }
    
    cross_table_html <-
      tags$table(
        tags$thead(
          tags$tr(table_head[[1]]),
          tags$tr(table_head[[2]])
        ),
        tags$tbody(
          table_rows
        ),
        class = paste(
          "table table-bordered st-table st-table-bordered st-cross-table",
          ifelse(is.na(parent.frame()$table.classes), "", 
                 parent.frame()$table.classes)
        )
      )
    
    div_list <- build_heading_html(format_info, data_info, method)
    
    if (length(div_list) > 0 &&
        !("shiny.tag" %in% class(div_list[[length(div_list)]]))) {
      div_list %+=% list(HTML(text = "<br/>"))
    }
    
    div_list %+=% list(cross_table_html)
    
    if (parent.frame()$footnote != "") {
      fn <- conv_non_ascii(parent.frame()[["footnote"]])
      div_list %+=% list(HTML(text = fn))
    }
  }
  
  return(div_list)
}

# Prepare descr objects for printing -------------------------------------------
#' @import htmltools
#' @keywords internal
print_descr <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "format_info")
  user_fmt    <- attr(x, "user_fmt")
  
  if (!isTRUE(parent.frame()$silent) &&
     "ignored" %in% names(attributes(x)) &&
     !isTRUE(format_info$group.only) && 
     (!"by_first" %in% names(data_info) || 
      isTRUE(as.logical(data_info$by_first)))) {
        message("Non-numerical variable(s) ignored: ",
            paste(attr(x, "ignored"), collapse = ", "))
  }
  
  justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                    l = "left",
                    c = "center",
                    r = "right")
  
  if (method == "pander") {
    
    # print_descr -- pander method ---------------------------------------------
    # Format numbers (avoids inconsistencies with pander rounding digits)
    x <- format(round(x, format_info$round.digits),
                nsmall = format_info$round.digits)
    
    # set encoding to native to allow proper display of accentuated characters
    if (parent.frame()$file == "") {
      row.names(x) <- enc2native(row.names(x))
      colnames(x)  <- enc2native(colnames(x))
    }
    
    pander_args <- append(list(style        = format_info$style,
                               plain.ascii  = format_info$plain.ascii,
                               split.tables = format_info$split.tables,
                               justify      = justify),
                          user_fmt)
    
    main_sect <- build_heading_pander(format_info, data_info)  
    
    main_sect %+=%
      paste(
        capture.output(
          do.call(pander, append(pander_args, list(x = quote(x))))
        ),
        collapse = "\n")
    
    if (isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect)]])
    }
    
    return(main_sect)
    
  } else {
    # print_descr -- html method -----------------------------------------------
    table_head <- list(tags$th(""))

    for(cn in colnames(x)) {
      if (nchar(cn) > 12) {
        cn <- smart_split(cn, 12)
      }
      table_head %+=% list(tags$th(HTML(cn), align = "center",
                                   class = "st-protect-top-border"))
    }
    
    table_rows <- list()
    for (ro in seq_len(nrow(x))) {
      table_row <- list(tags$td(tags$strong(rownames(x)[ro])))
      for (co in seq_len(ncol(x))) {
        # cell is NA
        if (is.na(x[ro,co])) {
          table_row %+=% list(tags$td(format_info$missing))
        } else if ((rownames(x)[ro] == trs("n.valid") || 
                    colnames(x)[co] == trs("n.valid")) && 
                   !"Weights" %in% names(data_info)) {
          table_row %+=% list(tags$td(tags$span(round(x[ro,co], 0))))
        } else {
          # When not NA, and not N.Valid row, format cell content
          cell <- sprintf(paste0("%.", format_info$round.digits, "f"), x[ro,co])
          table_row %+=% list(tags$td(tags$span(cell)))
        }
        # On last column, insert row to table_rows list
        if (co == ncol(x)) {
          table_rows %+=% list(tags$tr(table_row))
        }
      }
    }

    descr_table_html <-
      tags$table(
        tags$thead(tags$tr(table_head)),
        tags$tbody(table_rows),
        class = paste(
          "table table-bordered table-striped",
          "st-table st-table-bordered st-table-striped st-descr-table",
          ifelse(is.na(parent.frame()$table.classes), "", 
                 parent.frame()$table.classes))
      )
    
    # Cleanup some extra spacing & html linefeeds to avoid weirdness in layout
    # of source code
    descr_table_html <- as.character(descr_table_html)
    descr_table_html <- gsub(pattern = "\\s*(\\-?\\d*)\\s*(<span|</td>)",
                             replacement = "\\1\\2", x = descr_table_html,
                             perl = TRUE)
    descr_table_html <- gsub(pattern = "</span>\\s*</span>",
                             replacement = "</span></span>",
                             x = descr_table_html,
                             perl = TRUE)
    descr_table_html <- gsub(pattern = "<strong>\\s*</strong>",
                             replacement = "",
                             x = descr_table_html,
                             perl = TRUE)
    descr_table_html <- gsub(pattern = '(<td align="right">)\\s+(<)',
                             replacement = "\\1\\2",
                             x = descr_table_html,
                             perl = TRUE)
    descr_table_html <- conv_non_ascii(descr_table_html)
    
    # Prepare the main "div" for the html report
    div_list <- build_heading_html(format_info, data_info, method)
    if (length(div_list) > 0 &&
        !("shiny.tag" %in% class(div_list[[length(div_list)]]))) {
      div_list %+=% list(HTML(text = "<br/>"))
    }
    
    div_list %+=% list(HTML(text = descr_table_html))
    
    if (parent.frame()$footnote != "") {
      fn <- conv_non_ascii(parent.frame()[["footnote"]])
      div_list %+=% list(HTML(text = fn))
    }
  }
  
  return(div_list)
}

# Prepare dfSummary objects for printing ---------------------------------------
#' @import htmltools
#' @keywords internal
print_dfs <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "format_info")
  user_fmt    <- attr(x, "user_fmt")
  
  # make_tbl_cell --------------------------------------------------------------
  # Function to align the freqs / proportions in html outputs
  # A table is built to fit in a single cell of the final table
  make_tbl_cell <- function(cell) {
    
    if (identical(cell, trs("all.nas"))) {
      return(HTML(paste0('<td align="left">', cell, '</td>')))
    }
    
    rows <- strsplit(cell, "\\\n")[[1]]
    rows <- gsub("\\", "", rows, fixed = TRUE)
    rows <- gsub(" " , "", rows, fixed = TRUE)
    rows <- gsub(")$", "", rows)
    rows <- strsplit(rows, "[(:]")
    
    if (grepl(":", cell)) {
      
      notice <- NA
      if (length(rows[[length(rows)]]) == 1) {
        notice <- sub("!", "! ", rows[[length(rows)]], fixed = TRUE)
        length(rows) <- length(rows) - 1
      }
      
      vals <- vapply(X = rows, FUN = `[`,  FUN.VALUE = " ", 1)
      cnts <- vapply(X = rows, FUN = `[`,  FUN.VALUE = " ", 2)
      prps <- vapply(X = rows, FUN = `[`,  FUN.VALUE = " ", 3)
      
      cell <- 
        paste0(
          paste0(
            '<tr style="background-color:transparent">',
            '<td style="padding:0 0 0 7px;margin:0;border:0" align="right">'
          ),
          vals,
          paste0(
            '</td><td style="padding:0 2px;border:0;" align="left">:</td>',
            '<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">'
          ),
          cnts,
          paste0(
            '</td><td style="padding:0;border:0" align="left">(</td>',
            '<td style="padding:0 2px;margin:0;border:0" align="right">'
          ),
          prps,
          paste0('</td><td style="padding:0 4px 0 0;border:0" align="left">)',
                 '</td></tr>'
          ),
          collapse = ""
        )
      
      if (!is.na(notice)) {
        cell <- 
          paste0(cell, '<tr style="background-color:transparent">',
                 '<td style="padding:0 0 0 7px;border:0;margin:0" colspan=3>',
                 notice, "</td></tr>", collapse = "")
      }
    } else {
      
      cnts <- vapply(X = rows, FUN = `[`, FUN.VALUE = " ", 1)
      prps <- vapply(X = rows, FUN = `[`, FUN.VALUE = " ", 2)
      
      cell <- 
        paste0(
          paste0(
            '<tr style="background-color:transparent">',
            '<td style="padding:0 5px 0 7px;margin:0;border:0" align="right">'
          ),
          cnts,
          paste0(
            '</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td>',
            '<td style="padding:0;border:0" align="right">'
          ),
          prps,
          '</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr>',
          collapse = ""
        )
    }
    
    return(
      HTML(
        paste0(
          '<td align="left" style="padding:0;vertical-align:middle"><table ',
          'style="border-collapse:collapse;border:none;margin:0">',
          cell, '</table></td>'
          )
        )
      )
  }
  
  # Remove Var number ("No") column if specified in call to print/view
  if (trs("no") %in% names(x) && 
      "varnumbers" %in% names(format_info) && 
      !isTRUE(format_info$varnumbers)) {
    x <- x[ ,-which(names(x) == trs("no"))]
  }
  
  # Remove Label column if specified in call to print/view
  if (trs("label") %in% names(x) && 
      "labels.col" %in% names(format_info) && 
      !isTRUE(format_info$labels.col)) {
    x <- x[ ,-which(names(x) == trs("label"))]
  }
  
  # Remove Valid column if specified in call to print/view
  if (trs("valid") %in% names(x) && 
      "valid.col" %in% names(format_info) && 
      !isTRUE(format_info$valid.col)) {
    x <- x[ ,-which(names(x) == trs("valid"))]
  }
  
  # Remove Missing column if specified in call to print/view
  if (trs("missing") %in% names(x) && 
      "na.col" %in% names(format_info) && 
      !isTRUE(format_info$na.col)) {
    x <- x[ ,-which(names(x) == trs("missing"))]
  }
  
  # print_dfSummary - pander method --------------------------------------------  
  if (method == "pander") {
    
    # remove html graphs
    if (trs("graph") %in% names(x)) {
      x <- x[ ,-which(names(x) == trs("graph"))]
    }
    
    # Remove graph if specified in call to print/view
    if ("text.graph" %in% names(x) && "graph.col" %in% names(format_info) &&
        !isTRUE(format_info$graph.col)) {
      x <- x[ ,-which(names(x) == "text.graph")]
    } else {
      colnames(x)[which(names(x) == "text.graph")] <- trs("graph")
    }
    
    # Check that style is not "simple" or "rmarkdown"
    if (isTRUE(format_info$style == "simple")) {
      format_info$style <- "multiline"
    }
    
    if (!isTRUE(format_info$plain.ascii)) {
      # Escape symbols for words between <>'s to allow <NA> or factor
      # levels such as <ABC> to be rendered correctly
      if (trs("label") %in% names(x)) {
        x[[trs("label")]] <-
          gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
               x = x[[trs("label")]], perl=TRUE)
      }
      
      x[[trs("stats.values")]] <-
        gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
             x = x[[trs("stats.values")]], perl=TRUE)
      
      x[[trs("freqs.pct.valid")]] <-
        gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
             x = x[[trs("freqs.pct.valid")]], perl=TRUE)
      
      
      # Remove leading characters used for alignment in plain.ascii 
      x[[trs("freqs.pct.valid")]] <-
        gsub(pattern = "^\\\\ *", replacement = "",
             x = x[[trs("freqs.pct.valid")]], perl=TRUE)
      
      x[[trs("freqs.pct.valid")]] <- 
        gsub(pattern = "\\n\\\\ *", replacement = "\n",
             x = x[[trs("freqs.pct.valid")]], perl=TRUE)
    }
    
    # set column names encoding to native to allow proper display of non-ascii
    if (parent.frame()$file == "") {
      colnames(x) <- enc2native(colnames(x))
    }
    
    pander_args <- append(list(style            = format_info$style,
                               plain.ascii      = format_info$plain.ascii,
                               justify          = format_info$justify,
                               split.cells      = format_info$split.cells,
                               split.tables     = format_info$split.tables,
                               keep.line.breaks = TRUE),
                          user_fmt)
    
    main_sect <- build_heading_pander(format_info, data_info)

    main_sect %+=%
      paste(
        capture.output(
          do.call(pander, append(pander_args, list(x = quote(x))))
        ),
        collapse = "\n")
    
    if (isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect)]])
    }
    
    return(main_sect)
    
  } else {
    
    # print_dfs - html method --------------------------------------------------
    
    # remove text graph
    if ("text.graph" %in% names(x)) {
      x <- x[ ,-which(names(x) == "text.graph")]
    }
    
    # Remove graph if specified in call to print/view
    if (trs("graph") %in% names(x) && 
        "graph.col" %in% names(format_info) &&
        !isTRUE(format_info$graph.col)) {
      x <- x[ ,-which(names(x) == trs("graph"))]
    }
    
    table_head <- list()
    for(cn in colnames(x)) {
      table_head %+=% list(tags$th(tags$strong(HTML(conv_non_ascii(cn))), 
                                   class = inv_trs(cn),
                                   align = "center",
                                   class = "st-protect-top-border"))
    }

    colgroup <- NA    
    if ("col.widths" %in% names(format_info)) {
      if (length(format_info$col.widths) != ncol(x)) {
        stop("Number of elements in 'col.widths', (",
             (length(format_info$col.widths)), ") is not equal to number of ",
             "columns to display (", ncol(x), ")")
      }
      colgroup <- tags$colgroup()
      if (is.numeric(format_info$col.widths)) {
        for (i in format_info$col.widths) {
          colgroup <- tagAppendChild(
            colgroup, tags$col(style = paste0("width:", i, "px"))
          )
        }
      } else {
        for (i in format_info$col.widths) {
          colgroup <- tagAppendChild(
            colgroup, tags$col(style = paste("width", i, sep = ":"))
          )
        }
      }
    }

    table_rows <- list()
    for (ro in seq_len(nrow(x))) {
      table_row <- list()
      for (co in seq_len(ncol(x))) {
        cell <- x[ro,co]
        cell <- gsub("\\\\\n", "\n", cell)
        if (colnames(x)[co] %in% c(trs("no"), trs("valid"), trs("missing"))) {
          table_row %+=% list(tags$td(HTML(conv_non_ascii(cell)), 
                                      align = "center"))
        } else if (colnames(x)[co] == trs("label")) {
          cell <- gsub("(\\d+)\\\\\\.", "\\1.", cell)
          cell <- paste(strwrap(cell, width = format_info$split.cells, 
                                simplify = TRUE), collapse = "\n")
          table_row %+=% list(
            tags$td(HTML(conv_non_ascii(cell)), align = "left")
          )
        } else if (colnames(x)[co] %in% c(trs("variable"), 
                                          trs("stats.values"))) {
          cell <- gsub("(\\d+)\\\\\\.", "\\1.", cell)
          cell <- gsub("\\s{2,}", " ", cell)
          table_row %+=% list(
            tags$td(HTML(conv_non_ascii(cell)), align = "left")
          )
        } else if (colnames(x)[co] == trs("freqs.pct.valid")) {
          if (grepl(paste0("(",trs("distinct.value"), "|",
                           trs("distinct.values"), ")"), cell) || cell == "") {
            table_row %+=% list(
              tags$td(HTML(cell), align = "left",
                      style = "vertical-align:middle")
            )
          } else {
            table_row %+=% list(make_tbl_cell(cell))
          }
        } else if (colnames(x)[co] == trs("graph")) {
          table_row %+=% list(
            tags$td(HTML(cell), align = "left", 
                    style = paste0("vertical-align:middle;padding:0;",
                                   "background-color:transparent"))
          )
        }
      }
      table_rows %+=% list(tags$tr(table_row))
    }
    
    if (is.infinite(format_info$max.tbl.height)) {
      dfs_table_html <-
        tags$table(
          if (!identical(colgroup, NA)) 
            colgroup,
          tags$thead(tags$tr(table_head)),
          tags$tbody(table_rows),
          class = paste(
            "table table-striped table-bordered",
            "st-table st-table-striped st-table-bordered st-multiline",
            ifelse(is.na(parent.frame()$table.classes), 
                   "", parent.frame()$table.classes)
          )
        )
    } else {
      dfs_table_html <-
        tags$div(
          tags$table(
            if (!identical(colgroup, NA)) 
              colgroup,
            tags$thead(tags$tr(table_head)),
            tags$tbody(table_rows),
            class = paste(
              "table table-striped table-bordered",
              "st-table st-table-striped st-table-bordered st-multiline",
              ifelse(is.na(parent.frame()$table.classes), 
                     "", parent.frame()$table.classes)
            )
          ), style = paste0("max-height:", format_info$max.tbl.height,
                            "px;overflow-y:scroll;margin:10px 2px")
        )
    }
    
    dfs_table_html <-
      gsub(pattern = "(<th.*?>)\\s+(<strong>.*?</strong>)\\s+(</th>)",
           replacement = "\\1\\2\\3", x = dfs_table_html)
    
    # Prepare the main "div" for the html report
    div_list <- build_heading_html(format_info, data_info, method)
    
    if (length(div_list) > 0 &&
        !("shiny.tag" %in% class(div_list[[length(div_list)]]))) {
      div_list %+=% list(HTML(text = "<br/>"))
    }
    
    div_list %+=% list(HTML(text = dfs_table_html))
    
    if (parent.frame()$footnote != "") {
      fn <- conv_non_ascii(parent.frame()[["footnote"]])
      div_list %+=% list(HTML(text = fn))
    }
  }
  
  return(div_list)
}


# Build headings (pander) ------------------------------------------------------
#' @keywords internal
build_heading_pander <- function(format_info, data_info) {
  
  caller <- as.character(sys.call(-1))[1]
  head1 = NA # Main title (e.g. "Data Frame Summary")
  head2 = NA # The data frame, the variable, or the 2 variables for ctable
  head3 = NA # Additional elements (includes Variable exceptionnaly when
             # headings = FALSE and by() or lapply() were used
  
  add_markup <- function(str, h = 0) {
    if (!isTRUE(format_info$plain.ascii)) {
      if (h == 0) {
        re <- paste0("^(\\s*\\n)(.+)\\s", trs("by"), "\\s(.+)$")
        if (grepl(re, str, perl = TRUE)) {
          str <- sub(re, paste0("\\1**", "\\2** ", trs("by"), " **\\3**"), 
                     str, perl = TRUE)
        } else {
          str <- sub(pattern = "^(\\s*)(.+?)((:)\\s(.+))?\\s*$",
                     replacement = "\\1**\\2\\4** \\5",
                     x = str, perl = TRUE)
        }
      } else {
        str <- paste(paste0(rep(x = "#", times = h), collapse = ""), str)
      }
    }
    return(str)
  }
  
  append_items <- function(items, h = 0) {
    to_append <- c()
    for (item in items) {
      if (names(item) %in% names(data_info)) {
        if ((grepl(pattern = "label", names(item)) && 
             isTRUE(format_info$display.labels)) ||
            (names(item) == "Data.type" && 
             isTRUE(format_info$display.type)) ||
            !grepl("(label|Data\\.type)", names(item))) {

          if (item != "") {
            to_append <- 
              append(to_append,
                     paste0(
                       add_markup(
                         paste(item, data_info[[names(item)]], sep = ": "), h
                       ), 
                       "  \n"))
          } else {
            to_append <- 
              append(to_append,
                     paste0(
                       add_markup(
                         data_info[[names(item)]], h
                       ),
                       "  \n"))
          }
        }
      }
    }
    return(paste(to_append, collapse = ""))
  }
  
  # Special cases where no primary heading (title) is needed
  if (isTRUE(format_info$var.only)) {
    head2 <- append_items(
      list(c(Variable = "")),
      h = ifelse(isTRUE(st_options('subtitle.emphasis')), 4, 0)
    )
    head2 <- paste0("\n", enc2native(head2))
    
    if (isTRUE(format_info$headings)) {
      head3 <- append_items(list(c(Variable.label = trs("label")),
                                 c(Data.type      = trs("type")),
                                 c(N.obs          = trs("n"))))
    }

    if (!is.na(head3)) {
      head3 <- enc2native(head3)
    }
    
    tmp <- list(head2, head3)
    return(tmp[which(!is.na(tmp))])
    
  } else if (isTRUE(format_info$group.only)) {
    if (isTRUE(format_info$headings)) {
      head3 <- append_items(list(c(Group = trs("group")),
                                 c(N.Obs = trs("n")),
                                 c(Dimensions = trs("dimensions")),
                                 c(Duplicates = trs("duplicates"))))
    } else {
      head3 <- append_items(list(c(Group = trs("group"))))
    }
    
    head3[[1]] <- paste0("\n", enc2native(head3[[1]]))
    return(list(head3))
    
  } else if (!isTRUE(format_info$headings)) {
    if ("var.only" %in% names(format_info)) {
      head2 <- append_items(
        list(c(Variable = "")),
        h = ifelse(isTRUE(st_options('subtitle.emphasis')), 4, 0))
      return(list(enc2native(head2)))
    } else if ("Group" %in% names(data_info)) {
      head3 <- append_items(list(c(Group = trs("group"))))
      return(list(enc2native(head3)))
    } else {
      return(list())
    }
  }

  # Regular cases - Build the 3 heading elementss
  if (caller == "print_freq") {
    head1 <- paste(add_markup(ifelse("Weights" %in% names(data_info),
                                     trs("title.freq.weighted"),
                                     trs("title.freq")), h = 3), " \n")

    if ("Variable" %in% names(data_info)) {
      head2 <- append_items(
        list(c(Variable = "")),
        h = ifelse(isTRUE(st_options("subtitle.emphasis")), 4, 0)
      )
      
      head3 <- append_items(list(c(Variable.label = trs("label")),
                                 c(Data.type      = trs("type")),
                                 c(Weights        = trs("weights")),
                                 c(Group          = trs("group"))))
        
    }
    
  } else if (caller == "print_ctable") {
    head1 <- paste(
      add_markup(
        switch(data_info$Proportions,
               Row    = paste(trs("title.ctable"), trs("title.ctable.row"), 
                              sep = ", "),
               Column = paste(trs("title.ctable"), trs("title.ctable.col"), 
                              sep = ", "),
               Total  = paste(trs("title.ctable"), trs("title.ctable.tot"), 
                              sep = ", "),
               None   = trs("title.ctable")), 
        h = 3), 
      " \n")
    
    head2 <- append_items(
      list(c(Row.x.Col = "")),
      h = ifelse(isTRUE(st_options("subtitle.emphasis")), 4, 0)
    )
    head3 <- append_items(list(c(Data.frame       = trs("data.frame")),
                               c(Data.frame.label = trs("label")),
                               c(Group            = trs("group"))))
    
  } else if (caller == "print_descr") {
    head1 <- paste(add_markup(ifelse("Weights" %in% names(data_info),
                                     trs("title.descr.weighted"), 
                                     trs("title.descr")), h = 3), " \n")

    if ("by_var_special" %in% names(data_info)) {
      head2 <- paste(
        add_markup(
          paste(data_info$Variable, trs("by"), data_info$by_var_special),
          h = ifelse(isTRUE(st_options("subtitle.emphasis")), 4, 0)),
        " \n")
      head3 <- append_items(list(c(Data.frame     = trs("data.frame")),
                                 c(Variable.label = trs("label")),
                                 c(Weights        = trs("weights")),
                                 c(Group          = trs("group")),
                                 c(N.Obs          = trs("n"))))

    } else if ("Variable" %in% names(data_info)) {
      head2 <- append_items(
        list(c(Variable = "")),
        h = ifelse(isTRUE(st_options("subtitle.emphasis")), 4, 0)
      )
      head3 <- append_items(list(c(Variable.label = trs("label")),
                                 c(Weights        = trs("weights")),
                                 c(Group          = trs("group")),
                                 c(N.Obs          = trs("n"))))
      
    } else if ("Data.frame" %in% names(data_info)) {
      head2 <- append_items(
        list(c(Data.frame = "")),
        h = ifelse(isTRUE(st_options("subtitle.emphasis")), 4, 0)
      )
      head3 <- append_items(list(c(Data.frame.label = trs("label")),
                                 c(Weights          = trs("weights")),
                                 c(Group            = trs("group")),
                                 c(N.Obs            = trs("n"))))
      
    }
  } else if (caller == "print_dfs") {
    head1 <- paste(add_markup(trs("title.dfSummary"), h = 3), " \n")
    if ("Data.frame" %in% names(data_info)) {
      head2 <- append_items(
        list(c(Data.frame = "")),
        h = ifelse(isTRUE(st_options("subtitle.emphasis")), 4, 0)
      )
    }
    head3 <- append_items(list(c(Data.frame.label = trs("label")),
                               c(Group            = trs("group")),
                               c(Dimensions       = trs("dimensions")),
                               c(Duplicates       = trs("duplicates"))))
  }
  
  if (!is.na(head1))
    head1 <- enc2native(head1)
  if (!is.na(head2))
    head2 <- enc2native(head2)
  if (!is.na(head3))
    head3 <- enc2native(head3)
  
  tmp <- list(head1, head2, head3)
  return(tmp[which(!is.na(tmp))])
}

# Build headings (html) --------------------------------------------------------
#' @keywords internal
#' @import htmltools
build_heading_html <- function(format_info, data_info, method) {

  caller <- as.character(sys.call(-1))[1]
  head1  <- NA # uses h3()
  head2  <- NA # uses h4() or <strong> (see option subtitle.emphasis)
  head3  <- NA # uses <strong>...</strong>
  
  append_items <- function(items) {
    to_append_html <- character()
    for (item in items) {
      if (names(item) %in% names(data_info)) {
        if ((grepl(pattern = "label", names(item)) && 
             isTRUE(format_info$display.labels)) ||
            (names(item) == "Data.type" && 
             isTRUE(format_info$display.type)) ||
            !grepl("(label|Data\\.type)", names(item))) {
          
          div_str_item <-
            paste(paste0("<strong>", HTML(conv_non_ascii(item)), "</strong>"),
                  ifelse(is.character(data_info[[names(item)]]),
                         conv_non_ascii(data_info[[names(item)]]),
                         data_info[[names(item)]]),
                  sep = ": ")
          
          if (identical(to_append_html, character())) {
            to_append_html <- div_str_item
          } else {
            to_append_html <- paste(to_append_html,
                                    div_str_item,
                                    sep = "\n  <br/>")
          }
        }
      }
    }
    
    if (identical(to_append_html, character())) {
      return(NA)
    }
    
    return(HTML(to_append_html))
  }
  
  # Special cases where no primary heading (title) is needed
  if (isTRUE(format_info$var.only)) {
    if (!isTRUE(format_info$headings)) {
      return(list())
    } else {
      if ("Variable" %in% names(data_info)) {
        if (isTRUE(st_options("subtitle.emphasis"))) {
          head2 <- h4(HTML(conv_non_ascii(data_info$Variable)))
        } else {
          head2 <- strong(HTML(conv_non_ascii(data_info$Variable)), br())
        }
      }
      
      head3 <- append_items(list(c(Variable.label = trs("label")),
                                 c(Data.type      = trs("type"))))
      return(list(head2, head3))
    }
  } else if (isTRUE(format_info$group.only)) {
    if (isTRUE(format_info$headings)) {
      head3 <- append_items(list(c(Group      = trs("group")),
                                 c(N.Obs      = trs("n")),
                                 c(Dimensions = trs("dimensions")),
                                 c(Duplicates = trs("duplicates"))))
    } else {
      head3 <- append_items(list(c(Group = trs("group"))))
    }    
    return(list(head3))
  } else if (!isTRUE(format_info$headings)) {
    if ("Group" %in% names(data_info)) {
      head3 <- append_items(list(c(Group = trs("group"))))
      return(list(head3))
    } else {
      return(list())
    }
  }

  # Regular cases - Build the 3 heading elements
  if (caller == "print_freq") {
    head1 <- h3(HTML(conv_non_ascii(ifelse("Weights" %in% names(data_info),
                                           trs("title.freq.weighted"), 
                                           trs("title.freq")))))

    if ("Variable" %in% names(data_info)) {
      if (isTRUE(st_options("subtitle.emphasis"))) {
        head2 <- h4(HTML(conv_non_ascii(data_info$Variable)))
      } else {
        head2 <- strong(HTML(conv_non_ascii(data_info$Variable)), br())
      }
    }
    
    if ("var.only" %in% names(format_info)) {
      head3 <- append_items(list(c(Variable.label = trs("label")),
                                 c(Data.type      = trs("type"))))
    } else {
      head3 <- append_items(list(c(Variable.label = trs("label")),
                                 c(Data.type      = trs("type")),
                                 c(Weights        = trs("weights")),
                                 c(Group          = trs("group"))))
    }    
  } else if (caller == "print_ctable") {
    
    head1 <- switch(data_info$Proportions,
                    Row    = paste(trs("title.ctable"), trs("title.ctable.row"), 
                                   sep = ", "),
                    Column = paste(trs("title.ctable"), trs("title.ctable.col"), 
                                   sep = ", "),
                    Total  = paste(trs("title.ctable"), trs("title.ctable.tot"), 
                                   sep = ", "),
                    None   = trs("title.ctable"))
    
    head1 <- h3(HTML(conv_non_ascii(head1)))
    
    if ("Row.x.Col" %in% names(data_info)) {
      if (isTRUE(st_options("subtitle.emphasis"))) {
        head2 <- h4(HTML(conv_non_ascii(data_info$Row.x.Col)))
      } else {
        head2 <- strong(HTML(conv_non_ascii(data_info$Row.x.Col)), br())
      }
    }
    
    head3 <- append_items(list(c(Data.frame       = trs("data.frame")),
                               c(Data.frame.label = trs("label")),
                               c(Group            = trs("group"))))
    
  } else if (caller == "print_descr") {
    
    head1 <- h3(HTML(conv_non_ascii(ifelse("Weights" %in% names(data_info),
                                           trs("title.descr.weighted"), 
                                           trs("title.descr")))))
    
    if ("by_var_special" %in% names(data_info)) {
      if (isTRUE(st_options("subtitle.emphasis"))) {
        head2 <- HTML(paste0("<h4>", conv_non_ascii(data_info$Variable),
                             conv_non_ascii(trs("by")),
                             conv_non_ascii(data_info$by_var_special),
                             "</h4>"))
      } else {
        head2 <- HTML(paste0("<strong>", conv_non_ascii(data_info$Variable),
                             "</strong> ", conv_non_ascii(trs("by")), " <strong>",
                             conv_non_ascii(data_info$by_var_special),
                             "</strong><br/>"))
      } 
      
      head3 <- append_items(list(c(Data.frame     = trs("data.frame")),
                                 c(Variable.label = trs("label")),
                                 c(Weights        = trs("weights")),
                                 c(Group          = trs("group")),
                                 c(N.Obs          = trs("n"))))
      
    } else if ("Variable" %in% names(data_info)) {
      if (isTRUE(st_options("subtitle.emphasis"))) {
        head2 <- h4(HTML(conv_non_ascii(data_info$Variable)))
      } else {
        head2 <- strong(HTML(conv_non_ascii(data_info$Variable)), br())
      }
      
      head3 <- append_items(list(c(Variable.label = trs("label")),
                                 c(Weights        = trs("weights")),
                                 c(Group          = trs("group")),
                                 c(N.Obs          = trs("n"))))
    } else {
      
      if ("Data.frame" %in% names(data_info)) {
        if (isTRUE(st_options("subtitle.emphasis"))) {
          head2 <- h4(HTML(conv_non_ascii(data_info$Data.frame)))
        } else {
          head2 <- strong(HTML(conv_non_ascii(data_info$Data.frame)), br())
        }
      }
      
      head3 <- append_items(list(c(Data.frame.label = trs("label")),
                                 c(Weights          = trs("weights")),
                                 c(Group            = trs("group")),
                                 c(N.Obs            = trs("n"))))
    }
    
  } else if (caller == "print_dfs") {
    
    head1 <- h3(HTML(conv_non_ascii(trs("title.dfSummary"))))
    
    if ("Data.frame" %in% names(data_info)) {
      if (isTRUE(st_options("subtitle.emphasis"))) {
        head2 <- h4(HTML(conv_non_ascii(data_info$Data.frame)))
      } else {
        head2 <- strong(HTML(conv_non_ascii(data_info$Data.frame)), br())
      }
    }
    
    head3 <- append_items(list(c(Data.frame.label = trs("label")),
                               c(Group            = trs("group")),
                               c(Dimensions       = trs("dimensions")),
                               c(Duplicates       = trs("duplicates"))))
  }
  
  tmp <- list(head1, head2, head3)
  return(tmp[which(!is.na(tmp))])
}

