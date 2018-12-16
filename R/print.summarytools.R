#' Print Method for Objects of Class \code{summarytools}.
#'
#' Display \code{summarytools} objects in the console, in Web Browser or in
#'  \emph{RStudio}'s Viewer, or write content to file.
#'
#' @aliases print view
#'
#' @usage
#'  \method{print}{summarytools}(x, method = "pander", file = "",
#'    append = FALSE, report.title = NA, table.classes = NA,
#'    bootstrap.css = st_options('bootstrap.css'), 
#'    custom.css = st_options('custom.css'), silent = FALSE, 
#'    footnote = st_options('footnote'), 
#'    escape.pipe = st_options('escape.pipe'), \dots)
#'
#' view(x, method = "viewer", file = "", append = FALSE,
#'   report.title = NA, table.classes = NA, 
#'   bootstrap.css = st_options('bootstrap.css'), 
#'   custom.css = st_options('custom.css'), silent = FALSE, 
#'   footnote = st_options('footnote'), 
#'   escape.pipe = st_options('escape.pipe'), \dots)
#'    
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
#'   document is opened with default Web Browser.
#'
#' @details
#'   Plain ascii and \emph{rmarkdown} tables are generated via
#'   \code{\link[pander]{pander}}. See \emph{References} section
#'   for a list of all available \emph{pander} options.
#'
#' To print objects of class \dQuote{by}, use \code{\link{view}}. This
#'   function also makes it more practical to generate \emph{html} files (see
#'   examples).
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
#'      \item \code{Dataframe}
#'      \item \code{Dataframe.label}
#'      \item \code{Variable}
#'      \item \code{Variable.label}
#'      \item \code{Group}
#'      \item \code{date}
#'      \item \code{Weights} (\code{\link{freq}} & \code{\link{descr}} objects)
#'      \item \code{Data.type} (\code{\link{freq}} objects)
#'      \item \code{Row.variable} (\code{\link{ctable}} objects)
#'      \item \code{Col.variable} (\code{\link{ctable}} objects)
#'      \item \code{Row.variable.label} (\code{\link{ctable}} objects)
#'      \item \code{Col.variable.label} (\code{\link{ctable}} objects)
#'    }
#'
#' @references
#' \href{http://rstudio.com}{Rstudio}
#' \href{https://github.com/dcomtois/summarytools}{Summarytools on Github}
#' \href{http://rapporter.github.io/pander/#general-options}{List of pander options on Github}
#' \href{http://getbootstrap.com/css/#tables}{Bootstrap Cascading Stylesheets}
#'
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#'
#' @seealso
#' \code{\link[pander]{pander}}
#'
#' @examples
#'   \dontrun{
#'   data(tobacco)
#'   view(dfSummary(tobacco), footnote = NA)
#'   }
#'   data(exams)
#'   print(freq(exams$gender), style = 'rmarkdown')
#'   print(descr(exams), headings = FALSE)
#'
#' @keywords print methods
#'
#' @export
#' @import htmltools
#' @importFrom pander pander panderOptions
#' @importFrom utils capture.output packageVersion head
#' @importFrom checkmate test_logical test_path_for_output test_choice
#'             test_string check_file_exists
print.summarytools <- function(x, method = "pander", file = "", append = FALSE,
                               report.title = NA, table.classes = NA, 
                               bootstrap.css = st_options('bootstrap.css'),
                               custom.css = st_options('custom.css'), 
                               silent = FALSE, 
                               footnote = st_options('footnote'), 
                               escape.pipe = st_options('escape.pipe'), ...) {

  knitr.auto.asis.value <- panderOptions("knitr.auto.asis")
  panderOptions("knitr.auto.asis", FALSE)
  
  on.exit(panderOptions("knitr.auto.asis",knitr.auto.asis.value))
  
  dotArgs <- list(...)
  
  # Recup arguments from view() if present -------------------------------------
  if ("open.doc" %in% names(dotArgs)) {
    open.doc <- eval(dotArgs[["open.doc"]])
  } else {
    open.doc <- FALSE
  }

  if ("group.only" %in% names(dotArgs)) {
    attr(x, "formatting")$group.only <- eval(dotArgs[["group.only"]])
  } else {
    attr(x, "formatting")$group.only <- FALSE
  }

  if ("var.only" %in% names(dotArgs)) {
    attr(x, "formatting")$var.only <- eval(dotArgs[["var.only"]])
  } else {
    attr(x, "formatting")$var.only <- FALSE
  }

  # Parameter validation -------------------------------------------------------
  errmsg <- character()
  
  method <- switch(tolower(substring(method, 1, 1)),
                   p = "pander",
                   b = "browser",
                   v = "viewer",
                   r = "render")

  if (!isTRUE(test_choice(method, 
                          c("pander", "browser", "viewer", "render")))) {
    errmsg %+=% paste("'method' must be one of 'pander', 'browser', 'viewer',",
                      "or 'render'")
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

  if (method == "pander" && !is.na(table.classes)) {
    errmsg %+=% "'table.classes' option does not apply to method 'pander'"
  }
  
  if (method == "pander" && !is.na(custom.css)) {
    errmsg %+=% "'custom.css' option does not apply to method 'pander'"
  }

  if (!isTRUE(test_logical(silent, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'silent' must be either TRUE or FALSE"
  }

  if (file != "" && !isTRUE(test_path_for_output(file, overwrite = TRUE))) {
     errmsg %+=% "'file' path is not valid - check that directory exists"
  }
  
  if (file != "" && 
      grepl(pattern = "\\.html$", x = file, ignore.case = TRUE, perl = TRUE) &&
      !grepl(pattern = tempdir(), x = file, fixed = TRUE)) {
    method <- "viewer"
  }
  
  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "))
  }
  
  if (is.na(footnote)) {
    footnote <- ""
  }

  # Display message if list object printed with base print() method with pander
  if (method == "pander" && 
      (identical(deparse(sys.calls()[[sys.nframe()-1]][2]), "x[[i]]()") ||
       any(grepl(pattern = "fn_call = FUN(x = X[[i]]", 
                x = deparse(sys.calls()[[sys.nframe()-1]]), fixed = TRUE)))) {
    msg <- paste("For best results printing list objects with summarytools,",
                 "use view(x, method = 'pander')")
    if (.st_env$last.message$msg != msg || 
        Sys.time() - .st_env$last.message$time > 1) {
      .st_env$last.message$msg <- msg
      .st_env$last.message$time <- Sys.time()
      message(msg)
    }
  }
  
  # Override of x's attributes (formatting and heading info) -------------------
  if ("date" %in% names(dotArgs)) {
    attr(x, "date") <- dotArgs[["date"]]
  }
  
  # Override of formatting elements, first by looking at '...' (var dotArgs),
  # then by looking at the match.call() from x to set global parameters that 
  # were not explicit in the latter.
  # Here we check for arguments that can be specified at the function level for
  # freq, descr, ctable and dfSummary (we don't include  print/view args)
  overrided_args <- character()
  # Todo: remove 'omit.headings' in next release
  for (format_element in c("style", "plain.ascii", "round.digits",
                           "justify", "headings", "display.labels",
                           "display.type",  "varnumbers", "labels.col", 
                           "graph.col", "na.col", "valid.col", "split.tables",
                           "totals", "report.nas", "missing", "totals",
                           "omit.headings")) {
    if (format_element %in% names(dotArgs)) {
      if (format_element == 'omit.headings') {
        
        msg <- paste("'omit.headings' will disappear in future releases;",
                     "use 'headings' instead")
        if (.st_env$last.message$msg != msg || 
            Sys.time() - .st_env$last.message$time > 1) {
          .st_env$last.message$msg <- msg
          .st_env$last.message$time <- Sys.time()
          message(msg)
        }
        
        attr(x, "formatting")[['headings']] <- 
          !isTRUE(eval(dotArgs[["omit.headings"]]))
        overrided_args <- append(overrided_args, 'headings')
      } else {
        attr(x, "formatting")[[format_element]] <- dotArgs[[format_element]]
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
      if (!(format_element == 'headings' &&
            'omit.headings' %in% names(attr(x, "fn_call")))) {
        attr(x, "formatting")[[format_element]] <- st_options(format_element)
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
      attr(x, "formatting")[[format_element]] <- 
        st_options(paste0(prefix, format_element))
    }
  }

  # Override of data info attributes
  data_info_elements <- c("Dataframe", "Dataframe.label", "Variable", 
                          "Variable.label", "Data.type", "Group", "Weights",
                          "Row.variable", "Col.variable",
                          "Row.variable.label", "Col.variable.label")
  for (data_info_element in data_info_elements) {
    if (tolower(data_info_element) %in% tolower(names(dotArgs))) {
      attr(x, "data_info")[[data_info_element]] <- 
        dotArgs[grep(data_info_element, names(dotArgs), ignore.case = TRUE)]
      overrided_args <- append(overrided_args, data_info_element)
    }
  }
  
  # Add caption if present in dotArgs
  if ("caption" %in% names(dotArgs)) {
    attr(x, "user_fmt")$caption <- dotArgs$caption
  }

  # When style == 'rmarkdown', set plain.ascii to FALSE unless 
  # explicitly specified
  if (method == "pander" && attr(x, "formatting")$style == "rmarkdown" &&
      isTRUE(attr(x, "formatting")$plain.ascii) &&
      (!"plain.ascii" %in% (names(dotArgs)))) {
    attr(x, "formatting")$plain.ascii <- FALSE
  }

  # Evaluate formatting attributes that are symbols at this stage (F, T)
  for (i in seq_along(attr(x, "formatting"))) {
    if (is.symbol(attr(x, "formatting")[[i]])) {
      attr(x, "formatting")[[i]] <- eval(attr(x, "formatting")[[i]])
    }
  }
  
  # Extract formatting information from x attributes
  # format_info <- attr(x, "formatting")
  
  stpath <- find.package("summarytools")
  
  # Build footnote
  if (method %in% c("browser", "viewer", "render") && footnote == "default") {
    footnote <- 
      paste0(
        "<p>", conv_non_ascii(trs('generated.by')),
        " <a href='https://github.com/dcomtois/summarytools'>",
        "summarytools</a> ", packageVersion(pkg = "summarytools"),
        " (<a href='https://www.r-project.org/'>R</a> ", trs('version'), ' ', 
        getRversion(), ")", "<br/>", strftime(Sys.Date(),trs('date.fmt')),"</p>"
      )
  }

  # Concatenate data frame + $ + variable name where appropriate
  if (!("Variable" %in% overrided_args) && 
      length(attr(x, "data_info")$Variable) == 1 && 
      length(attr(x, "data_info")$Dataframe) == 1) {
    attr(x, "data_info")$Variable <- paste(attr(x, "data_info")$Dataframe,
                                           attr(x, "data_info")$Variable, 
                                           sep = "$")
  }
  
  # Dispatch to the right function for preparing output ------------------------
  if (attr(x, "st_type") == "freq") {
    res <- print_freq(x, method)
    if (is.na(report.title)) {
      if (!("Weights" %in% names(attr(x, "data_info")))) {
        report.title <- trs('title.freq')
      } else {
        report.title <- trs('title.freq.weighted')
      }
    }
  } else if (attr(x, "st_type") == "ctable") {
    res <- print_ctable(x, method)
    if (is.na(report.title)) {
      report.title <- trs('title.ctable')
    }
  } else if (attr(x, "st_type") == "descr") {
    res <- print_descr(x, method)
    if (is.na(report.title)) {
      if (!("Weights" %in% names(attr(x, "data_info")))) {
        report.title <- trs('title.descr')
      } else {
        report.title <- trs('title.descr.weighted')
      }
    }
  } else if(attr(x, "st_type") == "dfSummary") {
    res <- print_dfs(x, method)
    if (is.na(report.title)) {
      report.title <- trs('title.dfSummary')
    }
  }

  # Print or write to file - pander --------------------------------------------
  if (method == "pander") {

    # remove initial linefeed if headings is FALSE
    if (!isTRUE(attr(x, "formatting")$headings)) {
      res[[1]] <- sub("^\\n\\n", "\n", res[[1]])
    }

    cat(do.call(paste, res), file = file, append = append)
    
    if (file != "" && !isTRUE(silent)) {
      if (isTRUE(append))
        message(paste0("Output file appended: ", file))
      else
        message(paste0("Output file written: ", file))
      return(invisible())
    }

  } else {

    # Print or write to file - html --------------------------------------------
    
    if (isTRUE(append)) {
      f <- file(file, open = "r", encoding = "utf-8")
      html_content_in <- paste(readLines(f, warn = FALSE, encoding = "utf-8"), 
                               collapse="\n")
      close(f)
      top_part <- sub("(^.+)(</body>.+)", "\\1", html_content_in)
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
                includeCSS(
                  path = paste(stpath, "includes/stylesheets/bootstrap.min.css",
                               sep="/")),
              includeCSS(
                path = paste(stpath, "includes/stylesheets/summarytools.css",
                             sep="/")),
              if (!is.na(custom.css)) 
                includeCSS(path = custom.css)
            ),
            res)
        
      } else {
        # method == 'render'
        html_content <-
          tags$div(
            class="container st-container",
            tags$head(
              includeCSS(path = paste(stpath, 
                                      "includes/stylesheets/summarytools.css", 
                                      sep="/")),
              if (!is.na(custom.css))
                includeCSS(path = custom.css)),
            res)
      }
    }

    if (method == "render") {
      return(html_content)
    }

    outfile_path <- ifelse(file == "", paste0(tempfile(),".html"), file)

    if (isTRUE(append)) {
      capture.output(cat(html_content, "\n"), file = outfile_path)
    } else {
      save_html(html = html_content, file = outfile_path)
    }

    if (method == "viewer") {
      if (file == "" || open.doc) {
        if(.Platform$GUI == "RStudio") {
          viewer <- getOption("viewer")
          if (!is.null(viewer)) {
            viewer(outfile_path)
          } else {
            message("To view html content in RStudio, please run ",
                    "install.packages('rstudioapi').")
            message("Switching method to 'browser'")
            method <- "browser"
          }
        } else {
          message("Method 'viewer' only valid within RStudio. Switching method",
                  "to 'browser'.")
          method <- "browser"
        }
      }
    }

    # For method "browser", we don't use utils::browseURL() because of 
    # compatibility issues with RStudio
    if (method == "browser") {
      if (file == "" || open.doc) {
        switch(Sys.info()[["sysname"]],
               Windows = {shell.exec(file = paste0("file:///", outfile_path))},
               Linux   = {system(paste("/usr/bin/xdg-open", outfile_path), 
                                 wait = FALSE, ignore.stdout = TRUE)},
               Darwin  = {system(paste("open", outfile_path), wait = FALSE, 
                                 ignore.stderr = TRUE)})
      }
    }

    # return file path and update tmpfiles vector when method = browser / viewer
    if(file == "" && method %in% c("browser", "viewer")) {
      .st_env$tmpfiles <- c(.st_env$tmpfiles, outfile_path)
      if (!silent) {
        message(paste0("Output file written: ", outfile_path))
      }
      return(invisible(normalizePath(outfile_path, winslash = "\\", 
                                     mustWork = FALSE)))
    } else if (file != "") {
      if (!silent) {
        if (isTRUE(append)) {
          message(paste0("Output file appended: ", outfile_path))
        } else {
          message(paste0("Output file written: ", outfile_path))
        }
      }
      return(invisible())
    }
  }
}
