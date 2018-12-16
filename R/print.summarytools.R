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

# Prepare freq objects for printing --------------------------------------------
#' @import htmltools
print_freq <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  if (!isTRUE(format_info$report.nas)) {
    x[nrow(x), 1] <- x[nrow(x), 1] - x[nrow(x) -1, 1]
    x <- x[-(nrow(x)-1),1:3]
    colnames(x) <- c(trs('freq'), "%", trs('pct.cum'))
  }
  
  if (!isTRUE(format_info$totals)) {
    x <- x[-nrow(x),]
  }
  
  if(method=="pander") {
    
    # freq -- pander method ----------------------------------------------------
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
      freq_table[nrow(freq_table) -
                   as.numeric(isTRUE(format_info$totals)), 3] <- NA
    }
    
    # Remove .00 digits in Freq column when weights are not used
    if (!"Weights" %in% names(data_info))
      freq_table[ ,1] <- sub("\\.0+", "", freq_table[ ,1])
    
    
    # Escape "<" and ">" when used in pairs in rownames
    if (!isTRUE(format_info$plain.ascii)) {
      row.names(freq_table) <- gsub(pattern = "\\<(.*)\\>", 
                                    replacement = "\\\\<\\1\\\\>",
                                    x = row.names(freq_table), perl = TRUE)
    }
    
    # set encoding to native to allow proper display of accentuated characters
    if (parent.frame()$file == "") {
      row.names(freq_table) <- enc2native(row.names(freq_table))
    }
    
    pander_args <- append(list(style        = format_info$style,
                               plain.ascii  = format_info$plain.ascii,
                               justify      = justify,
                               missing      = format_info$missing,
                               split.tables = Inf),
                          attr(x, "user_fmt"))
    
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
    
    # freq -- html method ------------------------------------------------------
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
          table_row %+=% list(tags$th(row.names(x)[ro]))
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
    
    if (isTRUE(format_info$report.nas)) {
      table_head[[1]] <- list(tags$th("", colspan = 2),
                              tags$th(trs('valid'), colspan = 2),
                              tags$th(trs('total'), colspan = 2))
      table_head[[2]] <- list(tags$th(sub("^.*\\$(.+)$", "\\1", 
                                          data_info$Variable)),
                              tags$th(HTML(trs('freq'))),
                              tags$th("%"),
                              tags$th(HTML(trs('pct.cum'))),
                              tags$th("%"),
                              tags$th(HTML(trs('pct.cum'))))
      
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
      
      # no reporting of missing values (NA)
      table_head <- list(tags$th(data_info$Variable),
                         tags$th(trs('freq')),
                         tags$th("%"),
                         tags$th(HTML(trs('pct.cum'))))
      
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
      fn <- conv_non_ascii(parent.frame()[['footnote']])
      div_list %+=% list(HTML(text = fn))
    }
  }
  
  return(div_list)
}

# Prepare ctable objects for printing ------------------------------------------
#' @import htmltools
#' @keywords internal
print_ctable <- function(x, method) {
  
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
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  if (!isTRUE(format_info$totals)) {
    x$cross_table <-
      x$cross_table[which(rownames(x$cross_table) != trs('total')), 
                    which(colnames(x$cross_table) != trs('total'))]
    if (data_info$Proportions != "None") {
      x$proportions <- 
        x$proportions[which(rownames(x$proportions) != trs('total')), 
                      which(colnames(x$proportions) != trs('total'))]
    }
  }
  
  if(data_info$Proportions %in% c("Row", "Column", "Total")) {
    cross_table <- align_numbers(x$cross_table, x$proportions)
  } else {
    cross_table <- x$cross_table
  }
  
  justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                    l = "left",
                    c = "center",
                    r = "right")
  
  # ctable -- pander section ---------------------------------------------------
  if(method == "pander") {
    
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
                          attr(x, "user_fmt"))
    
    main_sect <- build_heading_pander(format_info, data_info)
    
    main_sect %+=%
      paste(
        capture.output(
          do.call(pander, append(pander_args, 
                                 list(x = quote(ftable(cross_table)))))
        ),
        collapse = "\n")
    
    if (isTRUE(format_info$headings) && format_info$style != 'grid') {
      main_sect[[length(main_sect)]] <- sub("^\n", "\n\n", 
                                            main_sect[[length(main_sect)]])
    }
    
    if (isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect)]])
    }
    
    return(main_sect)
    
  } else {
    
    # ctable -- html section ---------------------------------------------------
    dnn <- names(dimnames(cross_table))
    
    table_head <- list()
    table_rows <- list()
    
    table_head[[1]] <- 
      list(tags$th(""), tags$th(dnn[2], colspan = ncol(cross_table) - 
                                  as.numeric(isTRUE(format_info$totals))))
    
    if (isTRUE(format_info$totals)) {
      table_head[[1]][[3]] <- tags$th("")
    }
    
    table_head[[2]] <-list(tags$td(tags$strong(dnn[1]), align = "center"))
    
    for(cn in colnames(cross_table)) {
      if (nchar(cn) > 12) {
        cn <- smart_split(cn, 12)
      }
      cn <- sub("<", "&lt;", cn, fixed = TRUE)
      cn <- sub(">", "&gt;", cn, fixed = TRUE)
      table_head[[2]][[length(table_head[[2]]) + 1]] <- 
        tags$th(HTML(conv_non_ascii(cn)), align = "center")
    }
    
    table_rows <- list()
    for (ro in seq_len(nrow(cross_table))) {
      table_row <- list()
      for (co in seq_len(ncol(cross_table))) {
        if (co == 1) {
          table_row %+=%
            list(tags$td(tags$strong(row.names(cross_table)[ro]), 
                         align = "center"))
        }
        
        # No proportions
        if (length(x$proportions) == 0) {
          cell <- cross_table[ro,co]
          table_row %+=% list(tags$td(tags$span(cell)))
        } else {
          # With proportions
          cell <- sub("\\( *", "("     , cross_table[ro,co])
          cell <- sub(" *\\)", ")"     , cell)
          cell <- gsub(" "   , "&nbsp;", cell)
          cell <- sub("%"    , "&#37;" , cell, fixed = TRUE)
          
          table_row %+=% list(tags$td(tags$span(HTML(cell))))
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
      fn <- conv_non_ascii(parent.frame()[['footnote']])
      div_list %+=% list(HTML(text = fn))
    }
  }
  
  return(div_list)
}

# Prepare descr objects for printing
#' @import htmltools
#' @keywords internal
print_descr <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  if(!isTRUE(parent.frame()$silent) && !isTRUE(format_info$group.only) && 
     (!"by.first" %in% names(data_info) || 
      isTRUE(as.logical(data_info$by.last))) &&
     "ignored" %in% names(attributes(x))) {
    message("Non-numerical variable(s) ignored: ", attr(x, "ignored"))
  }
  
  justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                    l = "left",
                    c = "center",
                    r = "right")
  
  if(method=="pander") {
    
    # descr -- pander method ---------------------------------------------------
    # Format numbers (avoids inconsistencies with pander rounding digits)
    x <- format(round(x, format_info$round.digits),
                nsmall = format_info$round.digits)
    
    pander_args <- append(list(style        = format_info$style,
                               plain.ascii  = format_info$plain.ascii,
                               split.tables = format_info$split.tables,
                               justify      = justify),
                          attr("x", "user_fmt"))
    
    main_sect <- build_heading_pander(format_info, data_info)  
    
    main_sect %+=%
      paste(
        capture.output(
          do.call(pander, append(pander_args, list(x = quote(x))))
        ),
        collapse = "\n")
    
    if(isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect)]])
    }
    
    return(main_sect)
    
  } else {
    
    # descr -- html method -----------------------------------------------------
    
    if ("byvar" %in% names(data_info) && !data_info$transpose) {
      table_head <- list()
      table_head[[1]] <- list(tags$th(""),
                              tags$th(data_info$byvar,
                                      colspan = ncol(x)))
      
      table_head[[2]] <-  list(tags$th()) 
      
      for(cn in colnames(x)) {
        if (nchar(cn) > 12) {
          cn <- smart_split(cn, 12)
        }
        cn <- sub("<", "&lt;", cn, fixed = TRUE)
        cn <- sub(">", "&gt;", cn, fixed = TRUE)
        table_head[[2]][[length(table_head[[2]]) + 1]] <- 
          tags$th(HTML(cn), align = "center")
      } 
      
    } else {
      
      table_head <- list(tags$th(""))
      
      for(cn in colnames(x)) {
        if (nchar(cn) > 12) {
          cn <- smart_split(cn, 12)
        }
        table_head %+=% list(tags$th(HTML(cn), align = "center"))
      }
    }
    
    table_rows <- list()
    for (ro in seq_len(nrow(x))) {
      table_row <- list(tags$td(tags$strong(rownames(x)[ro])))
      for (co in seq_len(ncol(x))) {
        # cell is NA
        if (is.na(x[ro,co])) {
          table_row %+=% list(tags$td(format_info$missing))
        } else if ((rownames(x)[ro] == trs('n.valid') || 
                    colnames(x)[co] == trs('n.valid')) && 
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
    
    if ("byvar" %in% names(data_info) && !isTRUE(data_info$transpose)) {
      descr_table_html <-
        tags$table(
          tags$thead(
            tags$tr(table_head[[1]]),
            tags$tr(table_head[[2]])
          ),
          tags$tbody(
            table_rows
          ),
          class = paste(
            "table table-bordered table-striped",
            "st-table st-table-bordered st-table-striped st-freq-table",
            "st-descr-table",
            ifelse(is.na(parent.frame()$table.classes), "", 
                   parent.frame()$table.classes))
        )
      
    } else {
      
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
    }
    
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
                             replacement = '\\1\\2',
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
      fn <- conv_non_ascii(parent.frame()[['footnote']])
      div_list %+=% list(HTML(text = fn))
    }
  }
  
  return(div_list)
}

#' @import htmltools
#' @keywords internal
print_dfs <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  # Remove Var number ("No") column if specified in call to print/view
  if (trs('no') %in% names(x) && 
      "varnumbers" %in% names(format_info) && 
      !isTRUE(format_info$varnumbers)) {
    x <- x[ ,-which(names(x) == trs('no'))]
  }
  
  # Remove Label column if specified in call to print/view
  if (trs("label") %in% names(x) && 
      "Labels.col" %in% names(format_info) && 
      !isTRUE(format_info$labels.col)) {
    x <- x[ ,-which(names(x) == trs('label'))]
  }
  
  # Remove Valid column if specified in call to print/view
  if (trs('valid') %in% names(x) && 
      "valid.col" %in% names(format_info) && 
      !isTRUE(format_info$valid.col)) {
    x <- x[ ,-which(names(x) == trs('valid'))]
  }
  
  # Remove Missing column if specified in call to print/view
  if (trs('missing') %in% names(x) && 
      "na.col" %in% names(format_info) && 
      !isTRUE(format_info$na.col)) {
    x <- x[ ,-which(names(x) == trs('missing'))]
  }
  
  # pander section -------------------------------------------------------------  
  if (method == "pander") {
    
    # remove html graphs
    if (trs("graph") %in% names(x)) {
      x <- x[ ,-which(names(x) == trs("graph"))]
    }
    
    # Remove graph if specified in call to print/view
    if (trs('text.graph') %in% names(x) && 
        "graph.col" %in% names(format_info) &&
        !isTRUE(format_info$graph.col)) {
      x <- x[ ,-which(names(x) == trs('text.graph'))]
    }
    
    # Check that style is not 'simple'
    if (isTRUE(format_info$style == 'simple')) {
      format_info$style <- 'multiline'
    }
    
    if (!isTRUE(format_info$plain.ascii)) {
      # Escape symbols for words between <>'s to allow <NA> or factor
      # levels such as <ABC> to be rendered correctly
      if(trs("label") %in% names(x)) {
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
      
      # Remove txt histograms b/c not supported in rmarkdown (for now)
      if (trs("text.graph") %in% names(x)) {
        x[[trs("text.graph")]][which(grepl('[:.]', 
                                           x[[trs("text.graph")]]))] <- ""
      }
    }
    
    pander_args <- append(list(style            = format_info$style,
                               plain.ascii      = format_info$plain.ascii,
                               justify          = format_info$justify,
                               split.cells      = format_info$split.cells,
                               split.tables     = format_info$split.tables,
                               keep.line.breaks = TRUE),
                          attr(x, "user_fmt"))
    
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
    
    # html section -------------------------------------------------------------
    
    # remove text graph
    if (trs("text.graph") %in% names(x)) {
      x <- x[ ,-which(names(x) == trs("text.graph"))]
    }
    
    # Remove graph if specified in call to print/view
    if (trs("graph") %in% names(x) && 
        "graph.col" %in% names(format_info) &&
        !isTRUE(format_info$graph.col)) {
      x <- x[ ,-which(names(x) == trs("graph"))]
    }
    
    table_head <- list()
    for(cn in colnames(x)) {
      if (cn %in% c(trs("no"), trs("valid"), trs("missing"))) {
        table_head %+=% list(tags$th(tags$strong(cn), align = "center"))
      } else {
        table_head %+=% list(tags$th(tags$strong(cn), align = "center"))
      }
    }
    
    table_rows <- list()
    for (ro in seq_len(nrow(x))) {
      table_row <- list()
      for (co in seq_len(ncol(x))) {
        cell <- x[ro,co]
        cell <- gsub('\\\\\n', '\n', cell)
        if (colnames(x)[co] %in% c(trs("no"), trs("valid"), trs("missing"))) {
          table_row %+=% list(tags$td(HTML(conv_non_ascii(cell)), 
                                      align = "center"))
        } else if (colnames(x)[co] == trs("label")) {
          cell <- gsub('(\\d+)\\\\\\.', '\\1.', cell)
          cell <- paste(strwrap(cell, width = format_info$split.cells, 
                                simplify = TRUE), collapse = "\n")
          table_row %+=% list(tags$td(HTML(conv_non_ascii(cell)), 
                                      align = "left"))
        } else if (colnames(x)[co] %in% 
                   c(trs("variable"), trs("stats.values"))) {
          cell <- gsub('(\\d+)\\\\\\.', '\\1.', cell)
          table_row %+=% list(tags$td(HTML(conv_non_ascii(cell)),
                                      align = "left"))
        } else if (colnames(x)[co] == trs("freqs.pct.valid")) {
          cell <- gsub("\\\\", " ", cell)
          cell <- gsub(" *(\\d|\\:)", "\\1", cell)
          cell <- gsub("\\:", " : ", cell)
          table_row %+=% list(tags$td(HTML(conv_non_ascii(cell)),
                                      align = "left"))
        } else if (colnames(x)[co] == trs("graph")) {
          table_row %+=% list(tags$td(HTML(conv_non_ascii(cell)), 
                                      align = "center", border = "0"))
        }
      }
      table_rows %+=% list(tags$tr(table_row))
    }
    
    dfs_table_html <-
      tags$table(
        tags$thead(tags$tr(table_head)),
        tags$tbody(table_rows),
        class = paste(
          "table table-striped table-bordered",
          "st-table st-table-striped st-table-bordered st-multiline",
          ifelse(is.na(parent.frame()$table.classes), 
                 "", parent.frame()$table.classes)
        )
      )
    
    dfs_table_html <-
      gsub(pattern = '(<th.*?>)\\s+(<strong>.*?</strong>)\\s+(</th>)',
           replacement = "\\1\\2\\3", x = dfs_table_html)
    
    # Prepare the main "div" for the html report
    div_list <- build_heading_html(format_info, data_info, method)
    
    if (length(div_list) > 0 &&
        !("shiny.tag" %in% class(div_list[[length(div_list)]]))) {
      div_list %+=% list(HTML(text = "<br/>"))
    }
    
    div_list %+=% list(HTML(text = dfs_table_html))
    
    if (parent.frame()$footnote != "") {
      fn <- conv_non_ascii(parent.frame()[['footnote']])
      div_list %+=% list(HTML(text = fn))
    }
  }
  
  return(div_list)
}
