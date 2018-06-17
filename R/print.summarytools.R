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
#' @param table.classes Character.  Additional classes to assign to output tables. 
#'   All \emph{Bootstrap CSS} classes can be used. It also allows
#'   user-defined classes (see custom.css parameter). See \emph{details} section.
#'   \code{NA} by default.
#' @param bootstrap.css Logical. Set to \code{FALSE} to omit Bootstap css. \code{TRUE} 
#'   by default. To change this default value globally, see \code{\link{st_options}}.
#' @param custom.css Path to a user-defined \emph{.css} file. Classes
#'   defined in this file can be used in the \code{table.classes}
#'   parameter. \code{NA} by default. To change this default value globally, see 
#'   \code{\link{st_options}}. 
#' @param silent Hide console messages (such as ignored variables or \code{NaN}
#'   to \code{NA} transformations).
#' @param footnote footnote in \emph{html} output. When set to \dQuote{default},
#'   this is the package name and version, R version, and current date). Has no effect
#'   when \code{method} is \dQuote{pander}. Set to \dQuote{default}, provide your own text,
#'   or set to \code{NA} to omit. To change this default value globally, see 
#'   \code{\link{st_options}}.
#' @param escape.pipe Logical. Set to \code{TRUE} when using
#'   \code{style='grid'} and \code{file} argument is supplied if the intent
#'   is to generate a text file that can be converted to other formats using
#'   \emph{Pandoc}. To change this default value globally, see 
#'   \code{\link{st_options}}.
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
#'   formatting and other attributes stored in the object to be printed. Refer to the function's
#'   documentation for details on these arguments.
#'    \itemize{
#'      \item \code{style}
#'      \item \code{round.digits} (except for \code{\link{dfSummary}} objects)
#'      \item \code{justify}
#'      \item \code{plain.ascii}
#'      \item \code{missing}
#'      \item \code{Data.type}
#'      \item \code{Subset}
#'      \item \code{Group}
#'      \item \code{Weights}
#'      \item \code{date}      
#'      \item \code{omit.headings}
#'      \item \code{split.tables}
#'      \item \code{Dataframe}
#'      \item \code{Dataframe.label}
#'      \item \code{Variable}
#'      \item \code{Variable.label}
#'      \item \code{display.labels}
#'      \item \code{display.type}
#'      \item \code{totals} (\code{\link{freq}} and \code{\link{ctable}} objects)
#'      \item \code{report.nas} (\code{\link{freq}} objects only)
#'      \item \code{Row.variable} (\code{\link{ctable}} objects only)
#'      \item \code{Col.variable} (\code{\link{ctable}} objects only)
#'      \item \code{Row.variable.subset} (\code{\link{ctable}} objects only)
#'      \item \code{Col.variable.subset} (\code{\link{ctable}} objects only)
#'      \item \code{Row.variable.label} (\code{\link{ctable}} objects only)
#'      \item \code{Col.variable.label} (\code{\link{ctable}} objects only)
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
#'   print(descr(exams), omit.headings = TRUE)
#'
#' @keywords print methods
#'
#'@export
print.summarytools <- function(x, method = "pander", file = "", append = FALSE,
                               report.title = NA, table.classes = NA, 
                               bootstrap.css = st_options('bootstrap.css'),
                               custom.css = st_options('custom.css'), silent = FALSE, 
                               footnote = st_options('footnote'), 
                               escape.pipe = st_options('escape.pipe'), ...) {

  args_list <- match.call()
  
  # Recup arguments from view() if present ------------------------------------
  
  if ("open.doc" %in% names(args_list)) {
    open.doc <- args_list[["open.doc"]]
  } else {
    open.doc <- FALSE
  }

  if ("group.only" %in% names(args_list)) {
    group.only <- args_list[["group.only"]]
  } else {
    group.only <- FALSE
  }

  if ("var.only" %in% names(args_list)) {
    var.only <- args_list[["var.only"]]
  } else {
    var.only <- FALSE
  }


  # Parameter validation -----------------------------------------------------
  
  method <- switch(tolower(substring(method, 1, 1)),
                   p = "pander",
                   b = "browser",
                   v = "viewer",
                   r = "render")

  if (!method %in% c("pander", "browser", "viewer", "render")) {
    stop("'method' must be one of 'pander', 'browser', 'viewer', or 'render'")
  }

  if (file == "" && isTRUE(append)) {
    stop("'append' is set to TRUE but no file name has been specified")
  }

  if (file != "" && isTRUE(append) && !file.exists(file)) {
    stop("'append' is set to TRUE but specified file does not exist")
  }

  if (file != "" && isTRUE(append) && !is.na(report.title)) {
    message("Appending existing file -- 'report.title' argument will be ignored")
  }

  if (file != "" && grepl(pattern = "\\.html$", x = file, ignore.case = TRUE, perl = TRUE)) {
    method <- "viewer"
  }

  if (isTRUE(group.only) && !"Group" %in% names(attr(x, "data_info"))) {
    stop("'group.only' can only be used with objects created using by()")
  }

  if (!is.na(report.title) && !is.character(report.title)) {
    stop("'report.title' must either be NA or a character string")
  }

  if (!group.only %in% c(TRUE, FALSE)) {
    stop("'group.only' must be either TRUE or FALSE")
  }

  if (!var.only %in% c(TRUE, FALSE)) {
    stop("'var.only' must be either TRUE or FALSE")
  }

  if (!escape.pipe %in% c(TRUE, FALSE)) {
    stop("'escape.pipe' must be either TRUE or FALSE")
  }

  if (!is.na(custom.css) && !file.exists(custom.css)) {
    stop("'custom.css' argument must point to an existing file.")
  }

  if ((!is.na(table.classes) || !is.na(custom.css)) && method == "pander") {
    stop("'table.classes' and 'custom.css' options do not apply to method 'pander'")
  }

  if (!silent %in% c(TRUE, FALSE)) {
    stop("'silent' must be either TRUE or FALSE")
  }

  if (is.na(footnote)) {
    footnote <- ""
  }

  # Override of x's attributes ---------------------------------------------
  if ("date" %in% names(args_list)) {
    attr(x, "date") <- args_list$date
  }
  
  # Override of formatting elements, first by looking at args_list (match.call()),
  # then by looking at the match.call() from x to set global parameters that were
  # not explicit in the latter.
  overrided_args <- character()
  for (format_element in c("style", "plain.ascii", "round.digits", "justify", 
                           "missing", "omit.headings", "split.tables",
                           "display.type", "display.labels", "totals", 
                           "report.nas")) {
    if (format_element %in% names(args_list)) {
      attr(x, "formatting")[format_element] <- args_list[format_element]
      overrided_args <- append(overrided_args, format_element)
    }
  }
  
  # Global options that apply to all types of summarytools objects
  for (format_element in c("style", "plain.ascii", "round.digits", 
                           "omit.headings", "display.labels")) {
    if (!format_element %in% c(overrided_args, names(attr(x, "fn_call")))) {
      attr(x, "formatting")[format_element] <- st_options(format_element)
    }
  }
  
  # Global options specific to one type of summarytools object
  prefix <- paste0(attr(x, "st_type"), ".")
  for (format_element in sub(prefix, "", 
                             grep(prefix, names(st_options()), value = TRUE, fixed = TRUE),
                             fixed = TRUE)) {
    if (!format_element %in% c(overrided_args, names(attr(x, "fn_call")))) {
      attr(x, "formatting")[format_element] <- st_options(paste0(prefix, format_element))
    }
  }

  # Override of data info attributes
  for (data_info_element in c("Dataframe", "Dataframe.label",
                              "Variable", "Variable.label", 
                              "Variable.labels","Data.type",
                              "Subset", "Group", "Weights",
                              "Row.variable", "Col.variable",
                              "Row.variable.subet", "Col.variable.subset",
                              "Row.variable.label", "Col.variable.label")) {
    if (data_info_element %in% names(args_list)) {
      attr(x, "data_info")[data_info_element] <- args_list[data_info_element]
    }
  }

  # When style == 'rmarkdown', set plain.ascii to FALSE unless explicitly specified
  if (method == "pander" && attr(x, "formatting")$style == "rmarkdown" &&
      isTRUE(attr(x, "formatting")$plain.ascii) &&
      (!"plain.ascii" %in% (names(args_list)))) {
    attr(x, "formatting")$plain.ascii <- FALSE
  }

  # Declare functions ---------------------------------------------

  add_hash <- function(str, h = 4) {
    #if (isTRUE(attr(x, "formatting")$plain.ascii)) {
    if (isTRUE(format_info$plain.ascii)) {
      return(str)
    } else {
      if (h > 0) {
        return(paste(paste0(rep(x = "#", times = h), collapse = ""), str))
      } else {
        str_1 <- sub(pattern = "^(.*?:)(.+)$", replacement = "\\1", x = str, perl = TRUE)
        str_2 <- sub(pattern = "^(.*?:)(.+)$", replacement = "\\2", x = str, perl = TRUE)
        return(paste0("**", str_1, "**", str_2))
      }
    }
  }

  add_head_element <- function(elements, h = 4) {
    element_added <- FALSE
    if (method == "pander") {
      for (e in elements) {
        if (e[[1]] %in% names(data_info)) {
          if (grepl(pattern = "label", e[[1]])) {
            if (isTRUE(format_info$display.labels)) {
              output[[length(output) + 1]] <<-
                paste0("  \n", add_hash(paste(e[[2]], data_info[[e[[1]]]], sep = ": "), h), "  ")
              element_added <- TRUE
            }
          } else if (grepl(pattern = "type", e[[1]])) {
            if (isTRUE(format_info$display.type)) {
              output[[length(output) + 1]] <<-
                paste0("  \n", add_hash(paste(e[[2]], data_info[[e[[1]]]], sep = ": "), h), "  ")
              element_added <- TRUE
            }
          } else {
            output[[length(output) + 1]] <<-
              paste0("  \n", add_hash(paste(e[[2]], data_info[[e[[1]]]], sep = ": "), h), "  ")
            element_added <- TRUE
          }
        }
      }
    } else {
      div_str <- ""
      for (e in elements) {
        if (e[[1]] %in% names(data_info)) {
          if (grepl(pattern = "label", e[[1]])) {
            if (isTRUE(format_info$display.labels)) {
              div_str_item <- paste(paste0("<strong>",e[[2]],"</strong>"), data_info[[e[[1]]]], sep = ": ")
            } else {
              div_str_item <- NA
            }
          } else {
            div_str_item <- paste(paste0("<strong>",e[[2]],"</strong>"), data_info[[e[[1]]]], sep = ": ")
          }

          if (!is.na(div_str_item)) {
            if (nchar(div_str) > 0) {
              div_str <- paste(div_str,
                               div_str_item,
                               sep = "\n  <br>")
            } else {
              div_str <- div_str_item
            }
          }
        }
      }
      
      if (nchar(trimws(div_str)) > 0) {
        if (h == 0) {
          div_list[[length(div_list) + 1]] <<- HTML(div_str)
        } else {
          div_list[[length(div_list) + 1]] <<- get(paste0("h", h))(HTML(div_str))
        }
        element_added <- TRUE
      }
    }
    return(element_added)
  }

  # Function to vertically align frequencies and proportions in ctables
  align_numbers <- function(counts, props) {

    round.digits <- format_info$round.digits
    res <- sapply(1:ncol(counts), function(colnum) {

      if ("Weights" %in% names(data_info)) {
        maxchar_cnt <- nchar(as.character(round(max(counts[ ,colnum]), digits = round.digits)))
        maxchar_pct <- nchar(sprintf(paste0("%.", round.digits, "f"), max(props[ ,colnum]*100)))
        return(paste(sprintf(paste0("%", maxchar_cnt, ".", round.digits, "f"),
                             counts[ ,colnum]),
                     sprintf(paste0("(%", maxchar_pct, ".", round.digits, "f%%)"),
                             props[ ,colnum]*100)))
      } else {
        maxchar_cnt <- nchar(as.character(max(counts[ ,colnum])))
        maxchar_pct <- nchar(sprintf(paste0("%.", round.digits, "f"), max(props[ ,colnum]*100)))
        return(paste(sprintf(paste0("%", maxchar_cnt, "i"),
                             counts[ ,colnum]),
                     sprintf(paste0("(%", maxchar_pct, ".", round.digits, "f%%)"),
                             props[ ,colnum]*100)))
      }
    })

    dim(res) <- dim(counts)
    dimnames(res) <- dimnames(counts)

    return(res)
  }

  # Fn to smartly split variable names that are too long
  # ref: https://tinyurl.com/y7qv48z9
  smart_split <- function(str, maxlen) {
    re <- paste0("(?=.{1,", maxlen, "}(.*))",
                 "(?=.*?[^\\W._].*?[\\W._].*?\\1)",
                 ".{1,", maxlen, "}(?<=_|\\b|\\Z)",
                 "|.{1,", maxlen, "}")
    matchinfo <- gregexpr(pattern = re,
                          text = str, perl = TRUE)
    groups <- regmatches(x = str, m = matchinfo)[[1]]
    paste(groups, collapse = "<br/>")
  }


  # Extract non-NA "data_info" elements from x attributes
  if ("data_info" %in% names(attributes(x))) {
    data_info <- attr(x, "data_info")
    # data_info <- data_info[!is.na(data_info)]
  } else {
    data_info <- NA
  }

  # Extract formatting information from x attributes
  format_info <- attr(x, "formatting")

  stpath <- find.package("summarytools")

  # Build footnote
  if (method %in% c("browser", "viewer", "render") && footnote == "default") {
      footnote <- paste0("<p>Generated by <a href='https://github.com/dcomtois/summarytools'>",
                       "summarytools</a> ",
                       packageVersion(pkg = "summarytools"),
                       " (<a href='https://www.r-project.org/'>R</a> version ", getRversion(), ")",
                       "<br/>", Sys.Date(),"</p>")
  }

  # freq objects  -----------------------------------------------------------------------------------

  if(attr(x, "st_type") == "freq") {

    sect_title <- list()

    if (!var.only) {
      if ("Weights" %in% names(data_info)) {
        sect_title[[1]] <- "Weighted Frequencies  "
      } else {
        sect_title[[1]] <- "Frequencies  "
      }
    } else {
      sect_title[[1]] <- ""
    }


    if (all(c("Variable", "Dataframe") %in% names(data_info))) {
      sect_title[[2]] <- paste(data_info$Dataframe, data_info$Variable, sep = "$")
    } else if ("Variable" %in% names(data_info)) {
      sect_title[[2]] <- data_info$Variable
    } else {
      sect_title[[2]] <-""
    }

    if (!format_info$report.nas) {
      x[nrow(x), 1] <- x[nrow(x), 1] - x[nrow(x) -1, 1]
      x <- x[-(nrow(x)-1),1:3]
      colnames(x) <- c("Freq", "%", "% Cum.")
    }
    
    if (!format_info$totals) {
      x <- x[-nrow(x),]
    }
    
    if(method=="pander") {
      
      # freq objects, method = pander -----------------------------------------------------

      output <- list()

      if (group.only ||
          ("by.first" %in% names(data_info) && 
           (!as.logical(data_info$by.first) || format_info$omit.headings))) {
        he_added <- add_head_element(list(c("Group", "Group")), h = 0)
        
      } else if (var.only) {
        
        if (!format_info$omit.headings) {

          if (sect_title[[2]] != "") {
            if (format_info$plain.ascii) {
              output[[length(output) + 1]] <- paste0("\nVariable: ", sect_title[[2]], "  ")
            } else {
              output[[length(output) + 1]] <- paste0("\n**Variable:** ", sect_title[[2]], "  ")
            }
          }
          
          he_added <- add_head_element(list(c("Variable.label", "Label"),
                                            c("Data.type", "Type")),
                                       h = 0)
        }
        
      } else {

        if (!format_info$omit.headings) {
          if (sect_title[[1]] != "") {
            output[[1]] <- add_hash(sect_title[[1]], 3)
          }
  
          if (sect_title[[2]] != "") {
            if (format_info$plain.ascii) {
              output[[length(output) + 1]] <- paste0("\n", sect_title[[2]], "  ")
            } else {
              output[[length(output) + 1]] <- paste0("\n**Variable:** ", sect_title[[2]], "  ")
            }
          }
  
          he_added <- add_head_element(list(c("Variable.label", "Label"),
                                            c("Data.type", "Type"),
                                            c("Weights", "Weights"),
                                            c("Subset", "Subset"),
                                            c("Group", "Group")),
                                       h = 0)
        }
      }
      
      justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                        l = "left",
                        c = "centre",
                        d = "right",
                        r = "right")


      freq_table <- format(x = round(x, format_info$round.digits),
                           trim = FALSE,
                           nsmall = format_info$round.digits,
                           justify = justify)

      if (format_info$report.nas) {
        # Put NA in relevant cells so that pander recognizes them as such
        freq_table[nrow(freq_table) - as.numeric(format_info$totals), 2] <- NA
        freq_table[nrow(freq_table) - as.numeric(format_info$totals), 3] <- NA
      }
      
      # Remove .00 digits in Freq column when weights are not used
      if (!"Weights" %in% names(data_info))
        freq_table[ ,1] <- sub("\\.0+", "", freq_table[,1])


      # Escape "<" and ">" when used in pairs in rownames
      if (!format_info$plain.ascii) {
        row.names(freq_table) <- gsub(pattern = "\\<(.*)\\>", replacement = "\\\\<\\1\\\\>",
                                      x = row.names(freq_table), perl = TRUE)
      }

      pander_args <- append(list(style = format_info$style,
                                 plain.ascii = format_info$plain.ascii,
                                 justify = justify,
                                 missing = format_info$missing,
                                 split.tables = Inf),
                            attr(x, "user_fmt"))

      output[[length(output) + 1]]  <-
        paste(
          capture.output(
            do.call(pander, append(pander_args, list(x = quote(freq_table))))
          ),
          collapse = "\n")

      if (isTRUE(escape.pipe) && format_info$style == "grid") {
        output[[length(output)]] <- gsub("\\|","\\\\|", output[[length(output)]])
      }

    } else {

      # freq objects - method viewer / browser / render -----------------------------------

      table_head <- list()
      
      justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                        l = "left",
                        c = "center",
                        d = "center",
                        r = "right")
        
      table_rows <- list()

      for (ro in seq_len(nrow(x))) {
        table_row <- list()
        for (co in seq_len(ncol(x))) {
          if (co == 1) {
            table_row[[length(table_row) + 1]] <- tags$th(row.names(x)[ro])
            if (!"Weights" %in% names(data_info)) {
              cell <- sub(pattern = "\\.0+", replacement = "", x[ro,co], perl = TRUE)
              table_row[[length(table_row) + 1]] <- tags$td(cell, align = justify)
              next
            }
          }

          if (is.na(x[ro,co])) {
            table_row[[length(table_row) + 1]] <-
              tags$td(format_info$missing, align = justify)
          } else {
            cell <- sprintf(paste0("%.", format_info$round.digits, "f"), x[ro,co])
            table_row[[length(table_row) + 1]] <- tags$td(cell, align = justify)
          }

          if (co == ncol(x)) {
            table_rows[[length(table_rows) + 1]] <- tags$tr(table_row)
          }
        }
      }

      if (format_info$report.nas) {
        table_head[[1]] <- list(tags$th("", colspan = 2),
                                tags$th("Valid", colspan = 2),
                                tags$th("Total", colspan = 2))
        table_head[[2]] <- list(tags$th(data_info$Variable),
                                tags$th("Freq"),
                                tags$th("%"),
                                tags$th(HTML("% Cumul")),
                                tags$th("%"),
                                tags$th(HTML("% Cumul")))

        freq_table_html <-
          tags$table(
            tags$thead(tags$tr(table_head[[1]]),
                       tags$tr(table_head[[2]])),
            tags$tbody(table_rows),
            class = paste("table table-striped table-bordered",
                          "st-table st-table-striped st-table-bordered st-freq-table",
                          ifelse(is.na(table.classes), "", table.classes)))
        
      } else {

        # no reporting of missing values (NA)
        table_head <- list(tags$th(data_info$Variable),
                           tags$th("Freq"),
                           tags$th("%"),
                           tags$th(HTML("% Cumul")))
        
        freq_table_html <-
          tags$table(
            tags$thead(tags$tr(table_head)),
            tags$tbody(table_rows),
            class = paste("table table-striped table-bordered",
                          "st-table st-table-striped st-table-bordered st-freq-table-nomiss",
                          ifelse(is.na(table.classes), "", table.classes))
          )
      }


      # Cleanup extra spacing and linefeeds in html to correct layout issues
      freq_table_html <- as.character(freq_table_html)
      freq_table_html <- gsub(pattern = "\\s*(\\d*)\\s*(<span|</td>)",
                              replacement = "\\1\\2", x = freq_table_html,
                              perl = TRUE)
      freq_table_html <- gsub(pattern = "</span>\\s*</span>",
                              replacement = "</span></span>",
                              x = freq_table_html,
                              perl = TRUE)

      # Prepare the main "div" for the html report
      div_list <- list()

      if (group.only ||
          ("by.first" %in% names(data_info) && 
           (!as.logical(data_info$by.first) || format_info$omit.headings))) {
        
        he_added <- add_head_element(list(c("Group", "Group")), h = 0)
      
      } else if (var.only) {

        if (!format_info$omit.headings) {
          if (sect_title[[2]] != "") {
            if (method != "render") {
              div_list[[length(div_list) + 1]] <- h4(sect_title[[2]])
              he_added <- add_head_element(list(c("Variable.label", "Variable Label"),
                                                c("Data.type", "Type")), h = 0)
            } else {
              he_added <- add_head_element(list(c("Variable", "Variable"),
                                                c("Variable.label", "Variable Label"),
                                                c("Data.type", "Type")), h = 0)
            }
          }
        }

        div_list[[length(div_list) + 1]] <- HTML(text = "<br/>")
        
      } else {

        if (!format_info$omit.headings) {
            
          if (sect_title[[1]] != "") {
            div_list[[1]] <- h3(sect_title[[1]])
          }
  
          if (sect_title[[2]] != "") {
            if (method != "render") {
              div_list[[length(div_list) + 1]] <- h4(sect_title[[2]])
              he_added <- add_head_element(list(c("Variable.label", "Label"),
                                                c("Data.type", "Type"),
                                                c("Weights", "Weights"),
                                                c("Subset", "Subset"),
                                                c("Group", "Group")),
                                           h = 0)
            } else {
              he_added <- add_head_element(list(c("Variable", "Variable"),
                                                c("Variable.label", "Label"),
                                                c("Data.type", "Type"),
                                                c("Weights", "Weights"),
                                                c("Subset", "Subset"),
                                                c("Group", "Group")),
                                           h = 0)
            }
          }
  
          div_list[[length(div_list) + 1]] <- HTML(text = "<br/>")
        }
      }
      
      div_list[[length(div_list) + 1]] <- HTML(text = freq_table_html)

      if (footnote != "") {
        div_list[[length(div_list) + 1]] <- HTML(text = footnote)
      }
    }

  } else if(attr(x, "st_type") == "ctable") {

    # ctable objects -------------------------------------------------------------------------
    
    sect_title <- list()
    sect_title[[1]] <- "Cross-Tabulation"

    if (!format_info$totals) {
      x$cross_table <- x$cross_table[which(rownames(x$cross_table) != 'Total'), 
                                     which(colnames(x$cross_table) != 'Total')]
      if (attr(x, "proportions") != "None") {
        x$proportions <- x$proportions[which(rownames(x$proportions) != 'Total'), 
                                     which(colnames(x$proportions) != 'Total')]
      }
    }

    if(attr(x, "proportions") %in% c("Row", "Column", "Total")) {
      sect_title[[1]] <- paste0(sect_title[[1]], " / ", attr(x, "proportions"), " Proportions")
      cross_table <- align_numbers(x$cross_table, x$proportions)
    } else {
      cross_table <- x$cross_table
    }

    sect_title[[2]] <- data_info$Row.x.Col

    if(method == "pander") {
      
      # ctable objects, method = pander --------------------------------------------------------
      output <- list()

      if (group.only  ||
          ("by.first" %in% names(data_info) && !as.logical(data_info$by.first))) {
        he_added <- add_head_element(list(c("Group", "Group")), h = 0)
      
      } else {
        
        if (!format_info$omit.headings) {
          
          output[[1]] <- add_hash(sect_title[[1]], 3)

          # if (format_info$plain.ascii) {
          #   output[[2]] <- paste0("\n", sect_title[[2]])
          # } else {
          #   output[[2]] <- paste0("\n", "**", sect_title[[2]], "**")
          # }
  
          he_added <- add_head_element(list(c("Row.x.Col", "Variables"),
                                            c("Dataframe", "Data Frame"),
                                            c("Dataframe.label", "Data Frame Label"),
                                            c("Subset", "Subset"),
                                            c("Row.variable.subset", "Row Var Subset"),
                                            c("Col.variable.subset", "Col Var Subset"),
                                            c("Group", "Group")),
                                       h = 0)
        }
      }

      output[[length(output) + 1]] <- "\n  "
      
      pander_args <- append(list(style = format_info$style,
                                 plain.ascii = format_info$plain.ascii,
                                 justify = format_info$justify,
                                 split.tables = format_info$split.tables),
                            attr(x, "user_fmt"))

      # do not use argument keep.trailing.zeros = TRUE b/c of issue with pander + ftable
      output[[length(output) + 1]] <-
        paste(
          capture.output(
            do.call(pander, append(pander_args, list(x = quote(ftable(cross_table)))))
          ),
          collapse = "\n")

      if (isTRUE(escape.pipe) && format_info$style == "grid") {
        output[[length(output)]] <- gsub("\\|","\\\\|", output[[length(output)]])
      }

    } else {

      # ctable objects - method viewer / browser / render  ------------------------------

      dnn <- names(dimnames(cross_table))

      table_head <- list()
      table_head[[1]] <- list(tags$th(""),
                              tags$th(dnn[2],
                                      colspan = ncol(cross_table) - as.numeric(format_info$totals)))
      
      if (format_info$totals) {
        table_head[[1]][[3]] <- tags$th("")
      }

      table_head[[2]] <-  list(tags$td(tags$strong(dnn[1]), align = "center"))

      for(cn in colnames(cross_table)) {
        if (nchar(cn) > 12) {
          cn <- smart_split(cn, 12)
        }
        cn <- sub("<", "&lt;", cn, fixed = TRUE)
        cn <- sub(">", "&gt;", cn, fixed = TRUE)
        table_head[[2]][[length(table_head[[2]]) + 1]] <- tags$th(HTML(cn), align = "center")
      }

      table_rows <- list()
      for (ro in seq_len(nrow(cross_table))) {
        table_row <- list()
        for (co in seq_len(ncol(cross_table))) {
          if (co == 1) {
            table_row[[length(table_row) + 1]] <-
              tags$td(tags$strong(row.names(cross_table)[ro]), align = "center")
          }

          # Case where no proportions exist
          if (length(x$proportions) == 0) {
            cell <- cross_table[ro,co]
            table_row[[length(table_row) + 1]] <- tags$td(tags$span(cell))
          } else {
            # Proportions exist
            cell <- sub("\\( *", "("     , cross_table[ro,co])
            cell <- sub(" *\\)", ")"     , cell)
            cell <- gsub(" "   , "&nbsp;", cell)
            cell <- sub("%"    , "&#37;" , cell, fixed = TRUE)

            table_row[[length(table_row) + 1]] <- tags$td(tags$span(HTML(cell)))
          }

          # On last col, insert row into table_rows list
          if (co == ncol(cross_table)) {
            table_rows[[length(table_rows) + 1]] <- tags$tr(table_row)
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
            class = paste("table table-bordered st-table st-table-bordered st-cross-table",
                          ifelse(is.na(table.classes), "", table.classes))
          )

      div_list <- list()
      
      if (group.only) {
        he_added <- add_head_element(list(c("Group", "Group")), h = 0)
        
      } else {
        
        if (!format_info$omit.headings) {
          div_list[[1]] <- h3(sect_title[[1]])
          
          if (method == 'render') {
            he_added <- add_head_element(list(c("Row.x.Col", "Variables"),
                                              c("Dataframe", "Data Frame"),
                                              c("Subset", "Subset"),
                                              c("Group", "Group")),
                                         h = 0)
            
          } else {
            div_list[[2]] <- h4(sect_title[[2]])
            he_added <- add_head_element(list(c("Dataframe", "Data Frame"),
                                              c("Subset", "Subset"),
                                              c("Group", "Group")),
                                         h = 0)
            
          }
        }
      }
      
      div_list[[length(div_list) + 1]] <- cross_table_html
      
      if (footnote != "") {
        div_list[[length(div_list) + 1]] <- HTML(text = footnote)
      }
    }

  } else if(attr(x, "st_type") == "descr") {

    # descr objects ------------------------------------------------------------------------

    if(!silent && !group.only && 
       (!"by.first" %in% names(data_info) || as.logical(data_info["by.last"]) == TRUE) &&
       "ignored" %in% names(attributes(x))) {
      message("Non-numerical variable(s) ignored: ", attr(x, "ignored"))
    }

    sect_title <- list()
    
    if ("Weights" %in% names(data_info)) {
      sect_title[[1]] <- "Weighted Descriptive Statistics  "
    } else {
      sect_title[[1]] <- "Descriptive Statistics  "
    }

    if (all(c("Variable", "Dataframe") %in% names(data_info))) {
      sect_title[[2]] <- paste(data_info$Dataframe, data_info$Variable, sep = "$")
    } else if ("Variable" %in% names(data_info)) {
      sect_title[[2]] <- data_info$Variable
    } else {
      sect_title[[2]] <-""
    }
    
    if ("byvar" %in% names(data_info)) {
      sect_title[[2]] <- paste(sect_title[[2]], "by", data_info$byvar)
    }

    justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                      l = "left",
                      c = "center",
                      r = "right")

    if(method=="pander") {
      
      # descr object, pander method -----------------------------------------------------

      output <- list()

      if (group.only ||
          ("by.first" %in% names(data_info) && 
           (!as.logical(data_info$by.first) || format_info$omit.headings))) {
        he_added <- add_head_element(list(c("Group", "Group"),
                                          c("N.Obs", "N")),
                                     h = 0)
      } else {
        
        if (!format_info$omit.headings) {
          output[[1]] <- add_hash(sect_title[[1]], 3)
  
          if (sect_title[[2]] != "") {
            if (format_info$plain.ascii) {
              output[[length(output) + 1]] <- paste0("\n", sect_title[[2]], "  ")
            } else {
              output[[length(output) + 1]] <- paste0("\n**Variable:** ", sect_title[[2]], "  ")
            }
          }
  
          if ("Variable" %in% names(data_info)) {
            he_added <- add_head_element(list(c("Variable.label", "Label"),
                                              c("Weights", "Weights"),
                                              c("Subset", "Subset"),
                                              c("Group", "Group"),
                                              c("N.Obs", "N")),
                                         h = 0)
            
          } else {
            he_added <- add_head_element(list(c("Dataframe", "Data Frame"),
                                              c("Dataframe.label", "Label"),
                                              c("Weights", "Weights"),
                                              c("Subset", "Subset"),
                                              c("Group", "Group"),
                                              c("N.Obs", "N")),
                                         h = 0)
          }
        }
      }
      
      # if ('byvar' %in% names(data_info) && !data_info$transpose) {
      #   colnames(x)[1] <- paste(data_info$byvar, "=", colnames(x)[1])
      # }
      
      # Format numbers (avoids inconsistencies with pander rounding digits)
      x <- format(round(x, format_info$round.digits),
                  nsmall = format_info$round.digits)

      pander_args <- append(list(style        = format_info$style,
                                 plain.ascii  = format_info$plain.ascii,
                                 split.tables = format_info$split.tables,
                                 justify      = justify),
                            attr("x", "user_fmt"))

      output[[length(output) + 1]] <-
        paste(
          capture.output(
            do.call(pander, append(pander_args, list(x = quote(x))))
          ),
          collapse = "\n")

      if(isTRUE(escape.pipe) && format_info$style == "grid") {
        output[[length(output)]] <- gsub("\\|","\\\\|", output[[length(output)]])
        output[[length(output) - 2]] <- gsub("\\|","\\\\|", output[[length(output) - 2]])
      }

    } else {

      # descr objects - method viewer / browser / render --------------------------
      
      if ("byvar" %in% names(data_info) && !data_info$transpose) {
        table_head <- list()
        table_head[[1]] <- list(tags$th(""),
                                tags$th(data_info$byvar,
                                        colspan = ncol(x)))

        table_head[[2]] <-  list(tags$th()) #d(tags$strong(data_info$Variable), align = "center"))

        for(cn in colnames(x)) {
          if (nchar(cn) > 12) {
            cn <- smart_split(cn, 12)
          }
          cn <- sub("<", "&lt;", cn, fixed = TRUE)
          cn <- sub(">", "&gt;", cn, fixed = TRUE)
          table_head[[2]][[length(table_head[[2]]) + 1]] <- tags$th(HTML(cn), align = "center")
        } 
        
      } else {

        table_head <- list(tags$th(""))
        
        for(cn in colnames(x)) {
          if (nchar(cn) > 12) {
            cn <- smart_split(cn, 12)
          }
          table_head[[length(table_head) + 1]] <- tags$th(HTML(cn), align = "center")
        }
      }

      table_rows <- list()
      for (ro in seq_len(nrow(x))) {
        table_row <- list(tags$td(tags$strong(rownames(x)[ro])))
        for (co in seq_len(ncol(x))) {
          # cell is NA
          if (is.na(x[ro,co])) {
            table_row[[length(table_row) + 1]] <- tags$td(format_info$missing)
          } else if (rownames(x)[ro] == "N.Valid" && !"Weights" %in% names(data_info)) {
            table_row[[length(table_row) + 1]] <-
              tags$td(tags$span(round(x[ro,co])))
          } else {
            # When not NA, and not from N.Valid row, format cell content
            cell <- sprintf(paste0("%.", format_info$round.digits, "f"), x[ro,co])
            table_row[[length(table_row) + 1]] <-
              tags$td(tags$span(cell))
          }
          # On last column, insert row to table_rows list
          if (co == ncol(x)) {
            table_rows[[length(table_rows) + 1]] <- tags$tr(table_row)
          }
        }
      }

      if ("byvar" %in% names(data_info) && !data_info$transpose) {
        descr_table_html <-
          tags$table(
            tags$thead(
              tags$tr(table_head[[1]]),
              tags$tr(table_head[[2]])
            ),
            tags$tbody(
              table_rows
            ),
            class = paste("table table-bordered table-striped",
                          "st-table st-table-bordered st-table-striped st-freq-table st-descr-table",
                          ifelse(is.na(table.classes), "", table.classes))
          )
        
      } else {
        
        descr_table_html <-
          tags$table(
            tags$thead(tags$tr(table_head)),
            tags$tbody(table_rows),
            class = paste("table table-bordered table-striped",
                          "st-table st-table-bordered st-table-striped st-descr-table",
                          ifelse(is.na(table.classes), "", table.classes))
          )
      }
      
      # Cleanup some extra spacing and linefeeds in html to avoid weirdness in layout
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

      div_list <- list()

      if (group.only) {
        he_added <- add_head_element(list(c("Group", "Group"),
                                          c("N.Obs", "N")),
                                     h = 0)
      } else {
      
        if (!format_info$omit.headings) {
          
          div_list[[1]] <- h3(sect_title[[1]])
          
          if (sect_title[[2]] != "") {
            div_list[[2]] <- h4(sect_title[[2]])
          }
  
          if ("Variable" %in% names(data_info)) {
            he_added <- add_head_element(list(c("Variable.label", "Label"),
                                              c("Weights", "Weights"),
                                              c("Subset", "Subset"),
                                              c("Group", "Group"),
                                              c("N.Obs", "N")),
                                         h = 0)
          } else {
            he_added <- add_head_element(list(c("Dataframe", "Data Frame"),
                                              c("Dataframe.label", "Label"),
                                              c("Weights", "Weights"),
                                              c("Subset", "Subset"),
                                              c("Group", "Group"),
                                              c("N.Obs", "N")),
                                         h = 0)
            
          }
        }
      }
      
      div_list[[length(div_list) + 1]] <- HTML(text = descr_table_html)

      if (footnote != "") {
        div_list[[length(div_list) + 1]] <- HTML(text = footnote)
      }
    }

  } else if(attr(x, "st_type") == "dfSummary") {

    # dfSummary objects -----------------------------------------------------------------------

    sect_title <- list()
    
    sect_title[[1]] <- "Data Frame Summary  "

    if ("Dataframe" %in% names(data_info)) {
      sect_title[[2]] <- data_info$Dataframe
    }  else {
      sect_title[[2]] <- ""
    }

    if (method == "pander") {
      
      # remove html graphs
      if ("Graph" %in% names(x)) {
        x <- x[,-which(names(x) == "Graph")]
      }
      
      # Check that style is not 'simple'
      if (format_info$style == 'simple') {
        format_info$style <- 'multiline'
      }
      
      output <- list()

      if(!format_info$plain.ascii) {
        # Escape symbols for words between <>'s to allow <NA> or factor
        # levels such as <ABC> to be rendered correctly
        if("Label" %in% names(x)) {
          x[["Label"]] <- gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
                               x = x[["Label"]], perl=TRUE)
        }
        
        x[["Stats / Values"]] <- gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
                                      x = x[["Stats / Values"]], perl=TRUE)
        x[["Freqs (% of Valid)"]] <- gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
                                          x = x[["Freqs (% of Valid)"]], perl=TRUE)
        

        # Remove leading characters used for alignment in plain.ascii 
        x[["Freqs (% of Valid)"]] <- gsub(pattern = "^\\\\ *", replacement = "",
                                          x = x[["Freqs (% of Valid)"]], perl=TRUE)
        
        x[["Freqs (% of Valid)"]] <- gsub(pattern = "\\n\\\\ *", replacement = "\n",
                                      x = x[["Freqs (% of Valid)"]], perl=TRUE)
        
        # Remove txt histograms b/c not supported in rmarkdown (for now)
        if ("Text Graph" %in% names(x)) {
          x[["Text Graph"]][which(grepl('[:.]', x[["Text Graph"]]))] <- ""
        }
        
      }

      if (!format_info$omit.headings) {
        if (sect_title[[1]] != "") {
          output[[1]] <- add_hash(sect_title[[1]], 3)
        }
        
        if (sect_title[[2]] != "") {
          if (format_info$plain.ascii) {
            output[[length(output) + 1]] <- paste0("\n", sect_title[[2]], "  ")
          } else {
            output[[length(output) + 1]] <- paste0("\n**", sect_title[[2]], "**  ")
          }
        }

        he_added <- add_head_element(list(c("Dataframe.label", "Data Frame Label"),
                                          c("Subset", "Subset"),
                                          c("N.obs", "N")),
                                     h = 0)
      }

      pander_args <- append(list(style        = format_info$style,
                                 plain.ascii  = format_info$plain.ascii,
                                 justify      = format_info$justify,
                                 split.cells  = format_info$split.cells,
                                 split.tables = format_info$split.tables,
                                 keep.line.breaks = TRUE),
                            attr(x, "user_fmt"))

      output[[length(output) + 1]] <-
        paste(
          capture.output(
            do.call(pander, append(pander_args, list(x = quote(x))))
          ),
          collapse = "\n")

      if (isTRUE(escape.pipe) && format_info$style == "grid") {
        output[[length(output)]] <- gsub("\\|","\\\\|", output[[length(output)]])
      }

    } else {

      # dfSummary objects - method viewer / browser / render --------------------------------
      
      # remove text graph
      if ("Text Graph" %in% names(x)) {
        x <- x[,-which(names(x) == "Text Graph")]
      }
      
      table_head <- list()
      for(cn in colnames(x)) {
        if (cn %in% c("No", "Valid", "Missing")) {
          table_head[[length(table_head) + 1]] <- tags$th(tags$strong(cn),
                                                          align = "center")
        } else {
          table_head[[length(table_head) + 1]] <- tags$th(tags$strong(cn),
                                                          align = "center")
        }
      }

      table_rows <- list()
      for (ro in seq_len(nrow(x))) {
        table_row <- list()
        for (co in seq_len(ncol(x))) {
          cell <- x[ro,co]
          cell <- gsub('\\\\\n', '\n', cell)
          if (colnames(x)[co] %in% c("No", "Valid", "Missing")) {
            table_row[[length(table_row) + 1]] <- tags$td(cell, align = "center")
          } else if (colnames(x)[co] %in% c("Variable", "Label", "Properties", "Stats / Values")) {
            cell <- gsub('(\\d+)\\\\\\.', '\\1.', cell)
            table_row[[length(table_row) + 1]] <- tags$td(cell, align = "left")
          } else if (colnames(x)[co] == "Freqs (% of Valid)") {
            cell <- gsub("\\\\", " ", cell)
            cell <- gsub(" *(\\d|\\:)", "\\1", cell)
            cell <- gsub("\\:", " : ", cell)
            table_row[[length(table_row) + 1]] <- tags$td(cell, align = "left")
          } else if (colnames(x)[co] == "Graph") {
            table_row[[length(table_row) + 1]] <- tags$td(HTML(cell), align = "center", border = "0")
          }
        }
        
        table_rows[[length(table_rows) + 1]] <- tags$tr(table_row)
      }


      dfs_table_html <-
        tags$table(
          tags$thead(tags$tr(table_head)),
          tags$tbody(table_rows),
          class = paste("table table-striped table-bordered",
                        "st-table st-table-striped st-table-bordered st-multiline",
                        ifelse(is.na(table.classes), "", table.classes))
        )

      dfs_table_html <- gsub(pattern = '(<th.*?>)\\s+(<strong>.*?</strong>)\\s+(</th>)',
                             replacement = "\\1\\2\\3",
                             x = dfs_table_html)
      div_list <- list()
      
      if (!format_info$omit.headings) {
        
        if (sect_title[[1]] != "") {
          div_list[[1]] <- h3(sect_title[[1]])
        }
        
        if (sect_title[[2]] != "") {
          div_list[[length(div_list) + 1]] <- h4(sect_title[[2]])
        }
        
        he_added <- add_head_element(list(c("Dataframe.label", "Data Frame Label"),
                                          c("Subset", "Subset"),
                                          c("N.obs", "N")),
                                     h = 0)
      }
      
      div_list[[length(div_list) + 1]] <- HTML(text = dfs_table_html)

      if (footnote != "") {
        div_list[[length(div_list) + 1]] <- HTML(text = footnote)
      }
    }
  }

  # Print or write to file - pander --------------------------------------------------------------
  
  if (method == "pander") {
    
    # remove extra linefeed if omit.headings
    if (format_info$omit.headings) {
      if (output[[1]] == "\n  ") {
        output[[1]] <- NULL
      }
      output[[1]] <- sub("^\\n\\n", "\n", output[[1]])
    }

    cat(do.call(paste, output), file = file, append = append)
    if (file != "" && !isTRUE(silent)) {
      if (isTRUE(append))
        message(paste0("Output file appended: ", file))
      else
        message(paste0("Output file written: ", file))
      return(invisible())
    }

  } else {

    # Print or write to file - html --------------------------
    
    if (isTRUE(append)) {
      f <- file(file, open = "r", encoding = "utf-8")
      html_content_in <- paste(readLines(f, warn = FALSE, encoding = "utf-8"), collapse="\n")
      close(f)
      top_part <- sub("(^.+)(</body>.+)", "\\1", html_content_in)
      bottom_part <- sub("(^.+)(</body>.+)", "\\2", html_content_in)
      insert_part <- iconv(paste(capture.output(tags$div(class="container st-container", div_list)), collapse="\n"), to = "utf-8")
      html_content <- paste(capture.output(cat(top_part, insert_part, bottom_part)), collapse="\n")

    } else {

      if (method %in% c("browser", "viewer")) {
        html_content <-
          tags$div(class="container st-container",
                   #tags$br(),
                   tags$head(tags$title(ifelse(is.na(report.title), sect_title, report.title)),
                             if (isTRUE(bootstrap.css)) includeCSS(path = paste(stpath, "includes/stylesheets/bootstrap.min.css", sep="/")),
                             includeCSS(path = paste(stpath, "includes/stylesheets/summarytools.css", sep="/")),
                             if (!is.na(custom.css)) includeCSS(path = custom.css)),
                   div_list)
        
      } else {
        html_content <-
          tags$div(class="container st-container",
                   tags$head(includeCSS(path = paste(stpath, "includes/stylesheets/summarytools.css", sep="/")),
                             if (!is.na(custom.css)) includeCSS(path = custom.css)),
                   div_list)
        
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
            message("To view html content in RStudio, please run install.packages('rstudioapi').")
            message("Switching method to 'browser'")
            method <- "browser"
          }
        } else {
          message("Method 'viewer' only valid within RStudio. Switching method to 'browser'.")
          method <- "browser"
        }
      }
    }

    # For method "browser", we don't use utils::browseURL() because of compatibility issues with RStudio
    if (method == "browser") {
      if (file == "" || open.doc) {
        switch(Sys.info()[["sysname"]],
               Windows = {shell.exec(file = paste0("file:///", outfile_path))},
               Linux   = {system(paste("/usr/bin/xdg-open", outfile_path), wait = FALSE, ignore.stdout = TRUE)},
               Darwin  = {system(paste("open", outfile_path), wait = FALSE, ignore.stderr = TRUE)})
      }
    }

    # return file path and update tmpfiles vector when method = browser or viewer ----------------------
    if(file == "" && method %in% c("browser", "viewer")) {
      .st_env$tmpfiles <- c(.st_env$tmpfiles, outfile_path)
      if (!silent) {
        message(paste0("Output file written: ", outfile_path))
      }
      return(invisible(normalizePath(outfile_path, winslash = "\\", mustWork = FALSE)))
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
