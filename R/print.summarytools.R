#' Print Method for Objects of Class \code{summarytools}.
#'
#' Display \code{summarytools} objects in the console, in Web Browser or in
#'  \emph{RStudio}'s Viewer, or create formatted output files.
#'
#' @aliases print view
#'
#' @usage
#'  \method{print}{summarytools}(x, method = "pander", file = "", append = FALSE,
#'                               report.title = NA, group.only = FALSE,
#'                               escape.pipe = FALSE, silent = FALSE,
#'                               footer = FALSE, \dots)
#'
#'  view(x, method = "viewer", file = "", append = FALSE, report.title = NA,
#'       group.only = FALSE, escape.pipe = FALSE, silent = FALSE,
#'       footer = FALSE, \dots)
#'
#' @param x A summarytools object that was generated with \code{\link{freq}},
#'   \code{\link{descr}}, \code{\link{ctable}} or \code{\link{dfSummary}}.
#' @param method One of \dQuote{pander}, \dQuote{viewer}, or \dQuote{browser}.
#'   For \code{print()}, default is \dQuote{pander}; with \code{view()}, default
#'   is \dQuote{viewer}. If \dQuote{viewer} is used outside \emph{RStudio},
#'   \dQuote{browser} will be used instead.
#' @param file File name to write output to. Defaults to \dQuote{}.
#' @param append Logical. When \code{file} argument is supplied, this indicates
#'   whether to append output to existing file. \code{FALSE} by default.
#' @param report.title For \emph{html} reports, this goes into the
#'   \code{<title>} tag. Defaults to \code{NA}, in which case \code{<title>}
#'   will be generic.
#' @param group.only Logical. Used internally when printing objects created
#'   with \code{by()}.
#' @param escape.pipe Logical. Set to \code{TRUE} when using
#'   \code{style='grid'} and \code{file} argument is supplied if the intent
#'   is to generate a text file that can be converted to other formats using
#'   \emph{Pandoc}.
#' @param html.table.class All \emph{Bootstrap CSS} classes can be used. It also allows
#'   user-defined classes (see custom.css parameter). See \emph{details} section.
#' @param custom.css Path to a user-defined \emph{.css} file. Classes
#'   defined in this file can be used in the \code{html.table.classes}
#'   parameter.
#' @param silent Hide console messages (such as ignored variables or \code{NaN}
#'   to \code{NA} transformations).
#' @param footer Logical. Include footer (package name & version, R version,
#'   date) in \emph{html} outputs. \code{TRUE} by default. Has no effect when
#'   \code{method} is \dQuote{pander}.
#' @param \dots Additional arguments can be used to override parameters stored
#'   as attributes in the object being printed. See \emph{Details} section.
#'
#' @return \code{NULL} when \code{method="pander"}; a file path (returned
#'   invisibly) when \code{method="viewer"} or \code{method="browser"}. In the
#'   latter case, the file path is also passed to \code{shell.exec} so the
#'   document is opened with default Web Browser.
#'
#' @details
#' \strong{Plain ascii and \emph{rmarkdown} tables} are generated via
#'   \code{\link[pander]{pander}}. See \emph{References} section
#'   for a list of all available \emph{pander} options.
#'
#' \strong{\emph{Html} tables} are generated via
#'   \code{\link[xtable]{print.xtable}}
#'   and include \emph{Bootstrap Cascading Style Sheets}. To add custom
#'   \emph{CSS}, edit the \emph{custom.css} file located in package's
#'   \emph{includes/stylesheets} directory.
#'
#' To \strong{print objects of class \dQuote{by}}, use \code{\link{view}}. This
#'   function also makes it more practical to generate \emph{html} files (see
#'   examples).
#'
#' Default values for \code{html.table.attributes} are as follows:
#'   \describe{
#'     \item{freq}{\code{'class="table table-striped table-bordered
#'       table-responsive"'}}
#'     \item{ctable}{\code{'class="table table-striped table-bordered
#'       monospace-cells table-responsive"'}}
#'     \item{descr (stats table)}{\code{'class="table table-striped
#'       table-bordered table-responsive"'}}
#'     \item{descr (obs table)}{\code{'class="table table-striped
#'       table-bordered monospace-cells ctable"'}}
#'     \item{dfSummary}{\code{'class="table table-striped table-bordered
#'       table-narrow table-responsive"'}}}
#' When specifying this parameter, you must also state those attributes if you want them to be applied.
#'
#' The following additional arguments can be used to override
#'   formatting and other attributes stored in the object to be printed:
#'    \itemize{
#'      \item \code{date}
#'      \item \code{style}
#'      \item \code{round.digits}
#'      \item \code{justify}
#'      \item \code{plain.ascii}
#'      \item \code{missing}
#'      \item \code{split.table}
#'      \item \code{Dataframe}
#'      \item \code{Dataframe.label}
#'      \item \code{Variable}
#'      \item \code{Variable.label}
#'      \item \code{Subset}
#'      \item \code{Group}
#'      \item \code{Weights}
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
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com>}
#'
#' @seealso
#' \code{\link[xtable]{xtable}}, \code{\link[pander]{pander}}
#'
#' @examples
#'   \dontrun{
#'   data(tobacco)
#'   view(dfSummary(tobacco))
#'   }
#'   data(exams)
#'   print(freq(exams$gender))
#'   print(descr(exams), "pander", style = "grid")
#'
#' @keywords print methods
#'
#'@export
print.summarytools <- function(x, method = "pander", file = "", append = FALSE,
                               report.title = NA, group.only = FALSE,
                               escape.pipe = FALSE, html.table.class = NA,
                               custom.css = NA, silent = FALSE,
                               footer = FALSE, ...) {


  # Parameter validation ---------------------------------------
  method <- switch(tolower(substring(method, 1, 1)),
                   p = "pander",
                   b = "browser",
                   v = "viewer")

  if (!method %in% c("pander", "browser", "viewer"))
    stop("'method' must be one of 'pander', 'browser', 'viewer'")

  if (file == "" && isTRUE(append))
    stop("'append' is set to TRUE but no file name has been specified")

  if (file != "" && isTRUE(append) && !file.exists(file))
    stop("'append' is set to TRUE but specified file does not exist")

  if (file != "" && isTRUE(append) && !is.na(report.title))
    message("Appending existing file -- 'report.title' argument will be ignored")

  if (file != "" && grepl(pattern = "\\.html$", x = file, ignore.case = TRUE, perl = TRUE))
    method <- "viewer"

  if (isTRUE(group.only) && !"Group" %in% names(attr(x, "data_info")))
    stop("'group.only' can only be used with objects created using by()")

  if (!is.na(report.title) && !is.character(report.title))
    stop("'report.title' must either be NA or a character string")

  if (!group.only %in% c(TRUE, FALSE))
    stop("'group.only' must be either TRUE or FALSE")

  if (!escape.pipe %in% c(TRUE, FALSE))
    stop("'escape.pipe' must be either TRUE or FALSE")

  if (!is.na(custom.css) && !file.exists(custom.css))
    stop("'custom.css' argument must point to an existing file.")

  if ((!is.na(html.table.class) || !is.na(custom.css)) && method == "pander")
    stop("'html.table.class' and 'custom.css' options do not apply to method 'pander'")

  if (!silent %in% c(TRUE, FALSE))
    stop("'silent' must be either TRUE or FALSE")

  if (!footer %in% c(TRUE, FALSE))
    stop("'footer' must be either TRUE or FALSE")

  # Override of x's attributes ---------------------------------------------
  args_list <- match.call()

  if ("date" %in% names(args_list))
    attr(x, "date") <- args_list$date

  # Formatting attributes
  for (format_element in c("style", "round.digits", "justify", "plain.ascii",
                           "missing", "split.table", "display.labels")) {
    if (format_element %in% names(args_list))
      attr(x, "formatting")[format_element] <- args_list[format_element]
  }

  # Data info attributes
  for (data_info_element in c("Dataframe", "Dataframe.label",
                              "Variable", "Variable.label",
                              "Subset", "Group", "Weights",
                              "Row.variable", "Col.variable",
                              "Row.variable.subet", "Col.variable.subset",
                              "Row.variable.label", "Col.variable.label")) {
    if (data_info_element %in% names(args_list))
      attr(x, "data_info")[data_info_element] <- args_list[data_info_element]
  }

  # When style == 'rmarkdown', set plain.ascii to FALSE unless explicitly specified
  if (method == "pander" && attr(x, "formatting")$style == "rmarkdown" &&
      isTRUE(attr(x, "formatting")$plain.ascii) &&
      (!"plain.ascii" %in% (names(match.call())))) {
    attr(x, "formatting")$plain.ascii <- FALSE
  }

  # Declare functions ---------------------------------------------
  align_numbers <- function(counts, props) {

    round.digits <- format_info[["round.digits"]]
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

  add_hash <- function(str, n=4) {
    if (isTRUE(attr(x, "formatting")$plain.ascii)) {
      return(str)
    } else {
      return(paste(paste0(rep(x = "#", times = n), collapse = ""), str))
    }
  }

  add_head_element <- function(element, display_name = element, h = 3) {
    if (method == "pander") {
      if (element %in% names(data_info)) {
        if (element == "Group" && !format_info[["plain.ascii"]]) {
          output[[length(output) + 1 ]] <<- "***\n"
        }
        output[[length(output) + 1]] <<-
          paste0("\n", add_hash(paste(display_name, data_info[[element]], sep = ": "), h))

        if (isTRUE(format_info[["display.labels"]])
            && paste0(element, ".label") %in% names(data_info)) {
          output[[length(output)]] <<-
            paste0(output[[length(output)]], " (", data_info[[paste0(element, ".label")]], ")")
        }
      }
    } else {
      # method is viewer/browser
      if (element %in% names(data_info)) {
        if (element == "Group") {
          div_list[[length(div_list) + 1]] <<- hr()
        }
        div_str <- paste(display_name, data_info[[element]], sep = ": ")
        if (isTRUE(format_info[["display.labels"]])
            && paste0(element, ".label") %in% names(data_info)) {
          div_str <- paste0(div_str, " (", data_info[[paste0(element, ".label")]], ")")
        }
        div_list[[length(div_list) + 1]] <<- get(paste0("h", h))(div_str)
      }
    }
  }

  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
  }

  col_has_decimals <- function(x) {
    (colSums(apply(x, 2, is.wholenumber), na.rm = TRUE) /
       colSums(!apply(x, 2, is.na))) < 1
  }

  # Build footer note
  if (method %in% c("browser", "viewer")) {
    stpath <- find.package("summarytools")

    footer_note <- paste0("Generated by <a href='https://github.com/dcomtois/summarytools'>summarytools</a> package version ",
                          packageVersion(pkg = "summarytools"),
                          " (<a href='http://www.r-project.org/'>R</a> version ", getRversion(), ")",
                          "<br/>", Sys.Date())
  }

  # Extract non-NA "data_info" elements from x attributes
  if ("data_info" %in% names(attributes(x))) {
    data_info <- attr(x, "data_info")
    data_info <- data_info[!is.na(data_info)]
  } else {
    data_info <- NA
  }

  # Extract formatting information from x attributes
  format_info <- attr(x, "formatting")

  # Printing freq objects  --------------------------------------
  if(attr(x, "st_type") == "freq") {

    # define section title
    sect_title <- ifelse("Weights" %in% names(data_info),
                         "Weighted Frequencies",
                         "Frequencies")

    if(method == "pander") {
      output <- list()

      if (isTRUE(group.only) ||
          ("by.first" %in% names (data_info) && !as.logical(data_info[["by.first"]]))) {
        add_head_element("Group", h = 4)
      } else {
        output[[1]] <- add_hash(sect_title, 2)
        add_head_element("Variable", h = 3)
        add_head_element("Dataframe", "Data Frame", h = 4)
        add_head_element("Weights", h = 4)
        add_head_element("Subset", h = 4)
        add_head_element("Group", h = 4)
      }

      justif <- ifelse(format_info$justify == "center", "centre", format_info$justify)
      freq_table <- format(x = x,
                           trim = FALSE,
                           nsmall = format_info$round.digits,
                           digits = format_info$round.digits,
                           justify = justif)

      # Remove .00 digits in count column when weights are not used
      if (!"Weights" %in% names(data_info))
        freq_table[ ,1] <- sub("\\.0+", "", freq_table[,1])

      # Put NA in irrelevant cells so that pander recognizes them as such
      freq_table[nrow(freq_table)-1, 2] <- NA
      freq_table[nrow(freq_table)-1, 3] <- NA

      # Escape "<" and ">" when used in pairs in rownames
      # TODO: test potentially problematic rownames, including with style "simple"
      if (!format_info$plain.ascii &&
          format_info$style %in% c("rmarkdown", "grid")) {
        row.names(freq_table) <- gsub(pattern = "\\<(.*)\\>", replacement = "\\\\<\\1\\\\>",
                                      x = row.names(freq_table), perl = TRUE)
      }

      output[[length(output) + 1]]  <-
        paste(
          capture.output(
            pander::pander(x = freq_table,
                           style = format_info$style,
                           plain.ascii = format_info$plain.ascii,
                           justify = format_info$justify,
                           missing = format_info$missing)),
          collapse = "\n")

      if (isTRUE(escape.pipe) && format_info$style == "grid")
        output[[length(output)]] <- gsub("\\|","\\\\|", output[[2]])

    } else {

      # method is viewer or browser  -----------------------------
      if ("Variable" %in% names(data_info)) {
        table_head <- list(tags$td(data_info[["Variable"]], align="center"))
      } else {
        table_head <- list(tags$td(""))
      }

      for(cn in colnames(x)) {
        table_head[[length(table_head) + 1]] <- tags$th(cn, align = "center")
      }

      col_has_dec <- col_has_decimals(x)
      table_rows <- list()
      for (ro in seq_len(nrow(x))) {
        table_row <- list()
        for (co in seq_len(ncol(x))) {
          # On first column, insert row name
          if (co == 1) {
              table_row[[length(table_row) + 1]] <- tags$td(row.names(x)[ro], align = "center")
            }

          # cell is NA
          if (is.na(x[ro,co])) {
            table_row[[length(table_row) + 1]] <- tags$td(format_info$missing, align="center")
          } else {
            # When not NA, Format cell content
            cell <- sprintf(paste0("%.", format_info$round.digits, "f"), x[ro,co])
            if (col_has_dec[co]) {
              cell <- strsplit(cell, ".", fixed = TRUE)[[1]]
              table_row[[length(table_row) + 1]] <-
                tags$td(tags$span(cell[1], tags$span(paste0(".",cell[2]), class="cellRight"), class = "cellLeft"))
            } else {
              # Column has no decimals
              cell <- sub(pattern = "\\.0+", replacement = "", cell, perl = TRUE)
              table_row[[length(table_row) + 1]] <- tags$td(tags$span(cell, class="numSpan"))
            }
          }

          # On last column, insert row to table_rows list
          if (co == ncol(x)) {
            table_rows[[length(table_rows) + 1]] <- tags$tr(table_row)
          }
        }
      }

      freq_table_html <-
        withTags(
          table(
            tr(
              table_head, class="strong"
            ),
            table_rows,
            class = ifelse("html.table.class" %in% names(args_list),
                           args_list[["html.table.class"]],
                           "table table-striped table-bordered table-responsive")
          )
        )

      # Cleanup some extra spacing and linefeeds in html to avoid weirdness in layout
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

      if (isTRUE(group.only) ||
          ("by.first" %in% names (data_info) && !as.logical(data_info[["by.first"]]))) {
        add_head_element("Group", h = 4)
      } else {
        div_list[[1]] <- h2(sect_title)
        add_head_element("Variable", h = 3)
        add_head_element("Dataframe", "Data Frame", h = 4)
        add_head_element("Weights", h = 4)
        add_head_element("Subset", h = 4)
        add_head_element("Group", h = 4)
      }

      #div_list[[length(div_list) + 1]] <- HTML(text = gsub("<td> ", "<td>", freq_table_html))
      div_list[[length(div_list) + 1]] <- HTML(text = freq_table_html)
      if (isTRUE(footer)) {
        div_list[[length(div_list) + 1]] <- HTML(text = footer_note)
      }
    }
  }

  # Printing ctable objects -----------------------------------------------------
  if(attr(x, "st_type") == "ctable") {

    sect_title <- "Cross-Tabulation"

    if(attr(x, "proportions") %in% c("Row", "Column", "Total")) {
      sect_title <- paste(sect_title, "with", attr(x, "proportions"), "Proportions")
      cross_table <- align_numbers(x$cross_table, x$proportions)
    } else {
      cross_table <- x$cross_table
    }

    if(method == "pander") {
      output <- list()

      if (isTRUE(group.only) ||
          ("by.first" %in% names (data_info) && !as.logical(data_info[["by.first"]]))) {
        add_head_element("Group", h = 4)
      } else {
        output[[1]] <- paste0("\n", add_hash(sect_title, 2))
        if (!format_info[["plain.ascii"]]) {
          output[[2]] <- paste0("\n",
                                add_hash(paste0("_", data_info[["Row.variable"]],
                                                " * ", data_info[["Col.variable"]], "_"), 3))
        } else {
          output[[2]] <- paste0("\n", data_info[["Row.variable"]], " * ",
                                data_info[["Col.variable"]])
        }
        add_head_element("Dataframe", "Data Frame", h = 4)
        add_head_element("Subset", h = 4)
        #add_head_element("Row.variable", display_name = "Row var", h = 3)
        #add_head_element("Col.variable", display_name = "Col var", h = 3)
        # TODO: when Row.variable.subset != Col.variable.subset, for now nothing is shown.
      }

      # do not use argument keep.trailing.zeros = TRUE b/c of issue with pander + ftable
      output[[length(output) + 1]] <-
        paste(capture.output(pander::pander(ftable(cross_table),
                                            style = format_info[["style"]],
                                            plain.ascii = format_info[["plain.ascii"]],
                                            justify = format_info[["justify"]])),
              collapse = "\n")

      if (isTRUE(escape.pipe) && format_info$style == "grid")
        output[[length(output)]] <- gsub("\\|","\\\\|", output[[length(output)]])

    } else {

      # method = viewer / browser  ------------------------------
      dnn <- names(dimnames(cross_table))

      table_head_1 <- list(tags$th(""))
      table_head_1[[2]] <- tags$th(dnn[2], align = "center",
                                   colspan =  as.character(
                                     ncol(cross_table) -
                                       as.numeric("Total" %in% colnames(cross_table))))

      if ("Total" %in% colnames(cross_table)) {
        table_head_1[[3]] <- tags$th("")
      }

      table_head_2 <- list(tags$td(dnn[1], align = "center"))
      for(cn in colnames(cross_table)) {
        table_head_2[[length(table_head_2) + 1]] <- tags$th(cn, align = "center")
      }

      table_rows <- list()
      for (ro in seq_len(nrow(cross_table))) {
        table_row <- list()
        for (co in seq_len(ncol(cross_table))) {
          if (co == 1) {
            table_row[[length(table_row) + 1]] <-
              tags$td(row.names(cross_table)[ro], align = "center")
          }

          # Case where no proportions exist
          if (is.null(x$proportions)) {
            cell <- cross_table[ro,co]
            table_row[[length(table_row) + 1]] <- tags$td(tags$span(cell, class="numspan"))
          } else {
            # Proportions exist
            cell <- strsplit(gsub(" ", "", cross_table[ro,co]), "(", fixed = TRUE)[[1]]
            table_row[[length(table_row) + 1]] <-
              tags$td(tags$span(paste0(cell[1],"¦¦("),
                                tags$span(cell[2], class="cellRight"),
                                class = "cellLeft"))
          }
          # On last col, insert row into table_rows list
          if (co == ncol(cross_table)) {
            table_rows[[length(table_rows) + 1]] <- tags$tr(table_row)
          }
        }
      }

      cross_table_html <-
        withTags(
          table(
              tr(table_head_1, class="strong"),
              tr(table_head_2, class="strong"),
              table_rows,
              class = ifelse("html.table.class" %in% names(args_list),
                             args_list[["html.table.class"]],
                             "table table-striped table-bordered")
          )
        )

      #return(as.character(cross_table_html))
      # Cleanup some extra spacing and linefeeds in html to avoid weirdness in layout
      cross_table_html <- as.character(cross_table_html)
      cross_table_html <- gsub(pattern = "\\s*(\\d*)\\s*(<span|</td>)",
                              replacement = "\\1\\2", x = cross_table_html,
                              perl = TRUE)
      cross_table_html <- gsub(pattern = "</span>\\s*</span>",
                              replacement = "</span></span>",
                              x = cross_table_html,
                              perl = TRUE)
      cross_table_html <- gsub(pattern = "¦¦",
                               replacement = "&nbsp;&nbsp;",
                               x = cross_table_html,
                               fixed = TRUE)
      cross_table_html <- gsub(pattern = '"cellLeft">\\s+',
                               replacement = '"cellLeft">',
                               x = cross_table_html,
                               perl = TRUE)


      div_list <- list()
      if (isTRUE(group.only) ||
          ("by.first" %in% names (data_info) && !as.logical(data_info[["by.first"]]))) {
        add_head_element("Group", h = 4)
      } else {
        div_list[[1]] <- h2(sect_title)
        div_list[[length(div_list) + 1]] <- (h3(data_info[["Row.variable"]], " * ",
                                                data_info[["Col.variable"]]))
        add_head_element("Dataframe", "Data Frame", h = 4)
        add_head_element("Subset", h = 4)

        #add_head_element("Row.variable", display_name = "Row var", h = 3)
        #add_head_element("Col.variable", display_name = "Col var", h = 3)
      }

      div_list[[length(div_list) + 1]] <- HTML(text = cross_table_html)
      if (isTRUE(footer)) {
        div_list[[length(div_list) + 1]] <- HTML(text = footer_note)
      }
    }
  }

  # Printing descr objects ----------------------------------------------------
  if(attr(x, "st_type") == "descr") {

    if(!silent && "ignored" %in% names(attributes(x)))
      message("Non-numerical variable(s) ignored: ", attr(x, "ignored"))

    sect_title <- ifelse("Weights" %in% names(data_info),
                         "Weighted Descriptive Statistics",
                         "Descriptive Statistics")

    if(method=="pander") {

      output <- list()

      if (isTRUE(group.only) ||
          ("by.first" %in% names (data_info) && !as.logical(data_info[["by.first"]]))) {
        add_head_element("Group", h = 4)
      } else {
        output[[1]] <- add_hash(sect_title, 2)
        add_head_element("Variable", h = 3)
        add_head_element("Dataframe", "Data Frame", h = 3)
        add_head_element("Weights", h = 4)
        add_head_element("Subset", h = 4)
        add_head_element("Group", h = 4)
      }

      # Change <NA> for \<NA\> in markdown tables
      if (format_info[["style"]] == "rmarkdown" && !isTRUE(format_info[["plain.ascii"]])) {
        rownames(x$observ)[which(rownames(x$observ) == "<NA>")] <- "\\<NA\\>"
        colnames(x$observ)[which(colnames(x$observ) == "<NA>")] <- "\\<NA\\>"
      }

      # Format numbers (circumvents inconsistencies with pander round digits)
      x$stats <- format(round(x$stat, format_info[["round.digits"]]),
                          nsmall = format_info[["round.digits"]])

      output[[length(output) + 1]] <-
        paste(
          capture.output(
            pander::pander(x = x$stats,
                           style = format_info[["style"]],
                           #round.digits = format_info[["round.digits"]],
                           plain.ascii = format_info[["plain.ascii"]],
                           justify = format_info[["justify"]])
          ),
          collapse = "\n")

      output[[length(output) + 1]] <- paste0("\n", add_hash("Observations", 3))
    }

    obstable <- align_numbers(x$observ, x$observ_pct)

    dim(obstable) <- dim(x$observ)
    dimnames(obstable) <- dimnames(x$observ)
    #obstable <- align_numbers(obstable)

    if(method=="pander") {
      output[[length(output) + 1]] <-
        paste(capture.output(
          do.call(pander::pander, append(format_info,
                                         list(x = quote(obstable))))),
          collapse = "\n")

      if (isTRUE(escape.pipe) && format_info$style == "grid") {
        output[[length(output)]] <- gsub("\\|","\\\\|", output[[length(output)]])
        output[[length(output) - 2]] <- gsub("\\|","\\\\|", output[[length(output) - 2]])
      }
    }

    # method = viewer / browser --------------------------
    else {

      descr_table_html <-
        xtable::print.xtable(
          xtable::xtable(x = x$stats,
                         align = paste0("r", paste(rep("c",ncol(x$stats)), collapse="")),
                         digits = c(0,rep(format_info$round,
                                          ncol(x$stats)))),
          type = "html", print.results = FALSE,
          html.table.attributes = ifelse("html.table.attributes" %in% names(args_list),
                                         args_list[["html.table.attributes"]],
                                         'class="table table-striped table-bordered table-responsive"')
        )

      obs_table_html <-
        xtable::print.xtable(
          xtable::xtable(x = obstable,
                         align = paste0("r", paste(rep("c",ncol(x$observ)), collapse=""))),
          type = "html", print.results = FALSE,
          sanitize.text.function = function(x) gsub("_", "&nbsp;", x = x, fixed = TRUE),
          sanitize.colnames.function = function(x) sub(">", "&gt;", sub("<", "&lt;", x)),
          sanitize.rownames.function = function(x) sub(">", "&gt;", sub("<", "&lt;", x)),
          html.table.attributes = ifelse("html.table.attributes" %in% names(args_list),
                                         args_list[["html.table.attributes"]],
                                         'class="table table-striped table-bordered monospace-cells table-responsive"')
        )

      div_list <- list()

      if (isTRUE(group.only) ||
          ("by.first" %in% names (data_info) && !as.logical(data_info[["by.first"]]))) {
        add_head_element("Group", h = 4)
      } else {
        div_list[[1]] <- h2(sect_title)
        add_head_element("Variable", h = 3)
        add_head_element("Dataframe", "Data Frame", h = 4)
        add_head_element("Weights", h = 4)
        add_head_element("Subset", h = 4)
        add_head_element("Group", h = 4)
      }

      div_list[[length(div_list) + 1]] <- HTML(text = gsub("<td> ", "<td>", descr_table_html))
      div_list[[length(div_list) + 1]] <- h5("Observations")
      div_list[[length(div_list) + 1]] <- HTML(gsub("<td> ", "<td>", obs_table_html))

      if (isTRUE(footer)) {
        div_list[[length(div_list) + 1]] <- HTML(text = footer_note)
      }
    }
  }

  # Printing dfSummary objects ------------------------------------------------
  if(attr(x, "st_type") == "dfSummary") {
    sect_title <- "Data Frame Summary"
    if (method == "pander") {
      output <- list()

      # Escape symbols for words between <>'s in some columns to allow <NA> or factor
      # levels such as <ABC> to be rendered correctly
      if(!format_info[["plain.ascii"]]) {
        if("Label" %in% names(x))
          x[["Label"]] <- gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
                               x = x[["Label"]], perl=TRUE)

        x[["Stats"]] <- gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
                             x = x[["Stats"]], perl=TRUE)
        x[["Stats"]] <- gsub(pattern = "\n", replacement = " \\\\ \n",
                             x = x$Stats, perl=TRUE)
        x[["Freqs, % of Valid"]] <- gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
                                      x = x[["Freqs, % of Valid"]], perl=TRUE)
        x[["Freqs, % of Valid"]] <- gsub(pattern = "\n", replacement = " \\\\ \n",
                                      x = x[["Freqs, % of Valid"]], perl=TRUE)
      }

      output[[1]] <- add_hash(sect_title, 2)
      output[[2]] <- add_hash(data_info[["Dataframe"]], 3) # add_head_element("Dataframe", "Data Frame", h = 3)
      add_head_element("Subset", h = 4)
      add_head_element("N.obs", "Number of rows", h = 4)

      output[[length(output) + 1]] <-
        paste(
          capture.output(
            pander::pander(x,
                           style = format_info[["style"]],
                           plain.ascii = format_info[["plain.ascii"]],
                           justify = format_info[["justify"]],
                           split.cells = format_info[["split.cells"]],
                           split.table = format_info[["split.table"]],
                           keep.line.breaks = TRUE)),
          collapse = "\n")
      if (isTRUE(escape.pipe) && format_info[["style"]] == "grid")
        output[[length(output)]] <- gsub("\\|","\\\\|", output[[length(output)]])

    } else {
      # method = viewer / browser --------------------------------
      dfSummary_html <-
        xtable::print.xtable(
          xtable::xtable(x = x, digits = 0,
                         align = ifelse("No" %in% colnames(x),
                                        paste0("cc", paste(rep("l", ncol(x) - 1), collapse="")),
                                        paste0(paste(rep("l", ncol(x) + 1), collapse="")))),
          include.rownames = FALSE, type = "html", print.results = FALSE,
          sanitize.text.function = function(x) gsub("“|”", '"', x),
          sanitize.colnames.function = function(x) gsub("\\.", " ", x),
          html.table.attributes = ifelse("html.table.attributes" %in% names(args_list),
                                         args_list[["html.table.attributes"]],
                                         'class="table table-striped table-bordered table-narrow table-responsive"')
        )

      div_list <- list()
      div_list[[1]] <- h2(sect_title)
      div_list[[2]] <- h3(data_info[["Dataframe"]])
      add_head_element("Subset", h = 4)
      div_list[[length(div_list) + 1]] <- h4("Number of rows:", data_info[["N.obs"]])
      div_list[[length(div_list) + 1]] <- br()
      div_list[[length(div_list) + 1]] <- HTML(gsub("<td> ", "<td>", dfSummary_html))

      if (isTRUE(footer)) {
        div_list[[length(div_list) + 1]] <- HTML(text = footer_note)
      }
    }
  }

  # Do the actual printing or writing to file ---------------------------------------------
  if (method == "pander") {
    cat("\n")
    cat(do.call(paste, output), file = file, append = append)
    if (file != "") {
      if (isTRUE(append))
        message(paste0("Output file appended: ", file))
      else
        message(paste0("Output file written: ", file))
      return(invisible())
    }
  }

  # output has html format --------------------------
  else {

    if (isTRUE(append)) {
      f <- file(file, open = "r", encoding = "utf-8")
      html_content_in <- paste(readLines(f, warn = FALSE, encoding = "utf-8"), collapse="\n")
      close(f)
      top_part <- sub("(^.+)(</body>.+)", "\\1", html_content_in)
      bottom_part <- sub("(^.+)(</body>.+)", "\\2", html_content_in)
      insert_part <- paste(capture.output(tags$div(class="container", div_list)), collapse="\n")
      html_content <- paste(capture.output(cat(top_part, insert_part, bottom_part)), collapse="\n")

    } else {

      html_content <-
        tags$div(class="container",
                 tags$head(tags$title(ifelse(is.na(report.title), sect_title, report.title)),
                           includeCSS(path = paste(stpath, "includes/stylesheets/bootstrap.min.css", sep="/")),
                           includeCSS(path = paste(stpath, "includes/stylesheets/custom.css", sep="/")),
                           if (!is.na(custom.css)) includeCSS(path = custom.css)),
                 div_list
        )
    }

    outfile_path <- ifelse(file == "", paste0(tempfile(),".html"), file)

    if(isTRUE(append))
      capture.output(cat(html_content), file = outfile_path)
    else
      save_html(html = html_content, file = outfile_path)

    if (method == "viewer") {
      if (file == "" || ("open" %in% names(args_list) && isTRUE(args_list$open))) {
        if(.Platform$GUI == "RStudio")
          rstudioapi::viewer(outfile_path)
        else {
          message("Method 'viewer' only valid within RStudio. Switching method to 'browser'.")
          method <- "browser"
        }
      }
    }

    # For method "browser", we don't use utils::browseURL() because of compatibility issues with RStudio
    else if (method == "browser") {
      if (file == "" || ("open" %in% names(args_list) && isTRUE(args_list$open))) {
        switch(Sys.info()[["sysname"]],
               Windows = {shell.exec(file = paste0("file:///", outfile_path))},
               Linux   = {system(paste("/usr/bin/xdg-open", outfile_path), wait = FALSE, ignore.stdout = TRUE)},
               Darwin  = {system(paste("open", outfile_path), wait = FALSE, ignore.stderr = TRUE)})
      }
    }

    # return file path and update tmpfiles vector when method = browser or viewer ----------------------
    if(file == "" && method %in% c("browser", "viewer")) {
      .st_env$tmpfiles <- c(.st_env$tmpfiles, outfile_path)
      return(invisible(normalizePath(outfile_path, winslash = "\\", mustWork = FALSE)))
    } else if (file != "") {
      if (isTRUE(append))
        message(paste0("Output file appended: ", outfile_path))
      else
        message(paste0("Output file written: ", outfile_path))
      return(invisible())
    }
  }
}
