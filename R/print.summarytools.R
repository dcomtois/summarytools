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
#' @param silent Hide console messages indicating location of temporary
#'   \emph{html} files. Defaults to \code{FALSE}, and has no effect when
#'   \code{method} is \dQuote{pander}. .
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
#'   and include \emph{Bootstrap Cascading StyleSheets}. To add custom
#'   \emph{CSS}, edit the \emph{custom.css} file located in package's
#'   \emph{includes/stylesheets} directory.
#'
#' To \strong{print objects of class \dQuote{by}}, use \code{\link{view}}. This
#'   function also makes it more practical to generate \emph{html} files (see
#'   examples).
#'
#' The following additional arguments can be used to override
#'   formatting and other attributes stored in the object to be printed:
#'    \itemize{
#'      \item \code{date}
#'      \item \code{round.digits}
#'      \item \code{justify}
#'      \item \code{plain.ascii}
#'      \item \code{split.table}
#'      \item \code{Label}
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
                               escape.pipe = FALSE, silent = FALSE,
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

  if (!silent %in% c(TRUE, FALSE))
    stop("'silent' must be either TRUE or FALSE")

  if (!footer %in% c(TRUE, FALSE))
    stop("'footer' must be either TRUE or FALSE")

  # Override of x's attributes ---------------------------------------------
  args_list <- match.call()

  if ("date" %in% names(args_list))
    attr(x, "date") <- args_list$date

  # Formatting attributes
  for (format_element in c("style", "round.digits", "justify",
                           "plain.ascii", "split.table")) {
    if (format_element %in% names(args_list))
      attr(x, "formatting")[format_element] <- args_list[format_element]
  }

  # Data info attributes
  for (data_info_element in c("Label", "Subset", "Group", "Weights",
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
                     sprintf(paste0("(%", maxchar_pct, ".", round.digits, "f %%)"),
                             props[,colnum]*100)))
      } else {
        maxchar_cnt <- nchar(as.character(max(counts[ ,colnum])))
        maxchar_pct <- nchar(sprintf(paste0("%.", round.digits, "f"), max(props[ ,colnum]*100)))
        return(paste(sprintf(paste0("%", maxchar_cnt, "i"),
                             counts[ ,colnum]),
                     sprintf(paste0("(%", maxchar_pct, ".", round.digits, "f %%)"),
                             props[,colnum]*100)))
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

  add_subtitle_elements <- function() {
    if ("Dataframe" %in% names(data_info)) {
      output[[length(output) + 1]] <<-
        paste0("\n", add_hash(paste("Dataframe", data_info[["Dataframe"]], sep = ": "), 2))

      if ("Dataframe.label" %in% names(data_info))
        output[[length(output)]] <<-
          paste0(output[[length(output)]], " (", data_info[["Dataframe.label"]], ")")
    }

    if ("Variable" %in% names(data_info)) {
      output[[length(output) + 1]] <<-
        paste0("\n", add_hash(paste("Variable:", data_info[["Variable"]])))

      if ("Variable.label" %in% names(data_info))
        output[[length(output)]] <<-
          paste0(output[[length(output)]], " (", data_info[["Variable.label"]], ")")
    }

    if ("Weights" %in% names(data_info))
      output[[length(output) + 1]] <<-
        paste0("\n", add_hash(paste("Weights:", data_info[["Weights"]])))

    if ("Subset" %in% names(data_info))
      output[[length(output) + 1]] <<-
        paste0("\n", add_hash(paste("Subset", data_info[["Subset"]], sep = ": ")))

    if ("N.obs" %in% names(data_info))
      output[[length(output) + 1]] <<-
        paste0("\n", add_hash(paste0("Number of rows: ", data_info[["N.obs"]]), 4), "\n")

    if (attr(x, "st_type") == "ctable") {
      output[[length(output) + 1]] <<-
        paste0("\n", add_hash(paste0("Row Variable: ", data_info[["Row.variable"]])))

      if ("Row.variable.label" %in% names(data_info))
        output[[length(output)]] <<-
          paste0(output[[length(output)]], " (", data_info[["Row.variable.label"]], ")")

      if ("Row.variable.subset" %in% names(data_info))
        output[[length(output) + 1]] <<-
          paste0("      Subset: ", data_info[["Row.variable.subset"]], ")")

      output[[length(output) + 1]] <<-
        paste0("\n", add_hash(paste0("Col Variable: ", data_info[["Col.variable"]])))

      if ("Col.variable.label" %in% names(data_info))
        output[[length(output)]] <<-
        paste0(output[[length(output)]], " (", data_info[["Col.variable.label"]], ")")

      if ("Col.variable.subset" %in% names(data_info))
        output[[length(output) + 1]] <<-
        paste0(output[[length(output)]], "      Subset: ", data_info[["Col.variable.subset"]], ")")

      output[[length(output) + 1]] <<-
        paste0("\n", add_hash(paste("Proportions:", attr(x, "proportions"))))
    }
  }

  if (method %in% c("browser", "viewer")) {
    stpath <- find.package("summarytools")

    # build footer note
    footer_note <- paste0("Generated by <a href='https://github.com/dcomtois/summarytools'>summarytools</a> package version ",
                          packageVersion(pkg = "summarytools"),
                          " (<a href='http://www.r-project.org/'>R</a> version ", getRversion(), ")",
                          "<br/>", Sys.Date())
  }

  # Extract non-NA "data_info" elements from x attributes
  if ("data_info" %in% names(attributes(x))) {
    data_info <- attr(x, "data_info")
    data_info <- data_info[!is.na(data_info)]

    # Isolate "Group" element
    group_index <- which(names(data_info) == "Group")
    if (length(group_index) == 1) {
      Group <- data_info[group_index]
      data_info <- data_info[-group_index]
      rm(group_index)
    } else {
      Group <- NA
    }

    # Remove redundant dataframe info in Group string when appropriate
    if (!is.na(Group) && !is.na(data_info[["Dataframe"]])) {
      sub(pattern = paste0(data_info[["Dataframe"]], "$"), replacement = "", x = Group)
    }

  } else {
    data_info <- NA
    Group <- NA
  }

  # Extract formatting information from x attributes
  format_info <- attr(x, "formatting")

  # Printing freq objects -----------------------------------------------------
  if(attr(x, "st_type") == "freq") {

    # define section title
    sect_title <- ifelse("Weights" %in% names(data_info),
                         "Weighted Frequencies",
                         "Frequencies")

    if(method == "pander") {
      output <- list()
      if (isTRUE(group.only)) {
        output[[1]] <- paste0("\n", Group)
      } else {
        output[[1]] <- paste0("\n", add_hash(sect_title, 3), "\n")
        add_subtitle_elements()
      }

      justif <- ifelse(format_info$justify == "center", "centre", format_info$justify)
      freq_table <- format(x = x,
                           trim = FALSE,
                           nsmall = format_info$round.digits,
                           digits = format_info$round.digits,
                           justify = justif)

      # Remove .00 digits in count column when weights are not used
      if (!"Weights" %in% names(data_info))
        freq_table[,1] <- sub("\\.0+", "", freq_table[,1])

      # Put NA in irrelevant cells so that pander recognizes them as such
      freq_table[nrow(freq_table)-1, 2] <- NA
      freq_table[nrow(freq_table)-1, 3] <- NA

      # Escape "<" and ">" when used in pairs in rownames
      # TODO: test potentially problematic rownames, also with style "simple"
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

      # method = viewer / browser  -----------------------------

      freq_table_html <-
        xtable::print.xtable(xtable::xtable(x = x, align = "rccccc",
                                            digits = c(0,
                                                       format_info$round * as.numeric("weights" %in% names(attributes(x))),
                                                       rep(format_info$round, 4))),
                             type = "html", print.results = FALSE,
                             sanitize.rownames.function = function(x) sub(">", "&gt;", sub("<", "&lt;", x)),
                             html.table.attributes = 'class="table table-striped table-bordered"')

      # Prepare the main "div" for the html report
      div_list <- list(
        if (!isTRUE(group.only))
          h2(sect_title),
        if (!isTRUE(group.only) && !identical(data_info, NA))
          h4(HTML(text = paste(names(data_info), data_info, sep=": ", collapse="<br/>"))),
        if (!is.na(Group))
          hr(),
        if (!is.na(Group))
          h4(Group),
        HTML(text = gsub("<td> ", "<td>", freq_table_html)),
        if (isTRUE(footer))
          HTML(text = footer_note)
      )
    }
  }

  # Printing ctable objects -----------------------------------------------------
  if(attr(x, "st_type") == "ctable") {
    sect_title <- "Cross-Tabulation"
    if(attr(x, "proportions") %in% c("Rows", "Columns", "Total")) {
      cross_table <- align_numbers(x$cross_table, x$proportions)
    } else {
      cross_table <- x$cross_table
    }

    if(method == "pander") {
      output <- list()
      if (isTRUE(group.only)) {
        output[[1]] <- paste0("Group: ", data_info[["Group"]], "\n")
      } else {
        output[[1]] <- paste0("\n", add_hash(sect_title, 3))
        add_subtitle_elements()
      }

      # Note: do not use keep.trailing.zeros = TRUE -- causes issue with pander/ftable
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
      addtorow <- list()
      addtorow$pos <- list(0, 0)
      colnames(cross_table)[colnames(cross_table) == "<NA>"] <- "&lt;NA&gt;"
      addtorow$command <- c(paste0("<tr> <th> </th> <th colspan=", ncol(cross_table)-1,">",dnn[2],"</th><th></th></tr>"),
                            paste0("<tr> <th>", dnn[1], "</th> <th>",
                                   paste(colnames(cross_table), collapse = "</th> <th>"),
                                   "</th></tr>"))

      cross_table_html <-
        xtable::print.xtable(xtable::xtable(x = cross_table,
                                            align = paste0("r", paste(rep("c", ncol(cross_table)),
                                                                      collapse=""))),
                             type = "html", print.results = FALSE,
                             add.to.row = addtorow, include.colnames = FALSE,
                             sanitize.text.function = function(x) gsub("_", "&nbsp;", x = x, fixed = TRUE),
                             sanitize.rownames.function = function(x) sub(">", "&gt;", sub("<", "&lt;", x)),
                             sanitize.colnames.function = function(x) sub(">", "&gt;", sub("<", "&lt;", x)),
                             html.table.attributes = 'class="table table-striped table-bordered monospace-cells"')

      div_list <- list(
        if (!isTRUE(group.only))
          h2(sect_title),
        if (!isTRUE(group.only) && !is.na(data_info))
          h4(HTML(text = data_info)),
        if (!is.na(Group))
          hr(),
        if (!is.na(Group))
          h4(Group),
        HTML(text = gsub("<td> ", "<td>", cross_table_html)),
        if (isTRUE(footer))
          HTML(text = footer_note)
      )
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

      if (isTRUE(group.only)) {
        output[[1]] <- paste0("\n", Group)
      } else {
        output[[1]] <- paste0("\n", add_hash(sect_title, 3))
        add_subtitle_elements()
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

      output[[length(output) + 1]] <- paste0("\n", add_hash("Observations"))
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
        xtable::print.xtable(xtable::xtable(x = x$stats,
                                            align = paste0("r", paste(rep("c",ncol(x$stats)), collapse="")),
                                            digits = c(0,rep(format_info$round,
                                                             ncol(x$stats)))),
                             type = "html", print.results = FALSE,
                             html.table.attributes = 'class="table table-striped table-bordered"')

      obs_table_html <-
        xtable::print.xtable(xtable::xtable(x = obstable,
                                            align = paste0("r", paste(rep("c",ncol(x$observ)), collapse=""))),
                             type = "html", print.results = FALSE,
                             sanitize.text.function = function(x) gsub("_", "&nbsp;", x = x, fixed = TRUE),
                             sanitize.colnames.function = function(x) sub(">", "&gt;", sub("<", "&lt;", x)),
                             sanitize.rownames.function = function(x) sub(">", "&gt;", sub("<", "&lt;", x)),
                             html.table.attributes = 'class="table table-striped table-bordered monospace-cells"')

      div_list <- list(
        if (!isTRUE(group.only))
          h2(sect_title),
        if (!isTRUE(group.only) && !is.na(data_info))
          h4(HTML(text = data_info)),
        if (!is.na(Group))
          hr(),
        if (!is.na(Group))
          h4(Group),
        HTML(text = gsub("<td> ", "<td>", descr_table_html)),
        h5("Observations"),
        HTML(gsub("<td> ", "<td>", obs_table_html)),
        if (isTRUE(footer))
          HTML(text = footer_note)
      )
    }
  }

  # Printing dfSummary objects ------------------------------------------------
  if(attr(x, "st_type") == "dfSummary") {
    sect_title <- "Dataframe Summary"
    if (method == "pander") {
      output <- list()

      # Escape symbols for words between <>'s in some columns to allow <NA> or factor levels such as
      # <ABC> to be rendered correctly
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

      output[[1]] <- paste0("\n", add_hash(sect_title, 2), "\n")
      add_subtitle_elements()

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
        xtable::print.xtable(xtable::xtable(x = x, digits = 0,
                                            align = paste0("c", paste(rep("l", ncol(x)), collapse=""))),
                             include.rownames = FALSE, type = "html", print.results = FALSE,
                             sanitize.colnames.function = function(x) gsub("\\.", " ", x),
                             html.table.attributes = 'class="table table-striped table-bordered"')

      div_list <- list(
        h2(sect_title),
        h3(data_info[["df_name"]]),
        if("Subset" %in% names(data_info))
          h4("Rows subset:", data_info[["Subset"]], br(), "Number of rows:", data_info[["N.obs"]]),
        if(!"Subset" %in% names(data_info))
          h4("Number of rows:", data_info[["N.obs"]]),
        br(),
        HTML(gsub("<td> ", "<td>", dfSummary_html)),
        if (isTRUE(footer))
          HTML(text = footer_note)
      )
    }
  }


  # Do the actual printing or writing to file ---------------------------------------------
  if (method == "pander") {
    cat(do.call(paste, output), file = file, append = append)
    if (file != "") {
      if (isTRUE(append))
        message(paste0("Output file appended: ", file))
      else
        message(paste0("Output file written: ", file))
      return(invisible())
    }
  }

  # Put together output file content when output has html format --------------------------
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
                           includeCSS(path = paste(stpath, "includes/stylesheets/custom.css", sep="/"))),
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
      if(!silent)
        message(paste0("Temporary file created: ", outfile_path, "\nTo delete, use cleartmp()."))
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
