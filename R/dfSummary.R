#' Data frame Summary
#'
#' Summary of a data frame consisting of: variable names, labels if any, factor
#' levels, frequencies and/or numerical summary statistics, and valid/missing
#' observation counts.
#'
#' @param x A data frame.
#' @param round.digits Number of significant digits to display in numerical
#'   summaries and in frequency proportions. Defaults to \code{2}.
#' @param varnumbers Logical. Should the first column contain variable number? Defaults
#'   to \code{TRUE}.
#' @param labels.col Logical. If \code{TRUE}, variable labels (as defined with
#'   \pkg{rapportools}, \pkg{Hmisc} or \pkg{summarytools}' \code{label} functions)
#'   will be displayed. By default, the \emph{labels} column is shown if at least
#'   one column has a defined label.
#' @param valid.col Logical. Include column indicating count and proportion of valid
#'   (non-missing) values. \code{TRUE} by default.
#' @param na.col Logical. Include column indicating count and proportion of missing
#'   (NA) values. \code{TRUE} by default.
#' @param graph.col Logical. Display barplots / histograms column in \emph{html}
#'   reports. \code{TRUE} by default.
#' @param style Style to be used by \code{\link[pander]{pander}} when
#'   rendering output table. Defaults to \dQuote{multiline}. The only other valid
#'   option is \dQuote{grid}. Style \dQuote{simple} is not supported for this particular
#'   function, and \dQuote{rmarkdown} will fallback to \dQuote{multiline}.
#' @param plain.ascii Logical. \code{\link[pander]{pander}} argument; When
#'   \code{TRUE}, no markup characters will be generated (useful when printing
#'   to console). Defaults to \code{TRUE}.
#' @param justify String indicating alignment of columns; one of \dQuote{l} (left)
#'   \dQuote{c} (center), or \dQuote{r} (right). Defaults to \dQuote{l}.
#' @param omit.headings Logical. Set to \code{TRUE} to omit headings.
#' @param max.distinct.values The maximum number of values to display frequencies
#'   for. If variable has more distinct values than this number, the remaining
#'   frequencies will be reported as a whole, along with the number of additional
#'   distinct values. Defaults to 10.
#' @param trim.strings Logical; for character variables, should leading and
#'   trailing white space be removed? Defaults to \code{FALSE}. See \emph{details}
#'   section.
#' @param max.string.width Limits the number of characters to display in the
#'   frequency tables. Defaults to \code{25}.
#' @param split.cells A numeric argument passed to \code{\link[pander]{pander}}.
#'   It is the number of characters allowed on a line before splitting the cell.
#'   Defaults to \code{40}.
#' @param split.table \pkg{pander} argument which determines the maximum
#'   width of a table. Keeping the default value (\code{Inf}) is recommended.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}}.
#'
#' @return A data frame containing as many rows as there are columns in \code{x},
#'   with additional attributes to inform \code{print} function. Columns of the
#'   output data frame are:
#'   \describe{
#'     \item{No}{Number indicating the order in which column appears in
#'       the data frame.}
#'     \item{Variable}{Name of the variable, along with its class(es).}
#'     \item{Label}{Label of the variable (if applicable).}
#'     \item{Stats / Values}{For factors, a list of their values, limited by the
#'       \code{max.distinct.values} parameter. For character variables, the most
#'       common values (in descending frequency order), also limited by
#'       \code{max.distinct.values}. For numerical variables, common univariate
#'       statistics (mean, std. deviation, min, med, max, IQR and CV).}
#'     \item{Freqs (\% of Valid)}{For factors and character variables, the frequencies
#'       and proportions of the values listed in the previous column. For numerical
#'       vectors, number of distinct values, or frequency of distinct values if
#'       their number is not greater than \code{max.distinct.values}.}
#'     \item{Text Graph}{An ascii histogram for numerical variables, and ascii
#'       barplot for factors and character variables.}
#'     \item{Valid}{Number and proportion of valid values.}
#'     \item{Missing}{Number and proportion of missing (NA) values, including NaN's.}
#' }
#'
#' @details The default \code{plain.ascii = TRUE} option is there to make results
#'   appear cleaner in the console. When used in a context of \emph{rmarkdown} rendering,
#'   set this option to \code{FALSE}.
#'
#'   When the \code{trim.strings} is set to \code{TRUE}, trimming is done
#'   \emph{before} calculating frequencies, so those will be impacted
#'   accordingly.
#'
#' @examples
#' data(tobacco)
#' dfSummary(tobacco)
#' \dontrun{view(dfSummary(iris))}
#'
#' @keywords univar attribute classes category
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
dfSummary <- function(x, round.digits = 2, varnumbers = TRUE,
                      labels.col = length(label(x, all = TRUE)) > 0,
                      valid.col = TRUE, na.col = TRUE, graph.col = TRUE,
                      style = "multiline", plain.ascii = TRUE, justify = "left",
                      omit.headings = FALSE,  max.distinct.values = 10,
                      trim.strings = FALSE,  max.string.width = 25, split.cells = 40,
                      split.table = Inf, ...) {

  parse_info <- try(parse_args(sys.calls(), sys.frames(), match.call()), silent = TRUE)
  if (class(parse_info) == "try-catch") {
    parse_info <- list()
  }

  if (!is.data.frame(x)) {
    x <- try(as.data.frame(x))

    if (inherits(x, "try-error")) {
      stop("x is not a data frame and attempted conversion failed")
    }

    message("x was converted to a data frame")
    parse_info$df_name <- parse_info$var_name

  }

  if ("file" %in% names(match.call())) {
    message("file argument is deprecated; use print() or view() function to generate files")
  }

  if ("display.labels" %in% names(match.call())) {
    stop("'display.labels' argument is deprecated; use 'labels.col' instead")
  }

  if(style=="rmarkdown") {
    message("'rmarkdown' style not supported - using 'multiline' instead.")
    style <- "multiline"
  }

  # Declare functions
  align_numbers <- function(counts, props) {
    maxchar_cnt <- nchar(as.character(max(counts)))
    maxchar_pct <- nchar(sprintf(paste0("%.", 1, "f"), max(props*100)))
    res <- paste(sprintf(paste0("%", maxchar_cnt, "i"), counts),
                 sprintf(paste0("(%", maxchar_pct, ".", 1, "f%%)"), props*100))
    return(res)
  }

  encode_graph <- function(data, graph_type) {
    if (graph_type == "histogram") {
      png(img_png <- tempfile(fileext = ".png"), width = 150, height = 100,
          units = "px", bg = "transparent")
      par("mar" = c(0.03,0.01,0.07,0.01))
      data <- data[!is.na(data)]
      breaks_x <- pretty(range(data), n = nclass.FD(data), min.n = 1)
      hist_values <- hist(data, breaks = breaks_x, plot = FALSE)
      hist(data, freq = FALSE, breaks = breaks_x, axes = FALSE,
           xlab=NULL, ylab=NULL, main=NULL, col = "grey95", border = "grey65")
    } else if (graph_type == "barplot") {
      png(img_png <- tempfile(fileext = ".png"), width = 150,
          height = 26*length(data), units = "px",
          bg = "transparent")
      par("mar" = c(0.03,0.01,0.05,0.01))
      data <- rev(data)
      bp_values <- barplot(data, names.arg = "", axes = FALSE, space = 0.2,
                           col = "grey97", border = "grey65", horiz = TRUE)
    }

    dev.off()
    img_txt <- RCurl::base64Encode(txt = readBin(con = img_png, what = "raw",
                                                 n = file.info(img_png)[["size"]]),
                                   mode = "character")
    return(sprintf('<img src="data:image/png;base64,%s">', img_txt))
  }

  txtbarplot <- function(props, maxwidth = 16) {
    widths <- props / max(props) * maxwidth
    outstr <- character(0)
    for (i in seq_along(widths)) {
      outstr <- paste(outstr, paste0(rep(x = "I", times = widths[i]), collapse = ""),
                      sep = "  \n  ")
    }
    return(outstr)
  }

  txthist <- function(data) {
    data <- data[!is.na(data)]
    breaks_x <- pretty(range(data), n = nclass.FD(data), min.n = 1)
    if (length(breaks_x) <= 12) {
      counts <- hist(data, breaks = breaks_x, plot = FALSE)$counts
    } else {
      counts <- as.vector(table(cut(data, breaks = 12)))
    }

    # make counts top at 12
    counts <- matrix(round(counts / max(counts) * 12), nrow = 1, byrow = TRUE)
    graph <- matrix(data = "", nrow = 6, ncol = length(counts))
    for (ro in 6:1) {
      for (co in 1:length(counts)) {
        if (counts[co] > 1) {
          graph[ro,co] <- ": "
        } else if (counts[co] > 0) {
          graph[ro,co] <- ". "
        } else {
          if (sum(counts[1, co:length(counts)] > 0)) {
            graph[ro,co] <- "\\ \\ "
          }
        }
      }
      counts <- matrix(apply(X = counts - 2, MARGIN = 2, FUN = max, 0),
                       nrow = 1, byrow = TRUE)
    }
    graphlines <- character()
    for (ro in seq_len(nrow(graph))) {
      graphlines[ro] <-  trimws(paste(graph[ro,], collapse = ""), "right")
    }
    return(paste(graphlines, collapse = "\n"))
  }

  # Initialize the output data frame
  output <- data.frame(No = numeric(),
                       Variable = character(),
                       Label = character(),
                       Stats = character(),
                       Frequencies = character(),
                       Graph_html = character(),
                       Graph_ascii = character(),
                       Valid = character(),
                       Missing = character(),
                       stringsAsFactors = FALSE,
                       check.names = FALSE)

  n_tot <- nrow(x)

  # iterate over columns of x
  for(i in seq_len(ncol(x))) {

    # extract column data
    column_data <- x[[i]]

    # Add column number
    output[i,1] <- i

    # Add column name and class
    output[i,2] <- paste0(names(x)[i], "  \n[",
                          paste(class(column_data), collapse = ", "),
                          "]")

    # Add column label (if applicable)
    if (labels.col) {
      output[i,3] <- label(x[[i]])
      if (is.na(output[i,3]))
        output[i,3] <- ""
    }

    # Calculate valid vs missing data info
    n_miss <- sum(is.na(column_data))
    n_valid <- n_tot - n_miss

    if (is.factor(column_data)) {

      # For factors, display a column of levels and a column of frequencies

      n_levels <- nlevels(column_data)
      counts <- table(column_data, useNA = "no")
      props <- round(prop.table(counts), round.digits + 2)


      if (n_levels <= max.distinct.values) {
        output[i,4] <- paste0(1:n_levels,". ", levels(column_data), collapse = "  \n")
        counts_props <- align_numbers(counts, props)
        output[i,5] <- paste(counts_props, collapse = "  \n")
        if (graph.col) {
          output[i,6] <- encode_graph(counts, "barplot")
          output[i,7] <- txtbarplot(prop.table(counts))
        }

      } else {

        # more levels than allowed by max.distinct.values
        n_extra_levels <- n_levels - max.distinct.values
        output[i,4] <- paste0(1:max.distinct.values,". ",
                              levels(column_data)[1:max.distinct.values],
                              collapse="  \n")
        output[i,4] <- paste(output[i,4],
                             paste("[", n_extra_levels, "others", "]"),
                             sep="  \n")

        counts_props <- align_numbers(
          c(counts[1:max.distinct.values],
            sum(counts[(max.distinct.values + 1):length(counts)])),
          c(props[1:max.distinct.values],
            sum(props[(max.distinct.values + 1):length(props)]))
        )
        output[i,5] <- paste(counts_props, collapse = "  \n")

        if (graph.col) {
          # prepare data for barplot
          tmp_data <- column_data
          levels(tmp_data)[max.distinct.values + 1] <- paste("[", n_extra_levels, "others", "]")
          tmp_data[which(as.numeric(tmp_data) > max.distinct.values)] <-
            paste("[", n_extra_levels, "others", "]")
          levels(tmp_data)[(max.distinct.values + 2):n_levels] <- NA
          output[i,6] <- encode_graph(table(tmp_data), "barplot")
          output[i,7] <- txtbarplot(prop.table(table(tmp_data)))
        }
      }

    } else if (is.character(column_data)) {

      # For character data, display frequencies whenever possible

      if (trim.strings) {
        column_data <- sub(pattern="\\A\\s*(.+?)\\s*\\z",
                           replacement="\\1", x=column_data, perl=TRUE)
      }

      if (sum(column_data == "", na.rm = TRUE) == length(column_data)) {
        output[i,4] <- "All empty strings"
        output[i,5] <- ""
        output[i,6] <- NA
        output[i,7] <- ""

      } else if (n_miss == n_tot) {
        output[i,4] <- "All NA's"
        output[i,5] <- ""
        output[i,6] <- NA
        output[i,7] <- ""

      } else {
        counts <- table(column_data, useNA = "no")

        # Report all frequencies when allowed by max.distinct.values
        if (length(counts) <= max.distinct.values) {
          output[i,4] <- paste0(1:length(counts),". ", names(counts), collapse="  \n")
          props <- round(prop.table(counts), round.digits + 2)
          counts_props <- align_numbers(counts, props)
          output[i,5] <- paste(counts_props, collapse = "  \n")
          output[i,6] <- encode_graph(counts, "barplot")
          output[i,7] <- txtbarplot(prop.table(counts))

        } else {
          # Too many values - report most common strings
          counts <- sort(counts, decreasing = TRUE)
          props <- round(prop.table(counts), round.digits + 2)
          n_extra_values <- length(counts)-max.distinct.values
          n_extra_values <- length(counts)-max.distinct.values
          output[i,4] <- paste0(
            paste0(1:max.distinct.values,". ",
                   substr(names(counts), 1, max.string.width)[1:max.distinct.values],
                   collapse="  \n"),
            paste("  \n[", n_extra_values, "others", "]")
          )
          counts_props <- align_numbers(
            counts = c(counts[1:max.distinct.values],
                       sum(counts[(max.distinct.values + 1):length(counts)])),
            props = c(props[1:max.distinct.values],
                      sum(props[(max.distinct.values + 1):length(props)])))
          output[i,5] <- paste(counts_props, collapse = "  \n")

          if (graph.col) {
            # Prepare data for graph
            counts[max.distinct.values + 1] <-
              sum(counts[(max.distinct.values + 1):length(counts)])
            names(counts)[max.distinct.values + 1] <- paste0("[ ", n_extra_values, " others ]")
            counts <- counts[1:(max.distinct.values + 1)]
            output[i,6] <- encode_graph(counts, "barplot")
            output[i,7] <- txtbarplot(prop.table(counts))
          }
        }
      }

    } else if (is.numeric(column_data)) {

      # For numeric data, display a column of descriptive stats and a column of frequencies
      if (n_miss == n_tot) {
        output[i,4] <- "All NA's"
        output[i,5] <- ""
        output[i,6] <- NA
        output[i,7] <- ""
      } else {
        output[i,4] <- paste(
          "mean (sd) : ", round(mean(column_data, na.rm = TRUE), round.digits),
          " (", round(sd(column_data, na.rm = TRUE), round.digits), ")  \n",
          "min < med < max :  \n", round(min(column_data, na.rm = TRUE), round.digits),
          " < ", round(median(column_data, na.rm = TRUE), round.digits),
          " < ", round(max(column_data, na.rm = TRUE), round.digits), "  \n",
          "IQR (CV) : ", round(IQR(column_data, na.rm = TRUE), round.digits),
          " (", round(sd(column_data,na.rm = TRUE) / mean(column_data, na.rm = TRUE),
                      round.digits),
          ")", collapse="",sep=""
        )

        counts <- table(column_data, useNA = "no")
        extra_space <- FALSE

        if (length(counts) <= max.distinct.values &&
            all(abs(as.numeric(names(counts))) >= 0.01)) {
          props <- round(prop.table(counts), round.digits + 2)
          counts_props <- align_numbers(counts, props)
          output[i,5] <- paste(
            paste0(roundval <- round(as.numeric(names(counts)), round.digits),
                   ifelse(names(counts) != roundval, "!", " ")),
            counts_props, sep = ": ", collapse = "  \n"
          )
          if (any(names(counts) != roundval)) {
            extra_space <- TRUE
            output[i,5] <- paste(output[i,5], "! rounded", sep = "  \n")
          } 
        } else {
          output[i,5] <- paste(length(counts), "distinct val.")
        }

        if (graph.col) {
          if (length(counts) <= max.distinct.values) {
            output[i,6] <- encode_graph(counts, "barplot")
            if (isTRUE(extra_space)) {
              output[i,6] <- paste0(output[i,6], "  \n\n")
            }
            output[i,7] <- txtbarplot(prop.table(counts))
          } else {
            output[i,6] <- encode_graph(column_data, "histogram")
            output[i,7] <- txthist(column_data)
          }
        }
      }
      
    } else {

      # Data does not fit in previous categories (neither numeric, character, or factor)
      output[i,4] <- ""
      counts <- table(column_data, useNA = "no")

      if (n_miss == n_tot) {
        output[i,4] <- "Contains only NA's"
        output[i,5] <- ""
        output[i,6] <- NA
        output[i,7] <- ""

      } else if (length(counts) <= max.distinct.values) {
        props <- round(prop.table(counts), round.digits + 2)
        counts_props <- align_numbers(counts, props)
        output[i,5] <- paste(counts_props, collapse = "  \n")

      } else {
        output[i,5] <- paste(as.character(length(unique(column_data))), "distinct val.")
      }
      output[i,6] <- NA
      output[i,7] <- ""
    }

    output[i,8] <- paste0(n_valid, "  \n(", round(n_valid / n_tot * 100, round.digits), "%)")
    output[i,9] <- paste0(n_miss,  "  \n(", round(n_miss  / n_tot * 100, round.digits), "%)")
  }

  names(output) <- c("No", "Variable", "Label", "Stats / Values",
                     "Freqs (% of Valid)", "Graph", "Text Graph", "Valid", "Missing")

  if(!labels.col) {
    output$Label <- NULL
  }

  if(!varnumbers) {
    output$No <- NULL
  }

  if(!graph.col) {
    output$Graph <- NULL
    output[['Text Graph']] <- NULL
  }

  # Set output attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "dfSummary"
  attr(output, "date") <- Sys.Date()
  attr(output, "fn_call") <- as.character(match.call())

  data_info <-
    list(Dataframe = parse_info$df_name,
         Dataframe.label = ifelse("df_label" %in% names(parse_info),
                                  parse_info$df_label, NA),
         Subset = ifelse("rows_subset" %in% names(parse_info),
                         parse_info$rows_subset, NA),
         N.obs = nrow(x))

  attr(output, "data_info") <- data_info[!is.na(data_info)]

  attr(output, "formatting") <- list(style = style,
                                     round.digits = round.digits,
                                     plain.ascii = plain.ascii,
                                     justify = justify,
                                     omit.headings = omit.headings,
                                     split.cells = split.cells,
                                     split.table = split.table)

  attr(output, "user_fmt") <- list(... = ...)

  return(output)
}
