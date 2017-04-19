#' Data frame Summary
#'
#' Summary of a data frame consisting of: variable names, labels if any, factor
#' levels, frequencies and/or numerical summary statistics, and valid/missing
#' observations information.
#'
#' @param x A data frame.
#' @param round.digits Number of significant digits to display in numerical
#'   summaries and in frequency proportions. Defaults to \code{2}.
#' @param varnumbers Should the first column contain variable number? Defaults
#'   to \code{TRUE}.
#' @param labels.col Logical. If \code{TRUE}, variable labels (as defined with
#'   \pkg{rapportools}, \pkg{Hmisc} or \pkg{summarytools}' \code{label} functions)
#'   will be displayed. By default, the \emph{labels} column is shown if at least
#'   one of the columns has a defined label.
#' @param valid.col Logical. Include column indicating count and proportion of valid
#'   (non-missing) values. \code{TRUE} by default.
#' @param na.col Logical. Include column indicating count and proportion of missing
#'   (NA) values. \code{TRUE} by default.
#' @param graph.cal Logical. Display barplots / histograms column in \emph{html}
#'   reports. \code{TRUE} by default.
#' @param style The style to be used by \code{\link[pander]{pander}} when
#'   rendering in output table. Defaults to \dQuote{multiline}. Another option is
#'   \dQuote{grid}. Style \dQuote{simple} is not supported for this particular
#'   function, and \dQuote{rmarkdown} will fallback to \dQuote{multiline}.
#' @param plain.ascii Logical \code{\link[pander]{pander}} argument. When
#'   \code{TRUE}, no markup characters will be used (useful when printing
#'   to console). Defaults to \code{TRUE}.
#' @param justify String indicating alignment of columns; one of \dQuote{left}
#'   (or \dQuote{l}), \dQuote{center} (or \dQuote{c}), or \dQuote{right}
#'   (or \dQuote{r}). Defaults to \dQuote{right}.
#' @param max.distinct.values The maximum number of values to display frequencies
#'   for. If variable has more distinct values than this parameter, the remaining
#'   frequencies will be reported as a whole, along with the number of additional
#'   distinct values.
#' @param trim.strings Logical; for character variables, should leading and
#'   trailing white space be removed? When set to \code{TRUE}, this is done
#'   \emph{before} calculating frequencies, so those will be impacted
#'   accordingly. Interpret frequencies accordingly. Defaults to \code{FALSE}.
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
#'     \item{Variable}{Name of the variable.}
#'     \item{Label}{Label of the variable.}
#'     \item{Properties}{Type & class(es) of the variable.}
#'     \item{Stats / Values}{For factors, a list of their values, limited by the
#'       \code{max.distinct.values} parameter. For character variables, the most
#'       common values (in descending frequency order), also limited by
#'       \code{max.distinct.values}. For numerical variables, common univariate
#'       statistics (mean, std. deviation, min, med, max, IQR and CV).}
#'     \item{Freqs (\% of Valid)}{For factors and character variables, the frequencies
#'       and proportions of the values listed in the previous column. For numerical
#'       vectors, number of distinct values, or frequency of distinct values if
#'       their number is not greater than \code{max.distinct.values}.}
#'     \item{Valid}{Number and proportion of valid values.}
#'     \item{Missing}{Number and proportion of missing (NA) values.}
#' }
#'
#' @details The default \code{plain.ascii = TRUE} option is there to make results
#'   appear cleaner in the console. When used in a context of markdown rendering,
#'   set this option to \code{FALSE}.
#'
#' @seealso \code{\link[base]{summary.data.frame}}
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
                      max.distinct.values = 10, trim.strings = FALSE,
                      max.string.width = 25, split.cells = 40,
                      split.table = Inf, ...) {

  parse_info <- parse_args(sys.calls(), sys.frames(), match.call())

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
    message("'display.labels' argument is deprecated; use 'labels.col' instead")
    labels.col <- display.labels
  }

  if(style=="rmarkdown") {
    message("'rmarkdown' style not supported - using 'multiline' instead.")
    style <- "multiline"
  }

  # Declare functions
  align_numbers <- function(counts, props) {

    if (nchar(length(counts)) == 1) {
      extra_space <- 0
    } else {
      extra_space <- c(rep(1, 9), rep(0, length(counts) - 9))
    }
    maxchar_cnt <- nchar(as.character(max(counts))) + extra_space
    maxchar_pct <- nchar(sprintf(paste0("%.", 1, "f"), max(props*100)))
    res <- paste(sprintf(paste0("%", maxchar_cnt, "i"), counts),
                 sprintf(paste0("(%", maxchar_pct, ".", 1, "f%%)"), props*100))
    return(res)
  }

  encode_graph <- function(data, graph_type) {
    if (graph_type == "histogram") {
      png(img_png <- tempfile(fileext = ".png"), width = 140, height = 85,
          units = "px", bg = "transparent")
      par("mar" = c(0.05,0.05,0.05,0.05))
      #if (i%%2 == 0) {
      #  par("bg" = "#f9f9f9")
      #}
      data <- data[!is.na(data)]
      breaks_x <- pretty(range(data), n = nclass.FD(data), min.n = 1)
      hist_values <- hist(data, breaks = breaks_x, plot = FALSE)
      hist(data, freq = FALSE, breaks = breaks_x, axes = FALSE,
           xlab=NULL, ylab=NULL, main=NULL, col = "grey95",border = "grey65")
      text(x = range(hist_values$mids), y = 0.025*max(hist_values$density),
           labels = round(range(data),6), pos = 4, cex = 1, srt = 90,
           offset = 0)
    } else if (graph_type == "barplot") {
      png(img_png <- tempfile(fileext = ".png"), width = 140,
          height = 20*length(data), units = "px", bg = "transparent")
      par("mar" = c(0.05,0.05,0.05,0.05))
      #if (i%%2 == 0) {
      #  par("bg" = "#f9f9f9")
      #}
      data <- rev(data)
      bp_values <- barplot(data, names.arg = "", axes = FALSE, space = 0.15,
                           col = "grey97", border = "grey65", horiz = TRUE)
      text(y = bp_values, x = 1 + (0.025*max(data)), pos = 4,
           labels = names(data), cex = 1, offset = 0)
    }

    dev.off()
    img_txt <- RCurl::base64Encode(txt = readBin(con = img_png, what = "raw",
                                                 n = file.info(img_png)[["size"]]),
                                   mode = "character")
    return(sprintf('<img src="data:image/png;base64,%s">', img_txt))
  }

  # Initialize the output data frame
  output <- data.frame(No = numeric(),
                       Variable = character(),
                       Label = character(),
                       Properties = character(),
                       Stats = character(),
                       Frequencies = character(),
                       Graph = character(),
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

    # Add column name
    output[i,2] <- paste(names(x)[i])

    # Add column label (if applicable)
    if (labels.col) {
      output[i,3] <- label(x[[i]])
      if (is.na(output[i,3]))
        output[i,3] <- ""
    }

    # Add variable properties (typeof, class)
    output[i,4] <- paste("type:",typeof(column_data),
                         "\nclass:",paste(class(column_data),
                                          collapse="\n + "), sep = "")

    # Calculate valid vs missing data info
    n_miss <- sum(is.na(column_data))
    n_valid <- n_tot - n_miss

    # For factors, display a column of levels and a column of frequencies
    if (is.factor(column_data)) {

      n_levels <- nlevels(column_data)
      counts <- table(column_data, useNA = "no")
      props <- round(prop.table(counts), 3)


      if (n_levels <= max.distinct.values) {
        output[i,5] <- paste0(1:n_levels,". ", levels(column_data), collapse = "\n")
        counts_props <- align_numbers(counts, props)
        output[i,6] <- paste(counts_props, collapse = "\n")
        output[i,7] <- encode_graph(table(column_data, useNA = "no"), "barplot")

      } else {

        # more levels than allowed by max.distinct.values
        n_extra_levels <- n_levels - max.distinct.values
        output[i,5] <- paste0(1:max.distinct.values,". ",
                              levels(column_data)[1:max.distinct.values],
                              collapse="\n")
        output[i,5] <- paste(output[i,5],
                             paste0("(", n_extra_levels, " other levels...)"),
                             sep="\n")
        counts_props <- align_numbers(counts[1:max.distinct.values], props[1:max.distinct.values])
        output[i,6] <- paste(counts_props, collapse = "\n")
        output[i,6] <- paste(output[i,6],
                             paste0(tmp.sum <- sum(counts[(max.distinct.values + 1):length(counts)]),
                                    " (", round(tmp.sum/n_valid*100,1), "%)"),
                             sep = "\n")

        # prepare data for barplot
        tmp_data <- column_data
        levels(tmp_data)[max.distinct.values + 1] <- paste0("<", n_extra_levels, " others>")
        tmp_data[which(as.numeric(tmp_data) > max.distinct.values)] <- paste0("<", n_extra_levels, " others>")
        levels(tmp_data)[(max.distinct.values + 2):n_levels] <- NA
        output[i,7] <- encode_graph(table(tmp_data, useNA = "no"), "barplot")
      }

    } else if (is.character(column_data)) {
      # For character data, display frequencies whenever possible
      if (trim.strings) {
        column_data <- sub(pattern="\\A\\s*(.+?)\\s*\\z",
                           replacement="\\1", x=column_data, perl=TRUE)
      }

      if (sum(column_data == "", na.rm = TRUE) == length(column_data)) {
        output[i,5] <- "Contains only empty strings"
        output[i,6] <- ""
        output[i,7] <- NA

      } else if (n_miss == n_tot) {
        output[i,5] <- "Contains only NA's"
        output[i,6] <- ""
        output[i,7] <- NA

      } else {
        counts <- table(column_data, useNA = "no")

        # Report all frequencies when allowed by max.distinct.values
        if (length(counts) <= max.distinct.values) {
          output[i,5] <- paste0(1:length(counts),". ", dQuote(names(counts)), collapse="\n")
          props <- round(prop.table(counts), 3)
          counts_props <- align_numbers(counts, props)
          output[i,6] <- paste(counts_props, collapse = "\n")
          output[i,7] <- encode_graph(table(column_data, useNA = "no"), "barplot")

        } else {
          # Too many values - report most common strings
          counts <- sort(counts, decreasing = TRUE)
          n_extra_values <- length(counts)-max.distinct.values
          output[i,5] <- paste0(paste0(1:max.distinct.values,". ",
                                       dQuote(substr(names(counts), 1, max.string.width)
                                              [1:max.distinct.values]),
                                       collapse="\n"),
                                paste0("\n(", n_extra_values, " other values...)"))
          props <- round(prop.table(counts), 3)
          counts_props <- align_numbers(counts[1:max.distinct.values], props[1:max.distinct.values])

          output[i,6] <- paste(counts_props, collapse = "\n")
          output[i,6] <- paste(output[i,6],
                               paste0(tmp.sum <- sum(counts[(max.distinct.values + 1):length(counts)]),
                                      " (", round(tmp.sum/n_valid*100,1), "%)"),
                               sep="\n")

          # Prepare data for graph
          counts[max.distinct.values + 1] <- sum(counts[(max.distinct.values + 1):length(counts)])
          names(counts)[max.distinct.values + 1] <- paste0("<", n_extra_values, " others>")
          counts <- counts[1:(max.distinct.values + 1)]
          output[i,7] <- encode_graph(counts, "barplot")
        }
      }

    } else if (is.numeric(column_data)) {

      # For numeric data, display a column of descriptive stats and a column of frequencies
      if (n_miss == n_tot) {
        output[i,5] <- "Contains only NA's"
        output[i,6] <- ""
        output[i,7] <- NA
      } else {
        output[i,5] <- paste("mean (sd) : ", round(mean(column_data, na.rm = TRUE), round.digits),
                             " (", round(sd(column_data, na.rm = TRUE), round.digits), ")\n",
                             "min < med < max : \n", round(min(column_data, na.rm = TRUE), round.digits),
                             " < ", round(median(column_data, na.rm = TRUE), round.digits),
                             " < ", round(max(column_data, na.rm = TRUE), round.digits), "\n",
                             "IQR (CV) : ", round(IQR(column_data, na.rm = TRUE), round.digits),
                             " (", round(sd(column_data,na.rm = TRUE) / mean(column_data, na.rm = TRUE),
                                         round.digits),
                             ")", collapse="",sep="")

        if (length(unique(column_data)) <= max.distinct.values && all(unique(abs(column_data)) >= 0.01)) {
          counts <- table(column_data, useNA="no")
          props <- round(prop.table(counts), 3)
          counts_props <- align_numbers(counts, props)
          output[i,6] <- paste(round(as.numeric(names(counts)), round.digits),
                               counts_props, sep = ": ", collapse = "\n")
        } else {
          output[i,6] <- paste(as.character(length(unique(column_data))), "distinct values")
        }
        if (length(unique(column_data)) <= max.distinct.values) {
          output[i,7] <- encode_graph(table(column_data, useNA = "no"), "barplot")
        } else {
          output[i,7] <- encode_graph(column_data, "histogram")
        }
      }
    }

    # Data does not fit in previous categories (neither numeric, character, or factor)
    else {
      output[i,5] <- ""
      if (n_miss == n_tot) {
        output[i,5] <- "Contains only NA's"
        output[i,6] <- ""
        output[i,7] <- NA

      } else if (length(unique(column_data)) <= max.distinct.values) {
        counts <- table(column_data, useNA = "no")
        props <- round(prop.table(counts), 1)
        output[i,6] <- paste(substr(names(counts),1,max.string.width),": ",
                             counts," (",props,"%)",sep="",collapse="\n")
      } else {
        output[i,6] <- paste(as.character(length(unique(column_data))),"distinct values")
      }
      output[i,7] <- NA
    }

    output[i,8]  <- paste0(n_valid, "\n(", round(n_valid/n_tot*100, 2), "%)")
    output[i,9]  <- paste0(n_miss,  "\n(", round(n_miss /n_tot*100, 2), "%)")
  }

  names(output) <- c("No", "Variable", "Label", "Properties", "Stats / Values",
                     "Freqs (% of Valid)", "Graph", "Valid", "Missing")

  if(!labels.col) {
    output$Label <- NULL
  }

  if(!varnumbers) {
    output$No <- NULL
  }

  # Set output attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "dfSummary"
  attr(output, "date") <- Sys.Date()
  attr(output, "fn_call") <- as.character(match.call())

  attr(output, "data_info") <- list(Dataframe = parse_info$df_name,
                                    Dataframe.label = ifelse("df_label" %in% names(parse_info), parse_info$df_label, NA),
                                    Subset = ifelse("rows_subset" %in% names(parse_info),
                                                    parse_info$rows_subset, NA),
                                    N.obs = nrow(x))

  attr(output, "formatting") <- list(style = style,
                                     round.digits = round.digits,
                                     plain.ascii = plain.ascii,
                                     justify = justify,
                                     split.cells = split.cells,
                                     split.table = split.table,
                                     ... = ...)
  return(output)
}
