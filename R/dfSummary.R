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
#' @param display.labels If \code{TRUE}, variable labels (as defined with
#'   \pkg{rapportools}, \pkg{Hmisc} or \pkg{summarytools}' \code{label} functions)
#'   will be displayed. By default, the \emph{labels} column is shown if at least
#'   one of the columns has a defined label.
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
#'     \item{Freqs (% of Valid)}{For factors and character variables, the frequencies
#'       and proportions of the values listed in the previous column. For numerical
#'       vectors, number of distinct values, or frequency of distinct values if
#'       their number is not greater than \code{max.distinct.values}.}
#'     \item{Valid / NA}{Number and proportion of valid values and NA values for the variable.}
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
                      display.labels = length(label(x, all = TRUE)) > 0,
                      style = "multiline", plain.ascii = TRUE, justify = "left",
                      max.distinct.values = 10, trim.strings = FALSE,
                      max.string.width = 25, split.cells = 40,
                      split.table = Inf, ...) {

  # use the parsing function to identify particularities such as row indexing
  parse_info <- parse_args(sys.calls(), sys.frames(), match.call())

  if(!is.data.frame(x)) {
    x <- try(as.data.frame(x))

    if(inherits(x, "try-error")) {
      stop("x is not a data frame and attempted conversion failed")
    }

    message("x was converted to a data frame")
    parse_info$df_name <- parse_info$var_name
  }

  if ("file" %in% names(match.call()))
    message("file argument is deprecated; use print() or view() function to generate files")

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

  # Initialize the output data frame
  output <- data.frame(No = numeric(),
                       Variable = character(),
                       Label = character(),
                       Properties = character(),
                       Stats = character(),
                       Frequencies = character(),
                       N.Valid = numeric(),
                       stringsAsFactors = FALSE,
                       check.names = FALSE)

  # iterate over columns of x
  for(i in seq_len(ncol(x))) {

    # extract column data
    column_data <- x[[i]]

    # Add column number
    output[i,1] <- i

    # Add column name
    output[i,2] <- paste(names(x)[i])

    # Add column label (if applicable)
    if(display.labels) {
      output[i,3] <- label(x[[i]])
      if (is.na(output[i,3]))
        output[i,3] <- ""
    }

    # Add variable properties (typeof, class)
    output[i,4] <- paste("type:",typeof(column_data),
                         "\nclass:",paste(class(column_data),collapse="\n + "), sep = "")

    # Calculate valid vs missing data info
    n_tot <- nrow(x)
    n_miss <- sum(is.na(column_data))
    n_valid <- n_tot - n_miss


    # For factors, display a column of levels and a column of frequencies
    if(is.factor(column_data)) {

      n_levels <- nlevels(column_data)
      counts <- table(column_data, useNA = "no")
      props <- round(prop.table(counts), 3)

      if(n_levels <= max.distinct.values) {
        output[i,5] <- paste0(1:n_levels,". ", levels(column_data), collapse = "\n")
        counts_props <- align_numbers(counts, props)
        output[i,6] <- paste(1:n_levels, counts_props, sep = ": ", collapse = "\n")
      } else {
        # more levels than allowed by max.distinct.values
        output[i,5] <- paste0(1:max.distinct.values,". ",
                              levels(column_data)[1:max.distinct.values],
                              collapse="\n")
        output[i,5] <- paste(output[i,5],
                             paste0("... (", n_levels - max.distinct.values, " other levels)"),
                             sep="\n")
        counts_props <- align_numbers(counts[1:max.distinct.values], props[1:max.distinct.values])
        output[i,6] <- paste(1:max.distinct.values, counts_props, sep = ": ", collapse = "\n")
        output[i,6] <- paste(output[i,6],
                             paste0("Others: ",
                                    tmp.sum <- sum(counts[(max.distinct.values + 1):length(counts)]),
                                    " (", round(tmp.sum/n_valid*100,1), "%)"),
                             sep = "\n")
      }
    }

    # For character data, display frequencies whenever possible
    else if(is.character(column_data)) {
      trimmed <- sub(pattern="\\A\\s*(.+?)\\s*\\z", replacement="\\1", x=column_data, perl=TRUE)

      if(trim.strings)
        column_data <- trimmed

      # Report if column contains only empty strings
      if(identical(unique(trimmed),"")) {
        output[i,5] <- "Contains only empty strings"
        output[i,6] <- ""
      }

      # Report if column contains only NA's
      else if(identical(unique(column_data), as.character(NA))) {
        output[i,5] <- "Contains only NA's"
        output[i,6] <- ""
      } else {
        # Generate a frequency table
        counts <- table(column_data, useNA = "no")

        # Report all frequencies when allowed by max.distinct.values
        if(length(counts) <= max.distinct.values) {
          output[i,5] <- paste0(1:length(counts),". ", dQuote(names(counts)), collapse="\n")
          props <- round(prop.table(counts), 3)
          counts_props <- align_numbers(counts, props)
          output[i,6] <- paste(1:length(counts), counts_props, sep = ": ", collapse = "\n")

          #output[i,6] <- paste0(1:length(counts), ": ", counts," (", props, "%)", collapse="\n")

        } else {
          # Two many values - report most common strings
          counts <- sort(counts, decreasing = TRUE)
          output[i,5] <- paste0("Most frequent:\n",
                                paste0(1:max.distinct.values,". ",
                                       dQuote(substr(names(counts), 1, max.string.width)
                                              [1:max.distinct.values]),
                                       collapse="\n"),
                                paste0("\n... (", length(counts)-max.distinct.values, " other values)"))
          props <- round(prop.table(counts),3)
          counts_props <- align_numbers(counts[1:max.distinct.values], props[1:max.distinct.values])

          output[i,6] <- paste(1:max.distinct.values, counts_props, sep = ": ", collapse = "\n")
          output[i,6] <- paste(output[i,6],
                               paste0("Others: ",
                                      tmp.sum <- sum(counts[(max.distinct.values + 1):length(counts)]),
                                      " (", round(tmp.sum/n_valid*100,1), "%)"),
                               sep="\n")
          output[i,6] <- paste(strrep("- ", floor(max(nchar(strsplit(output[i,6], "\n")[[1]])) / 2)),
                               output[i,6], sep = "\n")
        }
      }
    }

    # For numeric data, display a column of descriptive stats and a column of frequencies
    else if(is.numeric(column_data)) {
      if(identical(unique(column_data), as.numeric(NA))) {
        output[i,5] <- "Contains only NA's"
        output[i,6] <- ""
      } else {
        output[i,5] <- paste("mean (sd) = ", round(mean(column_data, na.rm = TRUE), round.digits),
                             " (", round(sd(column_data, na.rm = TRUE), round.digits), ")\n",
                             "min < med < max = \n", round(min(column_data, na.rm = TRUE), round.digits),
                             " < ", round(median(column_data, na.rm = TRUE), round.digits),
                             " < ", round(max(column_data, na.rm = TRUE), round.digits), "\n",
                             "IQR (CV) = ", round(IQR(column_data, na.rm = TRUE), round.digits),
                             " (", round(sd(column_data,na.rm = TRUE) / mean(column_data, na.rm = TRUE),
                                         round.digits),
                             ")", collapse="",sep="")

        if(length(unique(column_data)) <= max.distinct.values && all(unique(column_data) >= 0.01)) {
          counts <- table(column_data, useNA="no")
          props <- round(prop.table(counts), 3)
          counts_props <- align_numbers(counts, props)

          output[i,6] <- paste(round(as.numeric(names(counts)), round.digits), counts_props, sep = ": ", collapse = "\n")
        } else {
          output[i,6] <- paste(as.character(length(unique(column_data))), "distinct values")
        }
      }
    }

    # Data does not fit in previous categories (neither numeric, character, or factor)
    else {
      output[i,5] <- ""
      if(identical(as.logical(unique(column_data)), NA)) {
        output[i,5] <- "Contains only NA's"
        output[i,6] <- ""
      } else if(length(unique(column_data)) <= max.distinct.values) {
        counts <- table(column_data,useNA="no")
        props <- round(prop.table(counts),1)
        output[i,6] <- paste(substr(names(counts),1,max.string.width),": ",
                             counts," (",props,"%)",sep="",collapse="\n")
      } else {
        output[i,6] <- paste(as.character(length(unique(column_data))),"distinct values")
      }
    }

    valid_missing <- align_numbers(counts = c(n_valid, n_miss), props = c(n_valid, n_miss) / n_tot)
    output[i,7] <- paste0("Val: ", valid_missing[1], "\nNA : ", valid_missing[2])
    output[i,7] <- gsub("(100|0)\\.0%", "\\1%", output[i,7])

  }

  names(output) <- c("No", "Variable", "Label", "Properties", "Stats / Values",
                     "Freqs (% of Valid)", "Valid / NA")

  if(!display.labels)
    output$Label <- NULL

  if(!varnumbers)
    output$No <- NULL

  # Set output attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "dfSummary"
  attr(output, "date") <- Sys.Date()
  attr(output, "fn_call") <- as.character(match.call())

  attr(output, "data_info") <- c(Dataframe = parse_info$df_name,
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
