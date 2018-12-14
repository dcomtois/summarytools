#' Data frame Summary
#'
#' Summary of a data frame consisting of: variable names, labels if any, factor
#' levels, frequencies and/or numerical summary statistics, and valid/missing
#' observation counts.
#'
#' @param x A data frame.
#' @param round.digits Number of significant digits to display. Defaults to
#'   \code{2} and can be set globally; see \code{\link{st_options}}.
#' @param varnumbers Logical. Should the first column contain variable number?
#'   Defaults to \code{TRUE}. Can be set globally; see \code{\link{st_options}},
#'   option \dQuote{dfSummary.varnumbers}.
#' @param labels.col Logical. If \code{TRUE}, variable labels (as defined with
#'   \pkg{rapportools}, \pkg{Hmisc} or \pkg{summarytools}' \code{label}
#'   functions) will be displayed. By default, the \emph{labels} column is shown
#'   if at least one column has a defined label.
#' @param valid.col Logical. Include column indicating count and proportion of
#'   valid (non-missing) values. \code{TRUE} by default, but can be set
#'   globally; see \code{\link{st_options}}, option
#'   \dQuote{dfSummary.valid.col}.
#' @param na.col Logical. Include column indicating count and proportion of
#'   missing (NA) values. \code{TRUE} by default, but can be set globally; see
#'   \code{\link{st_options}}, option \dQuote{dfSummary.na.col}.
#' @param graph.col Logical. Display barplots / histograms column in \emph{html}
#'   reports. \code{TRUE} by default, but can be set globally; see
#'   \code{\link{st_options}}, option \dQuote{dfSummary.graph.col}.
#' @param graph.magnif Numeric. Magnification factor, useful if the graphs show
#'   up too large (then use a value < 1) or too small (use a value > 1). Must be
#'   positive. Default to \code{1}. Can be set globally; see
#'   \code{\link{st_options}}, option \dQuote{dfSummary.graph.magnif}.
#' @param style Style to be used by \code{\link[pander]{pander}} when rendering
#'   output table. Defaults to \dQuote{multiline}. The only other valid option
#'   is \dQuote{grid}. Style \dQuote{simple} is not supported for this
#'   particular function, and \dQuote{rmarkdown} will fallback to
#'   \dQuote{multiline}.
#' @param plain.ascii Logical. \code{\link[pander]{pander}} argument; when
#'   \code{TRUE}, no markup characters will be used (useful when printing to
#'   console). Defaults to \code{TRUE}. Set to \code{FALSE} when in context of
#'   markdown rendering. To change the default value globally, see
#'   \code{\link{st_options}}.
#' @param justify String indicating alignment of columns; one of \dQuote{l}
#'   (left) \dQuote{c} (center), or \dQuote{r} (right). Defaults to \dQuote{l}.
#' @param headings Logical. Set to \code{FALSE} to omit headings. To change this
#'   default value globally, see \code{\link{st_options}}.
#' @param display.labels Logical. Should data frame label be displayed in the
#'   title section?  Default is \code{TRUE}. To change this default value
#'   globally, see \code{\link{st_options}}.
#' @param max.distinct.values The maximum number of values to display
#'   frequencies for. If variable has more distinct values than this number, the
#'   remaining frequencies will be reported as a whole, along with the number of
#'   additional distinct values. Defaults to 10.
#' @param trim.strings Logical; for character variables, should leading and
#'   trailing white space be removed? Defaults to \code{FALSE}. See
#'   \emph{details} section.
#' @param max.string.width Limits the number of characters to display in the
#'   frequency tables. Defaults to \code{25}.
#' @param split.cells A numeric argument passed to \code{\link[pander]{pander}}.
#'   It is the number of characters allowed on a line before splitting the cell.
#'   Defaults to \code{40}.
#' @param split.tables \pkg{pander} argument which determines the maximum width
#'   of a table. Keeping the default value (\code{Inf}) is recommended.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}}.
#'
#' @return A data frame with additional class \code{summarytools} containing as
#'   many rows as there are columns in \code{x}, with attributes to inform
#'   \code{print} method. Columns in the output data frame are:
#'   \describe{
#'     \item{No}{Number indicating the order in which column appears in the data
#'      frame.}
#'     \item{Variable}{Name of the variable, along with its class(es).}
#'     \item{Label}{Label of the variable (if applicable).} 
#'     \item{Stats / Values}{For factors, a list of their values, limited by the
#'       \code{max.distinct.values} parameter. For character variables, the most
#'        common values (in descending frequency order), also limited by
#'       \code{max.distinct.values}. For numerical variables, common univariate
#'       statistics (mean, std. deviation, min, med, max, IQR and CV).} 
#'     \item{Freqs (\% of Valid)}{For factors and character variables, the 
#'       frequencies and proportions of the values listed in the previous 
#'       column. For numerical vectors, number of distinct values, or frequency
#'       of distinct values if their number is not greater than 
#'       \code{max.distinct.values}.} 
#'     \item{Text Graph}{An ascii histogram for numerical variables, and ascii
#'       barplot for factors and character variables.} \item{Valid}{Number and 
#'       proportion of valid values.} 
#'     \item{Missing}{Number and proportion of missing (NA and NAN) values.} }
#'
#' @details The default \code{plain.ascii = TRUE} option is there to make
#'   results appear cleaner in the console. When used in a context of
#'   \emph{rmarkdown} rendering, set this option to \code{FALSE}.
#'
#'   When the \code{trim.strings} is set to \code{TRUE}, trimming is done
#'   \emph{before} calculating frequencies, so those will be impacted
#'   accordingly.
#'
#'   The package vignette \dQuote{Recommendations for Rmarkdown} provides
#'   valuable information for creating optimal \emph{Rmarkdown} documents with
#'   summarytools.
#'   
#' @examples
#' data(tobacco)
#' dfSummary(tobacco)
#' \dontrun{view(dfSummary(iris))}
#'
#' @keywords univar attribute classes category
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @importFrom dplyr n_distinct
#' @export
dfSummary <- function(x, round.digits = st_options('round.digits'), 
                      varnumbers = st_options('dfSummary.varnumbers'),
                      labels.col = length(label(x, all = TRUE)) > 0,
                      valid.col = st_options('dfSummary.valid.col'),
                      na.col = st_options('dfSummary.na.col'),
                      graph.col = st_options('dfSummary.graph.col'),
                      graph.magnif = st_options('dfSummary.graph.magnif'),
                      style = "multiline", 
                      plain.ascii = st_options('plain.ascii'),
                      justify = "left", headings = st_options('headings'),
                      display.labels = st_options('display.labels'),
                      max.distinct.values = 10, trim.strings = FALSE,  
                      max.string.width = 25, split.cells = 40,
                      split.tables = Inf, ...) {


  # Validate arguments ---------------------------------------------------------
  errmsg <- character()  # problems with arguments will be stored here
  
  # Flag to replace colname when x is not a data frame
  replace_colname <- FALSE
  if (!is.data.frame(x)) {
    xnames <- substitute(x)
    x <- try(as.data.frame(x))

    if (inherits(x, "try-error")) {
      errmsg %+=% paste(deparse(xnames), "is not coercible to a data frame")
    } else {
      message(deparse(xnames), " was converted to a data frame")
      replace_colname <- TRUE
    }
  }

  errmsg <- c(errmsg, check_arguments(match.call(), list(...)))
  
  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "))
  }
  
  # End of arguments validation ------------------------------------------------
  
  # Get info on x from parsing function ----------------------------------------
  parse_info <- try(parse_args(sys.calls(), sys.frames(), match.call(), 
                               max.varnames = as.numeric(replace_colname)),
                    silent = TRUE)
  
  if (inherits(parse_info, "try-error")) {
    parse_info <- list()
  }
  
  if (isTRUE(replace_colname) && identical(colnames(x), "x") &&
      'var_names' %in% names(parse_info) 
      && length(parse_info$var_names) == 1) {
    colnames(x) <- parse_info$var_names
  }
  
  # Initialize the output data frame -------------------------------------------
  
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

  # iterate over columns of x --------------------------------------------------
  
  for(i in seq_len(ncol(x))) {
    
    # extract column data
    column_data <- x[[i]]

    # Add column number
    output[i,1] <- i

    # Add column name and class
    output[i,2] <- paste0(names(x)[i], "\\\n[",
                          paste(class(column_data), collapse = ", "),
                          "]")

    # Add column label (if applicable)
    if (isTRUE(labels.col)) {
      output[i,3] <- label(x[[i]])
      if (is.na(output[i,3]))
        output[i,3] <- ""
    }

    # Calculate valid vs missing data info
    n_miss <- sum(is.na(column_data))
    n_valid <- n_tot - n_miss

    # Factors: display a column of levels and a column of frequencies ----------
    if (is.factor(column_data)) {
      output[i,4:7] <- crunch_factor()
    }
    
    # Character data: display frequencies whenever possible --------------------
    else if (is.character(column_data)) {
      output[i,4:7] <- crunch_character()
    }
    
    # Numeric data, display a column of descriptive stats + column of freqs ----
    else if (is.numeric(column_data)) {
      output[i,4:7] <- crunch_numeric()
    }
    
    # Time/date data -----------------------------------------------------------
    else if (inherits(column_data, c("Date", "POSIXct"))) {
      output[i,4:7] <- crunch_time_date()
    }
      
    # Data does not fit in previous categories ---------------------------------
    else {
      output[i,4:7] <- crunch_other()
    }

    output[i,8] <- 
      paste0(n_valid, "\\\n(", round(n_valid / n_tot * 100, round.digits), "%)")
    output[i,9] <- 
      paste0(n_miss,  "\\\n(", round(n_miss  / n_tot * 100, round.digits), "%)")
  }

  # Prepare output object ------------------------------------------------------
  
  names(output) <- c("No", "Variable", "Label", "Stats / Values",
                     "Freqs (% of Valid)", "Graph", "Text Graph", "Valid",
                     "Missing")

  if (!isTRUE(varnumbers)) {
    output$No <- NULL
  }

  if (!isTRUE(labels.col)) {
    output$Label <- NULL
  }

  if (!isTRUE(graph.col)) {
    output$Graph <- NULL
    output[['Text Graph']] <- NULL
  }

  if (!isTRUE(valid.col)) {
    output$Valid <- NULL
  }
  
  if (!isTRUE(na.col)) {
    output$Missing <- NULL
  }
  
  # Set output attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "dfSummary"
  attr(output, "date") <- Sys.Date()
  attr(output, "fn_call") <- match.call()

  data_info <-
    list(Dataframe = parse_info$df_name,
         Dataframe.label = ifelse("df_label" %in% names(parse_info),
                                  parse_info$df_label, NA),
         Dimensions = paste(n_tot, "x", ncol(x)),
         Duplicates = n_tot - n_distinct(x))

  attr(output, "data_info") <- data_info[!is.na(data_info)]

  attr(output, "formatting") <- list(style          = style,
                                     round.digits   = round.digits,
                                     plain.ascii    = plain.ascii,
                                     justify        = justify,
                                     headings       =  headings,
                                     display.labels = display.labels,
                                     labels.col     = labels.col,
                                     split.cells    = split.cells,
                                     split.tables   = split.tables)

  attr(output, "user_fmt") <- list(... = ...)

  return(output)
}
