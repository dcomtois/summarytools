#' Frequency Tables for Factors and Other Discrete Data
#'
#' Displays weighted or unweighted frequencies, including <NA> counts and
#' proportions.
#'
#' @param x Factor or vector
#' @param round.digits Number of significant digits to display. Defaults
#'   to \code{2}.
#' @param order Ordering of rows in frequency table; \dQuote{names} (default for
#'   non-factors), \dQuote{levels} (default for factors), or \dQuote{freq}
#'   (from most frequent to less frequent).
#' @param style Style to be used by \code{\link[pander]{pander}} when rendering
#'   output table; One of \dQuote{simple} (default), \dQuote{grid} or
#'   \dQuote{rmarkdown}.
#' @param plain.ascii Logical \code{\link[pander]{pander}} argument. When
#'   \code{TRUE}, no markup characters will be used (useful when printing
#'   to console). Defaults to \code{TRUE} when \code{style} is \dQuote{simple},
#'   and \code{FALSE} otherwise.
#' @param justify String indicating alignment of columns; one of \dQuote{left}
#'   (or \dQuote{l}), \dQuote{center} (or \dQuote{c}), or \dQuote{right}
#'   (or \dQuote{r}). Defaults to \dQuote{right}.
#' @param missing Characters to display in NA cells. Defaults to \dQuote{}.
#' @param display.type Logical. Should variable type be displayed? Default is \code{TRUE}.
#' @param display.labels Logical. Should variable / data frame labels be displayed?
#'   Default is \code{TRUE}.
#' @param weights Vector of weights; must be of the same length as \code{x}.
#' @param rescale.weights Logical parameter. When set to \code{TRUE}, the total
#'   count will be the same as the unweighted \code{x}. \code{FALSE} by default.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}}.
#'
#' @return A frequency table of class \code{matrix} with added attributes used
#'   by \pkg{summarytool}'s \emph{print} method.
#'
#' @details The default \code{plain.ascii = TRUE} option is there to make results
#'   appear cleaner in the console. To avoid rmarkdown rendering problems, the
#'   option is automatically set to \code{FALSE} whenever
#'   \code{style = "rmarkdown"} (unless \code{plain.ascii = TRUE} is made
#'   explicit). If the intent is to produce markdown text using {style = "simple"},
#'   set \code{plain.ascii} to \code{FALSE}.
#'
#' @examples
#' data(tobacco)
#' freq(tobacco$gender)
#' freq(tobacco$gender, style="rmarkdown")
#' with(tobacco, by(smoker, gender, freq))
#'
#' @seealso \code{\link[base]{table}}
#'
#' @keywords univar classes category
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
freq <- function(x, round.digits = 2, order = "names", style = "simple",
                 plain.ascii = TRUE, justify = "right",
                 missing = "", display.type = TRUE, display.labels = TRUE,
                 weights = NA, rescale.weights = FALSE, ...) {

  # Parameter validation ---------------------------------------

  # if x is a data.frame with 1 column, extract this column as x
  if (!is.null(ncol(x)) && ncol(x)==1) {
    x <- x[[1]]
  }

  if (!is.atomic(x)) {
    x <- try(as.vector(x), silent = TRUE)
    if (class(x) == "try-except" || !is.atomic(x)) {
      stop("argument x must be a vector or a factor")
    }
  }

  if (!is.numeric(round.digits) || round.digits < 1) {
    stop("'round.digits' argument must be numerical and >= 1")
  }

  order <- switch(tolower(substring(order, 1, 1)),
                  l = "levels",
                  f = "freq",
                  n = "names")

  if (!order %in% c("levels", "freq", "names")) {
    stop("'order' argument must be one of 'level', 'freq' or 'names'")
  }

  if (order == "levels" && !is.factor(x)) {
    stop("'order' argument can be set to 'factor' only for factors. Use 'names'
         or 'freq', or convert object to factor.")
  }

  if (!style %in% c("simple", "grid", "rmarkdown")) {
    stop("'style' argument must be one of 'simple', 'grid' or 'rmarkdown'")
  }

  if (!plain.ascii %in% c(TRUE, FALSE)) {
    stop("'plain.ascii' argument must either TRUE or FALSE")
  }

  justify <- switch(tolower(substring(justify, 1, 1)),
                    l = "left",
                    c = "center",
                    m = "center", # to allow 'middle'
                    r = "right")

  if (!justify %in% c("left", "center", "right")) {
    stop("'justify' argument must be one of 'left', 'center' or 'right'")
  }

  # When style is 'rmarkdown', make plain.ascii FALSE unless specified explicitly
  if (style %in% c("grid", "rmarkdown") && !"plain.ascii" %in% (names(match.call()))) {
    plain.ascii <- FALSE
  }

  if ("file" %in% names(match.call())) {
    message(paste0("'file' argument is deprecated; use for instance ",
                   "print(x, file='a.txt') or view(x, file='a.html') instead"))
  }

  # Replace NaN's by NA's (This simplifies matters a lot!)
  if (NaN %in% x)  {
    message(paste(sum(is.nan(x)), "NaN value(s) converted to NA\n"))
    x[is.nan(x)] <- NA
  }

  # Get into about x from parsing function
  parse_info <- try(parse_args(sys.calls(), sys.frames(), match.call()), silent = TRUE)
  if (class(parse_info) == "try-catch") {
    parse_info <- list()
  }

  # create a basic frequency table, always including the NA row
  if (identical(NA, weights)) {
    freq_table <- table(x, useNA = "always")

    # Order by frequency if needed
    if (order == "freq") {
      freq_table <- sort(freq_table, decreasing = TRUE)
      na_pos <- which(is.na(names(freq_table)))
      freq_table <- c(freq_table[-na_pos], freq_table[na_pos])
    }

    # Change the name of the NA item (last) to avoid potential problems when echoing to console
    names(freq_table)[length(freq_table)] <- "<NA>"

    # calculate proportions (valid, i.e excluding NA's)
    P_valid <- prop.table(freq_table[-length(freq_table)]) * 100

    # Add '<NA>' item to the proportions; this assures
    # proper length when cbind'ing later on
    P_valid["<NA>"] <- NA

    # calculate proportions (total, i.e. including NA's)
    P_tot <- prop.table(freq_table) * 100
  }

  # Weights are used
  else {

    # Check that weights vector is of the right length
    if (length(weights) != length(x)) {
      stop("weights vector must be of same length as x")
    }

    weights_string <- deparse(substitute(weights))
    weights_label <- try(label(weights), silent = TRUE)

    if (sum(is.na(weights)) > 0) {
      warning("Missing values on weight variable have been detected and were treated as zeroes.")
      weights[is.na(weights)] <- 0
    }

    if (isTRUE(rescale.weights)) {
      weights <- weights / sum(weights) * length(x)
    }

    freq_table <- xtabs(formula = weights ~ x)

    # Order by frequency if needed
    if (order == "freq")
      freq_table <- freq_table[order(freq_table, decreasing = TRUE)]

    P_valid <- prop.table(freq_table) * 100
    P_valid["<NA>"] <- NA
    freq_table["<NA>"] <- sum(weights) - sum(xtabs(formula = weights ~ x))
    P_tot <- prop.table(freq_table) * 100
  }

  # Calculate cumulative proportions
  P_valid_cum <- cumsum(P_valid)
  P_valid_cum["<NA>"] <- NA
  P_tot_cum <- cumsum(P_tot)

  # Combine the info to build the final frequency table
  output <- cbind(freq_table, P_valid, P_valid_cum, P_tot, P_tot_cum)
  output <- rbind(output, c(colSums(output, na.rm = TRUE)[1:2], rep(100,3)))
  colnames(output) <- c("Freq", "% Valid", "% Valid Cum.", "% Total", "% Total Cum.")
  rownames(output) <- c(names(freq_table), "Total")

  # Update the output class and attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "freq"
  attr(output, "fn_call") <- as.character(match.call())
  attr(output, "date") <- Sys.Date()

  data_info <-
    list(Dataframe       = ifelse("df_name" %in% names(parse_info), parse_info$df_name, NA),
         Dataframe.label = ifelse("df_label" %in% names(parse_info), parse_info$df_label, NA),
         Variable        = ifelse("var_names" %in% names(parse_info), parse_info$var_names, NA),
         Variable.label  = label(x),
         Data.type       = ifelse(is.factor(x) && is.ordered(x), "Factor (ordered)",
                                  ifelse(is.factor(x), "Factor (unordered)",
                                         ifelse(is.character(x), "Character",
                                                ifelse(is.numeric(x), "Numeric", class(x))))),
         Subset          = ifelse("rows_subset" %in% names(parse_info), parse_info$rows_subset, NA),
         Weights         = ifelse(identical(weights, NA), NA,
                                  sub(pattern = paste0(parse_info$df_name, "$"), replacement = "",
                                      x = weights_string, fixed = TRUE)),
         Weights.label   = ifelse(!identical(weights, NA) && class(weights_label) != "try-error",
                                  weights_label, NA),
         Group    = ifelse("by_group" %in% names(parse_info), parse_info$by_group, NA),
         by.first = ifelse("by_group" %in% names(parse_info), parse_info$by_first, NA),
         by.last  = ifelse("by_group" %in% names(parse_info), parse_info$by_last, NA))

  attr(output, "data_info") <- data_info[!is.na(data_info)]

  attr(output, "formatting") <- list(style = style,
                                     round.digits = round.digits,
                                     plain.ascii = plain.ascii,
                                     justify = justify,
                                     missing = missing,
                                     display.type = display.type,
                                     display.labels = display.labels,
                                     ... = ...)
  return(output)
}
