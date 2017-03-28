#' Cross-Tabulations
#'
#' Produces a cross-tabulation (for 2 categorical variables) with either row,
#' column, or total proportions as well as marginal sums.
#'
#' @param x First categorical variable - its values will appear as row names.
#' @param y Second categorical variable - its values will appear in as column names.
#' @param round.digits Number of significant digits to display. Defaults to
#'   \code{1}.
#' @param style Style to be used by \code{\link[pander]{pander}} when rendering
#'   output table; One of \dQuote{simple} (default), \dQuote{grid} or
#'   \dQuote{rmarkdown}.
#' @param justify String indicating alignment of columns; one of \dQuote{left}
#'   (or \dQuote{l}), \dQuote{center} (or \dQuote{c}), or \dQuote{right}
#'   (or \dQuote{r}). Defaults to \dQuote{right}.
#' @param prop What proportions to display; \dQuote{t} for \emph{total} (default),
#'   \dQuote{r} for \emph{rows}, \dQuote{c} for \emph{columns} or \code{NA} for
#'    none.
#' @param useNA Argument used by \code{\link[base]{table}}; One of \dQuote{ifany}
#'   (default), \dQuote{no}, or \dQuote{always}.
#' @param totals Should row and column totals be displayed? Defaults to \code{TRUE}.
#' @param plain.ascii Logical \code{\link[pander]{pander}} argument. When
#'   \code{TRUE}, no markup characters will be used (useful when printing
#'   to console). Defaults to \code{TRUE} when \code{style} is \dQuote{simple},
#'   and \code{FALSE} otherwise.
#' @param dnn Names to be used in output table. Vector of two strings; By default,
#'   the character values for arguments x and y are used.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}}.
#'
#' @return A frequency table of class \code{matrix} with added attributes used
#'   by \link{print} method.
#'
#' @details The default \code{plain.ascii = TRUE} option is there to make results
#'   appear cleaner in the console. To avoid rmarkdown rendering problems, the
#'   option is automatically set to \code{FALSE} whenever
#'   \code{style = "rmarkdown"} (unless \code{plain.ascii = TRUE} is made
#'   explicit). If the intent is to produce markdown text using {style = "simple"},
#'   set \code{plain.ascii} to \code{FALSE}.
#'
#' @examples
#' data("tobacco")
#' with(tobacco, ctable(gender, smoker, prop="r"))
#'
#' @seealso \code{\link[base]{table}}, \code{\link[stats]{xtabs}}
#'
#' @keywords classes category
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
ctable <- function(x, y, prop = "t", totals = TRUE, round.digits = 1, useNA = "ifany",
                   style = "simple", plain.ascii = TRUE, justify = "right",
                   dnn=c(substitute(x), substitute(y)), ...) {

  # When style is 'rmarkdown', make plain.ascii FALSE unless specified explicitly
  if (style=="rmarkdown" && plain.ascii==TRUE && (!"plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }

  # Replace NaN's by NA's (This simplifies matters a lot!)
  if (NaN %in% x)  {
    message(paste(sum(is.nan(x)), "NaN value(s) converted to NA in x\n"))
    x[is.nan(x)] <- NA
  }

  if (NaN %in% y)  {
    message(paste(sum(is.nan(y)), "NaN value(s) converted to NA in y\n"))
    y[is.nan(y)] <- NA
  }

  if ("file" %in% names(match.call()))
    message("file argument is deprecated; use print() or view() function to generate files")

  # Get into about x from parsing function
  parse_info <- parse_args(sys.calls(), sys.frames(), match.call(), y = TRUE)

  if (length(parse_info$df_name[["x"]]) == 1 &&
      length(parse_info$df_name[["y"]]) == 1 &&
      parse_info$df_name[["x"]] == parse_info$df_name[["y"]]) {
    df_name <- parse_info$df_name[["x"]]
    x_name  <- parse_info$var_names[["x"]]
    y_name  <- parse_info$var_names[["y"]]
  } else {
    x_name <- dnn[1]
    y_name <- dnn[2]
  }

  if (length(parse_info$df_label[["x"]]) == 1 &&
      length(parse_info$df_label[["y"]]) == 1 &&
      parse_info$df_label[["x"]] == parse_info$df_label[["y"]]) {
    df_label <- parse_info$df_label[["x"]]
  }


  if (length(parse_info$rows_subset[["x"]]) == 1) {
    x_subset <- parse_info$rows_subset[["x"]]
  } else {
    x_subset <- NA
  }

  if (length(parse_info$rows_subset[["y"]]) == 1) {
    y_subset <- parse_info$rows_subset[["y"]]
  } else {
    y_subset <- NA
  }

  # Create cross-freq table
  freq_table <- table(x, y, useNA = useNA)

  names(dimnames(freq_table)) <- c(x_name, y_name)

  prop <- tolower(substr(prop,1,1))
  prop_table <- switch(prop,
                       t = prop.table(freq_table),
                       r = prop.table(freq_table, 1),
                       c = prop.table(freq_table, 2))

  if (isTRUE(totals)) {
    freq_table <- addmargins(freq_table)
    rownames(freq_table)[nrow(freq_table)] <- "Total"
    colnames(freq_table)[ncol(freq_table)] <- "Total"
    if (!is.null(prop_table)) {
      if (prop == "t") {
        prop_table <- addmargins(prop_table)
      } else if (prop == "r") {
        prop_table <- addmargins(prop_table, 2)
        sum_props <- c(prop.table(freq_table[nrow(freq_table), -ncol(freq_table)]), Total=1)
        prop_table <- rbind(prop_table, sum_props)
      } else if (prop == "c") {
        prop_table <- addmargins(prop_table, 1)
        sum_props <- c(prop.table(freq_table[-nrow(freq_table), ncol(freq_table)]), Total=1)
        prop_table <- cbind(prop_table, sum_props)
      }
      rownames(prop_table)[nrow(prop_table)] <- "Total"
      colnames(prop_table)[ncol(prop_table)] <- "Total"
    }
  }

  # Change the name of NA items to avoid potential problems when echoing to console
  rownames(freq_table)[is.na(rownames(freq_table))] <- "<NA>"
  colnames(freq_table)[is.na(colnames(freq_table))] <- "<NA>"
  rownames(prop_table)[is.na(rownames(prop_table))] <- "<NA>"
  colnames(prop_table)[is.na(colnames(prop_table))] <- "<NA>"

  # Create output object
  output <- list(cross_table = freq_table, proportions = prop_table)

  # Set output object's attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "ctable"
  attr(output, "proportions") <- switch(prop,
                                       r = "Rows",
                                       c = "Columns",
                                       t = "Total")
  attr(output, "fn_call") <- as.character(match.call())
  attr(output, "date") <- Sys.Date()

  attr(output, "data_info") <-
    c(Dataframe = ifelse(exists("df_name"), df_name, NA),
      Dataframe.label = ifelse(exists("df_label"), df_label, NA),
      Row.variable = x_name,
      Row.variable.label = ifelse(!is.na(label(x)), label(x), NA),
      Col.variable = y_name,
      Col.variable.label = ifelse(!is.na(label(y)), label(y), NA),
      Subset = ifelse(length(x_subset) == 1 &&
                        length(y_subset) == 1 &&
                        x_subset == y_subset, x_subset, NA),
      Row.variable.subset = ifelse(length(x_subset) == 1, x_subset, NA),
      Col.variable.subset = ifelse(length(y_subset) == 1, y_subset, NA))

  attr(output, "formatting") <-
    list(style = style,
         round.digits = round.digits,
         plain.ascii = plain.ascii,
         justify = justify,
         ... = ...)

  return(output)
}
