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
#'   \dQuote{r} for \emph{rows}, \dQuote{c} for \emph{columns} or \dQuote{n} for
#'   \emph{None}.
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
#' ctable(tobacco$gender, tobacco$smoker, prop="r")
#'
#' @seealso \code{\link[base]{table}}, \code{\link[stats]{xtabs}}
#'
#' @keywords classes category
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
ctable <- function(x, y, prop = "t", totals = TRUE, round.digits = 1, useNA = "ifany",
                   style = "simple", plain.ascii = TRUE, justify = "right",
                   dnn=c(substitute(x), substitute(y)), ...) {

  # Parameter validation ---------------------------------------
  if (!is.factor(x) && !is.atomic(x)) {
    x <- try(as.vector(x), silent = TRUE)
    if (class(x) == "try-except") {
      stop("'x' argument must be a factor or an object coercible to a vector")
    }
  }

  if (!is.factor(y) && !is.atomic(x)) {
    y <- try(as.vector(y), silent = TRUE)
    if (class(y) == "try-except") {
      stop("'y' argument must be a factor or an object coercible to a vector")
    }
  }

  prop <- switch(tolower(substring(prop, 1, 1)),
                 t = "Total",
                 r = "Row",
                 c = "Column",
                 n = "None")

  if (!prop %in% c("Total", "Row", "Column", "None"))
    stop("invalid 'prop' argument; must be one of t, r, c, or n")

  if (!totals %in% c(TRUE, FALSE))
    stop("'totals' argument must either be TRUE or FALSE")

  if (!is.numeric(round.digits) || round.digits < 1)
    stop("'round.digits' argument must be numerical and >= 1")

  if (!useNA %in% c("ifany", "always", "never"))
    stop("'useNA' must be one of 'ifany', 'always', or 'no'")

  if (!style %in% c("simple", "grid", "rmarkdown"))
    stop("'style' argument must be one of 'simple', 'grid' or 'rmarkdown'")

  if (!plain.ascii %in% c(TRUE, FALSE))
    stop("'plain.ascii' argument must either TRUE or FALSE")

  justify <- switch(tolower(substring(justify, 1, 1)),
                    l = "left",
                    c = "center",
                    m = "center", # to allow 'middle'
                    r = "right")

  if (!justify %in% c("left", "center", "right"))
    stop("'justify' argument must be one of 'left', 'center' or 'right'")


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
  parse_info_x <- parse_args(sys.calls(), sys.frames(), match.call(), var = "x")
  parse_info_y <- parse_args(sys.calls(), sys.frames(), match.call(), var = "y")

  if (length(parse_info_x$df_name) == 1 &&
      length(parse_info_y$df_name) == 1 &&
      parse_info_x$df_name == parse_info_y$df_name) {
    df_name <- parse_info_x$df_name
    x_name  <- parse_info_x$var_names
    y_name  <- parse_info_y$var_names
  } else {
    x_name <- dnn[1]
    y_name <- dnn[2]
  }

  if (length(parse_info_x$df_label) == 1 &&
      length(parse_info_y$df_label) == 1 &&
      parse_info_x$df_label == parse_info_y$df_label) {
    df_label <- parse_info_x$df_label
  }

  if (length(parse_info_x$rows_subset) == 1) {
    x_subset <- parse_info_x$rows_subset
  } else {
    x_subset <- NA
  }

  if (length(parse_info_y$rows_subset) == 1) {
    y_subset <- parse_info_y$rows_subset
  } else {
    y_subset <- NA
  }

  # Create cross-freq table
  freq_table <- table(x, y, useNA = useNA)

  names(dimnames(freq_table)) <- c(x_name, y_name)

  prop_table <- switch(prop,
                       Total = prop.table(freq_table),
                       Row = prop.table(freq_table, 1),
                       Column = prop.table(freq_table, 2),
                       None = NULL)

  # When useNA = "always" and there are no NA's, we had nan's (0 div by 0)
  prop_table[is.nan(prop_table)] <- 0

  if (isTRUE(totals)) {
    freq_table <- addmargins(freq_table)
    rownames(freq_table)[nrow(freq_table)] <- "Total"
    colnames(freq_table)[ncol(freq_table)] <- "Total"
    if (!is.null(prop_table)) {
      if (prop == "Total") {
        prop_table <- addmargins(prop_table)
      } else if (prop == "Row") {
        prop_table <- addmargins(prop_table, 2)
        sum_props <- c(prop.table(freq_table[nrow(freq_table), -ncol(freq_table)]), Total=1)
        prop_table <- rbind(prop_table, sum_props)
      } else if (prop == "Column") {
        prop_table <- addmargins(prop_table, 1)
        sum_props <- c(prop.table(freq_table[-nrow(freq_table), ncol(freq_table)]), Total=1)
        prop_table <- cbind(prop_table, sum_props)
      }
      rownames(prop_table)[nrow(prop_table)] <- "Total"
      colnames(prop_table)[ncol(prop_table)] <- "Total"
    }
  }

  # Change the name of NA items to avoid potential problems when echoing to console
  if(NA %in% rownames(freq_table)) {
    row.names(freq_table)[is.na(row.names(freq_table))] <- "<NA>"
    if (prop != "None") {
      row.names(prop_table)[is.na(row.names(prop_table))] <- "<NA>"
    }
  }

  if (NA %in% colnames(freq_table)) {
    colnames(freq_table)[is.na(colnames(freq_table))] <- "<NA>"
    if (prop != "None") {
      colnames(prop_table)[is.na(colnames(prop_table))] <- "<NA>"
    }
  }
  # Create output object
  output <- list(cross_table = freq_table, proportions = prop_table)

  # Set output object's attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "ctable"
  attr(output, "proportions") <- prop
  attr(output, "fn_call") <- as.character(match.call())
  attr(output, "date") <- Sys.Date()

  data_info <-
    list(Dataframe = ifelse(exists("df_name"), df_name, NA),
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

  attr(output, "data_info") <-  data_info[!is.na(data_info)]

  attr(output, "formatting") <-
    list(style = style,
         round.digits = round.digits,
         plain.ascii = plain.ascii,
         justify = justify,
         ... = ...)

  return(output)
}
