#' Cross-Tabulation
#'
#' Cross-tabulation for a pair of categorical variables (or factors) with either
#' row, column, or total proportions, as well as marginal sums.
#'
#' @param x First categorical variable - values will appear as row names.
#' @param y Second categorical variable - values will appear in as column names.
#' @param prop Proportions to display;  \dQuote{r} for \emph{rows} (default),
#'   \dQuote{c} for \emph{columns}, \dQuote{t} for \emph{total}, or \dQuote{n} 
#'   for \emph{none}. This option can be set globally; see 
#'   \code{\link{st_options}}.
#' @param useNA Argument passed on to \code{\link[base]{table}}; One of 
#'    \dQuote{ifany} (default), \dQuote{no}, or \dQuote{always}.
#' @param totals Logical. Should row and column totals be displayed? Defaults to
#'   \code{TRUE}. To change this default value globally, see
#'   \code{\link{st_options}}.
#' @param style Style to be used by \code{\link[pander]{pander}} when rendering
#'   output table; One of \dQuote{simple} (default), \dQuote{grid}, or 
#'   \dQuote{rmarkdown}. This option can be set globally; 
#'   see \code{\link{st_options}}.
#' @param round.digits Number of significant digits to display. Defaults to
#'   \code{1}. To change this default value globally, see 
#'   \code{\link{st_options}}.
#' @param justify String indicating alignment of columns; one of \dQuote{l} 
#'   (left) \dQuote{c} (center), or \dQuote{r} (right). Defaults to \dQuote{r}.
#' @param plain.ascii Logical. \code{\link[pander]{pander}} argument; when
#'   \code{TRUE}, no markup characters will be used (useful when printing
#'   to console). Defaults to \code{TRUE} unless \code{style = 'rmarkdown'},
#'   in which case it will be set to \code{FALSE} automatically. To change the 
#'   default value globally, use \code{\link{st_options}}.
#' @param headings Logical. Set to \code{FALSE} to omit heading section. Can be
#'   set globally via \code{\link{st_options}}.
#' @param display.labels Logical. Should variable / data frame label be 
#'   displayed in the title section? Default is \code{TRUE}. To change this
#'   default value globally, use \code{\link{st_options}}.
#' @param split.tables Pander argument that specifies how many characters wide a
#'   table can be. \code{Inf} by default.
#' @param dnn Names to be used in output table. Vector of two strings; By 
#'   default, the character values for arguments x and y are used.
#' @param chisq Logical. Display chisq statistic along with p-value in a message.
#' @param weights Vector of weights; must be of the same length as \code{x}.
#' @param rescale.weights Logical parameter. When set to \code{TRUE}, the total
#'   count will be the same as the unweighted \code{x}. \code{FALSE} by default.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}}.
#'
#' @return A frequency table of classes \code{matrix} and \code{summarytools} 
#'   with added attributes used by \link{print} method.
#'
#' @details Rmarkdown does not, to this day, support multi-header tables. 
#'   Therefore, until such support is available, the recommended way to display 
#'   cross-tables in .Rmd documents is to use `method=render` with the `print()`
#'   or `view()` functions. See package vignettes for examples.
#'
#' @examples
#' data("tobacco")
#' ctable(tobacco$gender, tobacco$smoker)
#' 
#' # Use with() to simplify syntax
#' with(tobacco, ctable(smoker, diseased))
#'
#' # Show column proportions, without totals
#' with(tobacco, ctable(smoker, diseased, prop = "c", totals = FALSE))
#' 
#' # Simple 2 x 2 table
#' with(tobacco, ctable(gender, smoker, totals = FALSE, headings = FALSE, prop = "n"))
#' 
#' # Grouped cross-tabulations
#' with(tobacco, stby(list(x = smoker, y = diseased), gender, ctable))
#'
#' \dontrun{
#' ct <- ctable(tobacco$gender, tobacco$smoker)
#' 
#' # Show html results in browser
#' print(ct, method = "browser")
#' 
#' # Save results to html file
#' print(ct, file = "ct_gender_smoker.html")
#' 
#' # Save results to text file
#' print(ct, file = "ct_gender_smoker.txt")
#' }
#' @seealso \code{\link[base]{table}}, \code{\link[stats]{xtabs}}
#'
#' @keywords classes category
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
#' @importFrom stats addmargins na.omit
ctable <- function(x, y,
                   prop            = st_options("ctable.prop"),
                   useNA           = "ifany", 
                   totals          = st_options("ctable.totals"), 
                   style           = st_options("style"), 
                   round.digits    = 1,
                   justify         = "right", 
                   plain.ascii     = st_options("plain.ascii"),
                   headings        = st_options("headings"),
                   display.labels  = st_options("display.labels"),
                   split.tables    = Inf, 
                   dnn             = c(substitute(x), substitute(y)),
                   chisq           = FALSE,
                   weights         = NA, 
                   rescale.weights = FALSE, 
                   ...) {

  # Support for by()
  if (length(dim(x)) == 2) {
    x_tmp <- x[[1]]
    y <- x[[2]]
    x <- x_tmp
    flag_by <- TRUE
  } else {
    flag_by <- FALSE
  }
  
  # Convert 1-column data frames into vectors
  if (inherits(x, "data.frame") && ncol(x) == 1) {
    x <- x[[1]]
  }
  
  if (inherits(y, "data.frame") && ncol(y) == 1) {
    y <- y[[1]]
  }
  
  # Validate arguments ---------------------------------------------------------
  errmsg <- character()  # problems with arguments will be stored here
  
  if (!is.factor(x) && !is.atomic(x)) {
    x <- try(as.vector(x), silent = TRUE)
    if (inherits(x, "try-error")) {
      errmsg %+=% "'x' must be a factor or an object coercible to a vector"
    }
  }

  if (!is.factor(y) && !is.atomic(x)) {
    y <- try(as.vector(y), silent = TRUE)
    if (inherits(y, "try-error")) {
      errmsg %+=% "'y' must be a factor or an object coercible to a vector"
    }
  }

  errmsg <- c(errmsg, check_arguments(match.call(), list(...)))
  
  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "))
  }
  
  # When style is rmarkdown, make plain.ascii FALSE unless specified explicitly
  if (style=="rmarkdown" && isTRUE(plain.ascii) && 
      (!"plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }

  # Replace NaN's by NA's (This simplifies matters a lot)
  if (NaN %in% x)  {
    message(paste(sum(is.nan(x)), "NaN value(s) converted to NA in x\n"))
    x[is.nan(x)] <- NA
  }

  if (NaN %in% y)  {
    message(paste(sum(is.nan(y)), "NaN value(s) converted to NA in y\n"))
    y[is.nan(y)] <- NA
  }

  # Get info about x & y from parsing function
  if (isTRUE(flag_by)) {
    parse_info_x <- try(
      parse_args(sys.calls(), sys.frames(), match.call(), 
                 var = c("x", "y"), silent = "dnn" %in% names(match.call()),
                 var_label = FALSE, caller = "ctable"),
      silent = TRUE)
    
    if (inherits(parse_info_x, "try-error")) {
      parse_info_x <- list()
    } else {
      if (!is.null(parse_info_x$df_name)) {
        df_name <- parse_info_x$df_name
      }
      if (!is.null(parse_info_x$df_label)) {
        df_label <- parse_info_x$df_label
      }
    }
  } else {
    parse_info_x <- try(
      parse_args(sys.calls(), sys.frames(), match.call(), 
                 var = "x", silent = "dnn" %in% names(match.call()),
                 var_label = FALSE, caller = "ctable"),
      silent = TRUE)
    if (inherits(parse_info_x, "try-error")) {
      parse_info_x <- list()
    }
    
    parse_info_y <- try(
      parse_args(sys.calls(), sys.frames(), match.call(), 
                 var = "y", silent = "dnn" %in% names(match.call()),
                 var_label = FALSE, caller = "ctable"),
      silent = TRUE)
    if (inherits(parse_info_y, "try-error")) {
      parse_info_y <- list()
    }
    
    if (length(parse_info_x$df_name) == 1 &&
        length(parse_info_y$df_name) == 1 &&
        isTRUE(parse_info_x$df_name == parse_info_y$df_name)) {
      df_name <- parse_info_x$df_name
    }
  
    if (length(parse_info_x$df_label) == 1) {
      df_label <- parse_info_x$df_label
    }
  }
  
  if ("dnn" %in% names(match.call())) {
    x_name <- dnn[1]
    y_name <- dnn[2]
  } else if (!isTRUE(flag_by)) {
    x_name <- na.omit(c(parse_info_x$var_name, deparse(dnn[[1]])))[1]
    y_name <- na.omit(c(parse_info_y$var_name, deparse(dnn[[2]])))[1]
  } else {
    x_name <- na.omit(c(parse_info_x$var_name[1], deparse(dnn[[1]])))[1]
    y_name <- na.omit(c(parse_info_x$var_name[2], deparse(dnn[[2]])))[1]
  }

  # Create xfreq table ---------------------------------------------------------
  if (identical(NA, weights)) {
    freq_table <- table(x, y, useNA = useNA)
  } else {
    # Weights are used
    weights_string <- deparse(substitute(weights))
    
    if (sum(is.na(weights)) > 0) {
      warning("missing values on weight variable have been detected and were ",
              "treated as zeroes")
      weights[is.na(weights)] <- 0
    }
    
    if (isTRUE(rescale.weights)) {
      weights <- weights / sum(weights) * length(x)
    }
    
    freq_table <- xtabs(weights ~ x + y, addNA = TRUE)
  }
  
  if (isTRUE(chisq)) {
    tmp <- chisq.test(freq_table)
    tmp.chisq <- c(Chi.squared = round(tmp$statistic[[1]], 4), 
                   tmp$parameter, p.value = round(tmp$p.value, 4))
  }
  
  names(dimnames(freq_table)) <- c(x_name, y_name)

  prop_table <- switch(prop,
                       t = prop.table(freq_table),
                       r = prop.table(freq_table, 1),
                       c = prop.table(freq_table, 2),
                       n = NULL)

  # Add totals
  freq_table <- addmargins(freq_table)
  rownames(freq_table)[nrow(freq_table)] <- trs("total")
  colnames(freq_table)[ncol(freq_table)] <- trs("total")
  if (!is.null(prop_table)) {
    prop_table[is.nan(prop_table)] <- 0
    if (prop == "t") {
      prop_table <- addmargins(prop_table)
    } else if (prop == "r") {
      prop_table <- addmargins(prop_table, 2)
      sum_props <- 
        c(prop.table(freq_table[nrow(freq_table), -ncol(freq_table)]), Total=1)
      prop_table <- rbind(prop_table, sum_props)
    } else if (prop == "c") {
      prop_table <- addmargins(prop_table, 1)
      sum_props <- 
        c(prop.table(freq_table[-nrow(freq_table), ncol(freq_table)]), Total=1)
      prop_table <- cbind(prop_table, sum_props)
    }
    rownames(prop_table)[nrow(prop_table)] <- trs("total")
    colnames(prop_table)[ncol(prop_table)] <- trs("total")
  }

  # Change name of NA items to avoid potential problems when echoing to console
  if (NA %in% rownames(freq_table)) {
    row.names(freq_table)[is.na(row.names(freq_table))] <- "<NA>"
    if (prop != "n") {
      row.names(prop_table)[is.na(row.names(prop_table))] <- "<NA>"
    }
  }

  if (NA %in% colnames(freq_table)) {
    colnames(freq_table)[is.na(colnames(freq_table))] <- "<NA>"
    if (prop != "n") {
      colnames(prop_table)[is.na(colnames(prop_table))] <- "<NA>"
    }
  }

  # Create output object -------------------------------------------------------
  
  output <- list(cross_table = freq_table, 
                 proportions = prop_table)
  
  # Set output object's attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "ctable"
  attr(output, "fn_call") <- match.call()
  #attr(output, "proportions") <- prop
  attr(output, "date") <- Sys.Date()

  if (isTRUE(chisq)) {
    attr(output, "chisq") <- tmp.chisq
  }

  dfn <- ifelse(exists("df_name", inherits = FALSE), df_name, NA)
  data_info <-
    list(Data.frame          = dfn,
         Data.frame.label    = ifelse(exists("df_label", inherits = FALSE),
                                      df_label, NA),
         Row.variable        = x_name,
         Row.variable.label  = ifelse(!is.na(label(x)), label(x), NA),
         Col.variable        = y_name,
         Col.variable.label  = ifelse(!is.na(label(y)), label(y), NA),
         Row.x.Col           = paste(x_name, y_name, sep = " * "),
         Proportions         = switch(prop,
                                      r = "Row",
                                      c = "Column",
                                      t = "Total",
                                      n = "None"),
         Weights             = ifelse(identical(weights, NA), NA,
                                      ifelse(is.na(dfn), 
                                             weights_string,
                                             sub(pattern = paste0(dfn, "$"), 
                                                 replacement = "",
                                                 x = weights_string,
                                                 fixed = TRUE))),
         Group            = ifelse("by_group" %in% names(parse_info_x),
                                   parse_info_x$by_group, NA),
         by_first         = ifelse("by_group" %in% names(parse_info_x), 
                                   parse_info_x$by_first, NA),
         by_last          = ifelse("by_group" %in% names(parse_info_x), 
                                   parse_info_x$by_last , NA))

  attr(output, "data_info") <-  data_info[!is.na(data_info)]

  attr(output, "format_info") <-  list(style          = style,
                                       round.digits   = round.digits,
                                       plain.ascii    = plain.ascii,
                                       justify        = justify,
                                       totals         = totals,
                                       split.tables   = split.tables,
                                       headings       = headings,
                                       display.labels = display.labels)
  
  attr(output, "user_fmt") <- list(... = ...)

  attr(output, "lang") <- st_options("lang")
  
  return(output)
}
