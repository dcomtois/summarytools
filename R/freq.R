#' Frequency Tables for Factors and Other Discrete Data
#'
#' Displays weighted or unweighted frequencies, including <NA> counts and
#' proportions.
#'
#' @param x Factor or vector
#' @param round.digits Number of significant digits to display. Defaults to
#'   \code{2} and can be set globally; see \code{\link{st_options}}.
#' @param order Ordering of rows in frequency table; \dQuote{names} (default for
#'   non-factors), \dQuote{levels} (default for factors), or \dQuote{freq} (from
#'   most frequent to less frequent).
#' @param style Style to be used by \code{\link[pander]{pander}} when rendering
#'   output table; One of \dQuote{simple} (default), \dQuote{grid}, or
#'   \dQuote{rmarkdown} This option can be set globally; see
#'   \code{\link{st_options}}.
#' @param plain.ascii Logical. \code{\link[pander]{pander}} argument; when
#'   \code{TRUE}, no markup characters will be used (useful when printing to
#'   console). Defaults to \code{TRUE} unless \code{style = 'rmarkdown'}, in
#'   which case it will be set to \code{FALSE} automatically. To change the
#'   default value globally, see \code{\link{st_options}}.
#' @param justify String indicating alignment of columns. By default
#'   (\dQuote{default}), \dQuote{right} is used for text tables and
#'   \dQuote{center} is used for \emph{html} tables. You can force it to one of
#'   \dQuote{left}, \dQuote{center}, or \dQuote{right}.
#' @param totals Logical. Set to \code{FALSE} to hide totals from results. To
#'   change this value globally, see \code{\link{st_options}}.
#' @param report.nas Logical. Set to \code{FALSE} to turn off reporting of
#'   missing values. To change this default value globally, see
#'   \code{\link{st_options}}.
#' @param missing Characters to display in NA cells. Defaults to \dQuote{}.
#' @param display.type Logical. Should variable type be displayed? Default is
#'   \code{TRUE}.
#' @param display.labels Logical. Should variable / data frame labels be
#'   displayed? Default is \code{TRUE}. To change this default value globally,
#'   see \code{\link{st_options}}.
#' @param headings Logical. Set to \code{FALSE} to omit heading section. Can be
#'   set globally via \code{\link{st_options}}.
#' @param weights Vector of weights; must be of the same length as \code{x}.
#' @param rescale.weights Logical parameter. When set to \code{TRUE}, the total
#'   count will be the same as the unweighted \code{x}. \code{FALSE} by default.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}}.
#'
#' @return A frequency table of class \code{matrix} and \code{summarytools} with
#'   added attributes used by \emph{print} method.
#'
#' @details The default \code{plain.ascii = TRUE} option is there to make
#'   results appear cleaner in the console. To avoid rmarkdown rendering
#'   problems, this option is automatically set to \code{FALSE} whenever
#'   \code{style = "rmarkdown"} (unless \code{plain.ascii = TRUE} is made
#'   explicit in the function call).
#'
#' @examples
#' data(tobacco)
#' freq(tobacco$gender)
#' freq(tobacco$gender, totals = FALSE)
#' freq(tobacco$gender, display.nas = FALSE)
#' freq(tobacco$gender, style="rmarkdown")
#' with(tobacco, view(by(diseased, smoker, freq), method = "pander"))
#' 
#' @seealso \code{\link[base]{table}}
#'
#' @keywords univar classes category
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
#' @importFrom stats xtabs
freq <- function(x, round.digits = st_options('round.digits'), 
                 order = "default", style = st_options('style'), 
                 plain.ascii = st_options('plain.ascii'), 
                 justify = "default", totals = st_options('freq.totals'), 
                 report.nas = st_options('freq.report.nas'), 
                 missing = "", display.type = TRUE, 
                 display.labels = st_options('display.labels'), 
                 headings = st_options('headings'), weights = NA, 
                 rescale.weights = FALSE, ...) {

  # Validate arguments ---------------------------------------------------------
  errmsg <- character()  # problems with arguments will be stored in here
  
  # if x is a data.frame with 1 column, extract this column as x
  if (!is.null(ncol(x)) && ncol(x)==1) {
    varname <- colnames(x)
    x <- x[[1]]
  }

  if (!is.atomic(x)) {
    x <- try(as.vector(x), silent = TRUE)
    if (inherits(x, "try-error") || !is.atomic(x)) {
      errmsg %+=% "argument x must be a vector or a factor"
    }
  }

  errmsg <- c(errmsg, check_arguments(match.call(), list(...)))

  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "))
  }
  
  # End of arguments validation ------------------------------------------------
  
  # When style = 'rmarkdown', make plain.ascii FALSE unless specified explicitly
  if (style %in% c("grid", "rmarkdown") && 
      !("plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }
  
  # Replace NaN's by NA's (This simplifies matters a lot)
  if (NaN %in% x)  {
    message(paste(sum(is.nan(x)), "NaN value(s) converted to NA\n"))
    x[is.nan(x)] <- NA
  }
  
  # Get information about x from parsing function
  parse_info <- try(parse_args(sys.calls(), sys.frames(), match.call(),
                               silent = exists('varname')),
                    silent = TRUE)
  
  if (inherits(parse_info, "try-error")) {
    parse_info <- list()
  }
  
  if (!("var_names" %in% names(parse_info)) && exists('varname')) {
    parse_info$var_names <- varname
  }
  
  # No weights are used --------------------------------------------------------
  # create a basic frequency table, always including NA
  if (identical(NA, weights)) {
    freq_table <- table(x, useNA = "always")
    
    # Order by frequency if needed
    if (order == "freq") {
      freq_table <- sort(freq_table, decreasing = TRUE)
      na_pos <- which(is.na(names(freq_table)))
      freq_table <- c(freq_table[-na_pos], freq_table[na_pos])
    }
    
    # order by names if needed
    if (is.factor(x) && order == "names") {
      freq_table <- freq_table[order(names(freq_table))]
      na_pos <- which(is.na(names(freq_table)))
      freq_table <- c(freq_table[-na_pos], freq_table[na_pos])
    }
    
    # Change the name of the NA item (last) to avoid potential
    # problems when echoing to console
    names(freq_table)[length(freq_table)] <- "<NA>"
    
    # calculate proportions (valid, i.e excluding NA's)
    P_valid <- prop.table(freq_table[-length(freq_table)]) * 100
    
    # Add '<NA>' item to the proportions; this assures
    # proper length when cbind'ing later on
    P_valid["<NA>"] <- NA
    
    # calculate proportions (total, i.e. including NA's)
    P_tot <- prop.table(freq_table) * 100
  }
  
  # Weights are used -----------------------------------------------------------
  else {
    
    # Check that weights vector is of the right length
    if (length(weights) != length(x)) {
      stop("weights vector must be of same length as x")
    }
    
    weights_string <- deparse(substitute(weights))
    weights_label <- try(label(weights), silent = TRUE)
    
    if (sum(is.na(weights)) > 0) {
      warning("Missing values on weight variable have been detected and were ",
              "treated as zeroes.")
      weights[is.na(weights)] <- 0
    }
    
    if (isTRUE(rescale.weights)) {
      weights <- weights / sum(weights) * length(x)
    }
    
    freq_table <- xtabs(formula = weights ~ x)
    
    # Order by frequency if needed
    if (order == "freq") {
      freq_table <- sort(freq_table, decreasing = TRUE)
      na_pos <- which(is.na(names(freq_table)))
      freq_table <- c(freq_table[-na_pos], freq_table[na_pos])
    }
    
    # order by names if needed
    if (is.factor(x) && order == "names") {
      freq_table <- freq_table[sort(names(freq_table))]
      na_pos <- which(is.na(names(freq_table)))
      freq_table <- c(freq_table[-na_pos], freq_table[na_pos])
    }
    
    P_valid <- prop.table(freq_table) * 100
    P_valid["<NA>"] <- NA
    freq_table["<NA>"] <- sum(weights) - sum(xtabs(formula = weights ~ x))
    P_tot <- prop.table(freq_table) * 100
  }
  
  # Calculate cumulative proportions ------------------------------------------
  
  P_valid_cum <- cumsum(P_valid)
  P_valid_cum["<NA>"] <- NA
  P_tot_cum <- cumsum(P_tot)
  
  # Combine the info to build the final frequency table -----------------------
  
  output <- cbind(freq_table, P_valid, P_valid_cum, P_tot, P_tot_cum)
  output <- rbind(output, c(colSums(output, na.rm = TRUE)[1:2], rep(100,3)))
  colnames(output) <- c(trs('freq'), trs('pct.valid'), trs('pct.valid.cum'), 
                        trs('pct.total'), trs('pct.total.cum'))
  rownames(output) <- c(names(freq_table), trs('total'))
  
  # Update the output class and attributes ------------------------------------
  
  class(output) <- c("summarytools", class(output))
  
  attr(output, "st_type")    <- "freq"
  attr(output, "fn_call")    <- match.call()
  attr(output, "date")       <- Sys.Date()
  
  # Determine data "type", in a non-strict way
  if (all(c("ordered", "factor") %in% class(x))) {
    Data.type <- trs('factor.ordered')
  } else if ("factor" %in% class(x)) {
    Data.type <- trs('factor')
  } else if (all(c("POSIXct", "POSIXt") %in% class(x))) { # TODO: see what other classes correspond to datetime
    Data.type <- trs('datetime')
  } else if ("Date" %in% class(x)) {
    Data.type <- trs('date')
  } else if ("logical" %in% class(x)) {
    Data.type <- trs('logical')
  } else if ("character" %in% class(x)) {
    Data.type <- trs('character')
  } else if ("numeric" %in% class(x)) {
    Data.type <- trs('numeric')
  } else {
    Data.type <- NA
  }
    
  data_info <-
    list(
      Data.frame       = ifelse("df_name" %in% names(parse_info), 
                                parse_info$df_name, NA),
      Data.frame.label = ifelse("df_label" %in% names(parse_info), 
                                parse_info$df_label, NA),
      Variable         = ifelse("var_names" %in% names(parse_info), 
                                parse_info$var_names, NA),
      Variable.label   = label(x),
      Data.type        = Data.type,
      Weights          = ifelse(identical(weights, NA), NA,
                                sub(pattern = paste0(parse_info$df_name, "$"), 
                                    replacement = "",
                                    x = weights_string, fixed = TRUE)),
      Group            = ifelse("by_group" %in% names(parse_info),
                                parse_info$by_group, NA),
      by.first         = ifelse("by_group" %in% names(parse_info), 
                                parse_info$by_first, NA),
      by.last          = ifelse("by_group" %in% names(parse_info), 
                                parse_info$by_last , NA))
  
  attr(output, "data_info") <- data_info[!is.na(data_info)]

  attr(output, "formatting") <- list(style          = style,
                                     round.digits   = round.digits,
                                     plain.ascii    = plain.ascii,
                                     justify        = justify,
                                     totals         = totals,
                                     report.nas     = report.nas,
                                     missing        = missing,
                                     display.type   = display.type,
                                     display.labels = display.labels,
                                     headings       = headings)

  attr(output, "user_fmt") <- list(... = ...)

  return(output)
}
