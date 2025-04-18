#' Cross-Tabulation
#'
#' Cross-tabulation for a pair of categorical variables with either
#' row, column, or total proportions, as well as marginal sums. Works with
#' numeric, character, as well as factor variables.
#'
#' @param x First categorical variable - values will appear as row names.
#' @param y Second categorical variable - values will appear as column names.
#' @param prop Character. Indicates which proportions to show: \dQuote{r} 
#'   (rows, default), \dQuote{c} (columns), \dQuote{t} (total), or \dQuote{n}
#'   (none). Default value can be changed using \code{\link{st_options}},
#'   option \code{ctable.prop}.  
#' @param useNA Character. One of \dQuote{ifany} (default), \dQuote{no}, or 
#'   \dQuote{always}. This argument is passed on \sQuote{as is} to 
#'   \code{\link[base]{table}}, or adapted for \code{\link[stats]{xtabs}} when
#'   weights are used.
#' @param totals Logical. Show row and column totals. Defaults to
#'   \code{TRUE} but can be set globally with \code{\link{st_options}}, option 
#'   \code{ctable.totals}.
#' @param style Character. Style to be used by \code{\link[pander]{pander}}. One
#'   of \dQuote{simple} (default), \dQuote{grid}, \dQuote{rmarkdown}, or
#'   \dQuote{jira}. Can be set globally with \code{\link{st_options}}.
#' @param round.digits Numeric. Number of significant digits to keep. Defaults
#'   to \code{1}. To change this default value, use \code{\link{st_options}},
#'   option \code{ctable.round.digits}.
#' @param justify Character. Horizontal alignment; one of \dQuote{l} (left),
#'   \dQuote{c} (center), or \dQuote{r} (right, default).
#' @param plain.ascii Logical. Used by \code{\link[pander]{pander}}; when
#'   \code{TRUE}, no markup characters are generated (useful when printing
#'   to console). Defaults to \code{TRUE} unless \code{style = 'rmarkdown'},
#'   in which case it is set to \code{FALSE} automatically. To change the 
#'   default value globally, use \code{\link{st_options}}.
#' @param headings Logical. Show heading section. \code{TRUE} by default; can be
#'   set globally with \code{\link{st_options}}.
#' @param display.labels Logical. Display data frame label in the heading 
#'   section. \code{TRUE} by default, can be changed globally with
#'   \code{\link{st_options}}.
#' @param split.tables Numeric. \code{\link[pander]{pander}} argument that 
#'   specifies how many characters wide a table can be. \code{Inf} by default.
#' @param na.val Character. For factors and character vectors, consider this
#'   value as \code{NA}. Ignored if there are actual NA values or if it matches
#'   no value / factor level in the data. \code{NULL} by default.
#' @param dnn Character vector. Variable names to be used in output table. In
#'   most cases, setting this parameter is not required as the names are 
#'   automatically generated.
#' @param chisq Logical. Display chi-square statistic along with p-value.
#' @param OR Logical or numeric. Set to \code{TRUE} to show odds ratio with 95%
#'   confidence interval, or specify confidence level explicitly (\emph{e.g.},
#'   \code{.90}). CI's are calculated using Wald's method of normal approximation.
#' @param RR Logical or numeric. Set to \code{TRUE} to show risk ratio (also
#'   called \emph{relative risk} with 95% confidence interval, or specify
#'   confidence level explicitly (\emph{e.g.} \code{.90}). CI's are 
#'   calculated using Wald's method of normal approximation.
#' @param rev Character. Dimension(s) to reverse for calculation
#'   of risk/odds ratios. One of \dQuote{rows} / \dQuote{r}, \dQuote{columns} /
#'   \dQuote{c}, \dQuote{both} / \dQuote{b}, or \dQuote{none} / \dQuote{n} 
#'   (default). See \emph{details}.
#' @param weights Numeric. Vector of weights; must have the same length as
#'   \code{x}.
#' @param rescale.weights Logical. When \code{TRUE}, a global constant is
#'   applied so that the sum of counts equals \code{nrow(x)}. \code{FALSE} by
#'   default.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}} or
#'   \code{\link[base]{format}}.
#'
#' @details
#' For risk ratios and odds ratios, the expected structure of the contingency
#' table is as follows (using \dQuote{No} as reference):
#' 
#' \preformatted{
#'              Outcome
#'  Exposure      Yes     No
#'   Yes          a       b
#'   No           c       d
#' }
#'
#' The \emph{rev} parameter allows for different structures; use either one of
#' \dQuote{rows}, \dQuote{columns}, or \dQuote{both} to indicate which 
#' dimension(s) to reverse in order to match that structure. This does
#' \emph{not} affect display.
#'  
#' @return A list containing two matrices, \emph{cross_table} and 
#'   \emph{proportions}. The \emph{print} method takes care of assembling 
#'   figures from those matrices into a single table. The returned object has
#'   classes \dQuote{\emph{summarytools}} and \dQuote{\emph{list}}, unless 
#'   \code{\link[summarytools]{stby}} is used, in which case we have an
#'   object of class \dQuote{\emph{stby}}. 
#'   
#' @note Markdown does not fully support multi-header tables;
#'   until such support is available, the recommended way to display 
#'   cross-tables in .Rmd documents is to use `method=render`. See package
#'   vignettes for examples.
#'
#' @examples
#' data("tobacco")
#' ctable(tobacco$gender, tobacco$smoker)
#' 
#' # Use with() to simplify syntax
#' with(tobacco, ctable(gender, smoker))
#'
#' # Show column proportions, without totals
#' with(tobacco, ctable(smoker, diseased, prop = "c", totals = FALSE))
#' 
#' # Simple 2 x 2 table with odds ratio and risk ratio
#' with(tobacco, ctable(smoker, diseased, totals = FALSE, headings = FALSE,
#'                      prop = "r", OR = TRUE, RR = TRUE))
#' 
#' # Grouped cross-tabulations
#' with(tobacco, stby(data = list(x = smoker, y = diseased), 
#'                    INDICES = gender, FUN = ctable))
#'
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
#' @importFrom stats addmargins na.omit chisq.test qnorm
ctable <- function(x, 
                   y,
                   prop            = st_options("ctable.prop"),
                   useNA           = "ifany",
                   totals          = st_options("ctable.totals"),
                   style           = st_options("style"),
                   round.digits    = st_options("ctable.round.digits"),
                   justify         = "right",
                   plain.ascii     = st_options("plain.ascii"),
                   headings        = st_options("headings"),
                   display.labels  = st_options("display.labels"),
                   split.tables    = Inf,
                   na.val          = st_options("na.val"),
                   rev             = "none",
                   dnn             = c(substitute(x), substitute(y)),
                   chisq           = FALSE,
                   OR              = FALSE,
                   RR              = FALSE,
                   weights         = NA,
                   rescale.weights = FALSE,
                   ...) {

  # Initialize flag_by variable that will be set in check_args()
  flag_by <- logical()
  
  # Check for group_by()
  if (inherits(x, "grouped_df")) {
     stop("ctable() does not support group_by(); use stby() instead")
  }
  
  # Adjustment for by() / syby() or when variables are piped into ctable
  if (length(dim(x)) == 2) {
    x_tmp <- x[[1]]
    y <- x[[2]]
    x <- x_tmp
    flag_parse_xy <- TRUE
  } else {
    flag_parse_xy <- FALSE
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
  
  # Duplicate na.val to facilitate args validation
  na.val.x <- na.val
  na.val.y <- na.val

  errmsg <- c(errmsg, check_args(match.call(), list(...), "ctable"))
  
  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "))
  }
  
  # When style is rmarkdown, make plain.ascii FALSE unless specified explicitly
  if (style == "rmarkdown" && isTRUE(plain.ascii) && 
      (!"plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }

  # Replace NaN's by NA's (This simplifies matters a lot)
  if (NaN %in% x) {
    message(paste(sum(is.nan(x)), "NaN value(s) converted to NA in x\n"))
    x[is.nan(x)] <- NA
  }

  if (NaN %in% y) {
    message(paste(sum(is.nan(y)), "NaN value(s) converted to NA in y\n"))
    y[is.nan(y)] <- NA
  }

  # Get x & y metadata from parsing function
  if (isTRUE(flag_by) || isTRUE(flag_parse_xy)) {
    parse_info_x <- parse_call(mc = match.call(),
                               var = c("x", "y"),
                               var_label = FALSE, 
                               caller = "ctable")
    
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
      parse_info_x <- parse_call(mc = match.call(),
                                 var = "x",
                                 var_label = FALSE,
                                 caller = "ctable")
    
    if (inherits(parse_info_x, "try-error")) {
      parse_info_x <- list()
    }
    
    parse_info_y <- parse_call(mc = match.call(),
                               var = "y",
                               var_label = FALSE,
                               caller = "ctable")
    
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
  } else if (isTRUE(flag_by) || isTRUE(flag_parse_xy)) {
    x_name <- na.omit(c(parse_info_x$var_name[1], deparse(dnn[[1]])))[1]
    y_name <- na.omit(c(parse_info_x$var_name[2], deparse(dnn[[2]])))[1]
  } else {
    x_name <- na.omit(c(parse_info_x$var_name, deparse(dnn[[1]])))[1]
    y_name <- na.omit(c(parse_info_y$var_name, deparse(dnn[[2]])))[1]
  }

  # Replace values == na.val by NA in factors & char vars
  if (!is.null(na.val.x)) {
    if (is.factor(x)) {
      x[x == na.val.x] <- NA
      levels(x)[levels(x) == na.val.x] <- NA
    } else if (is.character(x)) {
      x[x == na.val.x] <- NA
    }
  }
  if (!is.null(na.val.y)) {
    if (is.factor(y)) {
      y[y == na.val.y] <- NA
      levels(y)[levels(y) == na.val.y] <- NA
    } else if (is.character(y)) {
      y[y == na.val.y] <- NA
    }
  }
  
  # Create xfreq table ---------------------------------------------------------
  if (identical(NA, weights)) {
    freq_table <- table(x, y, useNA = useNA)
    # Generate minimal table for calculation of chi-square, OR and RR
    freq_table_min <- table(x, y, useNA = "no")
  } else {
    # Weights are used
    weights_string <- deparse(substitute(weights))
    
    # Subset weights when called from by()/stby() to match current data subset
    if (isTRUE(flag_by)) {
      pf <- parent.frame(2)
      weights <- weights[pf$X[[pf$i]]]
    }
    
    if (sum(is.na(weights)) > 0) {
      warning("missing values on weight variable have been detected and were ",
              "treated as zeroes")
      weights[is.na(weights)] <- 0
    }
    
    if (isTRUE(rescale.weights)) {
      weights <- weights / sum(weights) * length(x)
    }
    
    if (useNA == "no") {
      freq_table <- xtabs(weights ~ x + y, addNA = FALSE)
      freq_table_min <- freq_table
    } else {
      freq_table <- xtabs(weights ~ x + y, addNA = TRUE)
      freq_table_min <- xtabs(weights ~ x + y, addNA = FALSE)
    }
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
      sum_props <- c(prop.table(freq_table[nrow(freq_table),
                                           -ncol(freq_table)]),
                     Total = 1)
      prop_table <- rbind(prop_table, sum_props)
    } else if (prop == "c") {
      prop_table <- addmargins(prop_table, 1)
      sum_props <- c(prop.table(freq_table[-nrow(freq_table), 
                                           ncol(freq_table)]),
                     Total = 1)
      prop_table <- cbind(prop_table, sum_props)
    }
    rownames(prop_table)[nrow(prop_table)] <- trs("total")
    colnames(prop_table)[ncol(prop_table)] <- trs("total")
  }

  # Change name of NA items to avoid potential problems when echoing to console
  if (anyNA(rownames(freq_table))) {
    if (is.null(na.val.x)) {
      rownames(freq_table)[is.na(rownames(freq_table))] <- "<NA>"
      if (prop != "n") {
        rownames(prop_table)[is.na(rownames(prop_table))] <- "<NA>"
      }
    } else {
      rownames(freq_table)[is.na(rownames(freq_table))] <- na.val.x
      if (prop != "n") {
        rownames(prop_table)[is.na(rownames(prop_table))] <- na.val.x
      }
    }
  }
  
  # repeat for column names
  if (anyNA(colnames(freq_table))) {
    if (is.null(na.val.y)) {
      colnames(freq_table)[is.na(colnames(freq_table))] <- "<NA>"
      if (prop != "n") {
        colnames(prop_table)[is.na(colnames(prop_table))] <- "<NA>"
      }
    } else {
      colnames(freq_table)[is.na(colnames(freq_table))] <- na.val.y
      if (prop != "n") {
        colnames(prop_table)[is.na(colnames(prop_table))] <- na.val.y
      }
    }
  }

  # Create output object -------------------------------------------------------
  
  output <- list(cross_table = freq_table, 
                 proportions = prop_table)
  
  # Set output object's attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "ctable"
  attr(output, "fn_call") <- match.call()
  attr(output, "date") <- Sys.Date()

  if (isTRUE(chisq)) {
    tmp.chisq <- chisq.test(freq_table_min)
    tmp.chisq <- c(Chi.squared = round(tmp.chisq$statistic[[1]], 4), 
                   tmp.chisq$parameter, 
                   p.value = round(tmp.chisq$p.value, 4))
    attr(output, "chisq") <- tmp.chisq
  }
  
  if (OR || RR) {
    if (identical(as.numeric(dim(freq_table_min)), c(2,2))) {
      
      # Apply rev parameter
      if (!missing("rev")) {
        if (grepl("^(r|row)", rev)) {
          freq_table_min <- freq_table_min[2:1,]
        } else if (grepl("^(c|col)", rev)) {
          freq_table_min <- freq_table_min[,2:1]
        } else if (grepl("^(b|both)", rev)) {
          freq_table_min <- freq_table_min[2:1,2:1]
        }
      }
        
      if (OR) {
        or <- prod(freq_table_min[c(1,4)]) / prod(freq_table_min[c(2,3)])
        se <- sqrt(sum(1/freq_table_min))
        attr(output, "OR") <- c(or,
                                exp(log(or) - qnorm(p = 1 - ((1 - OR)/2)) * se),
                                exp(log(or) + qnorm(p = 1 - ((1 - OR)/2)) * se))
        names(attr(output, "OR")) <- c("Odds Ratio", paste0("Lo - ", OR * 100, "%"),
                                       paste0("Hi - ", OR * 100, "%"))
        attr(output, "OR-level") <- OR
      }
      
      if (RR) {
        rr <- (freq_table_min[1] / sum(freq_table_min[c(1,3)])) / 
          (freq_table_min[2] / sum(freq_table_min[c(2,4)]))
        se <- sqrt(sum(1/freq_table_min[1], 
                       1/freq_table_min[2],
                       -1/sum(freq_table_min[c(1,3)]), 
                       -1/sum(freq_table_min[c(2,4)])))
        attr(output, "RR") <- c(rr,
                                exp(log(rr) - qnorm(p = 1 - ((1 - RR)/2)) * se),
                                exp(log(rr) + qnorm(p = 1 - ((1 - RR)/2)) * se))
        names(attr(output, "RR")) <- c("Risk Ratio", paste0("Lo - ", RR * 100, "%"), 
                                       paste0("Hi - ", RR * 100, "%"))
        attr(output, "RR-level") <- RR
      }
    } else {
      message("OR and RR can only be used with 2 x 2 tables; parameter(s) ignored")
    }
  }  

  # Determine data "type" for x, in a non-strict way
  if (all(c("ordered", "factor") %in% class(x))) {
    Data.type.x <- trs("factor.ordered")
  } else if ("factor" %in% class(x)) {
    Data.type.x <- trs("factor")
  } else if (all(c("POSIXct", "POSIXt") %in% class(x))) { 
    Data.type.x <- trs("datetime")
  } else if ("Date" %in% class(x)) {
    Data.type.x <- trs("date")
  } else if ("logical" %in% class(x)) {
    Data.type.x <- trs("logical")
  } else if ("character" %in% class(x)) {
    Data.type.x <- trs("character")
  } else if ("integer" %in% class(x)) {
    Data.type.x <- trs("integer")
  } else if ("numeric" %in% class(x)) {
    Data.type.x <- trs("numeric")
  } else {
    Data.type.x <- ifelse(mode(x) %in% rownames(.keywords_context),
                          trs(mode(x)), mode(x))
  }  
  
  # Determine data "type" for y, in a non-strict way
  if (all(c("ordered", "factor") %in% class(y))) {
    Data.type.y <- trs("factor.ordered")
  } else if ("factor" %in% class(y)) {
    Data.type.y <- trs("factor")
  } else if (all(c("POSIXct", "POSIXt") %in% class(y))) { 
    Data.type.y <- trs("datetime")
  } else if ("Date" %in% class(y)) {
    Data.type.y <- trs("date")
  } else if ("logical" %in% class(y)) {
    Data.type.y <- trs("logical")
  } else if ("character" %in% class(y)) {
    Data.type.y <- trs("character")
  } else if ("integer" %in% class(y)) {
    Data.type.y <- trs("integer")
  } else if ("numeric" %in% class(y)) {
    Data.type.y <- trs("numeric")
  } else {
    Data.type.y <- ifelse(mode(y) %in% rownames(.keywords_context),
                          trs(mode(y)), mode(y))
  }  
  
  # Store dataframe name in a variable since this will be used in
  # several places in next step
  dfn <- ifelse(exists("df_name", inherits = FALSE), df_name, NA)
  
  # Prepare metadata to be stored as the data_info attribute
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
         Data.type.x         = Data.type.x,
         Data.type.y         = Data.type.y,
         Weights             = ifelse(identical(weights, NA), NA,
                                      ifelse(is.na(dfn), 
                                             weights_string,
                                             sub(pattern = paste0(dfn, "$"), 
                                                 replacement = "",
                                                 x = weights_string,
                                                 fixed = TRUE))),
         by_var           = if ("by_group" %in% names(parse_info_x))
                                   parse_info_x$by_var else NA,
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
  
  # Keep ... arguments that could be relevant for pander of format
  user_fmt <- list()
  dotArgs <- list(...)
  for (i in seq_along(dotArgs)) {
    if (class(dotArgs[[i]]) %in% 
        c("character", "numeric", "integer", "logical") &&
        length(names(dotArgs[1])) == length(dotArgs[[i]]))
      user_fmt <- append(user_fmt, dotArgs[i])
  }
  
  if (length(user_fmt) > 0)
    attr(output, "user_fmt") <- user_fmt
  
  attr(output, "lang") <- st_options("lang")

  return(output)
}
