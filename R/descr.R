#' Univariate Statistics for Numerical Data
#'
#' Calculates mean, sd, min, Q1\*, median, Q3\*, max, MAD, IQR\*, CV, 
#' skewness\*, SE.skewness\*, and kurtosis\* on numerical vectors. (\*) Not 
#' available when using sampling weights.
#'
#' @param x A numerical vector or a data frame.
#' @param var Unquoted expression referring to a specific column in \code{x}.
#'   Provides support for piped function calls (e.g.
#'   \code{my_df |> descr(my_var)}.    
#' @param stats Character. Which stats to produce. Either \dQuote{all} (default),
#'   \dQuote{fivenum}, \dQuote{common} (see \emph{Details}), or a selection of :
#'   \dQuote{mean}, \dQuote{sd}, \dQuote{min}, \dQuote{q1}, \dQuote{med},
#'   \dQuote{q3}, \dQuote{max}, \dQuote{mad}, \dQuote{iqr}, \dQuote{cv},
#'   \dQuote{skewness}, \dQuote{se.skewness}, \dQuote{kurtosis},
#'   \dQuote{n.valid}, \dQuote{n}, and \dQuote{pct.valid}. Can be set globally
#'   via \code{\link{st_options}}, option \dQuote{descr.stats}. See 
#'   \emph{Details}.
#' @param na.rm Logical. Argument to be passed to statistical functions. 
#'   Defaults to \code{TRUE}.
#' @param round.digits Numeric. Number of significant digits to display. 
#'   Defaults to \code{2}. Can be set globally with \code{\link{st_options}}.
#' @param transpose Logical. Make variables appears as columns, and stats as
#'   rows. Defaults to \code{FALSE}. Can be set globally with
#'   \code{\link{st_options}}, option \dQuote{descr.transpose}.
#' @param order Character. When analyzing more than one variable, this parameter
#'   determines how to order variables. Valid values are \dQuote{sort} (or
#'   simply \dQuote{s}), \dQuote{preserve} (or \dQuote{p}), or a vector
#'   containing all variable names in the desired order. Defaults to
#'   \dQuote{sort}.
#' @param style Character. Style to be used by \code{\link[pander]{pander}}. One
#'   of \dQuote{simple} (default), \dQuote{grid}, \dQuote{rmarkdown}, or
#'   \dQuote{jira}. Can be set globally with \code{\link{st_options}}.
#' @param plain.ascii Logical. \code{\link[pander]{pander}} argument; when
#'   \code{TRUE} (default), no markup characters will be used (useful when
#'   printing to console). If \code{style = 'rmarkdown'} is specified, value
#'   is set to \code{FALSE} automatically. Can be set globally using
#'   \code{\link{st_options}}.
#' @param justify Character. Alignment of numbers in cells; \dQuote{l} for left,
#'   \dQuote{c} for center, or \dQuote{r} for right (default). Has no effect on 
#'   \emph{html} tables.
#' @param headings Logical. Set to \code{FALSE} to omit heading section. Can be
#'   set globally via \code{\link{st_options}}. \code{TRUE} by default.
#' @param display.labels Logical. Show variable / data frame labels in heading
#'   section. Defaults to \code{TRUE}. Can be set globally with
#'   \code{\link{st_options}}.
#' @param split.tables Character. \code{\link[pander]{pander}} argument that
#'   specifies how many characters wide a table can be. \code{100} by default.
#' @param weights Numeric. Vector of weights having same length as \emph{x}.
#'   \code{NULL} (default) indicates that no weights are used.
#' @param rescale.weights Logical. When set to \code{TRUE}, a global constant is
#'   apply to make the total count equal \code{nrow(x)}. \code{FALSE} by default.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}} or
#'   \code{\link[base]{format}}.
#'
#' @return An object having classes \dQuote{\emph{matrix}} and
#'   \dQuote{\emph{summarytools}} containing the statistics, with extra
#'   attributes useful to other functions/methods.
#'
#' @details
#'   Since version 1.1, the \emph{stats} argument can be set in a more flexible
#'   way; keywords (\emph{all}, \emph{common}, \emph{fivenum}) can be combined
#'   with single statistics, or their \dQuote{negation}. For instance, using 
#'   \code{stats = c("all", "-q1", "-q3")} would show
#'   \strong{all except q1 and q3}.
#'   
#'   For further customization, you could redefine any preset in the
#'   following manner: \code{.st_env$descr.stats$common <- c("mean", "sd", "n")}. 
#'   \emph{Use caution when modifying \code{.st_env}, and reload the package
#'   if errors ensue. Changes are temporary and will not persist across
#'   R sessions.}
#'
#' @examples
#' data("exams")
#' 
#' # All stats (default behavior) for all numerical variables
#' descr(exams)
#' 
#' # Show only "common" statistics, plus "n"
#' descr(exams, stats = c("common", "n"))
#' 
#' # Selection of statistics, transposing the results
#' descr(exams, stats = c("mean", "sd", "min", "max"), transpose = TRUE)
#' 
#' # Rmarkdown-ready
#' descr(exams, plain.ascii = FALSE, style = "rmarkdown")
#'
#' # Grouped statistics
#' data("tobacco")
#' with(tobacco, stby(BMI, gender, descr, check.nas = FALSE))
#'
#' # Grouped statistics in tidy table:
#' with(tobacco, stby(BMI, age.gr, descr, stats = "common")) |> tb()
#'
#' \dontrun{
#' # Show in Viewer (or browser if not in RStudio)
#' view(descr(exams))
#' 
#' # Save to html file with title
#' print(descr(exams),
#'       file = "descr_exams.html", 
#'       report.title = "BMI by Age Group",
#'       footnote = "<b>Schoolyear:</b> 2018-2019<br/><b>Semester:</b> Fall")
#' }
#'
#' @keywords univar
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
#' @importFrom matrixStats weightedMean weightedSd weightedMedian weightedMad
#' @importFrom rapportools skewness kurtosis nvalid
#' @importFrom stats IQR mad median sd quantile
#' @importFrom utils head
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% select starts_with summarise_all group_keys n
#' @importFrom tidyr separate gather spread
descr <- function(x,
                  var             = NULL,
                  stats           = st_options("descr.stats"),
                  na.rm           = TRUE,
                  round.digits    = st_options("round.digits"),
                  transpose       = st_options("descr.transpose"),
                  order           = "sort",
                  style           = st_options("style"),
                  plain.ascii     = st_options("plain.ascii"),
                  justify         = "r",
                  headings        = st_options("headings"),
                  display.labels  = st_options("display.labels"),
                  split.tables    = 100,
                  weights         = NULL,
                  rescale.weights = FALSE,
                  ...) {
  
  UseMethod("descr", x)
}

#' @export
descr.default <- function(x,
                          var             = NULL,
                          stats           = st_options("descr.stats"),
                          na.rm           = TRUE,
                          round.digits    = st_options("round.digits"),
                          transpose       = st_options("descr.transpose"),
                          order           = "sort",
                          style           = st_options("style"),
                          plain.ascii     = st_options("plain.ascii"),
                          justify         = "r",
                          headings        = st_options("headings"),
                          display.labels  = st_options("display.labels"),
                          split.tables    = 100,
                          weights         = NULL,
                          rescale.weights = FALSE,
                          ...) {
  
  # Initialize flag_by variable that will be set in check_args()
  flag_by <- logical()
  
  # x is data.frame, var is present -------------------------------------
  if (is.data.frame(x) && #ncol(x) > 1 &&
      "var" %in% names(match.call()) &&
      deparse(substitute(var)) != "list()") {
    
    # var might contain a function call -- such as df %>% descr(na.omit(var1))
    if (inherits(as.list(match.call()[-1])$var, "call")) {
      xx        <- eval(as.list(match.call()[-1])$var, envir = x)
      var_names <- intersect(colnames(x),
                             as.character(as.list(match.call()[-1])$var))
    } else {
      #xx        <- x[[as.list(match.call()[-1])$var]]
      var_names <- deparse(substitute(var))
      xx        <- x[ , var_names, drop = FALSE]
    }
  } else if (inherits(x, "data.frame")) {
    xx <- x
    var_names <- colnames(x)
  } else {
    xx <- as_tibble(x)
    var_names <- deparse(substitute(x))
  }
  
  # Validate arguments -------------------------------------------------------
  errmsg <- character()  # problems with arguments will be stored here
  
  if (is.null(x)) {
    tmp_x_name <- deparse(substitute(x))
    stop(tmp_x_name, " is either NULL or does not exist")
  }
  
  if (ncol(xx) == 1 && !is.numeric(xx[[1]])) {
    errmsg %+=% "'x' must be numeric"
  }
  
  # Get variable label
  if (ncol(xx) == 1) {
    var_label <- label(xx[[1]])
  } else {
    var_label <- NA
  }
  
  if (!is.data.frame(xx)) {
    errmsg %+=% paste("'x' must be a numeric vector, a data.frame, a tibble,",
                      "a data.table; attempted conversion to tibble failed")
  }
  
  errmsg <- c(errmsg, check_args(match.call(), list(...), "descr"))
  
  # keywords "all", "common", "fivenum" are used alone
  if (identical(stats, "all")) {
    stats <- .st_env$descr.stats.valid[[2 - as.numeric(missing(weights))]]
  } else if (identical(stats, "fivenum")) {
    if (!missing(weights)) {
      errmsg %+=% paste("fivenum is not supported when weights are used; valid",
                        "stats are:", 
                        paste(.st_env$descr.stats.valid$wgts, collapse = ", "))
      
    }
    stats <- .st_env$descr.stats$fivenum # c("min", "q1", "med", "q3", "max")
  } else if (identical(stats, "common")) {
    stats <- .st_env$descr.stats$common 
  } else {
    
    # keywords are used with other stats and/or negative stats like "-min"
    stats <- tolower(stats)
    ind_negat <- grep("^-", stats)
    stats_negat <- substr(stats[ind_negat], start = 2, stop = 20)
    
    if (length(ind_negat))
      stats <- stats[-ind_negat]
    
    # Replace keywords by the actual statistics
    if ("common" %in% stats)
      stats <- setdiff(unique(c(.st_env$descr.stats$common, stats)), "common")
    
    else if ("fivenum" %in% stats)
      stats <- setdiff(unique(c(.st_env$descr.stats$fivenum, stats)), "fivenum")
    
    else if ("all" %in% stats)
      stats <- setdiff(unique(c(.st_env$descr.stats$all, stats)), "all")
    
    # Remove the stats that had the format "-..."
    stats <- setdiff(stats, stats_negat)
    
    invalid_stats <- setdiff(
      stats,
      .st_env$descr.stats.valid[[2 - as.numeric(missing(weights))]]
      )
    
    if (length(invalid_stats) > 0) {
      errmsg %+=%
        paste("The following statistics are not recognized, or not allowed: ",
              paste(dQuote(invalid_stats), collapse = ", "))
    }
  }

  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "))
  }

  # End of arguments validation ------------------------------------------------
  
  # When style is rmarkdown, make plain.ascii FALSE unless specified explicitly
  if (style == "rmarkdown" && isTRUE(plain.ascii) && 
      (!"plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }
  
  # Get info about x from parsing function
  if ("skip_parse" %in% names(list(...))) {
    parse_info <- list()
  } else {
    parse_info <- parse_call(mc        = match.call(),
                             var_name  = (ncol(xx) == 1), 
                             var_label = (ncol(xx) == 1),
                             caller    = "descr")
  }

  # Identify and exclude non-numerical columns from x
  col_to_remove <- which(!vapply(xx, is.numeric, logical(1)))
  
  if (length(col_to_remove) > 0) {
    ignored    <- colnames(xx)[col_to_remove]
    xx         <- xx[ , -col_to_remove, drop = FALSE]
    order      <- setdiff(order, ignored)
    var_names  <- var_names[-col_to_remove]
  } else {
    ignored <- c()
  }
  
  if (ncol(xx) == 0) {
    stop("no numerical variables found in ", deparse(match.call()$x))
  }

  # Verify that the order argument is still valid after column removal
  if (length(order) > 1) {
    if (length(ind <- which(!colnames(xx) %in% order)) > 0) {
      message("column(s) not specified in 'order' (",
              paste(colnames(xx)[ind], collapse = ", "), 
              ") will appear at the end of the table")
      order <- c(order, colnames(xx)[ind])
    } 
  } else if (length(order) == 0) {
    warning("something went wrong with the order argument; using default value")
    order <- "sort"
  }
  
  # No weights being used ------------------------------------------------------
  if (missing(weights)) {
    
    # Prepare the summarizing functions for dplyr::summarise; there are 3 stats
    # that will be calculated later on so to not slow down the function
    dummy <- function(x) NA
    
    summar_funs <- list(~ mean(., na.rm = na.rm),
                        ~ sd(., na.rm = na.rm),
                        ~ min(., na.rm = na.rm),
                        ~ quantile(., probs = .25, type = 2, names = FALSE, 
                                   na.rm = na.rm),
                        ~ median(., na.rm = na.rm),
                        ~ quantile(., probs = .75, type = 2, names = FALSE, 
                                   na.rm = na.rm),
                        ~ max(., na.rm = na.rm),
                        ~ mad(., na.rm = na.rm),
                        ~ IQR(., na.rm = na.rm),
                        ~ dummy(.), # placeholder for cv
                        ~ rapportools::skewness(., na.rm = na.rm),
                        ~ dummy(.), # placeholder for se.skewnes
                        ~ rapportools::kurtosis(., na.rm = na.rm),
                        ~ rapportools::nvalid(., na.rm = na.rm),
                        ~ n(),
                        ~ dummy(.))  # placeholder for pct.valid
    
    fun_names <- c("mean", "sd", "min", "q1", "med", "q3", "max", "mad", "iqr",
                   "cv", "skewness", "se.skewness", "kurtosis", "n.valid",
                   "n", "pct.valid")
    
    names(summar_funs) <- fun_names
    summar_funs <- summar_funs[which(fun_names %in% stats)]

    # To avoid problems, (see issue #152) use generic colnames
    xxnames <- colnames(xx)
    colnames(xx) <- paste0("V", seq_along(xx))
    if (ncol(xx) > 1) {
      results <- suppressWarnings(
        xx %>% summarise_all(.funs = summar_funs) %>%
          gather("variable", "value") %>%
          separate("variable", c("var", "stat"), sep = "_(?=[^_]*$)") %>%
          spread("var", "value")
      )
      colnames(xx) <- xxnames
      colnames(results) <- c("stat", xxnames)
      
      # Transform results into output object
      output <- as.data.frame(t(results[ ,-1]))
      colnames(output) <- results$stat
    } else {
      # Suppress warnings for groups having 0 valid values
      suppressWarnings({
        output <- xx %>% 
          summarise_all(.funs = summar_funs, na.rm = na.rm) %>%
          as.data.frame
      })
      rownames(output) <- parse_info$var_name %||% var_names
    }

    # Calculate additional stats if needed
    if ("cv" %in% stats) {
      output$cv <- output$sd / output$mean
    }
    
    if ("se.skewness" %in% stats) {
      output$se.skewness <- 
        with(output, 
             sqrt((6 * n.valid * (n.valid - 1)) / 
                    ((n.valid - 2) * (n.valid + 1) * (n.valid + 3))))
    }
    
    if ("pct.valid" %in% stats) {
      output$pct.valid <- output$n.valid * 100 / nrow(xx)
    }
    
    # Apply corrections where n.valid = 0
    zerows <- which(output$n.valid == 0)
    if (length(zerows)) {
      warning("no non-missing arguments to numerical functions")
    }
    
  } else {
    
    # Weights being used -------------------------------------------------------
    
    weights_name <- deparse(substitute(weights))
    
    # Subset weights when called from by()/stby() to match current data subset
    if (isTRUE(flag_by)) {
      pf <- parent.frame(2)
      weights <- weights[pf$X[[pf$i]]]
    }

    if (sum(is.na(weights)) > 0) {
      warning("Missing values on weight variable have been detected and will",
              "be treated as zeroes")
      weights[is.na(weights)] <- 0
    }

    # If some weights are 0 or negative, delete rows
    zero_wgts <- which(weights <= 0)
    if (length(zero_wgts)) {
      xx <- xx[-zero_wgts, ]
      message(length(zero_wgts), " rows with weight <= 0 were deleted")
    }

    # If weights are in xx, remove them
    if (length(parse_info$df_name) == 1 && 
       grepl(parse_info$df_name, weights_name)) {
      wgts_vname <- sub(paste0(parse_info$df_name, "\\$"), "", weights_name)
      ind <- which(names(xx) == wgts_vname)
      if (length(ind) == 1) {
        xx <- xx[-ind]
        var_names <- setdiff(var_names, wgts_vname)  
      }
    }
    
    # Build skeleton for output dataframe
    output <- data.frame(mean      = numeric(),
                         sd        = numeric(),
                         min       = numeric(),
                         med       = numeric(),
                         max       = numeric(),
                         mad       = numeric(),
                         cv        = numeric(),
                         n.valid   = numeric(),
                         n         = numeric(),
                         pct.valid = numeric())
    
    # Rescale weights if necessary
    if (rescale.weights) {
      weights <- weights / sum(weights) * nrow(xx)
    }
    
    n <- sum(weights)      
    
    for (i in seq_along(xx)) {
      variable <- as.numeric(xx[[i]])
      # Extract number and proportion of missing and valid values
      
      if (any(c("n.valid", "pct.valid") %in% stats)) {
        n_valid <- sum(weights[which(!is.na(variable))])
        p_valid <- n_valid / n
      } else {
        # calculate n_valid for validation // all missing
        n_valid <- sum(!is.na(variable))
        p_valid <- NA
      }
      
      # Remove missing values from variable and from corresponding weights
      if (isTRUE(na.rm)) {
        ind <- which(!is.na(variable))
        variable <- variable[ind]
        weights_tmp <- weights[ind]
      }

      # Calculate mean and sd if necessary
      if (any(c("mean", "cv") %in% stats)) {
        variable.mean <- weightedMean(variable, weights_tmp, refine = TRUE, 
                                      na.rm = na.rm)
      }
      
      if (any(c("sd", "cv") %in% stats)) {
        variable.sd <- weightedSd(variable, weights_tmp, na.rm = na.rm)
      }
      
      # Calculate and insert stats into output dataframe
      # (suppress repeated warnings when no non-missing data)
      suppressWarnings({
        output[i, ] <-
          c(ifelse("mean" %in% stats, variable.mean, NA),
            ifelse("sd"   %in% stats, variable.sd, NA),
            ifelse("min"  %in% stats, min(variable, na.rm = na.rm), NA),
            ifelse("med"  %in% stats, weightedMedian(variable, weights_tmp, 
                                                     refine = TRUE, 
                                                     na.rm = na.rm), NA),
            ifelse("max" %in% stats, max(variable, na.rm = na.rm), NA),
            ifelse("mad" %in% stats, weightedMad(variable, weights_tmp, 
                                                  refine = TRUE, 
                                                  na.rm = na.rm), NA),
            ifelse("cv"        %in% stats, variable.sd/variable.mean, NA),
            ifelse("n.valid"   %in% stats, n_valid, NA),
            ifelse("n"         %in% stats, n, NA),
            ifelse("pct.valid" %in% stats, p_valid * 100, NA))
      })
    }
    
    rownames(output) <- var_names
    
    # Apply corrections where n.valid = 0
    zerows <- which(output$n.valid == 0)
    if (length(zerows)) {
      warning("no non-missing arguments to numerical functions")
    }
  }
  
  # Prepare output data -------------------------------------------------------
  
  # Apply order parameter (column ordering)
  if (identical(order, "sort")) {
    output <- output[sort(rownames(output)), ]
  } else if (length(order) > 1) {
    output <- output[order, ]
  }
  
  # Keep and order required stats from output
  output <- output[ , stats, drop = FALSE]
  
  # Corrections for special case where nrow = 0
  if (nrow(xx) == 0) {
    for (cn in colnames(output)) {
      if (cn == "n.valid") {
        next
      } else if (cn == "pct.valid") {
        output[[cn]] <- NaN
      } else {
        output[[cn]] <- NA
      }
    }
  }
  
  # Apply translations to colnames
  for (i in seq_along(output)) {
    if (colnames(output)[i] == "sd") {
      colnames(output)[i] <- trs("sd.long")
    } else {
      colnames(output)[i] <- trs(colnames(output)[i])
    }
  }

  # Transpose when transpose is FALSE; even though this is counter-intuitive,
  # we prefer that the "vertical" version be the default one and that at the
  # same time, the default value for transpose be FALSE.
  if (!isTRUE(transpose)) {
    output <- t(output)
  }
  
  # Set class/attributes
  class(output)            <- c("summarytools", class(output))
  attr(output, "st_type")  <- "descr"
  attr(output, "date")     <- Sys.Date()
  attr(output, "fn_call")  <- match.call()
  attr(output, "stats")    <- stats
  data_info <-
    list(
      Data.frame       = ifelse("df_name" %in% names(parse_info), 
                                parse_info$df_name, NA),
      Data.frame.label = ifelse("df_label" %in% names(parse_info), 
                                parse_info$df_label, NA),
      Variable         = ifelse("var_name" %in% names(parse_info) && 
                                  length(parse_info$var_name) == 1,
                                parse_info$var_name, NA),
      Variable.label   = ifelse("var_label" %in% names(parse_info) &&
                                  length(parse_info$var_label) == 1,
                                parse_info$var_label,
                                ifelse(!is.na(var_label), var_label, NA)),
      Weights          = ifelse(is.null(weights), NA,
                                sub(pattern = paste0(parse_info$df_name, "$"), 
                                    replacement = "", x = weights_name, 
                                    fixed = TRUE)),
      by_var           = if ("by_group" %in% names(parse_info))
                                parse_info$by_var else NA,
      Group            = ifelse("by_group" %in% names(parse_info),
                                parse_info$by_group, NA),
      by_first         = ifelse("by_group" %in% names(parse_info), 
                                parse_info$by_first, NA),
      by_last          = ifelse("by_group" %in% names(parse_info), 
                                parse_info$by_last, NA),
      transposed       = transpose,
      N.Obs            = ifelse(is.null(weights), nrow(xx),
                                round(n, round.digits))
    )
  
  attr(output, "data_info") <- data_info[!is.na(data_info)]
  
  attr(output, "format_info") <- list(style          = style,
                                      round.digits   = round.digits,
                                      plain.ascii    = plain.ascii,
                                      justify        = justify,
                                      headings       = headings,
                                      display.labels = display.labels,
                                      split.tables   = split.tables)
  
  if (nrow(xx) == 0) {
    attr(output, "format_info") %+=% list(missing = "N/A")
  }
  
  # Keep ... arguments that could be relevant for pander of format
  user_fmt <- list()
  dotArgs <- list(...)
  for (i in seq_along(dotArgs)) {
    if (class(dotArgs[[i]]) %in% 
        c("character", "numeric", "integer", "logical") &&
        length(names(dotArgs[1])) == length(dotArgs[[i]]) &&
        names(dotArgs[i]) != "skip_parse")
      user_fmt <- append(user_fmt, dotArgs[i])
  }
  if (length(user_fmt))
    attr(output, "user_fmt") <- user_fmt
  
  attr(output, "lang") <- st_options("lang")
  
  if (!is.null(ignored)) {
    if (length(ignored <- setdiff(ignored, data_info$by_var))) {
      attr(output, "ignored") <- ignored
    }
  }
  
  return(output)
}

#' @export
descr.grouped_df <- function(x,
                             var             = NULL,
                             stats           = st_options("descr.stats"),
                             na.rm           = TRUE,
                             round.digits    = st_options("round.digits"),
                             transpose       = st_options("descr.transpose"),
                             order           = "sort",
                             style           = st_options("style"),
                             plain.ascii     = st_options("plain.ascii"),
                             justify         = "r",
                             headings        = st_options("headings"),
                             display.labels  = st_options("display.labels"),
                             split.tables    = 100,
                             weights         = NULL,
                             rescale.weights = FALSE,
                             ...) {
  
  var_names  <- NA_character_
  #var_label <- NA_character_
  
  if ("var" %in% names(match.call())) {
    xx <- as_tibble(eval(substitute(var), envir = x))
    var_names <- deparse(substitute(var))
  } else if (inherits(x, "data.frame")) {
    xx <- x
    #xx <- x[ ,setdiff(colnames(x), group_vars(x)), drop = FALSE]
    var_names <- setdiff(colnames(x), group_vars(x))
  } else {
    xx <- as_tibble(x)
  }
    # whole data frame is to be descr'ed
    #xx <- x[ ,setdiff(colnames(x), group_vars(x)), drop = FALSE]
    #}
  
  # Check for weights
  if (missing(weights)) {
    weights_all <- NULL
    weights_name <- NULL
    weights_vname <- NULL
    weights_in_x <- FALSE
  } else {
    weights_name <- deparse(substitute(weights))
    if (exists(weights_name)) {
      weights_all <- eval(weights_name)
    } else if (weights_name %in% colnames(x)) {
      weights_all <- x[[weights_name]]
      weights_in_x <- TRUE
    } else {
      weights_all <- force(weights)
      weights_in_x <- FALSE
    }
    # If weights are in xx, remove name from var_names
    weights_vname <- sub(".+\\$", "", weights_name)
    weights_ind   <- which(names(xx) == weights_vname)
    if (length(weights_ind) == 1) {
      #xx <- xx[-ind]
      #parse_info$var_name <- setdiff(parse_info$var_name, wgts_vname)  
      var_names <- setdiff(var_names, weights_vname)  
    }
  }

  parse_info <- parse_call(mc = match.call(),
                           var_name  = (ncol(xx) == 1),
                           var_label = (ncol(xx) == 1),
                           caller = "descr")
  
  # Prepare for iterations
  outlist  <- list()
  gr_ks    <- map_groups(group_keys(x))
  gr_inds  <- attr(x, "groups")$.rows
  groups   <- group_keys(x)
  gr_var_ind <- which(colnames(xx) %in% group_vars(x))
  
  for (g in seq_along(gr_ks)) {
    
    if (is.null(weights_all)) {
      weights_gr  <- NULL # NA
      weights_ind <- numeric()
    } else {
      weights_gr <- weights_all[gr_inds[[g]]]
      weights_ind <- which(colnames(xx) == weights_vname)
    }
    
    neg_ind <- c(weights_ind, gr_var_ind)
    if (length(neg_ind))
      x_expr <- quote(xx[gr_inds[[g]], -neg_ind])
    else
      x_expr <- quote(xx[gr_inds[[g]], ])
    
    #if (length(weights_ind))
    #  xx <- xx[,-weights_ind]
    dotArgs <- list(...)
    
    args_list <- c(list(x               = x_expr, 
                        stats           = stats,
                        na.rm           = na.rm,
                        round.digits    = round.digits,
                        transpose       = transpose,
                        order           = order,
                        style           = style,
                        plain.ascii     = plain.ascii,
                        justify         = justify,
                        headings        = headings,
                        display.labels  = display.labels,
                        split.tables    = split.tables,
                        weights         = weights_gr,
                        rescale.weights = rescale.weights,
                        skip_parse      = TRUE),
                   dotArgs)
    
    if (missing(weights)) {
      args_list$weights <- NULL
      args_list$rescale.weights <- NULL
    }

    outlist[[g]] <- do.call("descr", args_list)
      
    # Build data_info attribute
    data_info <- list(
      Data.frame       = parse_info$df_name,
      Data.frame.label = parse_info$df_label,
      Variable         = parse_info$var_name,
      Variable.label   = parse_info$var_label,
      Weights          = weights_vname,
      by_var           = group_vars(x),
      Group            = gr_ks[g],
      by_first         = (g == 1),
      by_last          = (g == length(gr_ks)),
      transposed       = transpose,
      N.Obs            = length(gr_inds[[g]])
    )
    
    attr(outlist[[g]], "data_info") <-
      data_info[which(sapply(data_info, function(x) !is.null(x)))]
    
    if (identical("value", dimnames(outlist[[g]])[[2 - as.numeric(transpose)]]))
      dimnames(outlist[[g]])[[2 - as.numeric(transpose)]] <- parse_info$var_name
  }
  
  if (length(group_vars(x)) == 1 && ncol(xx) == 1) {
    names(outlist) <- sub(paste(group_vars(x), "= "), "", gr_ks)
  } else {
    names(outlist) <- gr_ks
  }
  class(outlist) <- c("stby")
  attr(outlist, "groups") <- group_keys(x)
  
  #.e_reset()	
  return(outlist)
}
