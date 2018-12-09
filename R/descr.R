#' Univariate Statistics for Numerical Data
#'
#' Calculates mean, sd, min, Q1*, median, Q3*, max, MAD, IQR*, CV, 
#' skewness*, SE.skewness*, and kurtosis* on numerical vectors. (*) Not 
#' available when using sampling weights.
#'
#' @param x A numerical vector or a data frame.
#' @param stats Which stats to produce. Either \dQuote{all} (default),
#'   \dQuote{fivenum}, \dQuote{common} (see Details), or a selection of :
#'   \dQuote{mean}, \dQuote{sd}, \dQuote{min}, \dQuote{q1}, \dQuote{med},
#'   \dQuote{q3}, \dQuote{max}, \dQuote{mad}, \dQuote{iqr}, \dQuote{cv},
#'   \dQuote{skewness}, \dQuote{se.skewness}, \dQuote{kurtosis},
#'   \dQuote{n.valid}, and \dQuote{pct.valid}. This can be set globally via
#'   \code{\link{st_options}} (\dQuote{descr.stats}).
#' @param na.rm Argument to be passed to statistical functions. Defaults to
#'   \code{TRUE}. Can be set globally; see \code{\link{st_options}}.
#' @param round.digits Number of significant digits to display. Defaults to
#'   \code{2}, and can be set globally (see \code{\link{st_options}}).
#' @param transpose Logical. Makes variables appears as columns, and stats as
#'   rows. Defaults to \code{FALSE}. To change this default value, see
#'   \code{\link{st_options}} (option \dQuote{descr.transpose}).
#' @param style Style to be used by \code{\link[pander]{pander}} when rendering
#'   output table; One of \dQuote{simple} (default), \dQuote{grid}, or
#'   \dQuote{rmarkdown} This option can be set globally; see
#'   \code{\link{st_options}}.
#' @param plain.ascii Logical. \code{\link[pander]{pander}} argument; when
#'   \code{TRUE}, no markup characters will be used (useful when printing to
#'   console). Defaults to \code{TRUE} unless \code{style = 'rmarkdown'}, in
#'   which case it will be set to \code{FALSE} automatically. To change the
#'   default value globally, see \code{\link{st_options}}.
#' @param justify Alignment of numbers in cells; \dQuote{l} for left, \dQuote{c}
#'   for center, or \dQuote{r} for right (default). Has no effect on \emph{html}
#'   tables.
#' @param headings Logical. Set to \code{FALSE} to omit heading section. Can be
#'   set globally via \code{\link{st_options}}.
#' @param display.labels Logical. Should variable / data frame labels be
#'   displayed in the title section?  Default is \code{TRUE}. To change this
#'   default value globally, see \code{\link{st_options}}.
#' @param split.tables Pander argument that specifies how many characters wide a
#'   table can be. \code{100} by default.
#' @param weights Vector of weights having same length as x. \code{NA} (default)
#'   indicates that no weights are used.
#' @param rescale.weights Logical. When set to \code{TRUE}, the total count will
#'   be the same as the unweighted \code{x}. \code{FALSE} by default.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}}.
#'
#' @return A nn object of classes \code{matrix} and \code{summarytools}
#'   containing the statistics, with extra attributes used by \link{print}
#'   method.
#'
#' @examples
#' data(exams)
#' descr(exams)
#' descr(exams, stats = "common")
#' descr(exams, stats = c("mean", "sd", "min", "max"), transpose = TRUE)
#' data(tobacco)
#' with(tobacco, view(by(BMI, gender, descr), method = "pander"))
#'
#' @keywords univar
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
#' @importFrom matrixStats weightedMean weightedSd weightedMedian weightedMad
#' @importFrom rapportools skewness kurtosis nvalid
#' @importFrom stats IQR mad median sd quantile
#' @importFrom utils head
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble funs select starts_with summarize_all
descr <- function(x, stats = st_options('descr.stats'), na.rm = TRUE, 
                  round.digits = st_options('round.digits'),
                  transpose = st_options('descr.transpose'), 
                  style = st_options('style'), 
                  plain.ascii = st_options('plain.ascii'),
                  justify = "right", headings = st_options('headings'), 
                  display.labels = st_options('display.labels'),  
                  split.tables = 100, weights = NA, rescale.weights = FALSE,
                  ...) {
  
  # Validate arguments ---------------------------------------------------------
  errmsg <- character()  # problems with arguments will be stored here
  
  if (is.atomic(x) && !is.numeric(x)) {
    errmsg %+=% "'x' must be numeric"
  }
  # make x a data.frame
  x.df <- as_tibble(x)
  
  if (!is.data.frame(x.df)) {
    errmsg %+=% paste("'x' must be a numeric vector, a data.frame, a tibble,",
                      "a data.table; attempted conversion to tibble failed")
  }
  
  errmsg <- check_arguments(match.call(), list(...), errmsg)
  
  valid_stats <- list(
    no_wgts = c("mean", "sd", "min", "q1", "med", "q3","max", "mad", 
                "iqr", "cv", "skewness", "se.skewness", "kurtosis", 
                "n.valid", "pct.valid"),
    wgts = c("mean", "sd", "min", "med", "max", "mad", "cv", 
             "n.valid", "pct.valid")
  )
  if (identical(stats,"all")) {
    stats <- valid_stats[[2 - as.numeric(identical(weights, NA))]]
  } else if (identical(stats, "fivenum")) {
    if (!identical(weights, NA)) {
      errmsg %+=% "fivenum is not supported when weights are used"
    }
    stats <- c("min", "q1", "med", "mean", "q3", "max")
  } else if (identical(stats, "common")) {
    stats <- c("mean", "sd", "min", "med", "max", "n.valid", "pct.valid")
  } else {
    stats <- tolower(stats)
    invalid_stats <- 
      setdiff(stats, valid_stats[[2 - as.numeric(identical(weights, NA))]])
    if (length(invalid_stats) > 0) {
      errmsg %+-% 
        paste("valid stats are: ", 
              paste(
                dQuote(valid_stats[[2 - as.numeric(identical(weights, NA))]]), 
                sep = ", "
              ), 
              ";\n  following statistics are not recognized, or not allowed: ",
              paste(dQuote(invalid_stats), collapse = ", ")
              )
    }
  }
  
  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "))
  }

  # When style is rmarkdown, make plain.ascii FALSE unless specified explicitly
  if (style=="rmarkdown" && isTRUE(plain.ascii) && 
      (!"plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }
  
  # Get info about x from parsing function
  parse_info <- try(parse_args(sys.calls(), sys.frames(), match.call()), 
                    silent = TRUE)
  
  if (inherits(parse_info, "try-error")) {
    parse_info <- list()
  }

  if (!"var_names" %in% names(parse_info)) {
    parse_info$var_names <- colnames(x.df)
  }
  
  # Identify and exclude non-numerical columns from x
  col_to_remove <- which(!vapply(x.df, is.numeric, logical(1)))
  
  if (length(col_to_remove) > 0) {
    ignored <- paste(colnames(x.df)[col_to_remove], collapse=", ")
    x.df <- x.df[-col_to_remove]
    parse_info$var_names <- parse_info$var_names[-col_to_remove]
  }
  
  if (ncol(x.df) == 0) {
    stop("no numerical variable(s) given as argument")
  }

  # No weights being used ------------------------------------------------------
  if (identical(weights, NA)) {
    
    # Prepare the summarizing functions; there are 3 that we'll calculate 
    # later so to not slow down the process
    summar_funs <- funs(mean, 
                        sd, 
                        min, 
                        q1 = quantile(., probs = .25, type = 2, names = FALSE),
                        med = median,
                        q3 = quantile(., probs = .75, type = 2, names = FALSE),
                        max,
                        mad,
                        iqr = IQR,
                        cv = -999,
                        skewness = rapportools::skewness,
                        se.skewness = -999,
                        kurtosis = rapportools::kurtosis,
                        n.valid = rapportools::nvalid,
                        pct.valid = -999)
    
    summar_funs <- summar_funs[which(names(summar_funs) %in% stats)]  

    results <- x.df %>% summarize_all(.funs = summar_funs, na.rm = na.rm)

    # Build skeleton for output dataframe
    output <- data.frame(mean        = numeric(ncol(x.df)),
                         sd          = numeric(ncol(x.df)),
                         min         = numeric(ncol(x.df)),
                         q1          = numeric(ncol(x.df)),
                         med         = numeric(ncol(x.df)),
                         q3          = numeric(ncol(x.df)),
                         max         = numeric(ncol(x.df)),
                         mad         = numeric(ncol(x.df)),
                         iqr         = numeric(ncol(x.df)),
                         cv          = numeric(ncol(x.df)),
                         skewness    = numeric(ncol(x.df)),
                         se.skewness = numeric(ncol(x.df)),
                         kurtosis    = numeric(ncol(x.df)),
                         n.valid     = numeric(ncol(x.df)),
                         pct.valid   = numeric(ncol(x.df)))
    
    output <- output[which(names(output) %in% stats)]  
    rownames(output) <- parse_info$var_names
    
    # Fill-in the output table
    if (ncol(x.df) == 1) {
      output[1,] <- results[1,]
    } else {
      for (rname in rownames(output)) {
        output[rname,] <- results %>% select(starts_with(rname))
      }
    }
    
    # Calculate additionnal stats if needed
    if ('cv' %in% stats) {
      output$cv <- output$sd / output$mean
    }
    
    if ('se.skewness' %in% stats) {
      output$se.skewness <- 
        with(output, 
             sqrt((6 * n.valid * (n.valid - 1)) / 
                    ((n.valid - 2) * (n.valid + 1) * (n.valid + 3))))
    }
    
    if ('pct.valid' %in% stats) {
      output$pct.valid <- output$n.valid *100 / nrow(x.df)
    }
    
  } else {
    
    # Weights being used -------------------------------------------------------
    
    # Check that weights vector has the right length
    if (length(weights) != nrow(x.df)) {
      stop("weights vector must have the same length as x")
    }
    
    weights_string <- deparse(substitute(weights))

    if (sum(is.na(weights)) > 0) {
      warning("Missing values on weight variable have been detected and will",
              "be treated as zeroes")
      weights[is.na(weights)] <- 0
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
                         pct.valid = numeric())
    
    # Rescale weights if necessary
    if (rescale.weights) {
      weights <- weights / sum(weights) * nrow(x.df)
    }
    
    n_tot <- sum(weights)
    
    for(i in seq_along(x.df)) {
      variable <- as.numeric(x.df[[i]])
      
      # Extract number and proportion of missing and valid values
      if (any(c("n.valid", "pct.valid") %in% stats)) {
        n_valid <- sum(weights[which(!is.na(variable))])
        p_valid <- n_valid / sum(weights)
      } else {
        n_valid <- NA
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
      output[i, ] <-
        c(ifelse("mean" %in% stats, variable.mean, NA),
          ifelse("sd"   %in% stats, variable.sd, NA),
          ifelse("min"  %in% stats, min(variable, na.rm = na.rm), NA),
          ifelse("med"  %in% stats, weightedMedian(variable, weights_tmp, 
                                                   refine = TRUE, 
                                                   na.rm = na.rm), NA),
          ifelse("max"  %in% stats, max(variable, na.rm = na.rm), NA),
          ifelse("mad"  %in% stats, weightedMad(variable, weights_tmp, 
                                                refine = TRUE, 
                                                na.rm = na.rm), NA),
          ifelse("cv"   %in% stats, variable.sd/variable.mean, NA),
          ifelse("n.valid"   %in% stats, n_valid, NA),
          ifelse("pct.valid" %in% stats, p_valid * 100, NA))
    }
    
    rownames(output) <- parse_info$var_names
    
  }
  
  
  # Prepare output data -------------------------------------------------------
  # Keep and order required stats from output
  output <- output[ ,stats]
  
  
  # Make column names prettier
  cnames <- c(sd = "Std.Dev", med = "Median", mad = "MAD", iqr = "IQR", 
                cv = "CV", se.skewness = "SE.Skewness", n.valid = "N.Valid",
                pct.valid = "Pct.Valid")
  
  for (i in seq_along(cnames)) {
    colnames(output)[which(colnames(output) == names(cnames[i]))] <- cnames[i]
  }
  colnames(output) <- rapportools::capitalise(colnames(output))
  
  # Transpose when transpose is FALSE; even though this is counter-intuitive,
  # we prefer that the "vertical" version be the default one and that at the
  # same time, the default value for transpose be FALSE.
  if (!isTRUE(transpose)) {
    output <- t(output)
  }
  
  # Set class/attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type")    <- "descr"
  attr(output, "date")       <- Sys.Date()
  attr(output, "fn_call")    <- match.call()
  
  
  data_info <-
    list(
      Dataframe       = ifelse("df_name" %in% names(parse_info), 
                               parse_info$df_name, NA),
      Dataframe.label = ifelse("df_label" %in% names(parse_info), 
                               parse_info$df_label, NA),
      Variable        = ifelse("var_names" %in% names(parse_info) && 
                                 length(parse_info$var_names) == 1,
                               parse_info$var_names, NA),
      Variable.label  = ifelse("var_label" %in% names(parse_info) &&
                                 length(parse_info$var_label) == 1,
                               parse_info$var_label, NA),
      Weights         = ifelse(identical(weights, NA), NA,
                               sub(pattern = paste0(parse_info$df_name, "$"), 
                                   replacement = "", x = weights_string, 
                                   fixed = TRUE)),
      Group           = ifelse("by_group" %in% names(parse_info),
                               parse_info$by_group, NA),
      by.first        = ifelse("by_group" %in% names(parse_info), 
                               parse_info$by_first, NA),
      by.last         = ifelse("by_group" %in% names(parse_info), 
                               parse_info$by_last, NA),
      transposed      = transpose,
      N.Obs           = nrow(x.df))
  
  attr(output, "data_info") <- data_info[!is.na(data_info)]
  
  attr(output, "formatting") <- list(style          = style,
                                     round.digits   = round.digits,
                                     plain.ascii    = plain.ascii,
                                     justify        = justify,
                                     headings       = headings,
                                     display.labels = display.labels,
                                     split.tables   = split.tables)
  
  attr(output, "user_fmt") <- list(... = ...)
  
  if (exists("ignored"))
    attr(output, "ignored") <- ignored
  
  return(output)
}
