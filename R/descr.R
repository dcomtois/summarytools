#' Univariate Statistics for Numerical Data
#'
#' Calculates mean, sd, min, Q1*, median, Q3*, max, MAD, IQR*, CV, 
#' skewness*, SE.skewness*, and kurtosis* on numerical vectors. (*) Not available 
#' when using sampling weights.
#'
#' @param x A numerical vector or a data frame.
#' @param stats Which stats to produce. Either \dQuote{all} (default), or a
#'   selection of : \dQuote{mean}, \dQuote{sd}, \dQuote{min}, \dQuote{q1}, \dQuote{med}, 
#'   \dQuote{q3}, \dQuote{max}, \dQuote{mad}, \dQuote{iqr}, \dQuote{cv}, \dQuote{skewness},
#'   \dQuote{se.skewness}, \dQuote{kurtosis}, \dQuote{n.valid}, and \dQuote{pct.valid}.
#'   This can be set globally via \code{\link{st_options}} (\dQuote{descr.stats}).
#' @param na.rm Argument to be passed to statistical functions. Defaults to
#'   \code{TRUE}. Can be set globally; see \code{\link{st_options}}.
#' @param round.digits Number of significant digits to display. Defaults to
#'   \code{2}, and can be set globally (see \code{\link{st_options}}).
#' @param style Style to be used by \code{\link[pander]{pander}} when rendering
#'   output table; One of \dQuote{simple} (default), \dQuote{grid}, or \dQuote{rmarkdown} 
#'   This option can be set globally; see \code{\link{st_options}}.
#' @param plain.ascii Logical. \code{\link[pander]{pander}} argument; when
#'   \code{TRUE}, no markup characters will be used (useful when printing
#'   to console). Defaults to \code{TRUE} unless \code{style = 'rmarkdown'},
#'   in which case it will be set to \code{FALSE} automatically. To change the default 
#'   value globally, see \code{\link{st_options}}.
#' @param justify Alignment of numbers in cells; \dQuote{l} for left, \dQuote{c} for center,
#'   or \dQuote{r} for right (default). Has no effect on \emph{html} tables.
#' @param omit.headings Logical. Set to \code{TRUE} to omit heading section. Can be set
#'   globally via \code{\link{st_options}}.
#' @param transpose Logical. Makes variables appears as columns, and stats as rows.
#'   Defaults to \code{FALSE}. To change this default value, see \code{\link{st_options}}
#'   (option \dQuote{descr.transpose}).
#' @param display.labels Logical. Should variable / data frame labels be displayed in
#'   the title section?  Default is \code{TRUE}. To change this default value globally,
#'   see \code{\link{st_options}}.
#' @param split.tables Pander argument that specifies how many characters wide a
#'   table can be. \code{100} by default.
#' @param weights Vector of weights having same length as x. \code{NA}
#'   (default) indicates that no weights are used.
#' @param rescale.weights Logical. When set to \code{TRUE}, the
#'   total count will be the same as the unweighted \code{x}. \code{FALSE} by
#'   default.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}}.
#'
#' @return A matrix object containing the statistics, with extra attributes used by
#'   \pkg{summarytool}'s print method.
#'
#' @examples
#' data(exams)
#' descr(exams)
#' descr(exams, stats = c("mean", "sd", "min", "max"), transpose = TRUE)
#' data(tobacco)
#' with(tobacco, by(age, smoker, descr))
#'
#' @keywords univar
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
descr <- function(x, stats = st_options('descr.stats'), na.rm = TRUE, 
                  round.digits = st_options('round.digits'),
                  transpose = st_options('descr.transpose'), 
                  style = st_options('style'), 
                  plain.ascii = st_options('plain.ascii'),
                  justify = "right", omit.headings = st_options('omit.headings'), 
                  display.labels = st_options('display.labels'),  
                  split.tables = 100, weights = NA, rescale.weights = FALSE, ...) {
  
  # Validate arguments --------------------------------------------------------
  
  if (is.atomic(x) && !is.numeric(x)) {
    stop("x is not numerical")
  }
  
  # make x a data.frame
  x.df <- as.data.frame(x)
  
  if (is.atomic(x) && !is.na(label(x))) {
    label(x.df[[1]]) <- label(x)
  }
  
  if (!is.data.frame(x.df)) {
    stop(paste("x must be a data.frame, a tibble, a data.table or a single vector, and",
               "attempted conversion failed"))
  }
  
  # check that all 'stats' elements are valid
  valid_stats <- list(no_wgts = c("mean", "sd", "min", "q1", "med", "q3","max", "mad", 
                                  "iqr", "cv", "skewness", "se.skewness", "kurtosis", 
                                  "n.valid", "pct.valid"),
                      wgts = c("mean", "sd", "min", "med", "max", "mad", "cv", 
                               "n.valid", "pct.valid"))
  
  if (identical(stats,"all")) {
    stats <- valid_stats[[2 - as.numeric(identical(weights, NA))]]
  } else {
    stats <- tolower(stats)
    invalid_stats <- setdiff(stats, valid_stats[[2 - as.numeric(identical(weights, NA))]])
    if (length(invalid_stats) > 0) {
      stop("valid stats are: ", paste(valid_stats[[2 - as.numeric(identical(weights, NA))]], 
                                      collapse = ", "),
           "\n  The following statistics are not recognized, or not allowed: ",
           paste(invalid_stats, collapse = ", "))
    }
  }
  
  if (!na.rm %in% c(TRUE, FALSE)) {
    stop("'na.rm' argument must be either TRUE or FALSE")
  }
  
  if (!is.numeric(round.digits) || round.digits < 1) {
    stop("'round.digits' argument must be numerical and >= 1")
  }
  
  if (!transpose %in% c(TRUE, FALSE)) {
    stop("'transpose' argument must be either TRUE or FALSE")
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
  
  # When style='rmarkdown', make plain.ascii FALSE unless specified explicitly
  if (style=="rmarkdown" && plain.ascii==TRUE && (!"plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }
  
  if ("file" %in% names(match.call())) {
    message("file argument is deprecated; use print() or view() function to generate files")
  }
  
  # Get info about x from parsing function
  parse_info <- try(parse_args(sys.calls(), sys.frames(), match.call()), silent = TRUE)
  if (class(parse_info) == "try-catch") {
    parse_info <- list()
  }
  
  # Identify and exclude non-numerical columns from x
  col_to_remove <- which(!sapply(x.df, is.numeric))
  
  if (length(col_to_remove) > 0) {
    ignored <- paste(colnames(x.df)[col_to_remove], collapse=", ")
    x.df <- x.df[-col_to_remove]
    parse_info$var_names <- parse_info$var_names[-col_to_remove]
  }
  
  if (ncol(x.df) == 0) {
    stop("no numerical variable(s) given as argument")
  }

  # No weigths being used -----------------------------------------------------  
  if (identical(weights, NA)) {
    # Build skeleton for output dataframe
    output <- data.frame(mean        = numeric(),
                         sd          = numeric(),
                         min         = numeric(),
                         q1          = numeric(),
                         med         = numeric(),
                         q3          = numeric(),
                         max         = numeric(),
                         mad         = numeric(),
                         iqr         = numeric(),
                         cv          = numeric(),
                         skewness    = numeric(),
                         se.skewness = numeric(),
                         kurtosis    = numeric(),
                         n.valid     = numeric(),
                         pct.valid   = numeric())
    
    # Iterate over columns in x
    for(i in seq_along(x.df)) {
      
      variable <- as.numeric(x.df[ ,i])
      if (i == 1) {
        n_tot <- length(variable)
      }
      
      # Extract number and proportion of missing and valid values
      if (any(c("n.valid", "pct.valid", "se.skewness") %in% stats)) {
        n_valid <- sum(!is.na(variable))
        p_valid <- n_valid / length(variable)
      }
      
      # Calculate mean and sd if necessary
      if (any(c("mean", "cv") %in% stats)) {
        variable.mean <- mean(variable, na.rm = na.rm)
      }
      
      if (any(c("sd", "cv") %in% stats)) {
        variable.sd <- sd(variable, na.rm = na.rm)
      }
      
      # Calculate and insert stats into output dataframe
      output[i, ] <- c(ifelse("mean" %in% stats, variable.mean, NA),
                       ifelse("sd"   %in% stats, variable.sd, NA),
                       ifelse("min"  %in% stats, min(variable, na.rm=na.rm), NA),
                       ifelse("q1"   %in% stats, quantile(variable, probs = 0.25, na.rm = na.rm, type = 2), NA),
                       ifelse("med"  %in% stats, median(variable, na.rm=na.rm), NA),
                       ifelse("q3"   %in% stats, quantile(variable, probs = 0.75, na.rm = na.rm, type = 2), NA),
                       ifelse("max"  %in% stats, max(variable, na.rm=na.rm), NA),
                       ifelse("mad"  %in% stats, mad(variable, na.rm=na.rm), NA),
                       ifelse("iqr"  %in% stats, IQR(variable, na.rm=na.rm), NA),
                       ifelse("cv"   %in% stats, variable.mean / variable.sd, NA),
                       ifelse("skewness"    %in% stats, skewness(variable, na.rm=na.rm), NA),
                       ifelse("se.skewness" %in% stats,
                              sqrt((6*n_valid*(n_valid-1)) / ((n_valid-2)*(n_valid+1)*(n_valid+3))), NA),
                       ifelse("kurtosis"    %in% stats, kurtosis(variable, na.rm=na.rm), NA),
                       ifelse("n.valid"     %in% stats, n_valid, NA),
                       ifelse("pct.valid"   %in% stats, p_valid * 100, NA))
    }
    
  } else {
    # Weights being used ------------------------------------------------------
    
    # Check that weights vector has the right length
    if (length(weights) != nrow(x.df)) {
      stop("weights vector must have the same length as x")
    }
    
    weights_string <- deparse(substitute(weights))
    weights_label <- try(label(weights), silent = TRUE)
    
    if (sum(is.na(weights)) > 0) {
      warning("Missing values on weight variable have been detected and will be treated as zeroes")
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
      variable <- as.numeric(x.df[ ,i])
      
      # Extract number and proportion of missing and valid values
      if (any(c("n.valid", "pct.valid") %in% stats)) {
        n_valid <- sum(weights[which(!is.na(variable))])
        p_valid <- n_valid / sum(weights)
      } else {
        n_valid <- NA
        p_valid <- NA
      }
      
      # Remove missing values from variable and from corresponding weights
      if (na.rm == TRUE) {
        ind <- which(!is.na(variable))
        variable <- variable[ind]
        weights_tmp <- weights[ind]
      }

      # Calculate mean and sd if necessary
      if (any(c("mean", "cv") %in% stats)) {
        variable.mean <- weightedMean(variable, weights_tmp, refine = TRUE, na.rm = na.rm)
      }
      
      if (any(c("sd", "cv") %in% stats)) {
        variable.sd <- weightedSd(variable, weights_tmp, na.rm = na.rm)
      }
      
      # Calculate and insert stats into output dataframe
      output[i, ] <-
        c(ifelse("mean" %in% stats, variable.mean, NA),
          ifelse("sd"   %in% stats, variable.sd, NA),
          ifelse("min"  %in% stats, min(variable, na.rm = na.rm), NA),
          ifelse("med"  %in% stats, weightedMedian(variable, weights_tmp, refine = TRUE, na.rm = na.rm), NA),
          ifelse("max"  %in% stats, max(variable, na.rm = na.rm), NA),
          ifelse("mad"  %in% stats, weightedMad(variable, weights_tmp, refine = TRUE, na.rm = na.rm), NA),
          ifelse("cv"   %in% stats, variable.mean/variable.sd, NA),
          ifelse("n.valid"   %in% stats, n_valid, NA),
          ifelse("pct.valid" %in% stats, p_valid * 100, NA))
    }
  }
  
  for(i in seq_along(x.df)) {
    
    # Add row names (from col.names/var.name of the parse_args function)
    if ("var_names" %in% names(parse_info)) {
      rownames(output)[i] <- parse_info$var_names[i]
    } else {
      # this is necessary in order to support by()
      rownames(output)[i] <- paste0("Var",i)
    }
  }
  
  # Prepare output data -------------------------------------------------------
  
  # Keep and order required stats from output
  output <- output[ ,stats]
  
  # Make column names prettier
  cnames <- c(sd = "Std.Dev", med = "Median", mad = "MAD", iqr = "IQR", cv = "CV",
              se.skewness = "SE.Skewness", n.valid = "N.Valid", pct.valid = "Pct.Valid")
  for (i in seq_along(cnames)) {
    colnames(output)[which(colnames(output) == names(cnames[i]))] <- cnames[i]
  }
  colnames(output) <- rapportools::capitalise(colnames(output))
  
  # Transpose when transpose is FALSE; even though this is counter-intuitive,
  # we prefer that the "vertical" version be the default one and that at the
  # same time, the default value for transpose be FALSE.
  if (!transpose) {
    output <- t(output)
  }
  
  # Set class/attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type")    <- "descr"
  attr(output, "date")       <- Sys.Date()
  attr(output, "fn_call")    <- match.call()
  
  # Extract labels
  tmp_labels <- label(x, all = TRUE, fallback = FALSE, simplify = TRUE)
  if (is.character(tmp_labels) && all(tmp_labels == "NA")) {
    tmp_labels <- NA
  }
  
  data_info <-
    list(Dataframe       = ifelse("df_name"   %in% names(parse_info), parse_info$df_name, NA),
         Dataframe.label = ifelse("df_label"  %in% names(parse_info), parse_info$df_label, NA),
         Variable        = ifelse("var_names" %in% names(parse_info) && length(parse_info$var_names) == 1,
                                  parse_info$var_names, NA),
         Variable.labels = tmp_labels,
         Subset          = ifelse("rows_subset" %in% names(parse_info), parse_info$rows_subset, NA),
         Weights         = ifelse(identical(weights, NA), NA,
                                  sub(pattern = paste0(parse_info$df_name, "$"), replacement = "",
                                      x = weights_string, fixed = TRUE)),
         Weights.label   = ifelse(!identical(weights, NA) && class(weights_label) != "try-error",
                                  weights_label, NA),
         Group           = ifelse("by_group" %in% names(parse_info), parse_info$by_group, NA),
         by.first        = ifelse("by_group" %in% names(parse_info), parse_info$by_first, NA),
         by.last         = ifelse("by_group" %in% names(parse_info), parse_info$by_last, NA),
         transposed      = transpose,
         N.Obs           = n_tot)
  
  attr(output, "data_info") <- data_info[!is.na(data_info)]
  
  attr(output, "formatting") <- list(style          = style,
                                     round.digits   = round.digits,
                                     plain.ascii    = plain.ascii,
                                     justify        = justify,
                                     omit.headings  = omit.headings,
                                     display.labels = display.labels,
                                     split.tables   = split.tables)
  
  attr(output, "user_fmt") <- list(... = ...)
  
  if (exists("ignored"))
    attr(output, "ignored") <- ignored
  
  return(output)
}
