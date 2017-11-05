#' Univariate Statistics for Numerical Data
#'
#' Calculates weighted or non-weighted mean, standard deviation, min, max,
#' median, mad, IQR*, CV, skewness*, SE.skewness*, and kurtosis* on numerical
#' vectors. (Items marked with an * are only available for non-weighted
#' vectors / data frames.
#'
#' @param x A numerical vector or a data frame.
#' @param stats Which stats to produce. Either \dQuote{all} (default), or a
#'   selection of : \dQuote{mean}, \dQuote{sd}, \dQuote{min}, \dQuote{med}, \dQuote{max},
#'   \dQuote{mad}, \dQuote{iqr}, \dQuote{cv}, \dQuote{skewness}, \dQuote{se.skewness},
#'   \dQuote{kurtosis}, \dQuote{n.valid}, and \dQuote{pct.valid}.
#' @param na.rm Argument to be passed to statistical functions. Defaults to
#'   \code{TRUE}.
#' @param round.digits Number of significant digits to keep. Defaults to
#'   \code{2}.
#' @param style Style to be used by \code{\link[pander]{pander}} when rendering
#'   output tables; One of \dQuote{simple} (default), \dQuote{grid} or
#'   \dQuote{rmarkdown}.
#' @param plain.ascii Logical \code{\link[pander]{pander}} argument. When
#'   \code{TRUE}, no markup characters will be used (useful when printing
#'   to console). Defaults to \code{TRUE} when \code{style} is \dQuote{simple},
#'   and \code{FALSE} otherwise.
#' @param justify String indicating alignment of columns; one of \dQuote{left}
#'   \dQuote{center}, or \dQuote{right} (default). Has no effect on \emph{html}
#'   tables.
#' @param transpose Makes variables appears as columns, and stats as rows.
#'   Defaults to \code{FALSE}.
#' @param use.labels Logical. Display label instead of variable name when
#'   label exists.
#' @param display.labels Logical. Should variable / data frame labels be displayed in
#'   the title section?  Default is \code{TRUE}.
#' @param weights Vector of weights having same length as x. Use \code{NA}
#'   (default) when no weights are provided.
#' @param rescale.weights Logical parameter. When set to \code{TRUE}, the
#'   total count will be the same as the unweighted \code{x}. \code{FALSE} by
#'   default.
#' @param \dots Additional arguments passed to \code{\link[pander]{pander}}.
#'
#' @return A matrix object with the statistics, with extra attributes used by
#'   \pkg{summarytool}'s print method.
#'
#' @examples
#' data(exams)
#' descr(exams)
#' descr(exams, transpose=TRUE)
#' descr(exams, stats = c("mean", "sd"))
#' data(tobacco)
#' with(tobacco, by(age, smoker, descr))
#'
#' @keywords univar
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
descr <- function(x, stats = "all", na.rm = TRUE, round.digits = 2,
                  transpose = FALSE, style = "simple", plain.ascii = TRUE,
                  justify = "right", use.labels = FALSE, display.labels = TRUE,
                  weights = NA, rescale.weights = FALSE, ...) {

  # Validate arguments
  if (is.atomic(x) && !is.numeric(x))
    stop("x is not numerical")

  # make x a data.frame
  x.df <- as.data.frame(x)

  if (is.atomic(x) && !is.na(label(x))) {
    label(x.df[[1]]) <- label(x)
  }
  
  if (!is.data.frame(x.df))
    stop(paste("x must be a data.frame, a tibble, a data.table or a single vector, and",
               "attempted conversion failed"))

  # check that all 'stats' elements are valid
  valid_stats <- list(no_wgts = c("mean", "sd", "min", "med", "max", "mad", "iqr", "cv",
                                  "skewness", "se.skewness", "kurtosis", "n.valid", "pct.valid"),
                      wgts = c("mean", "sd", "min", "med", "max", "mad", "cv", "n.valid", "pct.valid"))

  if (!identical(stats,"all")) {
    stats <- tolower(stats)
    invalid_stats <- setdiff(stats, valid_stats[[2 - as.numeric(identical(weights, NA))]])
    if (length(invalid_stats) > 0) {
      stop("allowed 'stats' are: ", paste(valid_stats, collapse = ", "))
    } else {
      stats_subset <- which(tolower(valid_stats[[2 - as.numeric(identical(weights, NA))]]) %in% stats)
    }
  } else {
    stats_subset <- seq_len(length(valid_stats[[2 - as.numeric(identical(weights, NA))]]))
  }

  if (!na.rm %in% c(TRUE, FALSE))
    stop("'na.rm' argument must be either TRUE or FALSE")

  if (!is.numeric(round.digits) || round.digits < 1)
    stop("'round.digits' argument must be numerical and >= 1")

  if (!transpose %in% c(TRUE, FALSE))
    stop("'transpose' argument must be either TRUE or FALSE")

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

  # When style='rmarkdown', make plain.ascii FALSE unless specified explicitly
  if (style=="rmarkdown" && plain.ascii==TRUE && (!"plain.ascii" %in% (names(match.call()))))
    plain.ascii <- FALSE

  if ("file" %in% names(match.call()))
    message("file argument is deprecated; use print() or view() function to generate files")

  # Get into about x from parsing function
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

  if (ncol(x.df) == 0)
    stop("no numerical variable(s) given as argument")

  # Initialise output list
  output <- list()

  if (identical(weights, NA)) {

    # Build skeleton (2 empty dataframes; one for stats and other
    # to report valid vs na counts)
    output <- data.frame(Mean = numeric(),
                         Std.Dev = numeric(),
                         Min = numeric(),
                         Median = numeric(),
                         Max = numeric(),
                         MAD = numeric(),
                         IQR = numeric(),
                         CV = numeric(),
                         Skewness = numeric(),
                         SE.Skewness = numeric(),
                         Kurtosis = numeric(),
                         N.Valid = numeric(),
                         Pct.Valid = numeric())

    # Iterate over columns in x
    for(i in seq_along(x.df)) {

      variable <- as.numeric(x.df[ ,i])
      if (i == 1) {
        n_tot <- length(variable)
      }

      # Extract number and proportion of missing and valid values
      n_valid <- sum(!is.na(variable))
      p_valid <- n_valid / length(variable)
      n_NA <- sum(is.na(variable))
      p_NA <- n_NA / length(variable)

      # Insert stats into output dataframe
      output[i, ] <- c(variable.mean <- mean(variable, na.rm=na.rm),
                       variable.sd <- sd(variable, na.rm=na.rm),
                       min(variable, na.rm=na.rm),
                       median(variable, na.rm=na.rm),
                       max(variable, na.rm=na.rm),
                       mad(variable, na.rm=na.rm),
                       IQR(variable, na.rm=na.rm),
                       variable.mean / variable.sd,
                       skewness(variable, na.rm=na.rm),
                       sqrt((6*n_valid*(n_valid-1)) /
                              ((n_valid-2)*(n_valid+1)*(n_valid+3))),
                       kurtosis(variable, na.rm=na.rm),
                       n_valid,
                       p_valid * 100)

    }

  } else {

    # Weights are used ---------------

    # Check that weights vector has the right length
    if (length(weights) != nrow(x.df))
      stop("weights vector must have the same length as x")

    weights_string <- deparse(substitute(weights))
    weights_label <- try(label(weights), silent = TRUE)

    if (sum(is.na(weights)) > 0) {
      warning("Missing values on weight variable have been detected and were treated as zeroes.")
      weights[is.na(weights)] <- 0
    }

    # Build skeleton (2 empty dataframes; one for stats and other to report valid vs na counts)
    output <- data.frame(Mean = numeric(),
                         Std.Dev = numeric(),
                         Min = numeric(),
                         Median = numeric(),
                         Max = numeric(),
                         MAD = numeric(),
                         CV = numeric(),
                         N.Valid = numeric(),
                         Pct.Valid = numeric())

    # Rescale weights if necessary
    if (rescale.weights)
      weights <- weights / sum(weights) * nrow(x.df)

    n_tot <- sum(weights)

    for(i in seq_along(x.df)) {
      variable <- as.numeric(x.df[ ,i])

      # Extract number and proportion of missing and valid values
      n_valid <- sum(weights[which(!is.na(variable))])
      p_valid <- n_valid / sum(weights)
      n_NA <- sum(weights[which(is.na(variable))])
      p_NA <- n_NA / sum(weights)

      # Remove missing values from variable and from corresponding weights
      ind <- which(!is.na(variable))
      variable <- variable[ind]
      weights <- weights[ind]

      # Calculate the weighted stats & fill in the row in output df
      output[i, ] <-
        c(variable.mean <- weightedMean(variable, weights, refine = TRUE),
          variable.sd <- weightedSd(variable, weights, refine = TRUE),
          min(variable),
          weightedMedian(variable, weights, refine = TRUE),
          max(variable),
          weightedMad(variable, weights, refine = TRUE),
          variable.mean/variable.sd,
          n_valid,
          p_valid * 100)

    }
  }

  for(i in seq_along(x.df)) {

    # Add row names (from col.names/var.name of the parse function)
    if ("var_names" %in% names(parse_info)) {
      rownames(output)[i] <- parse_info$var_names[i]
    } else {
      # this is necessary in order to support by()
      rownames(output)[i] <- paste0("Var",i)
    }
  }

  # Remove unwanted stats from output
  output <- output[ ,stats_subset]

  # Transpose when transpose is FALSE; even though this is counter-intuitive,
  # we prefer that the "vertical" version be the default one and that at the
  # same time, the default value for transpose be FALSE.
  if (!transpose) {
    output <- t(output)
  }

  # Set class/attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "descr"
  attr(output, "date") <- Sys.Date()
  attr(output, "fn_call") <- as.character(match.call())

  data_info <-
    list(Dataframe       = ifelse("df_name" %in% names(parse_info), parse_info$df_name, NA),
         Dataframe.label = ifelse("df_label" %in% names(parse_info), parse_info$df_label, NA),
         Variable        = ifelse("var_names" %in% names(parse_info) && length(parse_info$var_names) == 1,
                                  parse_info$var_names, NA),
         Variable.labels = label(x, all = TRUE, fallback = TRUE, simplify = TRUE),
         Subset          = ifelse("rows_subset" %in% names(parse_info), parse_info$rows_subset, NA),
         Weights         = ifelse(identical(weights, NA), NA,
                                  sub(pattern = paste0(parse_info$df_name, "$"), replacement = "",
                                      x = weights_string, fixed = TRUE)),
         Weights.label   = ifelse(!identical(weights, NA) && class(weights_label) != "try-error",
                                  weights_label, NA),
         Group           = ifelse("by_group" %in% names(parse_info), parse_info$by_group, NA),
         by.first        = ifelse("by_group" %in% names(parse_info), parse_info$by_first, NA),
         by.last         = ifelse("by_group" %in% names(parse_info), parse_info$by_last, NA),
         N.Obs           = n_tot)

  attr(output, "data_info") <- data_info[!is.na(data_info)]

  attr(output, "formatting") <- list(style = style,
                                     round.digits = round.digits,
                                     plain.ascii = plain.ascii,
                                     justify = justify,
                                     display.labels = display.labels)

  attr(output, "user_fmt") <- list(... = ...)

  if (exists("ignored"))
    attr(output, "ignored") <- ignored

  return(output)
}
