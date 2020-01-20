#' Univariate Statistics for Numerical Data
#'
#' Calculates mean, sd, min, Q1*, median, Q3*, max, MAD, IQR*, CV, 
#' skewness*, SE.skewness*, and kurtosis* on numerical vectors. (*) Not 
#' available when using sampling weights.
#'
#' @param x A numerical vector or a data frame.
#' @param var Unquoted expression referring to a specific column in x. Provides
#'   support for piped function calls (e.g. \code{df \%>\% descr(some_var)}.    
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
#'   set globally via \code{\link{st_options}}. \code{TRUE} by default.
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
#' @return An object having classes \code{matrix} and \code{summarytools}
#'   containing the statistics, with extra attributes used by \link{print}
#'   method.
#'
#' @examples
#' data("exams")
#' 
#' # All stats for all numerical variabls
#' descr(exams)
#' 
#' # Only common statistics
#' descr(exams, stats = "common")
#' 
#' # Arbitrary selection of statistics, transposed
#' descr(exams, stats = c("mean", "sd", "min", "max"), transpose = TRUE)
#' 
#' # Rmarkdown-ready
#' descr(exams, plain.ascii = FALSE, style = "rmarkdown")
#'
#' # Grouped statistics
#' data("tobacco")
#' with(tobacco, stby(BMI, gender, descr))
#'
#' # Grouped statistics, transposed
#' with(tobacco, stby(BMI, age.gr, descr, stats = "common", transpose = TRUE))
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
#' 
#' }
#'
#' @keywords univar
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
#' @importFrom matrixStats weightedMean weightedSd weightedMedian weightedMad
#' @importFrom rapportools skewness kurtosis nvalid
#' @importFrom stats IQR mad median sd quantile
#' @importFrom utils head
#' @importFrom dplyr %>% as_tibble funs select starts_with summarize_all group_keys
#' @importFrom tidyr separate gather spread
descr <- function(x,
                  var             = NULL,
                  stats           = st_options("descr.stats"),
                  na.rm           = TRUE,
                  round.digits    = st_options("round.digits"),
                  transpose       = st_options("descr.transpose"),
                  style           = st_options("style"),
                  plain.ascii     = st_options("plain.ascii"),
                  justify         = "r",
                  headings        = st_options("headings"),
                  display.labels  = st_options("display.labels"),
                  split.tables    = 100,
                  weights         = NA,
                  rescale.weights = FALSE,
                  ...) {
  
  # handle objects of class "grouped_df" (dplyr::group_by)
  if (inherits(x, "grouped_df")) {
    
    if ("var" %in% names(match.call())) {
      # var might contain a function call -- such as df %>% descr(na.omit(var1))
      if (inherits(as.list(match.call()[-1])$var, "call")) {
        var_obj <- eval(as.list(match.call()[-1])$var, envir = x)
        varname <- intersect(colnames(x), 
                             as.character(as.list(match.call()[-1])$var))
      } else {
        var_obj <- x[[as.list(match.call()[-1])$var]]
        varname <- deparse(substitute(var))
      }
    } else {
      var_obj  <- x[,setdiff(colnames(x), group_vars(x))]
    }
    
    parse_info <- try(
      parse_args(sys.calls(), sys.frames(), match.call(),
                 var_name  = (ncol(x) == 1),
                 var_label = (ncol(x) == 1), caller = "descr"),
      silent = TRUE)

    outlist  <- list()
    gr_ks    <- map_groups(group_keys(x))
    gr_inds  <- attr(x, "groups")$.rows
    
    for (g in seq_along(gr_ks)) {
      outlist[[g]] <- descr(x               = as_tibble(var_obj)[gr_inds[[g]], ],
                            stats           = stats,
                            na.rm           = na.rm,
                            round.digits    = round.digits,
                            transpose       = transpose,
                            style           = style,
                            plain.ascii     = plain.ascii,
                            justify         = justify,
                            headings        = headings,
                            display.labels  = display.labels,
                            split.tables    = split.tables,
                            weights         = weights,
                            rescale.weights = rescale.weights,
                            ...             = ...)
      
      if (!inherits(parse_info, "try-error")) {
        if (!is.null(parse_info$df_name))
          attr(outlist[[g]], "data_info")$Data.frame <- parse_info$df_name
        if (!is.null(parse_info$df_label))
          attr(outlist[[g]], "data_info")$Data.frame.label <- parse_info$df_label
        if (!is.null(parse_info$var_name)) {
          attr(outlist[[g]], "data_info")$Variable <- parse_info$var_name
        } else if (exists("varname")) {
          attr(outlist[[g]], "data_info")$Variable <- varname
          if (identical(colnames(outlist[[g]]), "value"))
            colnames(outlist[[g]]) <- varname
          if (identical(rownames(outlist[[g]]), "value"))
            rownames(outlist[[g]]) <- varname
        }
        if (!is.null(parse_info$var_label))
          attr(outlist[[g]], "data_info")$Variable.label <- parse_info$var_label
      }
      
      attr(outlist[[g]], "data_info")$by_var <- 
        setdiff(colnames(attr(x, "groups")), ".rows")
      
      attr(outlist[[g]], "data_info")$Group    <- gr_ks[g]
      attr(outlist[[g]], "data_info")$by_first <- g == 1
      attr(outlist[[g]], "data_info")$by_last  <- g == length(gr_ks)
    }
    
    if (length(group_vars(x)) == 1 && is.null(dim(var_obj))) {
      names(outlist) <- 
        sub(paste(group_vars(x), "= "), "", gr_ks)
    } else {
      names(outlist) <- gr_ks
    }
    class(outlist) <- c("stby")
    attr(outlist, "groups") <- group_keys(x)
    return(outlist)
  }
  
  # When var is provided, discard all other variables
  if (is.data.frame(x) && ncol(x) > 1 && "var" %in% names(match.call())) {
    
    # var might contain a function call -- such as df %>% descr(na.omit(var1))
    if (inherits(as.list(match.call()[-1])$var, "call")) {
      x_obj   <- eval(as.list(match.call()[-1])$var, envir = x)
      varname <- intersect(colnames(x),
                           as.character(as.list(match.call()[-1])$var))
    } else {
      x_obj   <- x[[as.list(match.call()[-1])$var]]
      varname <- deparse(substitute(var))
    }
  } else {
    x_obj <- x
    if (!is.null(colnames(x))) {
      varname  <- colnames(x)
    }
  }
  
  # Validate arguments -------------------------------------------------------
  errmsg <- character()  # problems with arguments will be stored here
  
  if (is.null(x)) {
    tmp_x_name <- deparse(substitute(x))
    stop(tmp_x_name, " is either NULL or does not exist")
  }
  
  if (is.atomic(x_obj) && !is.numeric(x_obj)) {
    errmsg %+=% "'x' must be numeric"
  }
  
  # make x_obj a tibble
  if (!inherits(x_obj, "tbl")) {
    x.df <- as_tibble(x_obj)
  } else {
    x.df <- x_obj
  }
  
  # Get variable label
  var_label <- label(x.df[[1]])
  
  if (!is.data.frame(x.df)) {
    errmsg %+=% paste("'x' must be a numeric vector, a data.frame, a tibble,",
                     "a data.table; attempted conversion to tibble failed")
  }
  
  errmsg <- c(errmsg, check_arguments(match.call(), list(...)))
  
  valid_stats <- list(
    no_wgts = c("mean", "sd", "min", "q1", "med", "q3","max", "mad", 
                "iqr", "cv", "skewness", "se.skewness", "kurtosis", 
                "n.valid", "pct.valid"),
    wgts = c("mean", "sd", "min", "med", "max", "mad", "cv", 
             "n.valid", "pct.valid")
  )
  
  if (identical(stats, "all")) {
    stats <- valid_stats[[2 - as.numeric(identical(weights, NA))]]
  } else if (identical(stats, "fivenum")) {
    if (!identical(weights, NA)) {
      errmsg %+=% paste("fivenum is not supported when weights are used; valid",
                        "stats are:", paste(valid_stats$wgts, collapse = ", "))
      
    }
    stats <- c("min", "q1", "med", "q3", "max")
  } else if (identical(stats, "common")) {
    stats <- c("mean", "sd", "min", "med", "max", "n.valid", "pct.valid")
  } else {
    stats <- tolower(stats)
    invalid_stats <- 
      setdiff(stats, valid_stats[[2 - as.numeric(identical(weights, NA))]])
    if (length(invalid_stats) > 0) {
      errmsg %+=%
        paste("The following statistics are not recognized, or not allowed: ",
              paste(dQuote(invalid_stats), collapse = ", ")
              )
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
  parse_info <- try(
    parse_args(sys.calls(), sys.frames(), match.call(),
               var_name = (ncol(x.df) == 1),
               var_label = (ncol(x.df) == 1), caller = "descr"),
    silent = TRUE)
  
  if (inherits(parse_info, "try-error")) {
    parse_info <- list()
  }

  if (!"var_name" %in% names(parse_info)) {
    if (exists("varname")) {
      parse_info$var_name <- varname
    } else {
      parse_info$var_name <- colnames(x.df)
    }
  }
  
  # Identify and exclude non-numerical columns from x
  col_to_remove <- which(!vapply(x.df, is.numeric, logical(1)))
  
  if (length(col_to_remove) > 0) {
    ignored <- colnames(x.df)[col_to_remove]
    x.df <- x.df[-col_to_remove]
    parse_info$var_name <- parse_info$var_name[-col_to_remove]
  }
  
  if (ncol(x.df) == 0) {
    stop("no numerical variable(s) given as argument")
  }

  # No weights being used ------------------------------------------------------
  if (identical(weights, NA)) {
    
    # Prepare the summarizing functions for dplyr::summarize; there are 3 stats
    # that will be calculated later on so to not slow down the function
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

    if (ncol(x.df) > 1) {
      results <- suppressWarnings(
        x.df %>% summarize_all(.funs = summar_funs, na.rm = na.rm) %>%
        gather("variable", "value") %>%
        separate("variable", c("var", "stat"), sep = "_(?=[^_]*$)") %>%
        spread("var", "value")
      )
      
      # Transform results into output object
      output <- as.data.frame(t(results[ ,-1]))
      colnames(output) <- results$stat
    } else {
      output <- x.df %>% summarize_all(.funs = summar_funs, na.rm = na.rm) %>%
        as.data.frame
      rownames(output) <- parse_info$var_name
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
      output$pct.valid <- output$n.valid *100 / nrow(x.df)
    }
    
    # Apply corrections where n.valid = 0
    zerows <- which(output$n.valid == 0)
    output[zerows, setdiff(stats, "n.valid")] <- NA
    
  } else {
    
    # Weights being used -------------------------------------------------------
    
    weights_string <- deparse(substitute(weights))

    if (sum(is.na(weights)) > 0) {
      warning("Missing values on weight variable have been detected and will",
              "be treated as zeroes")
      weights[is.na(weights)] <- 0
    }

    # If some weights are 0 or negative, delete rows
    zero_wgts <- which(weights <= 0)
    if (length(zero_wgts)) {
      x.df <- x.df[-zero_wgts, ]
      message(length(zero_wgts), " rows with weight <= 0 were deleted")
    }

    # If weights are in x.df, remove them
    if(length(parse_info$df_name) == 1 && 
       grepl(parse_info$df_name, weights_string)) {
      wgts_vname <- sub(paste0(parse_info$df_name, "\\$"), "", weights_string)
      ind <- which(names(x.df) == wgts_vname)
      if (length(ind) == 1) {
        x.df <- x.df[-ind]
        parse_info$var_name <- setdiff(parse_info$var_name, wgts_vname)  
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
                         pct.valid = numeric())
    
    # Rescale weights if necessary
    if (rescale.weights) {
      weights <- weights / sum(weights) * nrow(x.df)
    }
    
    for(i in seq_along(x.df)) {
      variable <- as.numeric(x.df[[i]])
      
      # Extract number and proportion of missing and valid values
      if (any(c("n.valid", "pct.valid") %in% stats)) {
        n_valid <- sum(weights[which(!is.na(variable))])
        p_valid <- n_valid / sum(weights)
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
    
    rownames(output) <- parse_info$var_name
    
    # Apply corrections where n.valid = 0
    zerows <- which(output$n.valid == 0)
    output[zerows, setdiff(stats, "n.valid")] <- NA
  }
  
  # Prepare output data -------------------------------------------------------
  # Keep and order required stats from output
  output <- output[ ,stats]
  
  # Corrections for special case where nrow = 0
  if (nrow(x.df) == 0) {
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
  class(output)              <- c("summarytools", class(output))
  attr(output, "st_type")    <- "descr"
  attr(output, "date")       <- Sys.Date()
  attr(output, "fn_call")    <- match.call()
  attr(output, "stats")      <- stats
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
      Weights          = ifelse(identical(weights, NA), NA,
                                sub(pattern = paste0(parse_info$df_name, "$"), 
                                    replacement = "", x = weights_string, 
                                    fixed = TRUE)),
      by_var           = NA,
      Group            = ifelse("by_group" %in% names(parse_info),
                                parse_info$by_group, NA),
      by_first         = ifelse("by_group" %in% names(parse_info), 
                                parse_info$by_first, NA),
      by_last          = ifelse("by_group" %in% names(parse_info), 
                                parse_info$by_last, NA),
      transposed       = transpose,
      N.Obs            = nrow(x.df))
  
  if ("by_var" %in% names(parse_info)) {
    data_info$by_var <- parse_info$by_var
  }
  
  attr(output, "data_info") <- data_info[!is.na(data_info)]
  
  attr(output, "format_info") <- list(style          = style,
                                      round.digits   = round.digits,
                                      plain.ascii    = plain.ascii,
                                      justify        = justify,
                                      headings       = headings,
                                      display.labels = display.labels,
                                      split.tables   = split.tables)
  
  if (nrow(x.df) == 0) {
    attr(output, "format_info") %+=% list(missing = "N/A")
  }
  
  attr(output, "user_fmt") <- list(... = ...)
  
  attr(output, "lang") <- st_options("lang")
  
  if (exists("ignored"))
    attr(output, "ignored") <- ignored
  
  return(output)
}
