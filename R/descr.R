descr <- function(x, stats = "all", na.rm = TRUE, round.digits = 2, style = "simple",
                  justify = "right", plain.ascii = TRUE, transpose = FALSE,
                  weights = NA, rescale.weights = FALSE, ...) {

  if (is.atomic(x) && !is.numeric(x))
    stop("x is not numerical")

  # When style='rmarkdown', make plain.ascii FALSE unless specified explicitly
  if (style=="rmarkdown" && plain.ascii==TRUE && (!"plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }

  if ("file" %in% names(match.call()))
    message("file argument is deprecated; use print() or view() function to generate files")

  # convert x to data.frame (useful is objet is a tibble or data.table)
  x <- as.data.frame(x)

  if (!is.data.frame(x)) {
    stop("x must be a data.frame, a tibble, a data.table or a single vector, and attempted conversion failed")
  }

  # Get into about x from parsing function
  parse_info <- .parse_arg(sys.calls(), sys.frames(), match.call())

  # Identify and exclude non-numerical columns
  col_to_remove <- which(!sapply(x, is.numeric))

  if (length(col_to_remove) > 0) {
    ignored <- paste(colnames(x)[col_to_remove], collapse=", ")
    # message("Non-numerical variable(s) ignored: ", ignored)
    x <- x[-col_to_remove]
    parse_info$var_names <- parse_info$var_names[-col_to_remove]
  }

  if (ncol(x) == 0) {
    stop("no numerical variable(s) given as argument")
  }

  # Initialise output list
  output <- list()

  if (identical(weights, NA)) {

    # Build skeleton (2 empty dataframes; one for stats and other to report valid vs na counts)
    output$stats <- data.frame(Mean = numeric(),
                               Std.Dev = numeric(),
                               Min = numeric(),
                               Median = numeric(),
                               Max = numeric(),
                               MAD = numeric(),
                               IQR = numeric(),
                               CV = numeric(),
                               Skewness = numeric(),
                               SE.Skewness = numeric(),
                               Kurtosis = numeric())
    output$observ <- data.frame(Valid = numeric(),
                                "<NA>" = numeric(),
                                Total = numeric(),
                                check.names = FALSE)
    output$observ_pct <- data.frame(Valid = numeric(),
                                    "<NA>" = numeric(),
                                    Total = numeric(),
                                    check.names = FALSE)

    # Iterate over columns in x
    for(i in seq_along(x)) {

      variable <- as.numeric(x[,i])

      # Extract number and proportion of missing and valid values
      n_valid <- sum(!is.na(variable))
      p_valid <- n_valid / length(variable)
      n_NA <- sum(is.na(variable))
      p_NA <- n_NA / length(variable)

      # Insert stats into output dataframe
      output$stats[i,] <- c(variable.mean <- mean(variable, na.rm=na.rm),
                            variable.sd <- sd(variable, na.rm=na.rm),
                            min(variable, na.rm=na.rm),
                            median(variable, na.rm=na.rm),
                            max(variable, na.rm=na.rm),
                            mad(variable, na.rm=na.rm),
                            IQR(variable, na.rm=na.rm),
                            variable.mean / variable.sd,
                            rapportools::skewness(variable, na.rm=na.rm),
                            sqrt((6*n_valid*(n_valid-1))/((n_valid-2)*(n_valid+1)*(n_valid+3))),
                            rapportools::kurtosis(variable, na.rm=na.rm))

      # Insert valid/missing info into output dataframe
      output$observ[i,] <- c(n_valid, n_NA, n_valid + n_NA)
      output$observ_pct[i,] <- c(p_valid, p_NA, p_valid + p_NA)
    }
  }

  # Weights are used
  else {

    # Check that weights vector has the right length
    if (length(weights) != nrow(x))
      stop("weights vector must have the same length as x")

    # use a copy of weights to be able to use substitute(weights) later on
    wgts <- weights

    if (sum(is.na(wgts)) > 0) {
      warning("Missing values on weight variable have been detected and were treated as zeroes.")
      wgts[is.na(wgts)] <- 0
    }

    # Build skeleton (2 empty dataframes; one for stats and other to report valid vs na counts)
    output$stats <- data.frame(Mean = numeric(),
                               Std.Dev = numeric(),
                               Min = numeric(),
                               Median = numeric(),
                               Max = numeric(),
                               MAD = numeric(),
                               CV = numeric())
    output$observ <- data.frame(Valid = numeric(),
                                "<NA>" = numeric(),
                                Total = numeric(),
                                check.names = FALSE)
    output$observ_pct <- data.frame(Valid = numeric(),
                                    "<NA>" = numeric(),
                                    Total = numeric(),
                                    check.names = FALSE)

    # Rescale weights if necessary
    if (rescale.weights)
      wgts <- wgts / sum(wgts) * nrow(x)

    for(i in seq_along(x)) {
      variable <- as.numeric(x[,i])

      # Extract number and proportion of missing and valid values
      n_valid <- sum(wgts[which(!is.na(variable))])
      p_valid <- n_valid / sum(wgts)
      n_NA <- sum(wgts[which(is.na(variable))])
      p_NA <- n_NA / sum(wgts)

      # Remove missing values from variable and from corresponding weights
      ind <- which(!is.na(variable))
      variable <- variable[ind]
      wgts <- wgts[ind]

      # Fill in the output dataframe; here na.rm is redundant since na's have been removed
      # Calculate the weighted stats
      output$stats[i,] <- c(variable.mean <- matrixStats::weightedMean(variable, wgts, refine = TRUE),
                            variable.sd <- matrixStats::weightedSd(variable, wgts, refine = TRUE),
                            min(variable),
                            matrixStats::weightedMedian(variable, wgts, refine = TRUE),
                            max(variable),
                            matrixStats::weightedMad(variable, wgts, refine = TRUE),
                            variable.mean/variable.sd)

      # Insert valid/missing info into output dataframe
      output$observ[i,] <- c(n_valid, n_NA, n_valid + n_NA)
      output$observ_pct[i,] <- c(p_valid, p_NA, p_valid + p_NA)
    }
  }

  for(i in seq_along(x)) {

    # Add row names (from col.names/var.name of the parse function)
    if ("var_names" %in% names(parse_info)) {
      rownames(output$stats)[i] <- parse_info$var_names[i]
    } else {
      # this is necessary in order to support by()
      rownames(output$stats)[i] <- paste0("Var",i)
    }
    rownames(output$observ)[i] <- rownames(output$stats)[i]
    rownames(output$observ_pct)[i] <- rownames(output$stats)[i]
  }

  # Filter statistics according to stats argument
  if (!identical(stats,"all")) {
    # Check that 'stats' argument has only valid stats names
    wrong.stats <- setdiff(stats, colnames(output$stats))
    if (length(wrong.stats) > 0)
      stop("allowed 'stats' are: ", paste(colnames(output$stats), collapse = ", "))
    output$stats <- output$stats[,stats]
  }

  # Transpose when transpose is FALSE; even though this is counter-intuitive,
  # we prefer that the "vertical" version be the default one and that at the
  # same time, the default value for transpose be FALSE.
  if (!transpose) {
    output$stats <- t(output$stats)
    output$observ <- t(output$observ)
    output$observ_pct <- t(output$observ_pct)
  }

  # Change <NA> for \<NA\> in markdown tables
  if (style=="rmarkdown" && !plain.ascii && !transpose) {
    rownames(output$observ)[2] <- "\\<NA\\>"
  } else if (style=="rmarkdown" && !plain.ascii && transpose){
    colnames(output$observ)[2] <- "\\<NA\\>"
  }

  # Set class/attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "descr"
  attr(output, "date") <- Sys.Date()
  attr(output, "fn_call") <- as.character(match.call())
  attr(output, "var_info") <- c(Dataframe = ifelse("df_name" %in% names(parse_info), parse_info$df_name, NA),
                                Variable = ifelse("var_names" %in% names(parse_info) && length(parse_info$var_names) == 1,
                                                  parse_info$var_names, NA),
                                Label = ifelse(length(Hmisc::label(x)) == 1 && Hmisc::label(x) != "",
                                               Hmisc::label(x), NA),
                                Subset = ifelse("rows_subset" %in% names(parse_info), parse_info$rows_subset, NA),
                                Weights = substitute(weights),
                                Group = ifelse("by_group" %in% names(parse_info), parse_info$by_group, NA))

  # For future use
  if ("by_group" %in% names(parse_info)) {
    attr(output, "by_first") <- parse_info$by_first
    attr(output, "by_last") <- parse_info$by_last
  }

  attr(output, "pander_args") <- list(style = style,
                                      round = round.digits,
                                      digits = 6,
                                      plain.ascii = plain.ascii,
                                      justify = justify,
                                      split.table = Inf,
                                      keep.trailing.zeros = TRUE,
                                      ... = ...)

  if (exists("ignored"))
    attr(output, "ignored") <- ignored

  return(output)
}
