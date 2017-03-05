descr <- function(x, stats = "all", na.rm = TRUE, round.digits = 2, style = "simple", 
                  justify = "right", plain.ascii = TRUE, transpose = FALSE, 
                  weights = NA, rescale.weights = FALSE, ...) {
  
  if (is.atomic(x) && !is.numeric(x))
    stop("x is not numerical")

  # When style='rmarkdown', make plain.ascii FALSE unless specified explicitly
  if (style=='rmarkdown' && plain.ascii==TRUE && (!"plain.ascii" %in% (names(match.call())))) {
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
  parse.info <- .parse_arg(sys.calls(), sys.frames(), match.call())
  
  # Identify and exclude non-numerical columns
  col.to.remove <- which(!sapply(x, is.numeric))

  if (length(col.to.remove) > 0) {
    ignored <- paste(colnames(x)[col.to.remove], collapse=", ")
    # message("Non-numerical variable(s) ignored: ", ignored)
    x <- x[-col.to.remove]
    parse.info$var.names <- parse.info$var.names[-col.to.remove]
  }

  if (ncol(x) == 0) {
    stop("no numerical variable(s) given as argument")
  }

  # Initialise output list and set its class/attributes
  output <- list()
  class(output) <- c("summarytools", class(output))
  attr(output, "st.type") <- "descr"
  attr(output, "date") <- Sys.Date()
  attr(output, "fn.call") <- as.character(match.call())
  #attr(output, "Group") <- ifelse("by.group" %in% names(parse.info), parse.info$by.group, NA)
  attr(output, "var.info") <- c(Dataframe = ifelse("df.name" %in% names(parse.info), parse.info$df.name, NA),
                                Variable = ifelse("var.names" %in% names(parse.info) && length(parse.info$var.names) == 1,
                                                  parse.info$var.names, NA),
                                Label = ifelse(length(Hmisc::label(x)) == 1 && Hmisc::label(x) != "",
                                               Hmisc::label(x), NA),
                                Subset = ifelse("rows.subset" %in% names(parse.info), parse.info$rows.subset, NA),
                                Weights = substitute(weights),
                                Group = ifelse("by.group" %in% names(parse.info), parse.info$by.group, NA))

  attr(output, "pander.args") <- list(style = style, 
                                      round = round.digits, 
                                      digits = 6,
                                      plain.ascii = plain.ascii,
                                      justify = justify, 
                                      split.table = Inf,
                                      keep.trailing.zeros = TRUE,
                                      ... = ...)
  
  if (exists("ignored"))
    attr(output, "ignored") <- ignored
  
  if (identical(weights, NA)) {

    # Build skeleton (2 empty dataframes; one for stats and other to report valid vs na counts)
    output$stats <- data.frame(Mean=numeric(), Std.Dev=numeric(), Min=numeric(), Median=numeric(), 
                               Max=numeric(), MAD=numeric(), IQR=numeric(), CV=numeric(),
                               Skewness=numeric(), SE.Skewness=numeric(), Kurtosis=numeric())
    output$observ <- data.frame(Valid=numeric(), "<NA>"=numeric(), Total=numeric(), check.names = FALSE)
    output$observ.pct <- data.frame(Valid=numeric(), "<NA>"=numeric(), Total=numeric(), check.names = FALSE)

    # Iterate over columns in x
    for(i in seq_along(x)) {

      variable <- as.numeric(x[,i])

      # Extract number and proportion of missing and valid values
      n.valid <- sum(!is.na(variable))
      p.valid <- n.valid / length(variable)
      n.NA <- sum(is.na(variable))
      p.NA <- n.NA / length(variable)

      # Insert stats into output dataframe
      output$stats[i,] <- c(variable.mean <- mean(variable, na.rm=na.rm),
                            variable.sd <- sd(variable, na.rm=na.rm),
                            min(variable, na.rm=na.rm),
                            median(variable, na.rm=na.rm),
                            max(variable, na.rm=na.rm),
                            mad(variable, na.rm=na.rm),
                            IQR(variable, na.rm=na.rm),
                            variable.mean/variable.sd,
                            rapportools::skewness(variable, na.rm=na.rm),
                            sqrt((6*n.valid*(n.valid-1))/((n.valid-2)*(n.valid+1)*(n.valid+3))),
                            rapportools::kurtosis(variable, na.rm=na.rm))
      
      # Insert valid/missing info into output dataframe
      output$observ[i,] <- c(n.valid, n.NA, n.valid + n.NA)
      output$observ.pct[i,] <- c(p.valid, p.NA, p.valid + p.NA)
    }
  }

  # Weights are used
  else {

    # Check that weights vector has the right length
    if (length(weights) != nrow(x))
      stop("weights vector must have the same length as x")
    
    if (sum(is.na(weights)) > 0) {
      warning("Missing values on weight variable have been detected and were treated as zeroes.")
      weights[is.na(weights)] <- 0
    }

    # Build skeleton (2 empty dataframes; one for stats and other to report valid vs na counts)
    output$stats <- data.frame(Mean=numeric(), Std.Dev=numeric(), Min=numeric(), Median=numeric(), 
                               Max=numeric(),  MAD=numeric(), CV=numeric())
    output$observ <- data.frame(Valid=numeric(), "<NA>"=numeric(), Total=numeric(), check.names = FALSE)
    output$observ.pct <- data.frame(Valid=numeric(), "<NA>"=numeric(), Total=numeric(), check.names = FALSE)
    
    # Rescale weights if necessary
    if (rescale.weights)
      weights <- weights / sum(weights) * nrow(x)
    
    # Add weights to output object attributes
    # attr(output, "weights") <- weights
    
    for(i in seq_along(x)) {
      variable <- as.numeric(x[,i])
      
      # Extract number and proportion of missing and valid values
      n.valid <- sum(weights[which(!is.na(variable))])
      p.valid <- n.valid / sum(weights)
      n.NA <- sum(weights[which(is.na(variable))])
      p.NA <- n.NA / sum(weights)
      
      # Remove missing values from variable and from corresponding weights
      ind <- which(!is.na(variable))
      variable <- variable[ind]
      weights <- weights[ind]
      
      # Fill in the output dataframe; here na.rm is redundant since na's have been removed
      # Calculate the weighted stats
      output$stats[i,] <- c(variable.mean <- matrixStats::weightedMean(variable, weights, refine = TRUE),
                            variable.sd <- matrixStats::weightedSd(variable, weights, refine = TRUE),
                            min(variable),
                            matrixStats::weightedMedian(variable, weights, refine = TRUE),
                            max(variable),
                            matrixStats::weightedMad(variable, weights, refine = TRUE),
                            variable.mean/variable.sd)
      
      # Insert valid/missing info into output dataframe
      output$observ[i,] <- c(n.valid, n.NA, n.valid + n.NA)
      output$observ.pct[i,] <- c(p.valid, p.NA, p.valid + p.NA)
    }
  }
  
  for(i in seq_along(x)) {
    
    # Add row names (from col.names/var.name of the parse function)
    if ("var.names" %in% names(parse.info)) {
      rownames(output$stats)[i] <- parse.info$var.names[i]
    } else {
      # this is necessary in order to support by()
      rownames(output$stats)[i] <- paste0("Var",i)
    }
    rownames(output$observ)[i] <- rownames(output$stats)[i]
    rownames(output$observ.pct)[i] <- rownames(output$stats)[i]
  }

  # Remove unwanted stats
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
    output$observ.pct <- t(output$observ.pct)
  }

  # Change <NA> for \<NA\> in markdown tables
  if (style=="rmarkdown" && !plain.ascii && !transpose) {
    rownames(output$observ)[2] <- "\\<NA\\>"
  } else if (style=="rmarkdown" && !plain.ascii && transpose){
    colnames(output$observ)[2] <- "\\<NA\\>"
  }
  
  return(output)
}
