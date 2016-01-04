descr <- function(x, na.rm=TRUE, style="simple", round.digits=2,
                  justify="right", plain.ascii=TRUE,
                  file=NA, append=FALSE, transpose=FALSE,
                  escape.pipe=FALSE, weights=NA,
                  rescale.weights=FALSE, ...) {

  if(is.atomic(x) && !is.numeric(x))
    stop("x is not numerical")

  # convert x to data.frame
  if(is.array(x) || is.atomic(x))
    x <- data.frame(x)

  if(!is.data.frame(x)) {
    stop("x must be a data frame or a single vector, and attempted conversion failed")
  }

  # Get into about x from parsing function
  var.info <- .parse.arg(as.character(match.call()[2]))

  # Identify and exclude non-numerical columns
  col.to.remove <- which(!sapply(x, is.numeric))

  if(length(col.to.remove) > 0) {
    message("Non-numerical variable(s) ignored: ",
            paste(colnames(x)[col.to.remove], collapse=", "))
    x <- x[-col.to.remove]
    var.info$var.names <- var.info$var.names[-col.to.remove]
  }

  if(ncol(x) == 0) {
    stop("no numerical variable(s) given as argument")
  }

  # Initialise output list and set its class/attributes
  output <- list()
  class(output) <- c("summarytools", class(output))
  attr(output, "st.type") <- "descr"
  attr(output, "date") <- Sys.Date()
  attr(output, "fn.call") <- as.character(match.call())
  attr(output, "var.info") <- c(Dataframe = ifelse("df.name" %in% names(var.info), var.info$df.name, NA),
                                Variable = ifelse("var.names" %in% names(var.info) && length(var.info$var.names) == 1,
                                                  var.info$var.names, NA),
                                Label = ifelse(length(Hmisc::label(x)) == 1 && Hmisc::label(x) != "",
                                               Hmisc::label(x), NA),
                                Subset = ifelse("rows.subset" %in% names(var.info), var.info$rows.subset, NA),
                                Weights = ifelse(!is.na(weights), deparse(substitute(weights)), NA))

  attr(output, "pander.args") <- list(style=style, round=round.digits, plain.ascii=plain.ascii,
                                      justify=justify, split.table=Inf, ...=...)

  if(is.na(weights)) {

    # Build skeleton (2 empty dataframes; one for stats and other to report valid vs na counts)
    output$stats <- data.frame(Mean=numeric(), Std.Dev=numeric(), Min=numeric(), Max=numeric(),
                               Median=numeric(), mad=numeric(), IQR=numeric(), CV=numeric(),
                               Skewness=numeric(), SE.Skewness=numeric(), Kurtosis=numeric())
    output$observ <- data.frame(Valid=numeric(), "<NA>"=numeric(), Total=numeric(),check.names = FALSE)

    # Iterate over columns in x
    for(i in seq_along(x)) {

      variable <- as.numeric(x[,i])

      # Extract number and proportion of missing and valid values
      n.valid <- sum(!is.na(variable))
      p.valid <- round(n.valid / length(variable) * 100, digits = round.digits)
      n.NA <- sum(is.na(variable))
      p.NA <- round(n.NA / length(variable) * 100, digits = round.digits)

      # Insert stats into output dataframe
      output$stats[i,] <- c(variable.mean <- mean(variable, na.rm=na.rm),
                            variable.sd <- sd(variable, na.rm=na.rm),
                            min(variable, na.rm=na.rm),
                            max(variable, na.rm=na.rm),
                            median(variable, na.rm=na.rm),
                            mad(variable, na.rm=na.rm),
                            IQR(variable, na.rm=na.rm),
                            variable.mean/variable.sd,
                            rapportools::skewness(variable, na.rm=na.rm),
                            sqrt((6*n.valid*(n.valid-1))/((n.valid-2)*(n.valid+1)*(n.valid+3))),
                            rapportools::kurtosis(variable, na.rm=na.rm))

    }
  }

  # Weights are used
  else {

    # Check that weights vector has the right length
    if(length(weights) != nrow(x))
      stop("weights vector must have the same length as x")

      # Build skeleton (2 empty dataframes; one for stats and other to report valid vs na counts)
      output$stats <- data.frame(Mean=numeric(), Std.Dev=numeric(), Min=numeric(), Max=numeric(),
                                 Median=numeric(), mad=numeric(), CV=numeric())
      output$observ <- data.frame(Valid=numeric(), "<NA>"=numeric(), Total=numeric(), check.names = FALSE)

      # Rescale weights if necessary
      if(rescale.weights)
        weights <- weights / sum(weights) * nrow(x)

      # Add weights to output object attributes
      attr(output, "weights") <- weights

      for(i in seq_along(x)) {
        variable <- as.numeric(x[,i])

        # Extract number and proportion of missing and valid values
        n.valid <- round(sum(weights[which(!is.na(variable))]), digits = round.digits)
        p.valid <- round(n.valid / sum(weights) * 100, digits = round.digits)
        n.NA <- round(sum(weights[which(is.na(variable))]), digits = round.digits)
        p.NA <- round(n.NA / sum(weights) * 100, digits = round.digits)

        # Remove missing values from variable and from corresponding weights
        ind <- which(!is.na(variable))
        variable <- variable[ind]
        weights <- weights[ind]

        # Fill in the output dataframe; here na.rm is redundant since na's have been removed
        # Calculate the weighted stats
        output$stats[i,] <- c(variable.mean <- matrixStats::weightedMean(variable, weights, refine = TRUE),
                              variable.sd <- matrixStats::weightedSd(variable, weights, refine = TRUE),
                              min(variable),
                              max(variable),
                              matrixStats::weightedMedian(variable, weights, refine = TRUE),
                              matrixStats::weightedMad(variable, weights, refine = TRUE),
                              variable.mean/variable.sd)

      }
  }

  for(i in seq_along(x)) {
    # Insert valid/missing info into output dataframe
    output$observ[i,] <- c(paste(n.valid, " (", p.valid, "%)",sep=""),
                           paste(n.NA, " (", p.NA, "%)",sep=""),
                           paste(n.valid + n.NA, "(100%)"))

    # Add row names (from col.names/var.name of the parse function)
    if("var.names" %in% names(var.info)) {
      rownames(output$stats)[i] <- var.info$var.names[i]
    } else {
      # this is necessary in order to support by()
      rownames(output$stats)[i] <- paste0("Var",i)
    }
    rownames(output$observ)[i] <- rownames(output$stats)[i]
  }

  # Transpose when transpose is FALSE; even though this is counter-intuitive,
  # we prefer that the "vertical" version be the default one and that at the
  # same time, the default value for transpose be FALSE.
  if(!transpose) {
    output$stats <- t(output$stats)
    output$observ <- t(output$observ)
  }

  if(!is.na(file)) {

    if(style=="grid" && escape.pipe) {
      output.esc.pipes <- paste(gsub(".\\|","\\\\|",capture.output(output)), collapse="\n")
      capture.output(cat(output.esc.pipes), file = file, append = append)
    } else if(grepl("\\.html$",file)) {
      if(isTRUE(append)) message("Append is not supported for html files. This parameter will be ignored")
      file.copy(from=print(output, method="browser",open=FALSE),to=normalizePath(file, mustWork = FALSE))
    } else {
      capture.output(output, file = file, append = append)
    }

    message("Output successfully written to file ", normalizePath(file))
    return(invisible(output))

  }

  return(output)
}
