descr <- function(x, na.rm=TRUE, style="simple", round.digits=2,
                  justify="right", plain.ascii=TRUE, file=NA,
                  append=FALSE, transpose=FALSE, ...) {

  if(is.atomic(x) && !is.numeric(x))
    stop("x is not numerical")

  if(is.array(x)) {
    x <- data.frame(x)
    notes <- paste(substitute(x), "was converted to data.frame")
  } else if(is.atomic(x)) {
    x <- data.frame(x)
  } else {
    notes <- character()
  }

  if(!is.data.frame(x)) {
    stop("x must be a data frame or a single vector, and attempted conversion failed")
  }

  # Initialise output list and set the class/attributes
  output <- list()

  class(output) <- c("summarytools", class(output))
  attr(output, "st.type") <- "descr"

  # Get dataframe name and subset from parsing function
  tmp.attr <- .parse.arg(as.character(match.call()[2]))
  attr(output, "df.name") <- tmp.attr$df.name
  attr(output, "rows.subset") <- tmp.attr$rows.subset
  attr(output, "var.name") <- tmp.attr$var.name

  # Identify and exclude non-numerical columns
  to.be.removed <- numeric()
  for(i in seq_along(x)) {
    if(!is.numeric(x[[i]]))
      to.be.removed <- append(to.be.removed, i)
  }

  if(length(to.be.removed)>0) {
    notes <- append(notes, paste("Non-numerical variable(s) ignored:",
                                 paste(colnames(x)[to.be.removed], collapse=", ")))
    x <- x[-to.be.removed]
    rm(to.be.removed)
  }


  if(ncol(x)==0) {
    stop("No numerical variable were given as arguments.")
  }

  attr(output, "col.names") <- colnames(x)


  # Add label to attributes if present
  if("label" %in% names(attributes(x)))
    attr(stats, "var.label") <- rapportools::label(x)

  attr(output, "date") <- Sys.Date()
  attr(output, "pander.args") <- list(style=style, round=round.digits, plain.ascii=plain.ascii,
                                      justify=justify, split.table=Inf, ...=...)


  # Build skeleton (2 empty dataframes; one for stats and other to report valid vs na counts)
  output$stats <- data.frame(Mean=numeric(), Std.Dev=numeric(), Min=numeric(), Max=numeric(),
                             Median=numeric(), mad=numeric(), IQR=numeric(), CV=numeric(),
                             Skewness=numeric(), SE.Skewness=numeric(), Kurtosis=numeric())
  output$observ <- data.frame(Valid=numeric(), "<NA>"=numeric(), Total=numeric(),check.names = FALSE)


  # Iterate over columns in x
  i <- 1
  for(variable in x) {

    # Remove variable labels to prevent potential problems
    if("label" %in% names(attributes(x))) {
      class(x) <- setdiff(class(variable),"labelled")
      attr(x,"label") <- NULL
    }

    # Calculate the actual stats
    variable <- variable * 1.0 # prevents potential problems
    n <- sum(!is.na(variable))

    # Insert stats into stats table
    output$stats[i,] <- c(x.mean<-mean(variable,na.rm=na.rm),
                          x.sd<-sd(variable,na.rm=na.rm),
                          min(variable,na.rm=na.rm),
                          max(variable,na.rm=na.rm),
                          median(variable,na.rm=na.rm),
                          mad(variable,na.rm=na.rm),
                          IQR(variable,na.rm=na.rm),
                          x.mean/x.sd,
                          rapportools::skewness(variable,na.rm=na.rm),
                          sqrt((6*n*(n-1))/((n-2)*(n+1)*(n+3))),
                          rapportools::kurtosis(variable,na.rm=na.rm))

    # Insert proper row names (from col.names/var.name of the parse function)
    rownames(output$stats)[i] <-
      ifelse(!is.null(attr(output,"var.name")[i]), attr(output,"var.name")[i],
             attr(output,"col.names")[i])

    # Report number of missing vs valid data
    n.valid <- sum(!is.na(variable))
    p.valid <- round(n.valid / length(variable) * 100, digits = round.digits)
    n.NA <- sum(is.na(variable))
    p.NA <- round(n.NA / length(variable) * 100, digits = round.digits)

    # Insert into observ table
    output$observ[i,] <- c(paste(n.valid, " (", p.valid, "%)",sep=""),
                           paste(n.NA, " (", p.NA, "%)",sep=""),
                           length(variable))

    rownames(output$observ)[i] <-
      ifelse(!is.null(attr(output,"var.name")[i]), attr(output,"var.name")[i],
             attr(output,"col.names")[i])
    i <- i+1
  }

  # Transpose when transpose is FALSE; even though this is counter-intuitive,
  # we prefer that the transposed version be the default one and that at the
  # same time, the default value for transpose be FALSE.
  if(!transpose) {
    output$stats <- t(output$stats)
    output$observ <- t(output$observ)
  }

  if(!is.na(file)) {
    capture.output(output, file = file, append = append)
    message("Output successfully written to file ", normalizePath(file))
  }

  if(exists("notes") && length(notes) > 0) {
    attr(output, "notes") <- paste(notes)
  }

  return(output)
}

