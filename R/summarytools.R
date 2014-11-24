freq <- function(x, round.digits=2, echo=TRUE, style="simple", justify="right",
                 plain.ascii=TRUE, file=NA, append=FALSE, ...) {

  if(!is.factor(x) && !is.vector(x)) {
    stop("argument must be a vector or a factor; for dataframes, use lapply(x,freq)")
  }

  if(echo && !("pander" %in% rownames(installed.packages()))) {
    stop("with echo = TRUE, package pander must be installed")
  }

  # Make sure no sink() is left open (in case of an error occuring)
  on.exit(expr = if(sink.number()) sink())

  # Replace NaN's by NA's
  # This simplifies matters a lot
  if(NaN %in% x)  {
    message("vector or factor contains ", sum(is.nan(x)), " NaN values -- converting to NA\n")
    msg.nan <- paste("Note:", sum(is.nan(x)), "NaN value(s) converted to NA\n")
    x[is.nan(x)] <- NA
  }

  # create a basic frequency table, including NA row
  freq.table <- table(x, useNA = "always")

  # Change the name of the NA item to avoid potential problems when echoing to console
  names(freq.table)[length(freq.table)] <- '<NA>'

  # calculate proportions (valid)
  P.valid <- prop.table(table(x, useNA="no"))*100

  # Add '<NA>' item to the proportions vector to make the vector
  # the proper length when cbind'ing later on
  P.valid['<NA>'] <- NA

  # Calculate cumulative proportions
  P.valid.cum <- cumsum(P.valid)
  P.valid.cum['<NA>'] <- NA

  # calculate proportions (total)
  P.tot <- prop.table(table(x, useNA="always"))*100
  P.tot.cum <- cumsum(P.tot)

  # Build the actual frequency table
  output <- cbind(freq.table, P.valid, P.valid.cum, P.tot, P.tot.cum)
  output <- rbind(output, c(length(x), rep(100,4)))
  output <- round(output, round.digits)
  colnames(output) <- c("N","%Valid","%Cum.Valid","%Total","%Cum.Total")
  rownames(output) <- c(names(freq.table),"Total")

  # Write to file when file argument is supplied
  if(!is.na(file)) {

    sink(file = file, append = append)
    cat("\nVariable name: ", as.character(match.call()[2]))
    if("label" %in% names(attributes(x)))
      cat("\nVariable label:", rapportools::label(x))
    cat("\n\nFrequencies")
    pander::pander(output, split.table=Inf,
                   style=style, plain.ascii=plain.ascii, justify=justify, ...)
    if(exists("msg.nan"))
      cat(msg.nan)
    sink()
    cat("Output successfully written to ", normalizePath(file))
    return(invisible(output))

  } else if(echo) {

    cat("\nVariable name: ", as.character(match.call()[2]))
    if("label" %in% names(attributes(x)))
      cat("\nVariable label:", rapportools::label(x))
    cat("\n\nFrequencies")
    pander::pander(output, split.table=Inf,
                   style=style, plain.ascii=plain.ascii, justify=justify, ...)
    if(exists("msg.nan"))
      cat(msg.nan)

    return(invisible(output))

  }

  # When echo=FALSE and file=NA, the returned output is made visible
  return(output)

}


desc <- function(x, na.rm=TRUE, round.digits=2, echo=TRUE, transpose=FALSE,
                 style="simple", justify="right", plain.ascii=TRUE, file=NA,
                 append=FALSE, ...) {

  if( (echo||file) && !("pander" %in% rownames(installed.packages()))) {
    stop("to write to file or with echo=TRUE, package pander must be installed")
  }

  # Make sure sink() is terminated (in case of an error occuring)
  on.exit(expr = if(sink.number()) sink())

  # Make sure x is of type "list" to allow proper iteration
  if(is.atomic(x))
    x <- list(x)

  # recup the argument
  arg.str <- as.character(match.call()[2])

  # According to type of argument, get the variable names
  if(length(grep("\\$",arg.str))==1) {
    varnames <- arg.str
  } else if(length(grep("dd\\[x", arg.str))==1) {
    varnames <- arg.str
  } else if(length(grep("\\[", arg.str))==1) {
    varnames <- names(eval(parse(text=arg.str)))
  } else if(length(grep("c\\(", arg.str))==1) {
    varnames <- ""
  } else {
    varnames <- names(get(arg.str))
  }

  # Build skeleton (2 empty dataframes; one for stats and other to report valid vs na counts)
  stats <- data.frame(Mean=numeric(), Std.Dev=numeric(), Min=numeric(), Max=numeric(),
                      Median=numeric(), MAD=numeric(), IQR=numeric(), CV=numeric(), Skewness=numeric(),
                      SE.Skewness=numeric(), Kurtosis=numeric())
  observ <- data.frame(Valid=numeric(), "<NA>"=numeric(), Total=numeric(),check.names = FALSE)

  # Iterate over variables present in x
  i <- 1
  for(variable in x) {

    if(!is.numeric(variable)) {
      varnames <- varnames[-i]
      next()
    }

    # Remove variable label to avoid potential problems
    if("label" %in% names(attributes(x))) {
      # remove Hmisc's "labelled" class if present
      class(variable) <- setdiff(class(variable),"labelled")
      attr(variable,"label") <- NULL
    }

    variable <- variable * 1.0 # prevents potential problems

    n <- sum(!is.na(variable))

    stats[i,] <- round(c(x.mean<-mean(variable,na.rm=na.rm),
                         x.sd<-sd(variable,na.rm=na.rm),
                         min(variable,na.rm=na.rm),
                         max(variable,na.rm=na.rm),
                         median(variable,na.rm=na.rm),
                         mad(variable,na.rm=na.rm),
                         IQR(variable,na.rm=na.rm),
                         x.mean/x.sd,
                         rapportools::skewness(variable,na.rm=na.rm),
                         sqrt((6*n*(n-1))/((n-2)*(n+1)*(n+3))),
                         rapportools::kurtosis(variable,na.rm=na.rm)),digits=round.digits)

    # Insert stats into stats table
    rownames(stats)[i] <- varnames[i]

    # Report number of missing vs valid data
    n.valid <- sum(!is.na(variable))
    p.valid <- round(n.valid / length(variable) * 100, digits = round.digits)
    n.NA <- sum(is.na(variable))
    p.NA <- round(n.NA / length(variable) * 100, digits = round.digits)

    # Insert into observ table
    observ[i,] <- c(paste(n.valid, " (", p.valid, "%)",sep=""),
                    paste(n.NA, " (", p.NA, "%)",sep=""),
                    length(variable))

    rownames(observ)[i] <- varnames[i]
    i <- i+1
  }

  if(nrow(stats)==0) {
    warning("No numerical variable were given as arguments. Returning NA")
    return(NA)
  }

  if(transpose) {
    stats <- t(stats)
    observ <- t(observ)
  }

  if(!is.na(file)) {

    sink(file = file, append = append)

    cat("Univariate Statistics (summarytools::desc)\n")
    cat("Written", as.character(Sys.time()))

    pander::pander(stats, style=style, plain.ascii=plain.ascii, justify=justify,
                   split.table=Inf, ...)

    cat("\nObservations")
    pander::pander(observ, style=style, plain.ascii=plain.ascii, justify=justify,
                   split.table=Inf, ...)
    sink()

    cat(paste("Output successfully written to", normalizePath(file)))

    return(invisible(list(stats,observ)))

  } else if(echo) {

    cat("\nUnivariate Statistics")
    pander::pander(stats, style=style, plain.ascii=plain.ascii, justify=justify,
                   split.table=Inf, ...)
    cat("\nObservations")
    pander::pander(observ, style=style, plain.ascii=plain.ascii, justify=justify,
                   split.table=Inf, ...)
    return(invisible(list(stats,observ)))

  } else {

    return(list(stats=stats, observ=observ))

  }

}


dfSummary <- function(x, echo=TRUE, style="grid", justify="left",
                      max.distinct.values=10, str.distinct.values="distinct values", trim.strings=FALSE,
                      max.string.width=15, round.digits=2, file=NA, display.labels=FALSE, ...) {

  if(!is.data.frame(x))
    stop("x must be a dataframe")

  # set the stringsAsFactors to FALSE to prevent problems
  op <- options("stringsAsFactors")
  options(stringsAsFactors=FALSE)
  on.exit(options(op))


  if((!is.na(file)|!is.na(echo)) && !"pander" %in% rownames(installed.packages()))
    stop("to display pander tables or write output to disk, package pander must first be installed")

  if(display.labels && !("rapportools" %in% rownames(installed.packages())))
    stop("to display labels, package rapportools must first be installed")

  # create an output dataframe
  output <- data.frame(num=numeric(),
                       variable.name=character(),
                       label=character(),
                       properties=character(),
                       factor.values.or.stats=character(),
                       frequencies=character(),
                       n.valid=numeric())

  # iterate over dataframe columns
  for(i in seq_len(ncol(x))) {

    # extract data
    column.data <- x[[i]]

    # Add column number to output dataframe
    output[i,1] <- i

    # Add column name
    output[i,2] <- names(x)[i]

    # Add column label (if applicable)
    if(display.labels)
      output[i,3] <- rapportools::label(x[i])

    # Add variable properties (typeof, class)
    output[i,4] <- paste("type:",typeof(column.data),
                         "\nclass:",paste(class(column.data),collapse="\n + "),sep="")

    # For factors, display a column of levels and a column of frequencies
    if(is.factor(column.data)) {
      n <- nlevels(column.data)
      if(n <= max.distinct.values) {
        output[i,5] <- paste(1:n,". ", levels(column.data), collapse="\n", sep="")

        fr <- table(column.data,useNA="no") # raw freqs
        pct <- round(prop.table(fr)*100,1) # percentage
        names(fr) <- 1:nlevels(column.data)
        output[i,6] <- paste(names(fr),": ", fr," (",pct,"%)",sep="",collapse="\n")
      } else {
        output[i,5] <- paste(1:max.distinct.values,". ", levels(column.data)[1:max.distinct.values], collapse="\n", sep="")
        output[i,5] <- paste(output[i,5], paste(n - max.distinct.values, "Other levels (not displayed)"),sep="\n")
        output[i,6] <- paste(as.character(length(unique(column.data))),str.distinct.values)
      }
    }

    # For numeric data, display a column of univariate stats and a column of frequencies
    else if(is.numeric(column.data)) {
      output[i,5] <- paste("avg (sd) = ",round(mean(column.data,na.rm=TRUE),round.digits),
                           " (",round(sd(column.data,na.rm=TRUE),round.digits), ")\n",
                           "min < med < max = ", round(min(column.data,na.rm=TRUE),round.digits),
                           " < ", round(median(column.data,na.rm=TRUE),round.digits),
                           " < ", round(max(column.data,na.rm=TRUE),round.digits),"\n",
                           "IQR (CV) = ", round(IQR(column.data,na.rm=TRUE),round.digits),
                           " (", round(sd(column.data,na.rm=TRUE)/mean(column.data,na.rm=TRUE),round.digits),
                           ")", collapse="",sep="")

      if(length(unique(column.data)) <= max.distinct.values) {
        fr <- table(column.data,useNA="no") # raw freqs
        pct <- round(prop.table(fr)*100,1)
        output[i,6] <- paste(round(as.numeric(names(fr)),round.digits),": ", fr," (",pct,"%)",sep="",collapse="\n")
      }
      else {
        output[i,6] <- paste(as.character(length(unique(column.data))),str.distinct.values)
      }
    }

    # For text data, skip a column and display a column of frequencies
    else if(is.character(column.data)) {
      output[i,5] <- ""

      if(trim.strings)
        column.data.tmp <- sub(pattern="\\A\\s*(.+?)\\s*\\z",replacement="\\1",x=column.data,perl=TRUE)
      else
        column.data.tmp <- column.data

      if(length(unique(column.data.tmp)) <= max.distinct.values) {
        fr <- table(column.data.tmp,useNA="no")
        pct <- round(prop.table(fr)*100,1)
        output[i,6] <- paste(substr(names(fr),1,max.string.width),": ",
                             fr," (",pct,"%)",sep="",collapse="\n")
      }
      else output[i,6] <- paste(as.character(length(unique(column.data.tmp))),str.distinct.values)
    }

    # For data that does not fit in previous categories (numeric, character, factor)
    else {
      output[i,5] <- ""
      if(length(unique(column.data)) <= max.distinct.values) {
        fr <- table(column.data,useNA="no")
        pct <- round(prop.table(fr)*100,1)
        output[i,6] <- paste(substr(names(fr),1,max.string.width),": ",
                             fr," (",pct,"%)",sep="",collapse="\n")
      }
      else output[i,6] <- paste(as.character(length(unique(column.data))),str.distinct.values)
    }

    # Add missing data info
    n.nas <- sum(is.na(column.data))
    n.val <- nrow(x) - n.nas

    output[i,7] <- paste(n.val," (",format(n.val/nrow(x)*100,digits=1,nsmall=1),"%)",collapse="",sep="")
  }

  if(!display.labels)
    output$label <- NULL

  if(!is.na(file)) {
    capture.output(pander::pander(output, split.table=Inf, split.cells=Inf,
                                  keep.line.breaks=TRUE, style=style, justify=justify, ...),
                   file = file)
  }

  if(echo) {
    pander::pander(output, split.table=Inf, split.cells=Inf, keep.line.breaks=TRUE,
                   style=style, justify=justify, ...)
  }

  if(echo || !is.na(file))
    return(invisible(output))
  else
    return(output)
}

prop <- function(x, echo=TRUE, style="grid", justify="left",
                       plain.ascii=TRUE, display.labels=FALSE, ...) {

  if(!is.data.frame(x)) {
    stop("x is not a dataframe. Try properties(as.data.frame(x)).")
  }
  if(display.labels && !("rapportools" %in% rownames(installed.packages())))
    stop("to display labels, package rapportools must be installed")

  # declare a function that avoids some redundancy
  bind.names <- function(item) {
    if(!is.null(names(item)))
       return(paste(names(item),sQuote(item), sep=" = ", collapse=",\n"))
       else
         return(paste(sQuote(item), collapse=", "))
  }

  # set the stringsAsFactors to FALSE to prevent problems
  op <- options("stringsAsFactors")
  options(stringsAsFactors=FALSE)
  on.exit(options(op))

  # extract dataframe attributes
  x.attr <- attributes(x)

  df.attributes <- character(0)

  # iterate over every root-level item
  for(i in seq(along.with=x.attr)) {

    if(is.atomic(x.attr[[i]]))
      df.attributes[labels(x.attr)[[i]]] <- bind.names(x.attr[[i]])

    # iterate over item when not atomic
    else {
      for(j in seq(along.with=x.attr[[i]])) {

        # when it's not a new list, output it
        if(is.atomic(x.attr[[i]][[j]]))
          df.attributes[paste(labels(x.attr)[[i]],
                              labels(x.attr[[i]])[[j]],
                              sep=" | ")] <- bind.names(x.attr[[i]][[j]])

        # and when it's another list, get at its elements
        else {
          for(k in seq(along.with=x.attr[[i]][[j]])) {
            if(!is.atomic(x.attr[[i]][[j]][[k]])) {
              stop("dataframe attributes has over 3 levels of nested lists. Set df.attributes to FALSE to get variable attributes.")
            }
            df.attributes[paste(labels(x.attr)[[i]],
                                labels(x.attr[[i]])[[j]],
                                labels(x.attr[[i]][[j]])[[k]],
                                sep=" | ")] <- bind.names(x.attr[[i]][[j]][[k]])
          }
        }
      }
    }
  }

  # check that row.names are not insanely long
  if(nchar(df.attributes["row.names"]) > 500)
    df.attributes["row.names"] <- paste(substr(df.attributes["row.names"],start = 1,stop = 500),
                                        "\n ... [ truncated at 500 characters ]")

  df.attributes <- data.frame(value=df.attributes)

  # extract Variable attributes
  n.cols <- ncol(x)
  var.attributes <- data.frame(name=character(n.cols),label=character(n.cols),
                               type=character(n.cols),class=character(n.cols),
                               other.attributes=character(n.cols),
                               first.obs=character(n.cols))

  # Add items that don't need iteration ("for" loop)
  var.attributes$name <- names(x)
  var.attributes$first.obs <- as.vector(t(x[1,]))

  if(display.labels) {
    var.attributes$label <- rapportools::label(x)
  } else {
    var.attributes$label <- NULL
  }

  # Iterate over columns in dataframe for other items
  for(i in 1:n.cols) {
    var.attributes$type[i] <- typeof(x[[i]])
    var.attributes$class[i] <- paste(class(x[[i]]),collapse="\n",sep="")

    # additional attributes (tricky part)
    tmp.attributes <- attributes(x[[i]])
    # tmp.attributes[["class"]] <- NULL # remove class to avoid redundancy with previous column

    # loop over attributes so that names items can be extracted properly

    for(j in seq(along.with=tmp.attributes)) {
      var.attributes$other.attributes[i] <- paste(var.attributes$other.attributes[i],
                                                  paste(paste("$",names(tmp.attributes[j]), sep=""),
                                                        bind.names(tmp.attributes[[j]]),
                                                        sep="\n", collapse="\n"),
                                                  sep="\n")
      # little hack to clean up line breaks
      var.attributes$other.attributes[i] <- sub("\\A\\n","",var.attributes$other.attributes[i],perl=TRUE)
      var.attributes$other.attributes[i] <- gsub("\\n\\$","\n\n$",var.attributes$other.attributes[i],perl=TRUE)
      var.attributes$other.attributes[i] <- gsub("\\n\\n\\n","\n\n",var.attributes$other.attributes[i],perl=TRUE)
    }
  }

  if(echo==TRUE) {
    if(!"pander" %in% rownames(installed.packages())) {
      stop("Please install package pander first: install.packages('pander')")
    }

    pander::pander(df.attributes, split.table=Inf, split.cells=60,
                   keep.line.breaks=TRUE, plain.ascii=plain.ascii, style=style,
                   justify=justify, ...)
    pander::pander(var.attributes, split.table=Inf, split.cells=45,
                   keep.line.breaks=TRUE, plain.ascii=plain.ascii, style=style,
                   justify=justify, ...)

    return(invisible(list(df.attributes=df.attributes,var.attributes=var.attributes)))
  }

  return(list(df.attributes=df.attributes,var.attributes=var.attributes))

}

# Shortcuts for backward-compatibility
frequencies <- freq
unistats <- desc
properties <- prop
