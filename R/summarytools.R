
#### DESCR ###################################################################################
descr <- function(x, na.rm=TRUE, style="simple", round.digits=2,
                  justify="right", plain.ascii=TRUE, file=NA,
                  append=FALSE, transpose=FALSE, ...) {

  if(is.atomic(x) && !is.numeric(x))
    stop("x is not numerical")

  if(is.atomic(x)) {
    x <- data.frame(x)
  }

  # Initialise output list and set the class/attributes
  output <- list()

  class(output) <- c("summarytools", class(output))
  attr(output, "type") <- "descr"

  tmp.attr <- .parse.arg(as.character(match.call()[2]))
  for(item in names(tmp.attr))
    attr(output, item) <- tmp.attr[[item]]

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


  # Identify and exclude non-numerical columns
  to.be.removed <- numeric()
  for(i in seq_along(x)) {
    if(!is.numeric(x[[i]]))
      to.be.removed <- append(to.be.removed, i)
  }

  if(length(to.be.removed)>0) {
    attr(output,"notes") <- paste("Non-numerical variable(s) ignored:", paste(colnames(x)[to.be.removed], collapse=", "))
    x <- x[-to.be.removed]
    attr(output, "col.names") <- attr(output, "col.names")[-to.be.removed]
    rm(to.be.removed)
  }

  if(ncol(x)==0) {
    stop("No numerical variable were given as arguments.")
  }

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
    #rownames(output$stats)[i] <- attr(output,"col.names")[i]
    #rownames(output$stats)[i] <- attr(output,"var.name")[i]

    # Report number of missing vs valid data
    n.valid <- sum(!is.na(variable))
    p.valid <- round(n.valid / length(variable) * 100, digits = round.digits)
    n.NA <- sum(is.na(variable))
    p.NA <- round(n.NA / length(variable) * 100, digits = round.digits)

    # Insert into observ table
    output$observ[i,] <- c(paste(n.valid, " (", p.valid, "%)",sep=""),
                           paste(n.NA, " (", p.NA, "%)",sep=""),
                           length(variable))

    rownames(output$observ)[i] <- colnames(x)[i]

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

  return(output)
}

#### dfSUMMARY #####################################################################

dfSummary <- function(x, style="multiline", justify="left",
                      max.distinct.values=10, trim.strings=FALSE,
                      max.string.width=15, round.digits=2, file=NA,
                      display.labels=FALSE, ...) {

  if(!is.data.frame(x)) {
    x <- try(as.data.frame(x))
    if(inherits(x, "try-error"))
      message("x is not a dataframe and attempted conversion failed")
    else
      message("x was converted to a dataframe")
  }

  # set the stringsAsFactors to FALSE to prevent problems
  op <- options("stringsAsFactors")
  options(stringsAsFactors=FALSE)
  on.exit(options(op))

  # create an output dataframe
  output <- data.frame(variable=character(),
                       label=character(),
                       properties=character(),
                       factor.levels.or.stats=character(),
                       frequencies=character(),
                       n.valid=numeric())

  # Set additional attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "type") <- "dfSummary"

  # Set additionnal attributes from the parsing function
  tmp.attr <- .parse.arg(as.character(match.call()[2]))
  for(item in names(tmp.attr))
    attr(output, item) <- tmp.attr[[item]]

  attr(output, "n.obs") <- nrow(x)
  attr(output, "date") <- Sys.Date()
  attr(output, "pander.args") <- list(style=style, round=round.digits, justify=justify,
                                      split.table=Inf, ...=...)

  # iterate over dataframe columns
  for(i in seq_len(ncol(x))) {

    # extract data
    column.data <- x[[i]]

    # Add column number to output dataframe
    # output[i,1] <- i

    # Add column name
    output[i,1] <- paste(i, names(x)[i], sep=". ")

    # Add column label (if applicable)
    if(display.labels)
      output[i,2] <- rapportools::label(x[i])

    # Add variable properties (typeof, class)
    output[i,3] <- paste("type:",typeof(column.data),
                         "\nclass:",paste(class(column.data),collapse="\n + "),sep="")

    # For factors, display a column of levels and a column of frequencies
    if(is.factor(column.data)) {
      n.levels <- nlevels(column.data)
      if(n.levels <= max.distinct.values) {
        output[i,4] <- paste(1:n.levels,". ", levels(column.data), collapse="\n", sep="")

        fr <- table(column.data,useNA="no") # raw freqs
        pct <- round(prop.table(fr)*100,1) # percentage
        names(fr) <- 1:n.levels
        output[i,5] <- paste(names(fr),": ", fr," (",pct,"%)",sep="",collapse="\n")
      } else {
        output[i,4] <- paste(1:max.distinct.values,". ", levels(column.data)[1:max.distinct.values], collapse="\n", sep="")
        output[i,4] <- paste(output[i,5], paste(n.levels - max.distinct.values, "Other levels (not displayed)"),sep="\n")
        output[i,5] <- paste(as.character(length(unique(column.data))),"distinct values")
      }
    }

    # For numeric data, display a column of descriptive stats and a column of frequencies
    else if(is.numeric(column.data)) {
      output[i,4] <- paste("mean (sd) = ",round(mean(column.data,na.rm=TRUE),round.digits),
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
        output[i,5] <- paste(round(as.numeric(names(fr)),round.digits),": ", fr," (",pct,"%)",sep="",collapse="\n")
      }
      else {
        output[i,5] <- paste(as.character(length(unique(column.data))),"distinct values")
      }
    }

    # For text data, skip a column and display a column of frequencies
    else if(is.character(column.data)) {
      output[i,4] <- ""

      if(trim.strings)
        column.data.tmp <- sub(pattern="\\A\\s*(.+?)\\s*\\z",replacement="\\1",x=column.data,perl=TRUE)
      else
        column.data.tmp <- column.data

      if(length(unique(column.data.tmp)) <= max.distinct.values) {
        fr <- table(column.data.tmp,useNA="no")
        pct <- round(prop.table(fr)*100,1)
        output[i,5] <- paste(substr(names(fr),1,max.string.width),": ",
                             fr," (",pct,"%)",sep="",collapse="\n")
      }
      else output[i,5] <- paste(as.character(length(unique(column.data.tmp))),"distinct values")
    }

    # For data that does not fit in previous categories (numeric, character, factor)
    else {
      output[i,4] <- ""
      if(length(unique(column.data)) <= max.distinct.values) {
        fr <- table(column.data,useNA="no")
        pct <- round(prop.table(fr)*100,1)
        output[i,5] <- paste(substr(names(fr),1,max.string.width),": ",
                             fr," (",pct,"%)",sep="",collapse="\n")
      }
      else output[i,5] <- paste(as.character(length(unique(column.data))),"distinct values")
    }

    # Add missing data info
    n.nas <- sum(is.na(column.data))
    n.val <- nrow(x) - n.nas

    output[i,6] <- paste(n.val," (",format(n.val/nrow(x)*100,digits=1,nsmall=1),"%)",collapse="",sep="")
  }

  if(!display.labels)
    output$label <- NULL

  # Write to file when file argument is supplied;
  if(!is.na(file)) {
    sink(file=file)
    print(output)
    sink()
    message("Output successfully written to file ", normalizePath(file))
  }

  return(output)
}


#### FREQ #############################################################################

freq <- function(x, round.digits=2, style="simple", justify="right",
                 plain.ascii=TRUE, file=NA, append=FALSE, ...) {

  # If x is a data.frame with 1 column, convert it to vector
  if(!is.null(ncol(x)) && ncol(x)==1) {
    x <- x[[1]]
    message("x converted to vector")
  }

  if(!is.factor(x) && !is.vector(x)) {
    stop("argument must be a vector or a factor; for dataframes, use lapply(x,freq)")
  }

  # Replace NaN's by NA's
  # This simplifies matters a lot
  if(NaN %in% x)  {
    msg.nan <- paste(sum(is.nan(x)), "NaN value(s) converted to NA\n")
    x[is.nan(x)] <- NA
  }

  # create a basic frequency table, always including the NA row
  freq.table <- table(x, useNA = "always")

  # Change the name of the NA item (last) to avoid potential problems when echoing to console
  names(freq.table)[length(freq.table)] <- '<NA>'

  # calculate proportions (valid, i.e excluding NA's)
  P.valid <- prop.table(table(x, useNA="no")) * 100

  # Add '<NA>' item to the proportions; this assures
  # proper length when cbind'ing later on
  P.valid['<NA>'] <- NA

  # Calculate cumulative proportions
  P.valid.cum <- cumsum(P.valid)
  P.valid.cum['<NA>'] <- NA

  # calculate proportions (total, i.e. including NA's)
  P.tot <- prop.table(table(x, useNA="always"))*100
  P.tot.cum <- cumsum(P.tot)

  # Build the actual frequency table
  output <- cbind(freq.table, P.valid, P.valid.cum, P.tot, P.tot.cum)
  output <- rbind(output, c(length(x), rep(100,4)))
  colnames(output) <- c("N","%Valid","%Cum.Valid","%Total","%Cum.Total")
  rownames(output) <- c(names(freq.table),"Total")


  # Set the class and attributes of the output object
  class(output) <- c("summarytools", class(output))
  attr(output, "type") <- "freq"
  attr(output, "fn.call") <- as.character(match.call()[2])

  # Set additionnal attributes from the parsing function
  tmp.attr <- .parse.arg(as.character(match.call()[2]))
  for(item in names(tmp.attr))
    attr(output, item) <- tmp.attr[[item]]

  # Add variable label if any
  if("label" %in% names(attributes(x)))
    attr(output, "var.label") <- rapportools::label(x)

  # Add other attributes to the output
  attr(output, "date") <- Sys.Date()
  attr(output, "pander.args") <- list(style=style, round=round.digits, plain.ascii=plain.ascii,
                                      justify=justify, split.table=Inf, ...=...)
  attr(output, "n.obs") <- length(x)

  if(exists("msg.nan"))
    attr(output, "notes") <- msg.nan

  # Write to file when file argument is supplied
  if(!is.na(file)) {
    capture.output(output, file = file, append = append)
    message("Output successfully written to file ", normalizePath(file))
  }

  return(output)
}

#### PRINT.SUMMARYTOOLS ################################################################

print.summarytools <- function(x, method="pander", ...) {

  # Build info.table and prepare the field  ----------------------------------------

  if(method=="pander") {
    info.table <- ""
    for(a in c("df.name", "col.names", "var.name", "var.label", "rows.subset", "date")) {
      if(a %in% names(attributes(x)))
        info.table <- paste(info.table,
                            paste(a, ":", as.character(attr(x, a)), sep = ""),
                            sep="\n")
    }

    info.table <- sub("\ndf.name:",    "\nDataframe name: ", info.table)
    info.table <- sub("\nvar.name:",  "\n Variable name: ", info.table)
    info.table <- sub("\nvar.label:",  "\nVariable label: ", info.table)
    info.table <- sub("\nrows.subset:","\n   Rows subset: ", info.table)
    info.table <- sub("\ndate:",       "\n          Date: ", info.table)
  }

  # for methods browser / viewer
  else {
    html.footer.line = paste("Generated by <a href='https://github.com/dcomtois/summarytools'>summarytools</a> package version ",
                             packageVersion(pkg = "summarytools"),
                             " (<a href='http://www.r-project.org/'>R</a> version ", getRversion(), ")",
                             "<br/>", Sys.Date(), sep="")
  }

  notes <- ifelse("notes" %in% names(attributes(x)),
                  yes = paste("Notes -- ", attr(x,"notes")), no = "")

  # Printing descr objects --------------------------------------------------------------
  if(attr(x, "type") == "descr") {

    # With method pander --------------------------------------
    if(method=="pander") {

      cat("\nDescriptive (Univariate) Statistics\n")
      cat(info.table)
      pander.args <- append(attr(x, "pander.args"), list(x=quote(x$stats)))
      do.call(pander::pander, pander.args)
      cat("Observations")
      pander.args <- append(attr(x, "pander.args"), list(x=quote(x$observ)))
      do.call(pander::pander, pander.args)
    }

    # With method viewer / browser --------------------------
    else if(grepl("(v|view)|(B|brow)",method)) {

      descr.table.html <-
        xtable::print.xtable(xtable::xtable(x = x$stats, align = paste("r", paste(rep("c",ncol(x$stats)),collapse=""),sep=""),
                                            digits = c(0,rep(attr(x, "pander.args")$round,ncol(x$stats)))),
                             type = "html", print.results = FALSE,
                             html.table.attributes = 'class="table table-striped table-bordered"')

      obs.table.html <-
        xtable::print.xtable(xtable::xtable(x = x$observ, align = paste("r", paste(rep("c",ncol(x$observ)),collapse=""),sep=""),
                                            digits = c(0,rep(attr(x, "pander.args")$round,ncol(x$observ)))),
                             type = "html", print.results = FALSE,
                             html.table.attributes = 'class="table table-striped table-bordered"')

      stpath <- find.package("summarytools")

      html.content <- tags$html(
        tags$header(
          includeCSS(path = paste(stpath,"includes/stylesheets/bootstrap.min.css", sep="/")),
          includeCSS(path = paste(stpath,"includes/stylesheets/custom.css", sep="/"))
        ),
        tags$body(
          div(class="container", # style="width:80%",
              h3("Descriptive Univariate Statistics"),
              h2(attr(x, "df.name")),
              if("rows.subset" %in% names(attributes(x)))
                p("Rows subset:",attr(x,"rows.subset")),
              #h4("Number of rows: ", attr(x, "n.obs")),
              br(),
              HTML(gsub("<td> ", "<td>", descr.table.html)),
              h3("Observations"),
              HTML(gsub("<td> ", "<td>", obs.table.html)),
              p(notes),
              HTML(text = html.footer.line)
          )
        )
      )

      htmlfile <- paste(tempfile(),".html",sep="")
      capture.output(html.content, file = htmlfile)
    }
  }

  # Printing dfSummary objects ------------------------------------------------------
  else if(attr(x, "type") == "dfSummary") {

    # With method pander --------------------------
    if(method=="pander") {
      cat("\nDataframe summary\n\n")

      cat(info.table)

      pander.args <- append(attr(x, "pander.args"), list(x=quote(x)))
      do.call(pander::pander, pander.args)
    }

    # with method viewer or browser ---------------
    else if(grepl("(v|view)|(B|brow)",method)) {

      sanitize.colnames <- function(x) {
        x <- gsub("\\.", " ", x)
        x <- rapportools::capitalise(x)
        x <- sub("levels or stats", "Levels / Stats", x)
        return(x)
      }

      dfSummary.html <-
        xtable::print.xtable(xtable::xtable(x = x,digits = 0,
                                            align = paste("c", paste(rep("l",ncol(x)),collapse=""),sep="")),
                             include.rownames = FALSE, type = "html", print.results = FALSE,
                             sanitize.colnames.function = sanitize.colnames,
                             html.table.attributes = 'class="table table-striped table-bordered"')

      stpath <- find.package("summarytools")

      html.content <- tags$html(
        tags$header(
          includeCSS(path = paste(stpath,"includes/stylesheets/bootstrap.min.css", sep="/")),
          includeCSS(path = paste(stpath,"includes/stylesheets/custom.css", sep="/"))
        ),
        tags$body(
          div(class="container", style="width:80%",
              h3("Dataframe Summary"),
              h2(attr(x, "df.name")),
              h4("Number of rows: ", attr(x, "n.obs")),
              br(),
              HTML(gsub("<td> ", "<td>", dfSummary.html)),
              p(notes),
              HTML(text = html.footer.line)
          )
        )
      )

      htmlfile <- paste(tempfile(),".html",sep="")
      capture.output(html.content, file = htmlfile)
    }
  }

  # printing freq objects ---------------------------------------------------------------
  else if(attr(x, "type") == "freq") {

    # with method pander -----------------------------
    if(method=="pander") {
      cat("\nFrequencies\n")
      cat(info.table)
      pander.args <- append(attr(x, "pander.args"), list(x=quote(x)))
      do.call(pander::pander, pander.args)
      cat(notes)
    }

    # with method viewer / browser --------------------
    else if(grepl("([v|V]iew)|(brow)",method)) {

      sanitize.colnames <- function(x) {
        x <- gsub("\\.", " ", x)
        x <- sub("\\%", "% ", x)
        return(x)
      }

      freq.table.html <-
        xtable::print.xtable(xtable::xtable(x = x, align = "rccccc",
                                            digits = c(0,0,rep(attr(x, "pander.args")$round,4))),
                             type = "html", print.results = FALSE,
                             sanitize.colnames.function = sanitize.colnames,
                             html.table.attributes = 'class="table table-striped table-bordered"')

      stpath <- find.package("summarytools")

      html.content <- tags$html(
        tags$header(
          includeCSS(path = paste(stpath,"includes/stylesheets/bootstrap.min.css", sep="/")),
          includeCSS(path = paste(stpath,"includes/stylesheets/custom.css", sep="/"))
        ),
        tags$body(
          div(class="container", style="width:80%",
              h1("Frequencies"),
              br(),
              HTML(gsub("<td> ", "<td>", freq.table.html)), # To avoid initial space in cells
              p(notes),
              HTML(text = html.footer.line)
          )
        )
      )
      htmlfile <- paste(tempfile(),".html",sep="")
      capture.output(html.content, file = htmlfile)
    }
  }


  # Open the output html file --------------------------------------------
  if(grepl("v|View",method)) {
    rstudio::viewer(htmlfile)
  } else if(grepl("b|Brow", method)) {
    shell.exec(htmlfile)
  }

  # return file path -----------------------------------------------------
  if(grepl("P|pand", method)) {
    return(invisible())
  } else if(grepl("V|view", "B|brow")) {
    return(htmlfile)
  }
}

# .parse.arg ####################################################################
#
# This function takes a string referring to existing data and parses it
# to get information on the data structure.
#
# info returned: df.name, var.name, col.names, rows.subset, col.index, data.struct
#
# Example:
#
# > .parse.arg("iris[1:200,c(1,4)]")
# $arg.str
# [1] "iris[1:200,c(1,4)]"
#
# $rows.subset
# [1] "1:200"
#
# $col.index
# [1] "c(1,4)"
#
# $df.name
# [1] "iris"
#
# $col.names
# [1] "Sepal.Length" "Petal.Width"

.parse.arg <- function(arg.str) {

  # Check if arg.str is a string
  if(!is.character(arg.str))
    stop("arg.str must be a string")

  # Recuperate the object designated by arg.str; this is to allow further work
  x <- try(eval(parse(text=arg.str)))
  if(inherits(x, "try-error")) {
    message("arg.str must match an existing object")
    return()
  }

  if(!is.data.frame(x) && !is.atomic(x)) {
    message("arg.str must match an atomic structure (vector/factor) or a dataframe")
    return()
  }

  # Initialise output list
  output <- list()

  # Store a copy of the arg.str in output object
  output$arg.str <- arg.str

  # Trim the string removing leading/trailing blanks
  arg.str <- gsub("^\\s+|\\s+$", "", arg.str)

  # Get rid of spaces next to brackets and next to comma in indexing brackets.
  # Note: that way assures us to not remove any spaces in quoted structures
  # such as ['var name']
  arg.str <- gsub("\\s*\\[\\s*","[", arg.str, perl=TRUE) # spaces near [
  arg.str <- gsub("\\s*\\]\\s*","]", arg.str, perl=TRUE) # spaces near ]
  arg.str <- gsub("^(.*)(\\[\\d+:\\d+)?\\s?,\\s?(.+)$", "\\1\\2,\\3", arg.str, perl=TRUE)

  # Change [[]] to [] for the last pair of brackets; this simplifies the work
  arg.str <- sub("\\[{2}(.*)\\]{2}$", "[\\1]", arg.str, perl=TRUE)

  # Change references to data with ['name'] or [['name']] into $name, also to simplify matters
  re.brack <- '\\[{1,2}[\'\"]'
  if(grepl(re.brack, arg.str)) {
    arg.str <- gsub('\\[{1,2}[\'\"]', "$", arg.str, perl=TRUE)
    arg.str <- gsub('[\'\"]\\]{1,2}', "", arg.str, perl=TRUE)
  }

  # Next we'll isolate indexing in the last brackets
  re.index <- "(.*?)\\[(.*?)\\]$"

  if(grepl(re.index, arg.str)) {
    indexes <- sub(re.index, "\\2", arg.str, perl=TRUE)

    # Further decompose the indexes
    # indexing having 2 elements (rows, columns), will be identified by this regex
    # [1:10,] or [,"Species] will also match
    re.split.index <- "^(.+)?,+(c\\(.*\\)|\\d+|\\d+:\\d+|'.*'|\".+\")$"
    if(grepl(re.split.index, indexes, perl = TRUE)) {
      output$rows.subset <- sub(re.split.index, "\\1", indexes, perl=TRUE)
      output$col.index <- sub(re.split.index, "\\2", indexes, perl=TRUE)

      # Remove any empty string
      if(nchar(output$rows.subset) == 0)
        output$rows.subset <- NULL
      if(nchar(output$col.index) == 0)
        output$col.index <- NULL
    }

    # When previous regex does not match, it means the index has only 1 element,
    # either row or column. When a comma is present:
    else if(substring(indexes,1,1) == ",")
      output$col.indexes <- sub("^,", "", indexes, perl = TRUE)

    else if(substring(indexes,nchar(indexes),nchar(indexes)) == ",")
      output$rows.subset <- sub(",$", "", indexes, perl = TRUE)

    # When there is no comma, we'll check if x is a dataframe or not.
    # If it is, the index refers to columns, and otherwise, to rows
    else {
      # first we need to reevaluate the arg.str
      x.tmp <- eval(parse(text = arg.str))
      if(is.data.frame(x.tmp))
        output$col.index <- indexes
      else
        output$rows.subset <- indexes
    }

    # Update the string to remove what's already accounted for
    arg.str <- sub(re.index, "\\1", arg.str, perl=TRUE)
  }

  # Split arg.str by "$" to identify structures
  output$data.struct <- strsplit(arg.str, "$", fixed = TRUE)[[1]]

  # If type of x is dataframe, normally the last element in the data structures
  # should be the df name
  if(is.data.frame(x)) {
    output$df.name <- tail(output$data.struct,1)
    output$col.names <- colnames(x)
  }

  # Otherwise, depending on the situation, we'll try to get at the df name and its colnames()
  else {

    # If vector is referred to via column indexing, recup the column's name
    # by an evaluation of the form df[col.index]
    if("col.index" %in% names(output)) {
      output$var.name <- eval(parse(text=paste("colnames(",arg.str,"[",output$col.index,"])")))
      #output$col.names <- eval(parse(text=paste("colnames(",arg.str,"[",output$col.index,"])")))
      output$df.name <- tail(output$data.struct,1)
    }

    # If there is no column indexing, it means the vector's name is in the
    # data.struc list, along with the df name one level higher, unless the vector
    # was "standalone"
    else {
      output$var.name <- tail(output$data.struct,1)
      #output$col.names <- tail(output$data.struct,1)
      if(length(output$data.struct)>1)
        output$df.name <- output$data.struct[length(output$data.struct)-1]
    }
  }

  # remove last item from data.struct when it's the same as var.name to avoid redundancy
  output$data.struct <- setdiff(output$data.struct, output$var.name)
  #output$data.struct <- setdiff(output$data.struct, output$col.names)

  # same with df.name and data.struct
  output$data.struct <- setdiff(output$data.struct, output$df.name)

  # cleanup
  if(length(output$data.struct)==0)
    output$data.struct <- NULL

  return(output)
}

# view is a wrapper function for print(x, "view"). Allows alternate "browser" or "pander" methods as well.
view <- function(x, method="viewer", ...) {
  print(x, method=method, ...)
}
