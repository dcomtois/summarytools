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

  # Set additionnal attributes from the parsing function
  tmp.attr <- .parse.arg.x(as.character(match.call()[2]))
  for(item in names(tmp.attr))
    attr(output, item) <- tmp.attr[item]
  if("label" %in% names(attributes(x)))
    attr(output, "var.label") <- rapportools::label(x)

  # Add other attributes
  attr(output, "date") <- Sys.Date()
  attr(output, "pander.args") <- list(style=style, round=round.digits, plain.ascii=plain.ascii,
                                      justify=justify, split.table=Inf, ...=...)
  if(exists("msg.nan"))
    attr(output, "notes") <- msg.nan

  # Write to file when file argument is supplied
  if(!is.na(file)) {
    capture.output(output, file = file, append = append)
    message("Output successfully written to file ", normalizePath(file))
  }

  return(output)
}


descr <- function(x, na.rm=TRUE, style="simple", round.digits=2,
                 justify="right", plain.ascii=TRUE, file=NA,
                 append=FALSE, transpose=FALSE, ...) {

  if(is.atomic(x) && !is.numeric(x))
    stop("x is not numerical")

  # Initialise output list and set basic class/attributes
  output <- list()
  class(output) <- c("summarytools", class(output))
  attr(output, "type") <- "descr"

  # if x is atomic, convert to dataframe with a single element and use regex techniques
  # to recuperate the column's name (and df's name when present)
  if(is.atomic(x)) {

    x <- data.frame(x)
    tmp.attr <- .parse.arg.x(as.character(match.call()[2]))
    for(item in names(tmp.attr))
      attr(output, item) <- tmp.attr[item]

    # Add label to attributes if present
    if("label" %in% names(attributes(x)))
      attr(stats, "var.label") <- rapportools::label(x)

  } else {
    # x not atomic; we must extract dataframe name from function call
    attr(output, "df.name") <- as.character(match.call()[2])
  }

  # Still more attributes
  attr(output, "date") <- Sys.time()
  attr(output, "pander.args") <- list(style=style, round=round.digits, plain.ascii=plain.ascii,
                                      justify=justify, split.table=Inf, ...=...)


  # Build skeleton (2 empty dataframes; one for stats and other to report valid vs na counts)
  output$stats <- data.frame(Mean=numeric(), Std.Dev=numeric(), Min=numeric(), Max=numeric(),
                             Median=numeric(), mad=numeric(), IQR=numeric(), CV=numeric(),
                             Skewness=numeric(), SE.Skewness=numeric(), Kurtosis=numeric())
  output$observ <- data.frame(Valid=numeric(), "<NA>"=numeric(), Total=numeric(),check.names = FALSE)


  # Identify non-numerical columns
  to.be.removed <- numeric()
  for(i in seq_along(x)) {
    if(!is.numeric(x[[i]]))
      to.be.removed <- append(to.be.removed, i)
  }

  # Remove identified non-numerical columns
  if(length(to.be.removed)>0) {
    attr(output,"notes") <- paste("Non-numerical variables ignored:", paste(colnames(x)[to.be.removed], collapse=", "))
    x <- x[-to.be.removed]
    rm(to.be.removed)
  }

  if(ncol(x)==0) {
    warning("No numerical variable were given as arguments. Returning nothing.")
    return()
  }

  # Iterate over variables in x
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
    output$stats[i,] <- round(c(x.mean<-mean(variable,na.rm=na.rm),
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

    # Insert proper row name (from var.names)
    rownames(output$stats)[i] <- colnames(x)[i]

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

  #   # Make sure sink() is terminated (in case of an error occuring)
  #   on.exit(expr = if(sink.number()) sink())

  if(!is.na(file)) {
    capture.output(output, file = file, append = append)
    message("Output successfully written to file ", normalizePath(file))
  }

  return(output)
}

dfSummary <- function(x, style="multiline", justify="left",
                      max.distinct.values=10, trim.strings=FALSE,
                      max.string.width=15, round.digits=2, file=NA,
                      display.labels=FALSE, ...) {

  if(!is.data.frame(x)) {
    x <- try(as.data.frame(x))
    if(class(x)=="try-error")
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
  #class(output) <- append(class(output), "st.output")
  class(output) <- c("summarytools", class(output))
  attr(output, "type") <- "dfSummary"
  attr(output, "df.name") <- substitute(x)
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


print.summarytools <- function(x, method="pander", ...) {

  # Check that when method=="viewer", the function is actually called from RStudio
  if(grepl("v|View", method) && !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) {
    message("method 'viewer' can only be used within RStudio. Switching method to 'browser'")
    method <- "browser"
  }

  info.table <- ""
  for(a in c("var.name", "df.name", "var.label", "rows.subset", "date")) {
    if(a %in% names(attributes(x)))
      info.table <- paste(info.table,
                          paste(a, ":", as.character(attr(x, a)), sep = ""),
                          sep="\n")
  }

  info.table <- sub("\ndf.name:",    "\nDataframe name: ", info.table)
  info.table <- sub("\nvar.name:",   "\n Variable name: ", info.table)
  info.table <- sub("\nvar.label:",  "\nVariable label: ", info.table)
  info.table <- sub("\nrows.subset:","\n   Rows subset: ", info.table)
  info.table <- sub("\ndate:",       "\n          Date: ", info.table)


  notes <- ifelse("notes" %in% names(attributes(x)), yes = paste("Notes:", attr(x,"notes")), no = "")

  html.footer.line = paste("Generated by <a href='https://github.com/dcomtois/summarytools'>summarytools</a> package version",
                           packageVersion(pkg = "summarytools"),
                           "(<a href='http://www.r-project.org/'>R</a>", getRversion(), ")",
                           "on", Sys.Date())

  # Frequency tables
  # Type = 'freq'
  if(attr(x, "type") == "freq") {

    if(method=="pander") {

      cat("\nFrequencies\n")

      cat(info.table)

      pander.args <- append(attr(x, "pander.args"), list(x=quote(x)))
      do.call(pander::pander, pander.args)

      cat(notes)

      #return(invisible())
    }

    else if(grepl("([v|V]iew)|(brow)",method)) {

        freq.table.html <-
        xtable::print.xtable(xtable::xtable(x = x, align = "rccccc",
                                            digits = c(0,0,rep(attr(x, "pander.args")$round,4))),
                             type = "html", print.results = FALSE,
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
              p(info.table),
              br(),
              HTML(gsub("<td> ", "<td>", freq.table.html)),
              p(notes),
              HTML(text = html.footer.line)
          )
        )
      )
      htmlfile <- paste(tempfile(),".html",sep="")
      capture.output(html.content, file = htmlfile)

    }
  }

  # Descriptive Stats
  # type = 'descr'
  else if(attr(x, "type") == "descr") {

    if(method=="pander") {

      cat("\nDescriptive Univariate Statistics\n\n")

      cat(info.table)

      pander.args <- append(attr(x, "pander.args"), list(x=quote(x$stats)))
      do.call(pander::pander, pander.args)

      cat("Observations")
      pander.args <- append(attr(x, "pander.args"), list(x=quote(x$observ)))
      do.call(pander::pander, pander.args)
    }

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
          div(class="container", style="width:80%",
              h1("Descriptive Univariate Statistics"),
              p(info.table),
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

  # Objet de type 'dfSummary'
  else if(attr(x, "type") == "dfSummary") {

    if(method=="pander") {
      cat("\nDataframe summary\n\n")

      cat(info.table)

      pander.args <- append(attr(x, "pander.args"), list(x=quote(x)))
      do.call(pander::pander, pander.args)
    }

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
              #p(attr(x, "date")),
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

  # Open the output html file when relevant
  if(grepl("v|View",method)) {
    rstudio::viewer(htmlfile)
  } else if(grepl("b|Brow", method)) {
    shell.exec(htmlfile)
  }

  # Return differently according to method; with browser and viewer, we want the file path
  # to be returned, silently?
  if(grepl("P|pand", method)) {
    return(invisible())
  } else if(grepl("V|view", "B|brow")) {
    return(htmlfile)
  }
}

# When called from within a function with .parse.arg.x(as.character(match.call)[2]),
# it returns a list of identified structures: df.name if any & var.name
# it is assumed that the data referred to by arg.str is an atomic structure
.parse.arg.x <- function(arg.str) {

  # First get rid of any space characters
  arg.str <- gsub("\\s","",arg.str)

  # Initialise output list
  output <- list()

  # Simplest case: variable name alone, no $ nor [ ]'s
  if(!grepl(pattern = "\\$|\\[", x = arg.str)) {
     output$var.name <- arg.str
     return(output)
  }

  # Second simplest case: df.name$var.name without [ ] 's;
  # there can be more than one $, but we'll consider only the 2
  # last elements from the split list
  if(grepl(".*\\$.*", arg.str) && !grepl("\\[", arg.str)) {
    tmp.split <- strsplit(arg.str,"\\$")[[1]]
    output$df.name <- tmp.split[length(tmp.split)-1]
    output$var.name <- tmp.split[length(tmp.split)]
    return(output)
  }

  # Now we know there are some [ ] 's ;
  # A more complex case: df.name$var.name[1:10]
  if(grepl(".*\\$.*\\[.*\\:.*\\]$", arg.str)) {
    str.ind <- sub(".*\\$.*\\[(.*\\:.*)\\]$", "\\1", arg.str)
    str.no.ind <- sub("(.*\\$.*)\\[.*\\:.*\\]$", "\\1", arg.str)
    tmp.split <- strsplit(str.no.ind,"\\$")[[1]]
    output$df.name <- tmp.split[length(tmp.split)-1]
    output$var.name <- tmp.split[length(tmp.split)]
    output$rows.subset <- str.ind
    return(output)

  }

  # When arg.str end with ]] -- and thus should have the form df.name[[4]] or df.name[["var.name"]],
  # we'll turn this into df.name[4] or df.name["var.name"] to access colnames;
  # there is some redundancy in getting colnames in the case the var.name is already
  # given, but for the sake of practicallity we'll leave it as is, at least for now
  if(grepl("\\]\\]$", arg.str)) {

    modif.str <- sub("\\[(\\[.*\\])\\]", replacement = "\\1", x = arg.str)

    # Evaluate the string to get the column name
    output$var.name <- colnames(eval(parse(text=modif.str)))

    # Extract the df.name (eliminating "parent" elements to the left of any other $)
    output$df.name <- sub("(.*\\$)?(.*)\\[.*\\]$", "\\2", modif.str)

    # Extract row indexing
    output$rows.subset <- sub(".*\\[(.*)\\,.*\\]$", "\\1", arg.str)

    return(output)
  }

  # when arg.str is in the form df.name[,4], we turn this into df.name[4];
  # this is to allow access to colnames
  # We also identify & remove any row indexing (ex: df.name[1:10,4] will also become df.name[4])
  if(grepl(".*\\[(.*)?\\,\\d*\\]$", arg.str)) {

    modif.str <- sub("(.*\\[)(.*)?\\,)?(\\d*)(\\])$", replacement = "\\1\\3\\4", x = arg.str)
    # Evaluate the string to get the column names
    output$var.name <- colnames(eval(parse(text=modif.str)))

    # Extract the df.name (eliminating "parent" elements to the left of any other $)
    output$df.name <- sub("(.*\\$)?(.*)\\[.*\\]$", "\\2", modif.str)

    # Extract row indexes
    output$rows.subset <- sub(".*\\[(.*)\\,.*\\]$", "\\1", arg.str)

      sub("(.*\\[)(.*)?\\,)?(\\d*)(\\])$", replacement = "\\1\\3\\4", x = arg.str)
    return(output)
  }

  output$var.name <- NA
  return(output)

}

# view is a wrapper function for print(x, "view"). Allows alternate "browser" or "pander" methods as well.
view <- function(x, method="viewer", ...) {
  print(x, method=method, ...)
}

