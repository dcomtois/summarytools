dfSummary <- function(x, style="multiline", justify="left",
                      max.distinct.values=10, trim.strings=FALSE,
                      max.string.width=15, round.digits=2, split.cells=40,
                      display.labels=FALSE, file=NA, append=FALSE,
                      escape.pipe=FALSE, ...) {

  df.convers <- FALSE
  if(!is.data.frame(x)) {
    x <- try(as.data.frame(x))
    if(inherits(x, "try-error")) {
      message("x is not a dataframe and attempted conversion failed")
    } else {
      message("x was converted to a dataframe")
      df.convers <- TRUE # in this case df.name will be taken from var.name
    }
  }

  # create an output dataframe
  output <- data.frame(variable=character(),
                       label=character(),
                       properties=character(),
                       factor.levels.or.stats=character(),
                       frequencies=character(),
                       n.valid=numeric(),
                       stringsAsFactors=FALSE)

  # Set additional attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st.type") <- "dfSummary"

  # use the parsing function to identify particularities such as row indexing
  tmp.attr <- .parse.arg(as.character(match.call()[2]))
  for(item in names(tmp.attr)) {
    attr(output, item) <- tmp.attr[[item]]
  }

  if(df.convers) {
    attr(output, "df.name") <- attr(output, "var.name")
    attr(output, "notes") <- paste(attr(output, "df.name"), "was converted from",
                                   typeof(x), "to data.frame")
    attr(output, "var.name") <- NULL
  }

  attr(output, "n.obs") <- nrow(x)
  attr(output, "date") <- Sys.Date()
  attr(output, "pander.args") <- list(style=style, round=round.digits, justify=justify,
                                      split.table=Inf, keep.line.breaks=TRUE,
                                      split.cells=split.cells, ...=...)

  # iterate over dataframe columns
  for(i in seq_len(ncol(x))) {

    # extract data
    column.data <- x[[i]]

    # Add column name
    output[i,1] <- paste(i, names(x)[i], sep=".\n")

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
        output[i,4] <- paste(output[i,4], paste(n.levels - max.distinct.values, "Other levels (not displayed)"),sep="\n")
        output[i,5] <- paste(as.character(length(unique(column.data))),"distinct val")
      }
    }

    # For numeric data, display a column of descriptive stats and a column of frequencies
    else if(is.numeric(column.data)) {
      if(identical(unique(column.data), as.numeric(NA))) {
        output[i,4] <- "Vector contains only NA's"
        output[i,5] <- ""
      } else {
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
        } else {
          output[i,5] <- paste(as.character(length(unique(column.data))),"distinct val")
        }
      }
    }

    # For text data, skip a column and display a column of frequencies
    else if(is.character(column.data)) {
      output[i,4] <- ""

      if(trim.strings)
        column.data.tmp <- sub(pattern="\\A\\s*(.+?)\\s*\\z", replacement="\\1", x=column.data, perl=TRUE)
      else
        column.data.tmp <- column.data

      if(identical(unique(column.data), as.character(NA))) {
        output[i,4] <- "Vector contains only NA's"
        output[i,5] <- ""
      } else if(length(unique(column.data.tmp)) <= max.distinct.values) {
        fr <- table(column.data.tmp,useNA="no")
        pct <- round(prop.table(fr)*100,1)
        output[i,5] <- paste(substr(names(fr),1,max.string.width),": ",
                             fr," (",pct,"%)",sep="",collapse="\n")
      } else {
        output[i,5] <- paste(as.character(length(unique(column.data.tmp))),"distinct val")
      }
    }

    # For data that does not fit in previous categories (neither numeric, character, or factor)
    else {
      output[i,4] <- ""
      if(identical(as.logical(unique(column.data)), NA)) {
        output[i,4] <- "Vector contains only NA's"
        output[i,5] <- ""
      } else if(length(unique(column.data)) <= max.distinct.values) {
        fr <- table(column.data,useNA="no")
        pct <- round(prop.table(fr)*100,1)
        output[i,5] <- paste(substr(names(fr),1,max.string.width),": ",
                             fr," (",pct,"%)",sep="",collapse="\n")
      } else {
        output[i,5] <- paste(as.character(length(unique(column.data))),"distinct val")
      }
    }

    # Add valid data info
    n.nas <- sum(is.na(column.data))
    n.val <- nrow(x) - n.nas

    output[i,6] <- paste(n.val,"/", nrow(x),"\n",
                         "(", format(n.val/nrow(x)*100, digits=1, nsmall=1), "%)",
                         sep="", collapse = "\n")
  }

  if(!display.labels)
    output$label <- NULL

  # Escape pipes when needed (this is for Pandoc to correctly handle grid tables with multiline cells)

  if(!is.na(file)) {

    if(style=="grid" && escape.pipe) {
      output.esc.pipes <- paste(gsub(".\\|","\\\\|",capture.output(output)), collapse="\n")
      capture.output(cat(output.esc.pipes), file = file, append = append)
    }
    else if(grepl("\\.html$",file)) {
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

