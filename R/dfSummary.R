dfSummary <- function(x, style="multiline", justify="left", varnumbers=FALSE,
                      max.distinct.values=10, trim.strings=FALSE,
                      max.string.width=15, round.digits=2, split.cells=40,
                      display.labels=FALSE, file=NA, append=FALSE,
                      escape.pipe=FALSE, ...) {

  # use the parsing function to identify particularities such as row indexing
  tmp.info <- .parse_arg(as.character(match.call()[2]))

  if(!is.data.frame(x)) {
    x <- try(as.data.frame(x))

    if(inherits(x, "try-error")) {
      stop("x is not a dataframe and attempted conversion failed")
    }

    message("x was converted to a dataframe")
    tmp.info$df.name <- tmp.info$var.name
  }

  if(!is.na(file) && grepl("\\.html$",file) && isTRUE(append)) {
    stop("Append is not supported for html files. No file has been written.")
  }  
  
  # Initialise the output dataframe
  output <- data.frame(No.=numeric(),
                       Variable=character(),
                       Label=character(),
                       Properties=character(),
                       "Stats or Factor Levels"=character(),
                       Frequencies=character(),
                       "N Valid"=numeric(),
                       stringsAsFactors=FALSE,
                       check.names = FALSE)


  # Set output's attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st.type") <- "dfSummary"
  attr(output, "date") <- Sys.Date()
  attr(output, "fn.call") <- as.character(match.call())
  attr(output, "n.obs") <- nrow(x)
  attr(output, "df.name") <- tmp.info$df.name
  if("subset" %in% names(tmp.info))
     attr(output, "subset") <- tmp.info$subset
  attr(output, "pander.args") <- list(style=style, round=round.digits, justify=justify,
                                      split.table=Inf, keep.line.breaks=TRUE,
                                      split.cells=split.cells, ...=...)

  # iterate over dataframe columns
  for(i in seq_len(ncol(x))) {

    # extract data
    column.data <- x[[i]]

    # Add column number
    output[i,1] <- i

    # Add column name
    output[i,2] <- paste(names(x)[i])

    # Add column label (if applicable)
    if(display.labels) {
      output[i,3] <- Hmisc::label(x[i])
    }

    # Add variable properties (typeof, class)
    output[i,4] <- paste("type:",typeof(column.data),
                         "\nclass:",paste(class(column.data),collapse="\n + "),sep="")

    # For factors, display a column of levels and a column of frequencies
    if(is.factor(column.data)) {
      n.levels <- nlevels(column.data)
      if(n.levels <= max.distinct.values) {
        output[i,5] <- paste(1:n.levels,". ", levels(column.data), collapse="\n", sep="")

        fr <- table(column.data,useNA="no") # raw freqs
        pct <- round(prop.table(fr)*100,1) # percentage
        names(fr) <- 1:n.levels
        output[i,6] <- paste(names(fr),": ", fr," (",pct,"%)",sep="",collapse="\n")
      } else {
        output[i,5] <- paste(1:max.distinct.values,". ", levels(column.data)[1:max.distinct.values], collapse="\n", sep="")
        output[i,5] <- paste(output[i,4], paste(n.levels - max.distinct.values, "Other levels (not displayed)"),sep="\n")
        output[i,6] <- paste(as.character(length(unique(column.data))),"distinct val")
      }
    }

    # For numeric data, display a column of descriptive stats and a column of frequencies
    else if(is.numeric(column.data)) {
      if(identical(unique(column.data), as.numeric(NA))) {
        output[i,5] <- "Vector contains only NA's"
        output[i,6] <- ""
      } else {
        output[i,5] <- paste("mean (sd) = ",round(mean(column.data,na.rm=TRUE),round.digits),
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
        } else {
          output[i,6] <- paste(as.character(length(unique(column.data))),"distinct val")
        }
      }
    }

    # For text data, skip a column and display a column of frequencies
    else if(is.character(column.data)) {
      output[i,5] <- ""

      if(trim.strings)
        column.data.tmp <- sub(pattern="\\A\\s*(.+?)\\s*\\z", replacement="\\1", x=column.data, perl=TRUE)
      else
        column.data.tmp <- column.data

      if(identical(unique(column.data), as.character(NA))) {
        output[i,5] <- "Vector contains only NA's"
        output[i,6] <- ""
      } else if(length(unique(column.data.tmp)) <= max.distinct.values) {
        fr <- table(column.data.tmp,useNA="no")
        pct <- round(prop.table(fr)*100,1)
        output[i,6] <- paste(substr(names(fr),1,max.string.width),": ",
                             fr," (",pct,"%)",sep="",collapse="\n")
      } else {
        output[i,6] <- paste(as.character(length(unique(column.data.tmp))),"distinct val")
      }
    }

    # For data that does not fit in previous categories (neither numeric, character, or factor)
    else {
      output[i,5] <- ""
      if(identical(as.logical(unique(column.data)), NA)) {
        output[i,5] <- "Vector contains only NA's"
        output[i,6] <- ""
      } else if(length(unique(column.data)) <= max.distinct.values) {
        fr <- table(column.data,useNA="no")
        pct <- round(prop.table(fr)*100,1)
        output[i,6] <- paste(substr(names(fr),1,max.string.width),": ",
                             fr," (",pct,"%)",sep="",collapse="\n")
      } else {
        output[i,6] <- paste(as.character(length(unique(column.data))),"distinct val")
      }
    }

    # Add valid data info
    n.nas <- sum(is.na(column.data))
    n.val <- nrow(x) - n.nas

    output[i,7] <- paste(n.val,"/", nrow(x),"\n",
                         "(", format(n.val/nrow(x)*100, digits=1, nsmall=1), "%)",
                         sep="", collapse = "\n")
  }

  if(!display.labels)
    output$Label <- NULL

  if(!varnumbers)
    output$No. <- NULL

  # Escape pipes when needed (this is for Pandoc to correctly handle grid tables with multiline cells)

  if(!is.na(file)) {

    if(style=="grid" && escape.pipe) {
      output.esc.pipes <- paste(gsub(".\\|","\\\\|",capture.output(output)), collapse="\n")
      capture.output(cat(output.esc.pipes), file = file, append = append)
    }
    else if(grepl("\\.html$",file)) {
      file.copy(from=print(output, method="html_noshow", silent=TRUE), to=normalizePath(file), overwrite = TRUE)
      cleartmp(silent=TRUE)
    } else {
      capture.output(output, file = file, append = append)
    }
    message("Output successfully written to file ", normalizePath(file, mustWork = FALSE))
    return(invisible(output))
  }

  return(output)
}
