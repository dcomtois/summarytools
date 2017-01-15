dfSummary <- function(x, round.digits=2, style="multiline", justify="left",
                      plain.ascii=TRUE, varnumbers=FALSE, 
                      display.labels=any(sapply(X = x, FUN = Hmisc::label) != ""),
                      max.distinct.values=10, trim.strings=FALSE, max.string.width=15,
                      split.cells=40, ...) {

  # use the parsing function to identify particularities such as row indexing
  var.info <- .parse_arg(sys.calls(), sys.frames(), match.call())

  if(!is.data.frame(x)) {
    x <- try(as.data.frame(x))

    if(inherits(x, "try-error")) {
      stop("x is not a dataframe and attempted conversion failed")
    }

    message("x was converted to a dataframe")
    var.info$df.name <- var.info$var.name
  }

  if ("file" %in% names(match.call()))
    message("file argument is deprecated; use print() or view() function to generate files")

  if(style=="rmarkdown") {
    message("'rmarkdown' style not supported - using 'multiline' instead.")
    style <- 'multiline'
  }
  
  # Initialise the output dataframe
  output <- data.frame(No.=numeric(),
                       Variable=character(),
                       Label=character(),
                       Properties=character(),
                       Stats=character(),
                       Frequencies=character(),
                       N.Valid=numeric(),
                       stringsAsFactors=FALSE,
                       check.names = FALSE)

  # Set output's attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st.type") <- "dfSummary"
  attr(output, "Date") <- Sys.Date()
  attr(output, "fn.call") <- as.character(match.call())
  attr(output, "n.obs") <- nrow(x)
  attr(output, "df.name") <- var.info$df.name
  if("subset" %in% names(var.info))
     attr(output, "Subset") <- var.info$subset
  attr(output, "pander.args") <- list(style=style, round=round.digits, justify=justify,
                                      split.table=Inf, keep.line.breaks=TRUE,
                                      split.cells=split.cells, plain.ascii=plain.ascii, 
                                      ...=...)
  
  # iterate over columns of x
  for(i in seq_len(ncol(x))) {

    # extract column data
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
        output[i,5] <- paste0(1:n.levels,". ", levels(column.data), collapse="\n")
        fr <- table(column.data, useNA="no")
        pct <- round(prop.table(fr)*100, 1)
        names(fr) <- 1:n.levels
        output[i,6] <- paste0(names(fr),": ", fr, " (", pct, "%)", collapse="\n")
      } 
      
      # more levels than allowed by max.distinct.values
      else {
        output[i,5] <- paste0(1:max.distinct.values,". ", 
                              levels(column.data)[1:max.distinct.values], 
                              collapse="\n")
        output[i,5] <- paste(output[i,5], 
                             paste("...", n.levels - max.distinct.values, "other levels"),
                             sep="\n")
        fr <- table(column.data, useNA="no")
        pct <- round(prop.table(fr)*100, 1)
        output[i,6] <- paste(paste0(1:max.distinct.values, ": ", fr[1:max.distinct.values],
                                    " (", pct[1:max.distinct.values], "%)", collapse="\n"), 
                             paste0("others: ", tmp.sum <- sum(fr[(max.distinct.values+1):length(fr)]), 
                                    " (", round(tmp.sum/length(column.data)*100,1), "%)"), 
                             sep="\n")
      }
    }

    # For character data, display frequencies whenever possible
    else if(is.character(column.data)) {
      trimmed <- sub(pattern="\\A\\s*(.+?)\\s*\\z", replacement="\\1", x=column.data, perl=TRUE)
      
      if(trim.strings)
        column.data <- trimmed

      # Report if column contains only empty strings
      if(identical(unique(trimmed),"")) {
        output[i,5] <- "Contains only empty strings"
        output[i,6] <- ""
      }
        
      # Report if column contains only NA's      
      else if(identical(unique(column.data), as.character(NA))) {
        output[i,5] <- "Contains only NA's"
        output[i,6] <- ""
      }
      
      else {
        # Generate a frequency table
        fr <- table(column.data, useNA = "no")

        # Report all frequencies when allowed by max.distinct.values        
        if(length(fr) <= max.distinct.values) {
          output[i,5] <- paste0(1:length(fr),". ", dQuote(names(fr)), collapse="\n")
          pct <- round(prop.table(fr)*100,1)
          output[i,6] <- paste0(1:length(fr),": ", fr," (", pct, "%)", collapse="\n")
        } 
        
        # Report most common strings otherwise
        else {
          fr <- sort(fr, decreasing = TRUE)
          output[i,5] <- paste("Most frequent:\n", 
                               paste0(1:max.distinct.values,". ", 
                                      dQuote(substr(names(fr), 1, max.string.width)[1:max.distinct.values]),
                                      collapse="\n"),
                               paste("\n...", length(fr)-max.distinct.values, "other values"))
          pct <- round(prop.table(fr)*100,1)
          output[i,6] <- paste("-",
                               paste0(1:max.distinct.values,": ",
                                     fr[1:max.distinct.values],
                                     " (", pct[1:max.distinct.values], "%)",
                                     collapse="\n"),
                               paste0("others: ", tmp.sum <- sum(fr[(max.distinct.values+1):length(fr)]), 
                                      " (", round(tmp.sum/length(column.data)*100,1), "%)"),
                               sep="\n")
        }
      }
    }

    # For numeric data, display a column of descriptive stats and a column of frequencies
    else if(is.numeric(column.data)) {
      if(identical(unique(column.data), as.numeric(NA))) {
        output[i,5] <- "Contains only NA's"
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
          fr <- table(column.data, useNA="no")
          pct <- round(prop.table(fr)*100, 1)
          output[i,6] <- paste(round(as.numeric(names(fr)), round.digits), ": ", fr,
                               " (",pct,"%)",sep="",collapse="\n")
        } else {
          output[i,6] <- paste(as.character(length(unique(column.data))),"distinct values")
        }
      }
    }

    # Data does not fit in previous categories (neither numeric, character, or factor)
    else {
      output[i,5] <- ""
      if(identical(as.logical(unique(column.data)), NA)) {
        output[i,5] <- "Contains only NA's"
        output[i,6] <- ""
      } else if(length(unique(column.data)) <= max.distinct.values) {
        fr <- table(column.data,useNA="no")
        pct <- round(prop.table(fr)*100,1)
        output[i,6] <- paste(substr(names(fr),1,max.string.width),": ",
                             fr," (",pct,"%)",sep="",collapse="\n")
      } else {
        output[i,6] <- paste(as.character(length(unique(column.data))),"distinct values")
      }
    }

    # Add valid data info
    n.nas <- sum(is.na(column.data))
    n.val <- nrow(x) - n.nas

    output[i,7] <- paste(n.val,"/", nrow(x),"\n",
                         "(", format(n.val/nrow(x)*100, digits=1, nsmall=1), "%)",
                         sep="", collapse = "\n")
  }

  # Escape symbols for words between <>'s in some columns to allow <NA> or factor levels such as
  # <ABC> to be rendered correctly 
  if(!plain.ascii) {
    if(isTRUE(display.labels)) {
      output$Label <- gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>", 
                           x = output$Label, perl=TRUE)
    }
    
    output$Stats <- gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>", 
                         x = output$Stats, perl=TRUE)
    output$Stats <- gsub(pattern = "\n", replacement = " \\\\ \n", 
                         x = output$Stats, perl=TRUE)
    output$Frequencies <- gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>", 
                               x = output$Frequencies, perl=TRUE)
    output$Frequencies <- gsub(pattern = "\n", replacement = " \\\\ \n", x = output$Frequencies, perl=TRUE)
  }
  
  names(output) <- c("No.", "Variable", "Label", "Properties", "Stats / Values",
                     "Freqs, % Valid", "N Valid")
  
  if(!display.labels)
    output$Label <- NULL

  if(!varnumbers)
    output$No. <- NULL
  
  
  # # Escape pipes when needed (this is for Pandoc to correctly handle grid tables with multiline cells)
  # if(!is.na(file)) {
  # 
  #   if(style=="grid" && escape.pipe) {
  #     output.esc.pipes <- paste(gsub(".\\|","\\\\|",capture.output(output)), collapse="\n")
  #     capture.output(cat(output.esc.pipes), file = file, append = append)
  #   }
  #   else if(grepl("\\.html$",file)) {
  #     file.copy(from=print(output, method="html_noshow", silent=TRUE), to=normalizePath(file), overwrite = TRUE)
  #     cleartmp(silent=TRUE)
  #   } else {
  #     capture.output(output, file = file, append = append)
  #   }
  #   message("Output successfully written to file ", normalizePath(file, mustWork = FALSE))
  #   return(invisible(output))
  # }

  return(output)
}
