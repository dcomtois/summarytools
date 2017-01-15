ctable <- function(x, y, round.digits=1, style = "simple", justify = "right", prop = "t", 
                   useNA = "ifany", totals = TRUE, plain.ascii = TRUE, 
                   dnn=c(substitute(x), substitute(y)), ...) {

  # When style is 'rmarkdown', make plain.ascii FALSE unless specified explicitly
  if (style=='rmarkdown' && plain.ascii==TRUE && (!"plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }
  
  # Replace NaN's by NA's (This simplifies matters a lot!)
  if (NaN %in% x)  {
    message(paste(sum(is.nan(x)), "NaN value(s) converted to NA in x\n"))
    x[is.nan(x)] <- NA
  }
  
  if (NaN %in% y)  {
    message(paste(sum(is.nan(y)), "NaN value(s) converted to NA in y\n"))
    y[is.nan(y)] <- NA
  }
  
  if ("file" %in% names(match.call()))
    message("file argument is deprecated; use print() or view() function to generate files")
  
  # Add dimnames names
  freq.table <- table(x, y, useNA = useNA)
  names(dimnames(freq.table)) <- dnn
  
  proportions <- switch(prop,
                        t = prop.table(freq.table),
                        r = prop.table(freq.table, 1),
                        c = prop.table(freq.table, 2))
  
  if (isTRUE(totals)) {
    freq.table <- addmargins(freq.table)
    rownames(freq.table)[nrow(freq.table)] <- "Total"
    colnames(freq.table)[ncol(freq.table)] <- "Total"
    if (!is.null(proportions)) {
      if (prop=="t") {
        proportions <- addmargins(proportions)
      } else if (prop=="r") {
        proportions <- addmargins(proportions, 2)
        sumprops <- c(prop.table(freq.table[nrow(freq.table),-ncol(freq.table)]), Total=1)
        proportions <- rbind(proportions, sumprops)
      } else if (prop=="c") {
        proportions <- addmargins(proportions, 1)
        sumprops <- c(prop.table(freq.table[-nrow(freq.table),ncol(freq.table)]), Total=1)
        proportions <- cbind(proportions, sumprops)
      }
    }
  }
    
  # Change the name of NA items to avoid potential problems when echoing to console
  rownames(freq.table)[is.na(rownames(freq.table))] <- "<NA>"
  colnames(freq.table)[is.na(colnames(freq.table))] <- "<NA>"

  # create output object and set class / attributes
  output <- list(ctable=freq.table, prop=proportions)
  class(output) <- c("summarytools", class(output))
  attr(output, "st.type") <- "ctable"
  attr(output, "fn.call") <- as.character(match.call())
  attr(output, "date") <- Sys.Date()
  attr(output, "prop.type") <- prop
  attr(output, "round.digits") <- round.digits
  attr(output, "pander.args") <- list(style = style,
                                      #round = round.digits,
                                      plain.ascii = plain.ascii,
                                      justify = justify,
                                      split.table = Inf,
                                      ... = ...)
  
  # if (file != "") {
  #   if (style=="grid" && escape.pipe && !grepl("\\.html$",file)) {
  #     output.esc.pipes <- paste(gsub(".\\|","\\\\|",capture.output(output)), collapse="\n")
  #     capture.output(cat(output.esc.pipes), file = file, append = append)
  #   } else if (grepl("\\.html$",file)) {
  #     #file.copy(from=print(output, method = "html_file", silent=TRUE), to=normalizePath(file), overwrite = TRUE)
  #     #cleartmp(silent=TRUE)
  #     print(x = output, method = 'html_file', silent = TRUE, file = file, append = append) 
  #   } else {
  #     capture.output(output, file = file, append = append)
  #   }
  #   
  #   message('Output successfully written to file "', normalizePath(file, mustWork = FALSE), '"') 
  #   return(invisible(output))
  # }
  
  return(output)
}
