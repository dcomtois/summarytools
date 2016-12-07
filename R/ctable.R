ctable <- function(x, y, round.digits=2, style="simple", justify="right", prop = "t", useNA = "ifany",
                   totals=TRUE, plain.ascii=TRUE, dnn=c(substitute(x), substitute(y)), file=NA, 
                   append=FALSE, escape.pipe=FALSE, ...) {

  if(!is.na(file) && grepl("\\.html$",file) && isTRUE(append)) {
    stop("Append is not supported for html files. No file has been written.")
  }
  
  # When style is 'rmarkdown', make plain.ascii FALSE unless specified explicitly
  if(style=='rmarkdown' && plain.ascii==TRUE && (!"plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }
  
  # Replace NaN's by NA's (This simplifies matters a lot!)
  if(NaN %in% x)  {
    message(paste(sum(is.nan(x)), "NaN value(s) converted to NA in x\n"))
    x[is.nan(x)] <- NA
  }
  
  if(NaN %in% y)  {
    message(paste(sum(is.nan(y)), "NaN value(s) converted to NA in y\n"))
    y[is.nan(y)] <- NA
  }
  
  # Add dimnames names
  freq.table <- table(x, y, useNA = useNA)
  names(dimnames(freq.table)) <- dnn
  
  proportions <- switch(prop,
                        t = prop.table(freq.table),
                        r = prop.table(freq.table, 1),
                        c = prop.table(freq.table, 2))
  
  if(isTRUE(totals)) {
    freq.table <- addmargins(freq.table)
    if(!is.null(proportions)) 
      proportions <- addmargins(proportions) 
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
  
  if(!is.na(file)) {
    if(style=="grid" && escape.pipe) {
      output.esc.pipes <- paste(gsub(".\\|","\\\\|",capture.output(output)), collapse="\n")
      capture.output(cat(output.esc.pipes), file = file, append = append)
    } else if(grepl("\\.html$",file)) {
      file.copy(from=print(output, method="html_noshow", silent=TRUE), to=normalizePath(file), overwrite = TRUE)
      cleartmp(silent=TRUE)
    } else {
      capture.output(output, file = file, append = append)
    }
    
    message('Output successfully written to file "', normalizePath(file, mustWork = FALSE), '"') 
    return(invisible(output))
  }
  
  return(output)
}

#pander::pander(output, style=style, plain.ascii=plain.ascii) 

(ct <- ctable(tobacco$gender, tobacco$smoker))
(p <- prop.table(ct))
#paste(ct, p)
(ct[1,1] <- paste0(ct[1,1], " (", round(p[1,1]*100, 2), "%)"))
#

ff <- function(x) {
  print(quote(x))
  #print(deparse(x))
  print(substitute(x))
}

ff(tobacco$gender)

CrossTable()
x <- tobacco$gender
y <- tobacco$smoker

ft <- table(x,y)
labels(ft)
names(dimnames(ft)) <- c("Gender","Smoker")


# Paste raw freqs with 
# Add row totals
#  if (totals %in% c("b", "r")) {
#    output <- addmargins(output, 1)
#    rownames(output)[nrow(output)] <- "Total"
#  }

# Add col totals
#  if(totals %in% c("b", "c")) {
#    output <- addmargins(output, 2)
#    colnames(output)[ncol(output)] <- "Total"
#  }
#  
# Add proportions - total
#  if(prop == "t") {
#    # for rows
#    if(isTRUE(totals) {
#      for(i in seq_len(nrow(output)))
#    }
