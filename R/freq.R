freq <- function(x, round.digits=2, style="simple", justify="right",
                 plain.ascii=TRUE, file=NA, append=FALSE, escape.pipe=FALSE, 
                 weights=NA, rescale.weights=FALSE, ...) {
  
  # If x is a data.frame with 1 column, extract this column as x
  if(!is.null(ncol(x)) && ncol(x)==1) {
    x <- x[[1]]
  }

  if(!is.atomic(x)) {
    stop("argument must be a vector or a factor; for dataframes, use lapply(x, freq)")
  }

  if(!is.na(file) && grepl("\\.html$",file) && isTRUE(append)) {
    stop("Append is not supported for html files. No file has been written.")
  }

  # When style is 'rmarkdown', make plain.ascii FALSE unless specified explicitly
  if(style=='rmarkdown' && plain.ascii==TRUE && (!"plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }
  
  # Replace NaN's by NA's (This simplifies matters a lot!)
  if(NaN %in% x)  {
    message(paste(sum(is.nan(x)), "NaN value(s) converted to NA\n"))
    x[is.nan(x)] <- NA
  }

  # create a basic frequency table, always including the NA row
  if(identical(NA, weights)) {
    freq.table <- table(x, useNA = "always")
    # Change the name of the NA item (last) to avoid potential problems when echoing to console
    names(freq.table)[length(freq.table)] <- '<NA>'

    # calculate proportions (valid, i.e excluding NA's)
    P.valid <- prop.table(table(x, useNA="no")) * 100

    # Add '<NA>' item to the proportions; this assures
    # proper length when cbind'ing later on
    P.valid['<NA>'] <- NA

    # calculate proportions (total, i.e. including NA's)
    P.tot <- prop.table(table(x, useNA="always"))*100

  }

  # Weights are used
  else {

    # Check that weights vector is of the right length
    if(length(weights)!=length(x)) {
      stop("weights vector must be of same length as x")
    }

    if(isTRUE(rescale.weights)) {
      wgts <- weights/sum(weights)*length(x)
    } else {
      wgts <- weights
    }

    freq.table <- xtabs(wgts ~ x)
    P.valid <- prop.table(freq.table) * 100
    P.valid["<NA>"] <- NA
    freq.table["<NA>"] <- sum(wgts) - sum(xtabs(wgts ~ x))
    P.tot <- prop.table(freq.table) * 100

  }

  # Calculate cumulative proportions
  P.valid.cum <- cumsum(P.valid)
  P.valid.cum['<NA>'] <- NA
  P.tot.cum <- cumsum(P.tot)
  
  # Combine the info to build the final frequency table
  output <- cbind(freq.table, P.valid, P.valid.cum, P.tot, P.tot.cum)
  output <- rbind(output, c(colSums(output,na.rm = TRUE)[1:2], rep(100,3)))
  colnames(output) <- c("N","% Valid","% Cum.Valid","% Total","% Cum.Total")
  rownames(output) <- c(names(freq.table),"Total")

  # Escape < and > when used in pairs in rownames, so that <NA> or <ABCD> are rendered correctly 
  # with style "rmarkdown" or "grid"
  if(!plain.ascii && style %in% c('rmarkdown', 'grid')) {
    row.names(output) <- gsub(pattern = '<',replacement = '\\<',x = row.names(output), fixed = TRUE)
    row.names(output) <- gsub(pattern = '>',replacement = '\\>',x = row.names(output), fixed = TRUE)
  }
    
  # Update the output class and attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st.type") <- "freq"
  attr(output, "fn.call") <- as.character(match.call()) #as.character(match.call()[2])
  attr(output, "date") <- Sys.Date()

  var.info <- .parse_arg(sys.calls(), sys.frames(), match.call())
  
  attr(output, "var.info") <- c(Dataframe = ifelse("df.name" %in% names(var.info), var.info$df.name, NA),
                                Variable = ifelse("var.names" %in% names(var.info), var.info$var.names, NA),
                                Label = ifelse(Hmisc::label(x) != "", Hmisc::label(x), NA),
                                Subset = ifelse("rows.subset" %in% names(var.info), var.info$rows.subset, NA),
                                Group = ifelse("by.group" %in% names(var.info), var.info$by.group, NA),
                                Weights = ifelse(!identical(weights,NA), substitute(weights), NA))

  attr(output, "pander.args") <- list(style = style,
                                      round = round.digits,
                                      plain.ascii = plain.ascii,
                                      justify = justify,
                                      split.table = Inf,
                                      ... = ...)
  if(exists("wgts"))
     attr(output, "weights") <- wgts


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
