freq <- function(x, round.digits=2, style="simple", justify="right",
                 plain.ascii=TRUE, file=NA, append=FALSE,
                 escape.pipe=FALSE, weights=NA,
                 rescale.weights=TRUE, ...) {

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
  if(is.null(weights)) {
    freq.table <- table(x, useNA = "always")
    # Change the name of the NA item (last) to avoid potential problems when echoing to console
    names(freq.table)[length(freq.table)] <- '<NA>'

  } else {

    # Check that there are enough weights
    if(length(weights)!=length(x)) {
      stop("weights vector must be of same length as x")
    } else if(isTRUE(rescale.weights)) {
      wgts <- weights/sum(weights)*length(x)
    } else {
      wgts <- weights
    }

    freq.table <- xtabs(wgts ~ x)
    freq.table["<NA>"] <- sum(wgts) - sum(xtabs(wgts ~ x))

  }

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
  output <- rbind(output, c(colSums(output,na.rm = TRUE)[1:2], rep(100,3)))
  colnames(output) <- c("N","%Valid","%Cum.Valid","%Total","%Cum.Total")
  rownames(output) <- c(names(freq.table),"Total")


  # Set the class and attributes of the output object
  class(output) <- c("summarytools", class(output))
  attr(output, "st.type") <- "freq"
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
  attr(output, "weights.var") <- deparse(substitute(weights))
  attr(output, "weights") <- wgts

  if(exists("msg.nan"))
    attr(output, "notes") <- msg.nan

#   if(exists("wgts"))
#     attr(output, "notes") <-
#       append(attr(output, "notes"), "Weights were applied; see attributes of the returned object to see weights")

  if(!is.na(file)) {

    if(style=="grid" && escape.pipe) {
      output.esc.pipes <- paste(gsub(".\\|","\\\\|",capture.output(output)), collapse="\n")
      capture.output(cat(output.esc.pipes), file = file, append = append)
    }
    else if(grepl("\\.html$",file)) {
      if(isTRUE(append)) message("Append is not supported for html files. This parameter will be ignored")
      file.copy(from=print(output, method="browser", open=FALSE), to=normalizePath(file, mustWork = FALSE))
    } else {
      capture.output(output, file = file, append = append)
    }
    message("Output successfully written to file ", normalizePath(file))
    return(invisible(output))
  }
  #print(wgts[1:20])
  return(output)

}
