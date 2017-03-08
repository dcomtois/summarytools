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

  # Add dimnames
  freq_table <- table(x, y, useNA = useNA)
  names(dimnames(freq_table)) <- dnn

  prop <- tolower(substr(prop,1,1))
  prop_table <- switch(prop,
                       t = prop.table(freq_table),
                       r = prop.table(freq_table, 1),
                       c = prop.table(freq_table, 2))

  if (isTRUE(totals)) {
    freq_table <- addmargins(freq_table)
    rownames(freq_table)[nrow(freq_table)] <- "Total"
    colnames(freq_table)[ncol(freq_table)] <- "Total"
    if (!is.null(prop_table)) {
      if (prop == "t") {
        prop_table <- addmargins(prop_table)
      } else if (prop == "r") {
        prop_table <- addmargins(prop_table, 2)
        sum_props <- c(prop.table(freq_table[nrow(freq_table), -ncol(freq_table)]), Total=1)
        prop_table <- rbind(prop_table, sum_props)
      } else if (prop == "c") {
        prop_table <- addmargins(prop_table, 1)
        sum_props <- c(prop.table(freq_table[-nrow(freq_table), ncol(freq_table)]), Total=1)
        prop_table <- cbind(prop_table, sum_props)
      }
      rownames(prop_table)[nrow(prop_table)] <- "Total"
      colnames(prop_table)[ncol(prop_table)] <- "Total"
    }
  }

  # Change the name of NA items to avoid potential problems when echoing to console
  rownames(freq_table)[is.na(rownames(freq_table))] <- "<NA>"
  colnames(freq_table)[is.na(colnames(freq_table))] <- "<NA>"
  rownames(prop_table)[is.na(rownames(prop_table))] <- "<NA>"
  colnames(prop_table)[is.na(colnames(prop_table))] <- "<NA>"

  # create output object and set class / attributes
  output <- list(ctable=freq_table, prop=prop_table)
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "ctable"
  attr(output, "fn_call") <- as.character(match.call())
  attr(output, "date") <- Sys.Date()
  attr(output, "prop_type") <- prop

  attr(output, "pander_args") <- list(style = style,
                                      round = round.digits,
                                      digits = 6,
                                      plain.ascii = plain.ascii,
                                      justify = justify,
                                      split.table = Inf,
                                      #keep.trailing.zeros = TRUE, # do NOT put it -- causes issue w/ pander
                                      ... = ...)
  return(output)
}
