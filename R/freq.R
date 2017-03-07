freq <- function(x, round.digits = 2, style = "simple", justify = "right", order = "names",
                 plain.ascii = TRUE, weights = NA, rescale.weights = FALSE, ...) {
  
  # If x is a data.frame with 1 column, extract this column as x
  if (!is.null(ncol(x)) && ncol(x)==1) {
    x <- x[[1]]
  }

  if (!is.atomic(x)) {
    stop("argument must be a vector or a factor; for dataframes, use lapply(x, freq)")
  }

  if ("file" %in% names(match.call()))
    message("file argument is deprecated; use print(x, file=) or view(x, file=) instead")
  
  # When style is 'rmarkdown', make plain.ascii FALSE unless specified explicitly
  if (style == 'rmarkdown' && isTRUE(plain.ascii) && (!"plain.ascii" %in% (names(match.call())))) {
    plain.ascii <- FALSE
  }
  
  # Replace NaN's by NA's (This simplifies matters a lot!)
  if (NaN %in% x)  {
    message(paste(sum(is.nan(x)), "NaN value(s) converted to NA\n"))
    x[is.nan(x)] <- NA
  }
  
  # create a basic frequency table, always including the NA row
  if (identical(NA, weights)) {
    freq_table <- table(x, useNA = "always")
    
    # Order by frequency if needed
    if (order == "freq") {
      freq_table <- sort(freq_table, decreasing = TRUE)
      na_pos <- which(is.na(names(freq_table)))
      freq_table <- c(freq_table[-na_pos], freq_table[na_pos])
    }
    
    # Change the name of the NA item (last) to avoid potential problems when echoing to console
    names(freq_table)[length(freq_table)] <- '<NA>'

    # calculate proportions (valid, i.e excluding NA's)
    P_valid <- prop.table(freq_table[-length(freq_table)]) * 100

    # Add '<NA>' item to the proportions; this assures
    # proper length when cbind'ing later on
    P_valid['<NA>'] <- NA

    # calculate proportions (total, i.e. including NA's)
    P_tot <- prop.table(freq_table) * 100
  }

  # Weights are used
  else {

    # Check that weights vector is of the right length
    if (length(weights) != length(x)) {
      stop("weights vector must be of same length as x")
    }

    # use a copy of weights to be able to use substitute(weights) later on
    wgts <- weights
    if (sum(is.na(wgts)) > 0) {
      warning("Missing values on weight variable have been detected and were treated as zeroes.")
      wgts[is.na(wgts)] <- 0
    }
    
    if (isTRUE(rescale.weights)) {
      wgts <- wgts / sum(wgts) * length(x)
    }

    freq_table <- xtabs(wgts ~ x)
  
    # Order by frequency if needed
    if (order == "freq")
      freq_table <- freq_table[order(freq_table, decreasing = TRUE)]

    P_valid <- prop.table(freq_table) * 100
    P_valid["<NA>"] <- NA
    freq_table["<NA>"] <- sum(wgts) - sum(xtabs(wgts ~ x))
    P_tot <- prop.table(freq_table) * 100

  }

  # Calculate cumulative proportions
  P_valid_cum <- cumsum(P_valid)
  P_valid_cum['<NA>'] <- NA
  P_tot_cum <- cumsum(P_tot)
  
  # Combine the info to build the final frequency table
  output <- cbind(freq_table, P_valid, P_valid_cum, P_tot, P_tot_cum)
  output <- rbind(output, c(colSums(output, na.rm = TRUE)[1:2], rep(100,3)))
  colnames(output) <- c("N", "% Valid", "% Valid Cum.", "% Total", "% Total Cum.")
  rownames(output) <- c(names(freq_table), "Total")

  # Escape < and > when used in pairs in rownames, so that <NA> or <ABCD> are rendered correctly 
  # with style "rmarkdown" or "grid"
  if (!plain.ascii && style %in% c('rmarkdown', 'grid')) {
    row.names(output) <- gsub(pattern = '<',replacement = '\\<',x = row.names(output), fixed = TRUE)
    row.names(output) <- gsub(pattern = '>',replacement = '\\>',x = row.names(output), fixed = TRUE)
  }

  # Update the output class and attributes
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "freq"
  attr(output, "fn_call") <- as.character(match.call())
  attr(output, "Date") <- Sys.Date()

  parse_info <- .parse_arg(sys.calls(), sys.frames(), match.call())
  attr(output, "var_info") <- c(Dataframe = ifelse("df_name" %in% names(parse_info), parse_info$df_name, NA),
                                Variable = ifelse("var_names" %in% names(parse_info), parse_info$var_names, NA),
                                Label = ifelse(Hmisc::label(x) != "", Hmisc::label(x), NA),
                                Subset = ifelse("rows_subset" %in% names(parse_info), parse_info$rows_subset, NA),
                                Weights = substitute(weights),
                                Group = ifelse("by_group" %in% names(parse_info), parse_info$by_group, NA))

  attr(output, "pander_args") <- list(style = style,
                                      round = round.digits,
                                      digits = 6,
                                      plain.ascii = plain.ascii,
                                      justify = justify,
                                      split.table = Inf,
                                      keep.trailing.zeros = TRUE,
                                      ... = ...)
  return(output)
}
