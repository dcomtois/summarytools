#' @keywords internal
build_heading_pander <- function(format_info, data_info) {
  
  add_markup <- function(str, h) {
    if (!isTRUE(format_info$plain.ascii)) {
      if (h == 0) {
        str <- trimws(sub(pattern = '^(.+?)((:)\\s(.+))?$', 
                          replacement = '**\\1\\3** \\4',
                          x = str, perl = TRUE))
      } else {
        str <- paste(paste0(rep(x = '#', times = h), collapse = ""), str)
      }
    }
    return(str)
  }
  
  append_items <- function(items, h = 0) {
    to_append <- c()
    for (item in items) {
      if (names(item) %in% names(data_info)) {
        if (item == items[[1]] && 
            names(item) %in% c("Variable", "Dataframe", "Row.x.Col")) {
          to_append <- paste0('  \n', add_markup(data_info[[names(item)]], h))
        }
        
        else if ((grepl(pattern = 'label', names(item)) && 
                  isTRUE(format_info$display.labels)) ||
                 (names(item) == 'Data.type' && 
                  isTRUE(format_info$display.type)) ||
                 !grepl('(label|Data\\.type)', names(item))) {
          
          to_append <- append(to_append,
                              paste0('  \n', 
                                     add_markup(paste(item,
                                                      data_info[[names(item)]],
                                                      sep = ': '), h), 
                                     '  '))
        }
      }
    }
    return(paste(to_append, collapse = ""))
  }
  
  # head1 = main title (e.g. 'Data Frame Summary')
  # head2 = the data frame, the variable, or the 2 variables for ctable
  # head3 = additional other elements
  
  
  # Special cases where no primary heading (title) is needed
  if (isTRUE(format_info$var.only)) {
    head2 <- append_items(list(c(Variable       = trs('variable')),
                               c(Variable.label = trs('label')),
                               c(Data.type      = trs('type'))))
    return(list(head2))
  }
  
  else if (isTRUE(format_info$group.only) ||
           ('by.first' %in% names(data_info) && 
            (!isTRUE(data_info$by.first) || !isTRUE(format_info$headings)))) {
    head2 <- append_items(list(c(Group = trs('group'))))
    return(list(head2))
  }
  
  else if (!isTRUE(format_info$headings)) {
    return(list())
  }
  
  # Regular cases - Build primary and secondary headings
  caller <- as.character(sys.call(-1))[1]
  head1  <- NA
  head2  <- NA
  
  if (caller == 'prep_freq') {
    head1 <- ifelse('Weights' %in% names(data_info),
                    trs('title.freq'), trs('title.freq.weighted'))
    head1 <- add_markup(head1, h = 3)
    
    head2 <- append_items(list(c(Variable       = trs('variable')),
                               c(Variable.label = trs('label')),
                               c(Data.type      = trs('type')),
                               c(Weights        = trs('weights')),
                               c(Group          = trs('group'))))
    
  } else if (caller == 'prep_ctable') {
    head1 <- switch(data_info$Proportions,
                    Row    = paste(trs('title.ctable'), trs('title.ctable.row'), 
                                   sep = ' / '),
                    Column = paste(trs('title.ctable'), trs('title.ctable.col'), 
                                   sep = ' / '),
                    Total  = paste(trs('title.ctable'), trs('title.ctable.tot'), 
                                   sep = ' / '),
                    None   = trs('title.ctable'))
    head1 <- add_markup(head1, h = 3)
    
    head2 <- append_items(list(c(Row.x.Col       = trs('variables')),
                               c(Dataframe       = trs('data.frame')),
                               c(Dataframe.label = trs('label')),
                               c(Group           = trs('group'))))
    
  } else if (caller == 'prep_descr') {
    head1 <- ifelse('Weights' %in% names(data_info),
                    trs('title.descr.weighted'), 
                    trs('title.descr'))
    head1 <- add_markup(head1, h = 3)
    
    if ('Variable' %in% names(data_info)) {
      head2 <- append_items(list(c(Variable       = trs('variable')),
                                 c(Variable.label = trs('label')),
                                 c(Weights        = trs('weights')),
                                 c(Group          = trs('group')),
                                 c(N.Obs          = trs('n'))))
      
    } else {
      head2 <- append_items(list(c(Dataframe       = trs('data.frame')),
                                 c(Dataframe.label = trs('label')),
                                 c(Weights         = trs('weights')),
                                 c(Group           = trs('group')),
                                 c(N.Obs           = trs('n'))))
    }
  } else if (caller == 'prep_dfs') {
    head1 <- add_markup(trs('title.dfSummary'), h = 3)
    
    head2 <- append_items(list(c(Dataframe       = trs('data.frame')),
                               c(Dataframe.label = trs('label')),
                               c(Dimensions      = trs('dimensions')),
                               c(Duplicates      = trs('duplicates'))))
  }
  
  tmp <- list(head1, head2)
  return(tmp[which(!is.na(tmp))])
}

#' @keywords internal
#' @import htmltools
build_heading_html <- function(format_info, data_info, method) {
  
  append_items <- function(items) {
    to_append_html <- character()
    for (item in items) {
      if (names(item) %in% names(data_info)) {
        if ((grepl(pattern = 'label', names(item)) && 
             isTRUE(format_info$display.labels)) ||
            (names(item) == 'Data.type' && 
             isTRUE(format_info$display.type)) ||
            !grepl('(label|Data\\.type)', names(item))) {
          
          div_str_item <- paste(paste0('<strong>', item, '</strong>'),
                                ifelse(is.character(data_info[[names(item)]]),
                                       conv_non_ascii(data_info[[names(item)]]),
                                       data_info[[names(item)]]),
                                sep = ': ')
          
          if (identical(to_append_html, character())) {
            to_append_html <- div_str_item
          } else {
            to_append_html <- paste(to_append_html,
                                    div_str_item,
                                    sep = '\n  <br/>')
          }
        }
      }
    }
    
    if (identical(to_append_html, character())) {
      return(NA)
    }
    
    return(HTML(to_append_html))
  }
  
  # Special cases where no title is needed
  if (isTRUE(format_info$var.only)) {
    head2 <- append_items(list(c(Variable       = trs('variable')),
                               c(Variable.label = trs('label')),
                               c(Data.type      = trs('type'))))
    return(list(head2))
  }
  
  else if (isTRUE(format_info$group.only) ||
           ('by.first' %in% names(data_info) && 
            (!isTRUE(data_info$by.first) || !isTRUE(format_info$headings)))) {
    head2 <- append_items(list(c(Group = trs('group'))))
    
    return(list(head2))
  }
  
  else if (!isTRUE(format_info$headings)) {
    return(list())
  }
  
  # Regular cases - Build primary, secondary and third headings
  head1  <- NA # uses h3()
  head2  <- NA # uses h4() for viewer/browser, and <strong> for render
  head3  <- NA # uses <strong>...</strong>
  
  caller <- as.character(sys.call(-1))[1]
  
  if (caller == 'prep_freq') {
    
    head1 <- h3(ifelse('Weights' %in% names(data_info),
                       trs('title.freq.weighted'), trs('title.freq')))
    
    if ('Variable' %in% names(data_info)) {
      if (method == 'render') {
        head2 <- strong(data_info$Variable)
      } else {
        head2 <- h4(data_info$Variable)
      }
    }
    
    head3 <- append_items(list(c(Variable.label = trs('label')),
                               c(Data.type      = trs('type')),
                               c(Weights        = trs('weights')),
                               c(Group          = trs('group'))))
    
  } else if (caller == 'prep_ctable') {
    
    head1 <- 
      h3(switch(data_info$Proportions,
                Row    = paste(trs('title.ctable'), trs('title.ctable.row'), 
                               sep = ' / '),
                Column = paste(trs('title.ctable'), trs('title.ctable.col'), 
                               sep = ' / '),
                Total  = paste(trs('title.ctable'), trs('title.ctable.tot'), 
                               sep = ' / '),
                None   = trs('title.ctable')))
    
    if ('Row.x.Col' %in% names(data_info)) {
      if (method == 'render') {
        head2 <- strong(data_info$Row.x.Col)
      } else {
        head2 <- h4(data_info$Row.x.Col)
      }
    }
    
    head3 <- append_items(list(c(Dataframe       = trs('data.frame')),
                               c(Dataframe.label = trs('label')),
                               c(Group           = trs('group'))))
    
  } else if (caller == 'prep_descr') {
    
    head1 <- h3(ifelse('Weights' %in% names(data_info),
                       trs('title.descr.weighted'), 
                       trs('title.descr')))
    
    if ('Variable' %in% names(data_info)) {
      
      if (method == 'render') {
        head2 <- strong(data_info$Variable)
      } else {
        head2 <- h4(data_info$Variable)
      }
      
      head3 <- append_items(list(c(Variable.label = trs('label')),
                                 c(Weights        = trs('weights')),
                                 c(Group          = trs('group')),
                                 c(N.Obs          = trs('n'))))
    } else {
      if (method == 'render') {
        head2 <- strong(data_info$Dataframe)
      } else {
        head2 <- h4(data_info$Dataframe)
      }
      
      head3 <- append_items(list(c(Dataframe.label = trs('label')),
                                 c(Weights         = trs('weights')),
                                 c(Group           = trs('group')),
                                 c(N.Obs           = trs('n'))))
    }
    
  } else if (caller == 'prep_dfs') {
    
    head1 <- h3(trs('title.dfSummary'))
    if (method == 'render') {
      head2 <- strong(data_info$Dataframe)
    } else {
      head2 <- h4(data_info$Dataframe)
    }
    
    head3 <- append_items(list(c(Dataframe.label = trs('label')),
                               c(Dimensions      = trs('dimensions')),
                               c(Duplicates      = trs('duplicates'))))
  }
  
  tmp <- list(head1, head2, head3)
  return(tmp[which(!is.na(tmp))])
}

# Arguments validation for freq, ctable, descr and dfSummary functions.
# Another function for validating st_options arguments follows.
#' @importFrom checkmate test_int test_logical test_choice test_string
#' @keywords internal
check_arguments <- function(mc, dotArgs) {
  
  caller <- as.character(sys.call(-1))[1]
  pf <- parent.frame()
  msg <- character()
  
  # Deprecated arguments -------------------------------------------------------
  if ("file" %in% names(dotArgs)) {
    message(paste("'file' argument is deprecated; use with print() or view(),",
                  "e.g. print(x, file=", dotArgs$file))
  }
  
  if ("omit.headings" %in% names(dotArgs)) {
    message(paste0("'omit.headings' argument has been replaced by 'headings'; ",
                   "setting headings = ", 
                   !isTRUE(dotArgs$omit.headings)))
    assign(x = "headings", value = !isTRUE(dotArgs$omit.headings), 
           envir = parent.frame())
  }
  
  
  # Arguments common to all functions ------------------------------------------
  
  if ('round.digits' %in% names(mc) && 
      !isTRUE(test_int(pf$round.digits))) {
    msg %+=% "'round.digits' must be a whole number"
  }
  
  if ('style' %in% names(mc)) {
    if (caller %in% c("freq", "descr", "ctable")) {
      if (!isTRUE(test_choice(pf$style, 
                              c("simple", "grid", "rmarkdown")))) {
        msg %+=% "'style' must be one of 'simple', 'grid', or 'markdown'"
      }
    } else if (!isTRUE(test_choice(pf$style, 
                                   c("grid", "rmarkdown")))) {
      msg %+=% "'style' must be either 'grid' or 'markdown'"
    }
  }
  
  if ('plain.ascii' %in% names(mc) && 
      !isTRUE(test_logical(pf$plain.ascii, 
                           len = 1, any.missing = FALSE))) {
    msg %+=% "'plain.ascii' must be either TRUE or FALSE"
  }
  
  if ('justify' %in% names(mc)) {
    if (caller == 'freq') {
      if (!isTRUE(test_string(pf$justify, min.chars = 1)) ||
          !isTRUE(test_choice(substr(pf$justify,1,1), 
                              c("l", "c", "m", "r", "d")))) {
        msg %+=% "'justify' must be one of 'l', 'c', 'r', or 'd' (default)"
      }
    } else if (!isTRUE(test_choice(pf$justify, 
                                   c("l", "c", "m", "r")))) {
      msg %+=% "'justify' must be one of 'l', 'c', 'r'"
    }
    
    justify <- switch(substring(pf$justify, 1, 1),
                      l = "left",
                      c = "center",
                      m = "center",
                      d = "default",
                      r = "right")
    
    assign("justify", justify, parent.frame())
  }
  
  if ('display.labels' %in% names(mc) &&
      !isTRUE(test_logical(pf$display.labels, 
                           len = 1, any.missing = FALSE))) {
    msg %+=% "'display.labels' must be either TRUE or FALSE"
  }
  
  if ('headings' %in% names(mc) &&
      !isTRUE(test_logical(pf$headings, 
                           len = 1, any.missing = FALSE))) {
    msg %+=% "'headings' must be either be TRUE or FALSE"
  }
  
  # freq-specific  arguments ---------------------------------------------------
  if (caller == "freq") {
    
    if ('report.nas' %in% names(mc) && 
        !isTRUE(test_logical(pf$report.nas, 
                             len = 1, any.missing = FALSE))) {
      msg %+=% "'report.nas' must be either TRUE or FALSE"
    }
    
    if ('display.type' %in% names(mc) &&
        !isTRUE(test_logical(pf$display.type, 
                             len = 1, any.missing = FALSE))) {
      msg %+=% "'display.type' must be either TRUE or FALSE"
    }
    
    if ('order' %in% names(mc)) {
      
      order <- switch(tolower(substring(pf$order, 1, 1)),
                      d = "default",
                      l = "levels",
                      f = "freq",
                      n = "names")
      
      if (!isTRUE(test_choice(order, 
                              c("default", "levels", "freq", "names")))) {
        msg %+=% paste("'order' must be one of 'default', 'levels',",
                       "'freq', or 'names'")
      } else if (order == "levels" && !is.factor(pf$x)) {
        msg %+=% paste("'order' can be set to 'factor' only for factors.",
                       "Use 'names' or 'freq', or convert object to factor",
                       "prior to calling freq()")
      }
      assign("order", order, envir = parent.frame())
    }
  }
  
  # freq & ctable arguments ----------------------------------------------------
  if (caller %in% c("freq", "ctable")) {
    if ('totals' %in% names(mc) && 
        !isTRUE(test_logical(pf$totals, 
                             len = 1, any.missing = FALSE))) {
      msg %+=% "'totals' must be either TRUE or FALSE"
    }
  }
  
  # freq & descr arguments -----------------------------------------------------
  if (caller %in% c("freq", "descr")) {
    if ('weights' %in% names(mc) && length(pf$weights) != length(pf$x)) {
      msg %+=% "'weights' must have same length as 'x'"      
    }
    
    if ('rescale.weights' %in% names(mc) &&
        !isTRUE(test_logical(pf$rescale.weights, 
                             len = 1, any.missing = FALSE))) {
      msg %+=% "'rescale.weights' must be either TRUE or FALSE"
    }
  }
  
  # ctable-specific arguments --------------------------------------------------
  if (caller == "ctable") {
    if ('prop' %in% names(mc)) {
      prop <- tolower(substr(pf$prop, 1, 1))
      if(!isTRUE(test_choice(prop, c('t', 'r', 'c', 'n')))) {
        msg %+=% "'prop' must be one of 't', 'r', 'c', or 'n'"
      }
      if (nchar(prop > 1)) {
        assign("prop", prop, envir = parent.frame())
      }
    }
    
    if ('useNA' %in% names(mc) &&
        !isTRUE(test_choice(pf$useNA, 
                            c("ifany", "always", "no")))) {
      msg %+=% "'useNA' must be one of 'ifany', 'always', or 'no'"
    }
    
    if ('dnn' %in% names(mc) &&
        !isTRUE(test_character(pf$dnn, any.missing = FALSE, 
                               len = 2, unique = TRUE))) {
      msg %+=% "'dnn' must be a character vector of 2 distinct values"
    }
  }
  
  # descr arguments ------------------------------------------------------------
  if (caller == "descr") {
    if ('na.rm' %in% names(mc) &&
        !isTRUE(test_logical(pf$na.rm, 
                             len = 1, any.missing = FALSE))) {
      msg %+=% "'na.rm' must be either TRUE or FALSE"
    }
    
    if ('transpose' %in% names(mc) &&
        !isTRUE(test_logical(pf$transpose))) {
      msg %+=% "'transpose' must be either TRUE or FALSE"
    }
  }
  
  # dfSummary arguments --------------------------------------------------------
  if (caller == "dfSummary") {
    if ('varnumbers' %in% names(mc) &&
        !isTRUE(test_logical(pf$varnumbers, 
                             len = 1, any.missing = FALSE))) {
      msg %+=% "'varnumbers' must be either TRUE or FALSE"
    }
    
    if ('labels.col' %in% names(mc) &&
        !isTRUE(test_logical(pf$labels.col, 
                             len = 1, any.missing = FALSE))) {
      msg %+=% "'labels.col' must be either TRUE or FALSE"
    }
    
    if ('valid.col' %in% names(mc) &&
        !isTRUE(test_logical(pf$valid.col, 
                             len = 1, any.missing = FALSE))) {
      msg %+=% "'valid.col' must be either TRUE or FALSE"
    }
    
    if ('na.col' %in% names(mc) &&
        !isTRUE(test_logical(pf$na.col, 
                             len = 1, any.missing = FALSE))) {
      msg %+=% "'na.col' must be either TRUE or FALSE"
    }
    
    if ('graph.col' %in% names(mc) &&
        !isTRUE(test_logical(pf$graph.col, 
                             len = 1, any.missing = FALSE))) {
      msg %+=% "'graph.col' must be either TRUE or FALSE"
    }
    
    if ('graph.magnif' %in% names(mc) && pf$graph.magnif <= 0) {
      msg %+=% "'graph.magnif' must be > 0"
    }
    
    if ('style' %in% names(mc) && pf$style == "rmarkdown") {
      message("'rmarkdown' style not supported - using 'multiline' instead")
      assign("style", "multiline", envir = parent.frame())
    }
    
    if ('trim.strings' %in% names(mc) &&
        !isTRUE(test_logical(pf$trim.strings, 
                             len = 1, any.missing = FALSE))) {
      msg %+=% "'trim.strings' must be either TRUE or FALSE"
    }
  }
  
  # Order the messages according to arguments order
  ord <- numeric()
  for(a in names(mc)[-1]) {
    ord %+=% grep(pattern = a, 
                  x = sub(pattern = "^'(.+?)'.+$", 
                          replacement = "\\1", x = msg, 
                          perl = TRUE), 
                  fixed = TRUE)
  }
  
  return(msg[ord])
}


#' @importFrom checkmate test_int test_logical test_choice 
#' test_file_exists test_character
check_arguments_st_options <- function(mc) {
  
  pf <- parent.frame()
  msg <- character()
  
  if ('style' %in% names(mc)) {
    if (!isTRUE(test_choice(pf$style, 
                            c("simple", "grid", "rmarkdown")))) {
      msg %+=% "'style' must be one of 'simple', 'grid', or 'markdown'"
    }
  }
  
  if ('round.digits' %in% names(mc) && !isTRUE(test_int(pf$round.digits))) {
    msg %+=% "'round.digits' must be a whole number"
  }
  
  if ('plain.ascii' %in% names(mc) && 
      !isTRUE(test_logical(pf$plain.ascii, len = 1, any.missing = FALSE))) {
    msg %+=% "'plain.ascii' must be either TRUE or FALSE"
  }
  
  if ('headings' %in% names(mc) &&
      !isTRUE(test_logical(pf$headings, len = 1, any.missing = FALSE))) {
    msg %+=% "'headings' must be either be TRUE or FALSE"
  }
  
  if ('footnote' %in% names(mc) &&
      !isTRUE(test_character(pf$footnote)) && !is.na(pf$footnote)) {
    msg %+=% "'footnote' must be either be a string or NA"
  }
  
  if ('display.labels' %in% names(mc) &&
      !isTRUE(test_logical(pf$display.labels, 
                           len = 1, any.missing = FALSE))) {
    msg %+=% "'display.labels' must be either TRUE or FALSE"
  }
  
  if ('bootstrap.css' %in% names(mc) &&
      !isTRUE(test_logical(pf$bootstrap.css, 
                           len = 1, any.missing = FALSE))) {
    msg %+=% "'bootstrap.css' must be either TRUE or FALSE"
  }
  
  if ('custom.css' %in% names(mc) && !is.na(pf$custom.css) &&
      !isTRUE(test_file_exists(pf$custom.css, access = "r"))) {
    msg %+=% "'custom.css' file not found"
  }
  
  if ('escape.pipe' %in% names(mc) &&
      !isTRUE(test_logical(pf$escape.pipe, len = 1, any.missing = FALSE))) {
    msg %+=% "'escape.pipe' must be either TRUE or FALSE"
  }
  
  if ('freq.totals' %in% names(mc) &&
      !isTRUE(test_logical(pf$freq.totals, len = 1, any.missing = FALSE))) {
    msg %+=% "'freq.totals' must be either TRUE or FALSE"
  }
  
  if ('freq.report.nas' %in% names(mc) &&
      !isTRUE(test_logical(pf$freq.report.nas, len = 1, any.missing = FALSE))) {
    msg %+=% "'freq.report.nas' must be either TRUE or FALSE"
  }
  
  if ('ctable.prop' %in% names(mc) &&
      !isTRUE(test_logical(pf$ctable.prop, len = 1, any.missing = FALSE))) {
    msg %+=% "'ctable.prop' must be either TRUE or FALSE"
  }
  
  if ('ctable.totals' %in% names(mc) &&
      !isTRUE(test_logical(pf$ctable.totals, len = 1, any.missing = FALSE))) {
    msg %+=% "'ctable.totals' must be either TRUE or FALSE"
  }
  
  if ('descr_stats' %in% names(mc)) {
    valid_stats <- c("mean", "sd", "min", "q1", "med", "q3","max", "mad", 
                     "iqr", "cv", "skewness", "se.skewness", "kurtosis", 
                     "n.valid", "pct.valid")
    
    if (length(pf$descr_stats) == 1 && 
        !(pf$descr_stats %in% c("fivevnum", "common")) &&
        !(pf$descr_stats %in% valid_stats)) {
      msg %+=%
        paste("'descr_stats' value", dQuote(pf$descr_stats), "not recognized;",
              "allowed values are: ", 
              paste('"fivenum", "common", or a combination of :',
                    paste0(dQuote(valid_stats), sep = ", ")))
    }
  }
  
  if ('descr.transpose' %in% names(mc) &&
      !isTRUE(test_logical(pf$descr.transpose, len = 1, any.missing = FALSE))) {
    msg %+=% "'descr.transpose' must be either TRUE or FALSE"
  }
  
  if ('freq.totals' %in% names(mc) &&
      !isTRUE(test_logical(pf$freq.totals, 
                           len = 1, any.missing = FALSE))) {
    msg %+=% "'freq.totals' must be either TRUE or FALSE"
  }
  
  if ('dfSummary.varnumbers' %in% names(mc) &&
      !isTRUE(test_logical(pf$dfSummary.varnumbers, 
                           len = 1, any.missing = FALSE))) {
    msg %+=% "'dfSummary.varnumbers' must be either TRUE or FALSE"
  }
  
  if ('dfSummary.labels.col' %in% names(mc) &&
      !isTRUE(test_logical(pf$dfSummary.labels.col,
                           len = 1, any.missing = FALSE))) {
    msg %+=% "'dfSummary.labels.col' must be either TRUE or FALSE"
  }
  if ('dfSummary.valid.col' %in% names(mc) &&
      !isTRUE(test_logical(pf$dfSummary.valid.col,
                           len = 1, any.missing = FALSE))) {
    msg %+=% "'dfSummary.valid.col' must be either TRUE or FALSE"
  }
  if ('dfSummary.na.col' %in% names(mc) &&
      !isTRUE(test_logical(pf$dfSummary.na.col, 
                           len = 1, any.missing = FALSE))) {
    msg %+=% "'dfSummary.na.col' must be either TRUE or FALSE"
  }
  
  if ('dfSummary.graph.col' %in% names(mc) &&
      !isTRUE(test_logical(pf$dfSummary.graph.col,
                           len = 1, any.missing = FALSE))) {
    msg %+=% "'dfSummary.graph.col' must be either TRUE or FALSE"
  }
  
  if ('dfSummary.graph.magnif' %in% names(mc) && 
      pf$dfSummary.graph.magnif <= 0) {
    msg %+=% "'dfSummary.graph.magnif' must be > 0"
  }
  
  return(msg)
}


# Fn to smartly split variable names that are too long
# ref: https://tinyurl.com/y7qv48z9
#' @keywords internal
smart_split <- function(str, maxlen) {
  re <- paste0("(?=.{1,", maxlen, "}(.*))",
               "(?=.*?[^\\W._].*?[\\W._].*?\\1)",
               ".{1,", maxlen, "}(?<=_|\\b|\\Z)",
               "|.{1,", maxlen, "}")
  matchinfo <- gregexpr(pattern = re,
                        text = str, perl = TRUE)
  groups <- regmatches(x = str, m = matchinfo)[[1]]
  paste(groups, collapse = "<br/>")
}

# infix to simplify append()ing
#' @keywords internal
`%+=%`<- function(x, value) {
  eval.parent(substitute(x <- append(x, value)))
}

# Remove quotation marks inside a string
#' @keywords internal
unquote <- function(x) {
  x <- sub("^\\'(.+)\\'$", '\\1', x)
  x <- sub('^\\"(.+)\\"$', '\\1', x)
  x
}

# Replace accentuated characters by their html decimal entity
#' @keywords internal
conv_non_ascii <- function(...) {
  out <- character()
  for (s in list(...)) {
    splitted <- unlist(strsplit(s, ""))
    intvalues <- utf8ToInt(enc2utf8(s))
    pos_to_modify <- which(intvalues >=161 & intvalues <= 255)
    splitted[pos_to_modify] <- paste0("&#0",  intvalues[pos_to_modify], ";")
    out <- c(out, paste0(splitted, collapse = ""))
  }
  out
}


# Shorcut function to get translation strings
#' @keywords internal
trs <- function(item, l = st_options('lang')) {
  tr[force(l),item]
}
