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
