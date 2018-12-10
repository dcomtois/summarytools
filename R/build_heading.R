#' @keywords internal
build_heading_pander <- function(format_info, data_info) {
  
  add_markup <- function(str, h) {
    if (!isTRUE(format_info$plain.ascii)) {
      if (h == 0) {
        str <- sub(pattern = "^(.*?:)(.+)$", replacement = "**\\1**\\2",
                   x = str, perl = TRUE)
      } else {
        str <- paste(paste0(rep(x = "#", times = h), collapse = ""), str)
      }
    }
    return(str)
  }
  
  append_items <- function(items, h = 0) {
    to_append <- c()
    for (item in items) {
      if (names(item) %in% names(data_info)) {
        if ((grepl(pattern = "label", names(item)) && 
             isTRUE(format_info$display.labels)) ||
            (names(item) == 'Data.type' && 
             isTRUE(format_info$display.type)) ||
            !grepl("(label|Data\\.type)", names(item))) {
          
          to_append <- append(to_append,
                              paste0("  \n", 
                                     add_markup(paste(item,
                                                      data_info[[names(item)]],
                                                      sep = ": "), h), 
                                     "  "))
        }
      }
    }
    return(paste(to_append, collapse = ""))
  }
  
  # Special cases where no primary heading (title) is needed
  if (isTRUE(format_info$var.only)) {
    head2 <- append_items(list(c(Variable       = "Variable"),
                               c(Variable.lable = "Label"),
                               c(Data.type      = "Type")))
    return(list(head2))
  }
  
  else if (isTRUE(format_info$group.only) ||
           ("by.first" %in% names(data_info) && 
            (!isTRUE(data_info$by.first) || !isTRUE(format_info$headings)))) {
    head2 <- append_items(list(c(Group = "Group")))
    return(list(head2))
  }
  
  else if (!isTRUE(format_info$headings)) {
    return(list())
  }
  
  # Regular cases - Build primary and secondary headings
  caller <- as.character(sys.call(-1))[1]
  head1  <- NA
  head2  <- NA
  
  if (caller == "prep_freq") {
    head1 <- ifelse("Weights" %in% names(data_info),
                    "Weighted Frequencies", "Frequencies")
    head1 <- add_markup(head1, h = 3)
    
    head2 <- append_items(list(c(Variable       = "Variable"),
                               c(Variable.label = "Label"),
                               c(Data.type      = "Type"),
                               c(Weights        = "Weights"),
                               c(Group          = "Group")))
  } else if (caller == "prep_ctable") {
    head1 <- switch(data_info$Proportions,
                    Row    = "Cross-Tabulation / Row Proportions",
                    Column = "Cross-Tabulation / Column Proportions",
                    Total  = "Cross-Tabulation / Total Proportions",
                    None   = "Cross-Tabulation")
    head1 <- add_markup(head1, h = 3)
    
    head2 <- append_items(list(c(Row.x.Col       = "Variables"),
                               c(Dataframe       = "Data Frame"),
                               c(Dataframe.label = "Label"),
                               c(Group           = "Group")))
    #head2 <- paste(head2, "\n")
  } else if (caller == "prep_descr") {
    head1 <- ifelse("Weights" %in% names(data_info),
                    "Weighted Descriptive Statistics", 
                    "Descriptive Statistics")
    head1 <- add_markup(head1, h = 3)
    
    if ("Variable" %in% names(data_info)) {
      head2 <- append_items(list(c(Variable       = "Variable"),
                                 c(Variable.label = "Label"),
                                 c(Weights        = "Weights"),
                                 c(Group          = "Group"),
                                 c(N.Obs          = "N")))
      
    } else {
      head2 <- append_items(list(c(Dataframe       = "Data Frame"),
                                 c(Dataframe.label = "Label"),
                                 c(Weights         = "Weights"),
                                 c(Group           = "Group"),
                                 c(N.Obs           = "N")))
    }
  } else if (caller == "prep_dfs") {
    head1 <- add_markup("Data Frame Summary", h = 3)
    
    head2 <- append_items(list(c(Dataframe       = "Data Frame"),
                               c(Dataframe.label = "Label"),
                               c(Dimensions      = "Dimensions"),
                               c(Duplicates      = "Duplicates")))
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
        if ((grepl(pattern = "label", names(item)) && 
             isTRUE(format_info$display.labels)) ||
            (names(item) == 'Data.type' && 
             isTRUE(format_info$display.type)) ||
            !grepl("(label|Data\\.type)", names(item))) {
          
          div_str_item <- paste(paste0("<strong>", item, "</strong>"),
                                data_info[[names(item)]], sep = ": ")
          
          if (identical(to_append_html, character())) {
            to_append_html <- div_str_item
          } else {
            to_append_html <- paste(to_append_html,
                                     div_str_item,
                                     sep = "\n  <br/>")
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
    head2 <- append_items(list(c(Variable       = "Variable"),
                               c(Variable.lable = "Label"),
                               c(Data.type      = "Type")))
    return(list(head2))
  }
  
  else if (isTRUE(format_info$group.only) ||
           ("by.first" %in% names(data_info) && 
            (!isTRUE(data_info$by.first) || !isTRUE(format_info$headings)))) {
    head2 <- append_items(list(c(Group = "Group")))
    
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
  
  if (caller == "prep_freq") {
    
    head1 <- h3(ifelse("Weights" %in% names(data_info),
                       "Weighted Frequencies", "Frequencies"))
    
    if ("Variable" %in% names(data_info)) {
      if (method == "render") {
        head2 <- strong(data_info$Variable)
      } else {
        head2 <- h4(data_info$Variable)
      }
    }
    
    head3 <- append_items(list(c(Variable.label = "Label"),
                               c(Data.type      = "Type"),
                               c(Weights        = "Weights"),
                               c(Group          = "Group")))
    
  } else if (caller == "prep_ctable") {
    
    head1 <- h3(switch(data_info$Proportions,
                       Row    = "Cross-Tabulation / Row Proportions",
                       Column = "Cross-Tabulation / Column Proportions",
                       Total  = "Cross-Tabulation / Total Proportions",
                       None   = "Cross-Tabulation"))
    
    if ("Row.x.Col" %in% names(data_info)) {
      if (method == "render") {
        head2 <- strong(data_info$Row.x.Col)
      } else {
        head2 <- h4(data_info$Row.x.Col)
      }
    }
    
    head3 <- append_items(list(c(Dataframe       = "Data Frame"),
                               c(Dataframe.label = "Label"),
                               c(Group           = "Group")))
    
  } else if (caller == "prep_descr") {
    
    head1 <- h3(ifelse("Weights" %in% names(data_info),
                       "Weighted Descriptive Statistics", 
                       "Descriptive Statistics"))
    
    if ("Variable" %in% names(data_info)) {
      
      if (method == "render") {
        head2 <- strong(data_info$Variable)
      } else {
        head2 <- h4(data_info$Variable)
      }

      head3 <- append_items(list(c(Variable.label = "Label"),
                                 c(Weights        = "Weights"),
                                 c(Group          = "Group"),
                                 c(N.Obs          = "N")))
    } else {
      if (method == "render") {
        head2 <- strong(data_info$Dataframe)
      } else {
        head2 <- h4(data_info$Dataframe)
      }
      
      head3 <- append_items(list(c(Dataframe.label = "Label"),
                                 c(Weights         = "Weights"),
                                 c(Group           = "Group"),
                                 c(N.Obs           = "N")))
    }
    
  } else if (caller == "prep_dfs") {
    
    head1 <- h3("Data Frame Summary")
    if (method == "render") {
      head2 <- strong(data_info$Dataframe)
    } else {
      head2 <- h4(data_info$Dataframe)
    }
    
    head3 <- append_items(list(c(Dataframe.label = "Label"),
                               c(Dimensions      = "Dimensions"),
                               c(Duplicates      = "Duplicates")))
  }
  
  tmp <- list(head1, head2, head3)
  return(tmp[which(!is.na(tmp))])
}
