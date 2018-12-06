add_hash <- function(str, h = 4) {
  if (isTRUE(parent.frame(2)$format_info$plain.ascii)) {
    return(str)
  } else {
    if (h > 0) {
      return(paste(paste0(rep(x = "#", times = h), collapse = ""), str))
    } else {
      str_1 <- sub(pattern = "^(.*?:)(.+)$", replacement = "\\1", x = str,
                   perl = TRUE)
      str_2 <- sub(pattern = "^(.*?:)(.+)$", replacement = "\\2", x = str, 
                   perl = TRUE)
      return(paste0("**", str_1, "**", str_2))
    }
  }
}

add_head_element <- function(elements, h = 4) {
  
  # initialise output variable for pander method  
  to_append_pander <- list()
  
  # initialise output variable for html/render methods
  to_append_html <- ""
  
  for (e in elements) {
    
    if (e[[1]] %in% names(parent.frame()$data_info)) {
      if ((grepl(pattern = "label", e[[1]]) && 
           isTRUE(parent.frame()$format_info$display.labels)) ||
          (e[[1]] == 'Data.type' && 
           isTRUE(parent.frame()$format_info$display.type)) ||
          !grepl("(label|Data\\.type)", e[[1]])) {
        
        if (parent.frame()$method == "pander") {
          to_append_pander <- 
            append(to_append_pander,
                   paste0("  \n", 
                          add_hash(paste(e[[2]], 
                                         parent.frame()$data_info[[e[[1]]]],
                                         sep = ": "), h), "  "))
        } 
        
        # html / render methods
        else {
          div_str_item <- paste(paste0("<strong>",e[[2]],"</strong>"),
                                parent.frame()$data_info[[e[[1]]]], sep = ": ")
          
          if (nchar(to_append_html) > 0) {
            to_append_html <- paste(to_append_html,
                                    div_str_item,
                                    sep = "\n  <br>")
          } else {
            to_append_html <- div_str_item
          }
        }
      }
    }
  }
  
  # Further process output if method html/render
  if (parent.frame()$method != "pander") {
    if (nchar(trimws(to_append_html)) > 0) {
      if (h == 0) {
        to_append_html <- HTML(to_append_html)
      } else {
        to_append_html <- get(paste0("h", h))(HTML(to_append_html))
      }
    }
    
    return(to_append_html)
    
  } else {
    
    return(to_append_pander)
    
  }
}

# Function to vertically align frequencies and proportions in ctables
align_numbers <- function(counts, props) {
  round.digits <- parent.frame()$format_info$round.digits
  res <- sapply(seq_len(ncol(counts)), function(colnum) {
    
    if ("Weights" %in% names(parent.frame()$data_info)) {
      maxchar_cnt <- nchar(as.character(round(max(counts[ ,colnum]), 
                                              digits = round.digits)))
      maxchar_pct <- nchar(sprintf(paste0("%.", round.digits, "f"),
                                   max(props[ ,colnum]*100)))
      return(paste(sprintf(paste0("%", maxchar_cnt, ".", round.digits, "f"),
                           counts[ ,colnum]),
                   sprintf(paste0("(%", maxchar_pct, ".", round.digits, "f%%)"),
                           props[ ,colnum]*100)))
    } else {
      maxchar_cnt <- nchar(as.character(max(counts[ ,colnum])))
      maxchar_pct <- nchar(sprintf(paste0("%.", round.digits, "f"), 
                                   max(props[ ,colnum]*100)))
      return(paste(sprintf(paste0("%", maxchar_cnt, "i"),
                           counts[ ,colnum]),
                   sprintf(paste0("(%", maxchar_pct, ".", round.digits, "f%%)"),
                           props[ ,colnum]*100)))
    }
  })
  
  dim(res) <- dim(counts)
  dimnames(res) <- dimnames(counts)
  
  return(res)
}

# Fn to smartly split variable names that are too long
# ref: https://tinyurl.com/y7qv48z9
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


# Prepare freq objects for printing --------------------------------------------
#' @import htmltools
prep_freq <- function(x, method) {

  # prep_freq -- Common section  -----------------------------------------------
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  # Build headers section
  title_sect  <- list()
  
  if (!isTRUE(format_info$var.only)) {
    if ("Weights" %in% names(data_info)) {
      title_sect[[1]] <- "Weighted Frequencies  "
    } else {
      title_sect[[1]] <- "Frequencies  "
    }
  } else {
    title_sect[[1]] <- ""
  }
  
  if (all(c("Variable", "Dataframe") %in% names(data_info)) &&
      !"Variable" %in% parent.frame()$overrided_args) {
    title_sect[[2]] <- paste(data_info$Dataframe, 
                             data_info$Variable, sep = "$")
  } else if ("Variable" %in% names(data_info)) {
    title_sect[[2]] <- data_info$Variable
  } else {
    title_sect[[2]] <-""
  }
  
  if (!isTRUE(format_info$report.nas)) {
    x[nrow(x), 1] <- x[nrow(x), 1] - x[nrow(x) -1, 1]
    x <- x[-(nrow(x)-1),1:3]
    colnames(x) <- c("Freq", "%", "% Cum.")
  }
  
  if (!isTRUE(format_info$totals)) {
    x <- x[-nrow(x),]
  }
  
  if(method=="pander") {
    
    # prep_freq -- pander section ----------------------------------------------
    main_sect <- list()
    to_append <- list()
    
    # Handle group.only / var.only (by() / lapply())
    if (isTRUE(format_info$group.only) ||
        ("by.first" %in% names(data_info) && 
         (!isTRUE(data_info$by.first) || !isTRUE(format_info$headings)))) {
      
      to_append <- add_head_element(list(c("Group", "Group")), h = 0)
      
    } else if (isTRUE(format_info$var.only)) {
      
      if (title_sect[[2]] != "") {
        if (isTRUE(format_info$plain.ascii)) {
          main_sect[[length(main_sect) + 1]] <- 
            paste0("\nVariable: ", title_sect[[2]], "  ")
        } else {
          main_sect[[length(main_sect) + 1]] <- 
            paste0("\n**Variable:** ", title_sect[[2]], "  ")
        }
      }
      
      to_append <- add_head_element(list(c("Variable.label", "Label"),
                                         c("Data.type", "Type")),
                                    h = 0)
    } else {
      
      # No by() or lapply() involved
      
      if (isTRUE(format_info$headings)) {
        if (title_sect[[1]] != "") {
          main_sect[[1]] <- add_hash(title_sect[[1]], 3)
        }
        
        if (title_sect[[2]] != "") {
          if (isTRUE(format_info$plain.ascii)) {
            main_sect[[length(main_sect) + 1]] <- 
              paste0("\n", title_sect[[2]], "  ")
          } else {
            main_sect[[length(main_sect) + 1]] <- 
              paste0("\n**Variable:** ", title_sect[[2]], "  ")
          }
        }
        
        to_append <- add_head_element(list(c("Variable.label", "Label"),
                                           c("Data.type", "Type"),
                                           c("Weights", "Weights"),
                                           #c("Subset", "Subset"),
                                           c("Group", "Group")),
                                      h = 0)
      }
    }
    
    if (!identical(to_append, list())) {
      main_sect <- append(main_sect, to_append)
    }

    justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                      l = "left",
                      c = "centre",
                      d = "right",
                      r = "right")
    
    freq_table <- format(x = round(x, format_info$round.digits),
                         trim = FALSE,
                         nsmall = format_info$round.digits,
                         justify = justify)
    
    if (isTRUE(format_info$report.nas)) {
      # Put NA in relevant cells so that pander recognizes them as such
      freq_table[nrow(freq_table) - 
                   as.numeric(isTRUE(format_info$totals)), 2] <- NA
      freq_table[nrow(freq_table) -
                   as.numeric(isTRUE(format_info$totals)), 3] <- NA
    }
    
    # Remove .00 digits in Freq column when weights are not used
    if (!"Weights" %in% names(data_info))
      freq_table[ ,1] <- sub("\\.0+", "", freq_table[,1])
    
    
    # Escape "<" and ">" when used in pairs in rownames
    if (!isTRUE(format_info$plain.ascii)) {
      row.names(freq_table) <- gsub(pattern = "\\<(.*)\\>", 
                                    replacement = "\\\\<\\1\\\\>",
                                    x = row.names(freq_table), perl = TRUE)
    }
    
    # set encoding to native to allow proper of accentuated characters
    if (parent.frame()$file == "") {
      row.names(freq_table) <- enc2native(row.names(freq_table))
    }
    
    pander_args <- append(list(style        = format_info$style,
                               plain.ascii  = format_info$plain.ascii,
                               justify      = justify,
                               missing      = format_info$missing,
                               split.tables = Inf),
                          attr(x, "user_fmt"))
    
    main_sect[[length(main_sect) + 1]]  <-
      paste(
        capture.output(
          do.call(pander, append(pander_args, list(x = quote(freq_table))))
        ),
        collapse = "\n")
    
    if (isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- gsub("\\|","\\\\|", 
                                             main_sect[[length(main_sect)]])
    }
    
    return(list(main_sect = main_sect))
    
  } else {
    
    # prep_freq -- viewer-browser-render section -------------------------------
    
    table_head <- list()
    to_append <- ""
    
    justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                      l = "left",
                      c = "center",
                      d = "center",
                      r = "right")
    
    table_rows <- list()
    
    for (ro in seq_len(nrow(x))) {
      table_row <- list()
      for (co in seq_len(ncol(x))) {
        if (co == 1) {
          table_row[[length(table_row) + 1]] <- tags$th(row.names(x)[ro])
          if (!"Weights" %in% names(data_info)) {
            cell <- sub(pattern = "\\.0+", replacement = "", x[ro,co], 
                        perl = TRUE)
            table_row[[length(table_row) + 1]] <- tags$td(cell, align = justify)
            next
          }
        }
        
        if (is.na(x[ro,co])) {
          table_row[[length(table_row) + 1]] <-
            tags$td(format_info$missing, align = justify)
        } else {
          cell <- sprintf(paste0("%.", format_info$round.digits, "f"), x[ro,co])
          table_row[[length(table_row) + 1]] <- tags$td(cell, align = justify)
        }
        
        if (co == ncol(x)) {
          table_rows[[length(table_rows) + 1]] <- tags$tr(table_row)
        }
      }
    }
    
    if (isTRUE(format_info$report.nas)) {
      table_head[[1]] <- list(tags$th("", colspan = 2),
                              tags$th("Valid", colspan = 2),
                              tags$th("Total", colspan = 2))
      table_head[[2]] <- list(tags$th(data_info$Variable),
                              tags$th("Freq"),
                              tags$th("%"),
                              tags$th(HTML("% Cumul")),
                              tags$th("%"),
                              tags$th(HTML("% Cumul")))
      
      freq_table_html <-
        tags$table(
          tags$thead(tags$tr(table_head[[1]]),
                     tags$tr(table_head[[2]])),
          tags$tbody(table_rows),
          class = paste(
            "table table-striped table-bordered",
            "st-table st-table-striped st-table-bordered st-freq-table",
            ifelse(is.na(parent.frame()$table.classes), 
                   "", parent.frame()$table.classes)
            )
          )
      
    } else {
      
      # no reporting of missing values (NA)
      
      table_head <- list(tags$th(data_info$Variable),
                         tags$th("Freq"),
                         tags$th("%"),
                         tags$th(HTML("% Cumul")))
      
      freq_table_html <-
        tags$table(
          tags$thead(tags$tr(table_head)),
          tags$tbody(table_rows),
          class = paste(
            "table table-striped table-bordered",
            "st-table st-table-striped st-table-bordered st-freq-table-nomiss",
            ifelse(is.na(parent.frame()$table.classes),
                   "", parent.frame()$table.classes)
          )
        )
    }
    
    # Cleanup extra spacing and linefeeds in html to correct layout issues
    freq_table_html <- as.character(freq_table_html)
    freq_table_html <- gsub(pattern = "\\s*(\\d*)\\s*(<span|</td>)",
                            replacement = "\\1\\2", x = freq_table_html,
                            perl = TRUE)
    freq_table_html <- gsub(pattern = "</span>\\s*</span>",
                            replacement = "</span></span>",
                            x = freq_table_html,
                            perl = TRUE)
    
    # Prepare the main "div" for the html report
    div_list <- list()

    # Deal with group.only and var.only when by() or lapply() has been used
    if (isTRUE(format_info$group.only) ||
        ("by.first" %in% names(data_info) && 
         (!isTRUE(data_info$by.first) || !isTRUE(format_info$headings)))) {
      
      to_append <- add_head_element(list(c("Group", "Group")), h = 0)
      if (to_append != "") {
        div_list[[length(div_list) + 1]] <- to_append
      }
      
    } else if (isTRUE(format_info$var.only)) {
      
      if (isTRUE(format_info$headings)) {
        if (title_sect[[2]] != "") {
          if (method != "render") {
            div_list[[length(div_list) + 1]] <- h4(title_sect[[2]])
            to_append  <-
              add_head_element(list(c("Variable.label", "Variable Label"),
                                    c("Data.type", "Type")), h = 0)
          } else {
            to_append <- 
              add_head_element(list(c("Variable", "Variable"),
                                    c("Variable.label", "Variable Label"),
                                    c("Data.type", "Type")), h = 0)
          }
        }
      }
      
      if (to_append != "") {
        div_list[[length(div_list) + 1]] <- to_append
      }
      
      div_list[[length(div_list) + 1]] <- HTML(text = "<br/>")
      
    } else {
      
      # No by() / lapply()
      
      if (isTRUE(format_info$headings)) {
        
        if (title_sect[[1]] != "") {
          div_list[[1]] <- h3(title_sect[[1]])
        }
        
        if (title_sect[[2]] != "") {
          if (method != "render") {
            div_list[[length(div_list) + 1]] <- h4(title_sect[[2]])
            to_append <- add_head_element(list(c("Variable.label", "Label"),
                                               c("Data.type", "Type"),
                                               c("Weights", "Weights"),
                                               c("Group", "Group")),
                                          h = 0)
          } else {
            to_append <- add_head_element(list(c("Variable", "Variable"),
                                               c("Variable.label", "Label"),
                                               c("Data.type", "Type"),
                                               c("Weights", "Weights"),
                                               c("Group", "Group")),
                                          h = 0)
          }
        }
        
        if (to_append != "") {
          div_list[[length(div_list) + 1]] <- to_append
        }
        
        div_list[[length(div_list) + 1]] <- HTML(text = "<br/>")
      }
    }
    
    div_list[[length(div_list) + 1]] <- HTML(text = freq_table_html)
    
    if (parent.frame()$footnote != "") {
      div_list[[length(div_list) + 1]] <- HTML(text = parent.frame()$footnote)
    }
  }
  return(list(title_sect = title_sect, div_list = div_list))
}


#' @import htmltools
prep_ctable <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  title_sect  <- list()
  
  title_sect[[1]] <- "Cross-Tabulation"
  
  if (!isTRUE(format_info$totals)) {
    x$cross_table <- x$cross_table[which(rownames(x$cross_table) != 'Total'), 
                                   which(colnames(x$cross_table) != 'Total')]
    if (attr(x, "proportions") != "None") {
      x$proportions <- x$proportions[which(rownames(x$proportions) != 'Total'), 
                                     which(colnames(x$proportions) != 'Total')]
    }
  }
  
  if(attr(x, "proportions") %in% c("Row", "Column", "Total")) {
    title_sect[[1]] <- paste0(title_sect[[1]], " / ",
                              attr(x, "proportions"), " Proportions")
    cross_table <- align_numbers(x$cross_table, x$proportions)
  } else {
    cross_table <- x$cross_table
  }
  
  title_sect[[2]] <- data_info$Row.x.Col
  
  if(method == "pander") {
    
    main_sect <- list()
    to_append <- list()
    
    if (isTRUE(format_info$group.only)  ||
        ("by.first" %in% names(data_info) && !isTRUE(data_info$by.first))) {
      to_append <- add_head_element(list(c("Group", "Group")), h = 0)
      
    } else {
      
      if (isTRUE(format_info$headings)) {
        
        main_sect[[1]] <- add_hash(title_sect[[1]], 3)
        
        to_append <- add_head_element(list(c("Row.x.Col", "Variables"),
                                           c("Dataframe", "Data Frame"),
                                           c("Dataframe.label", "Label"),
                                           c("Group", "Group")),
                                      h = 0)
      }
    }
    
    if(!identical(to_append, list())) {
      main_sect <- append(main_sect, to_append)
    }
    
    main_sect[[length(main_sect) + 1]] <- "\n  "
    
    # Escape "<" and ">" when used in pairs in rownames or colnames
    if (!isTRUE(format_info$plain.ascii)) {
      row.names(cross_table) <-
        gsub(pattern = "\\<(.*)\\>", replacement = "\\\\<\\1\\\\>",
             x = row.names(cross_table), perl = TRUE)
      colnames(cross_table) <- 
        gsub(pattern = "\\<(.*)\\>", replacement = "\\\\<\\1\\\\>",
             x = colnames(cross_table), perl = TRUE)
    }
    
    pander_args <- append(list(style        = format_info$style,
                               plain.ascii  = format_info$plain.ascii,
                               justify      = format_info$justify,
                               split.tables = format_info$split.tables),
                          attr(x, "user_fmt"))
    
    # do not use arg keep.trailing.zeros = TRUE b/c of issue with pander+ftable
    main_sect[[length(main_sect) + 1]] <-
      paste(
        capture.output(
          do.call(pander, append(pander_args, 
                                 list(x = quote(ftable(cross_table)))))
        ),
        collapse = "\n")
    
    if (isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect)]])
    }
    
    return(list(title_sect = title_sect, main_sect = main_sect))
    
  } else {
    
    # ctable objects - method viewer / browser / render  -----------------------
    
    dnn <- names(dimnames(cross_table))
    
    table_head <- list()
    table_head[[1]] <- 
      list(tags$th(""), tags$th(dnn[2], colspan = ncol(cross_table) - 
                                  as.numeric(isTRUE(format_info$totals))))
    
    if (isTRUE(format_info$totals)) {
      table_head[[1]][[3]] <- tags$th("")
    }
    
    table_head[[2]] <-  list(tags$td(tags$strong(dnn[1]), align = "center"))
    
    for(cn in colnames(cross_table)) {
      if (nchar(cn) > 12) {
        cn <- smart_split(cn, 12)
      }
      cn <- sub("<", "&lt;", cn, fixed = TRUE)
      cn <- sub(">", "&gt;", cn, fixed = TRUE)
      table_head[[2]][[length(table_head[[2]]) + 1]] <- 
        tags$th(HTML(cn), align = "center")
    }
    
    table_rows <- list()
    for (ro in seq_len(nrow(cross_table))) {
      table_row <- list()
      for (co in seq_len(ncol(cross_table))) {
        if (co == 1) {
          table_row[[length(table_row) + 1]] <-
            tags$td(tags$strong(row.names(cross_table)[ro]), align = "center")
        }
        
        # Case where no proportions exist
        if (length(x$proportions) == 0) {
          cell <- cross_table[ro,co]
          table_row[[length(table_row) + 1]] <- tags$td(tags$span(cell))
        } else {
          # Proportions exist
          cell <- sub("\\( *", "("     , cross_table[ro,co])
          cell <- sub(" *\\)", ")"     , cell)
          cell <- gsub(" "   , "&nbsp;", cell)
          cell <- sub("%"    , "&#37;" , cell, fixed = TRUE)
          
          table_row[[length(table_row) + 1]] <- tags$td(tags$span(HTML(cell)))
        }
        
        # On last col, insert row into table_rows list
        if (co == ncol(cross_table)) {
          table_rows[[length(table_rows) + 1]] <- tags$tr(table_row)
        }
      }
    }
    
    cross_table_html <-
      tags$table(
        tags$thead(
          tags$tr(table_head[[1]]),
          tags$tr(table_head[[2]])
        ),
        tags$tbody(
          table_rows
        ),
        class = paste(
          "table table-bordered st-table st-table-bordered st-cross-table",
          ifelse(is.na(parent.frame()$table.classes), "", 
                 parent.frame()$table.classes)
        )
      )
    
    div_list  <- list()
    to_append <- list()
    
    if (isTRUE(format_info$group.only)) {
      
      to_append <- add_head_element(list(c("Group", "Group")), h = 0)
      
    } else {
      
      if (isTRUE(format_info$headings)) {
        div_list[[1]] <- h3(title_sect[[1]])
        
        if (method == 'render') {
          to_append <- add_head_element(list(c("Row.x.Col", "Variables"),
                                             c("Dataframe", "Data Frame"),
                                             c("Dataframe.label", "Label"),
                                             c("Group", "Group")),
                                        h = 0)
          
        } else {
          div_list[[2]] <- h4(title_sect[[2]])
          to_append <- add_head_element(list(c("Dataframe", "Data Frame"),
                                             c("Dataframe.label", "Label"),
                                             c("Group", "Group")),
                                        h = 0)
        }
      }
    }
    
    if (!identical(to_append, list())) {
      div_list[[length(div_list) + 1]] <- to_append
    }
    
    div_list[[length(div_list) + 1]] <- cross_table_html
    
    if (parent.frame()$footnote != "") {
      div_list[[length(div_list) + 1]] <- HTML(text = parent.frame()$footnote)
    }
  }
  
  return(list(title_sect = title_sect, div_list = div_list))
}


#' @import htmltools
prep_descr <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  if(!isTRUE(parent.frame()$silent) && !isTRUE(format_info$group.only) && 
     (!"by.first" %in% names(data_info) || 
      as.logical(data_info$by.last) == TRUE) &&
     "ignored" %in% names(attributes(x))) {
    message("Non-numerical variable(s) ignored: ", attr(x, "ignored"))
  }
  
  title_sect <- list()
  
  if ("Weights" %in% names(data_info)) {
    title_sect[[1]] <- "Weighted Descriptive Statistics  "
  } else {
    title_sect[[1]] <- "Descriptive Statistics  "
  }
  
  if (all(c("Variable", "Dataframe") %in% names(data_info))) {
    title_sect[[2]] <- paste(data_info$Dataframe, data_info$Variable, sep = "$")
  } else if ("Variable" %in% names(data_info)) {
    title_sect[[2]] <- data_info$Variable
  } else {
    title_sect[[2]] <- ""
  }
  
  if ("byvar" %in% names(data_info)) {
    title_sect[[2]] <- paste(title_sect[[2]], "by", data_info$byvar)
  }
  
  justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                    l = "left",
                    c = "center",
                    r = "right")
  
  if(method=="pander") {
    
    # descr object, pander method ----------------------------------------------
    
    main_sect    <- list()
    to_append <- list()
    
    if (isTRUE(format_info$group.only) ||
        ("by.first" %in% names(data_info) && 
         (!isTRUE(data_info$by.first) || !isTRUE(format_info$headings)))) {
      to_append <- add_head_element(list(c("Group", "Group"),
                                         c("N.Obs", "N")),
                                    h = 0)
    } else {
      
      if (isTRUE(format_info$headings)) {
        main_sect[[1]] <- add_hash(title_sect[[1]], 3)
        
        if (title_sect[[2]] != "") {
          if (isTRUE(format_info$plain.ascii)) {
            main_sect[[length(main_sect) + 1]] <- 
              paste0("\n", title_sect[[2]], "  ")
          } else {
            main_sect[[length(main_sect) + 1]] <- 
              paste0("\n**Variable:** ", title_sect[[2]], "  ")
          }
        }
        
        if ("Variable" %in% names(data_info)) {
          to_append <- add_head_element(list(c("Variable.label", "Label"),
                                             c("Weights", "Weights"),
                                             c("Group", "Group"),
                                             c("N.Obs", "N")),
                                        h = 0)
          
        } else {
          to_append <- add_head_element(list(c("Dataframe", "Data Frame"),
                                             c("Dataframe.label", "Label"),
                                             c("Weights", "Weights"),
                                             c("Group", "Group"),
                                             c("N.Obs", "N")),
                                        h = 0)
        }
      }
    }
    
    if (!identical(to_append, list())) {
      main_sect <- append(main_sect, to_append)
    }
    
    # Format numbers (avoids inconsistencies with pander rounding digits)
    x <- format(round(x, format_info$round.digits),
                nsmall = format_info$round.digits)
    
    pander_args <- append(list(style        = format_info$style,
                               plain.ascii  = format_info$plain.ascii,
                               split.tables = format_info$split.tables,
                               justify      = justify),
                          attr("x", "user_fmt"))
    
    main_sect[[length(main_sect) + 1]] <-
      paste(
        capture.output(
          do.call(pander, append(pander_args, list(x = quote(x))))
        ),
        collapse = "\n")
    
    if(isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect)]])
      main_sect[[length(main_sect) - 2]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect) - 2]])
    }
    
    return(list(title_sect = title_sect, main_sect = main_sect))
    
  } else {
    
    # descr objects - method viewer / browser / render -------------------------
    
    if ("byvar" %in% names(data_info) && !data_info$transpose) {
      table_head <- list()
      table_head[[1]] <- list(tags$th(""),
                              tags$th(data_info$byvar,
                                      colspan = ncol(x)))
      
      table_head[[2]] <-  list(tags$th()) 
      
      for(cn in colnames(x)) {
        if (nchar(cn) > 12) {
          cn <- smart_split(cn, 12)
        }
        cn <- sub("<", "&lt;", cn, fixed = TRUE)
        cn <- sub(">", "&gt;", cn, fixed = TRUE)
        table_head[[2]][[length(table_head[[2]]) + 1]] <- 
          tags$th(HTML(cn), align = "center")
      } 
      
    } else {
      
      table_head <- list(tags$th(""))
      
      for(cn in colnames(x)) {
        if (nchar(cn) > 12) {
          cn <- smart_split(cn, 12)
        }
        table_head[[length(table_head) + 1]] <- 
          tags$th(HTML(cn), align = "center")
      }
    }
    
    table_rows <- list()
    for (ro in seq_len(nrow(x))) {
      table_row <- list(tags$td(tags$strong(rownames(x)[ro])))
      for (co in seq_len(ncol(x))) {
        # cell is NA
        if (is.na(x[ro,co])) {
          table_row[[length(table_row) + 1]] <- tags$td(format_info$missing)
        } else if ((rownames(x)[ro] == "N.Valid" || 
                    colnames(x)[co] == "N.Valid") && 
                   !"Weights" %in% names(data_info)) {
          table_row[[length(table_row) + 1]] <-
            tags$td(tags$span(round(x[ro,co], 0)))
        } else {
          # When not NA, and not N.Valid row, format cell content
          cell <- sprintf(paste0("%.", format_info$round.digits, "f"), x[ro,co])
          table_row[[length(table_row) + 1]] <-
            tags$td(tags$span(cell))
        }
        # On last column, insert row to table_rows list
        if (co == ncol(x)) {
          table_rows[[length(table_rows) + 1]] <- tags$tr(table_row)
        }
      }
    }
    
    if ("byvar" %in% names(data_info) && !isTRUE(data_info$transpose)) {
      descr_table_html <-
        tags$table(
          tags$thead(
            tags$tr(table_head[[1]]),
            tags$tr(table_head[[2]])
          ),
          tags$tbody(
            table_rows
          ),
          class = paste(
            "table table-bordered table-striped",
            "st-table st-table-bordered st-table-striped st-freq-table",
            "st-descr-table",
            ifelse(is.na(parent.frame()$table.classes), "", 
                   parent.frame()$table.classes))
        )
      
    } else {
      
      descr_table_html <-
        tags$table(
          tags$thead(tags$tr(table_head)),
          tags$tbody(table_rows),
          class = paste(
            "table table-bordered table-striped",
            "st-table st-table-bordered st-table-striped st-descr-table",
            ifelse(is.na(parent.frame()$table.classes), "", 
                   parent.frame()$table.classes))
        )
    }
    
    # Cleanup some extra spacing & html linefeeds to avoid weirdness in layout
    # of source code
    descr_table_html <- as.character(descr_table_html)
    descr_table_html <- gsub(pattern = "\\s*(\\-?\\d*)\\s*(<span|</td>)",
                             replacement = "\\1\\2", x = descr_table_html,
                             perl = TRUE)
    descr_table_html <- gsub(pattern = "</span>\\s*</span>",
                             replacement = "</span></span>",
                             x = descr_table_html,
                             perl = TRUE)
    descr_table_html <- gsub(pattern = "<strong>\\s*</strong>",
                             replacement = "",
                             x = descr_table_html,
                             perl = TRUE)
    descr_table_html <- gsub(pattern = '(<td align="right">)\\s+(<)',
                             replacement = '\\1\\2',
                             x = descr_table_html,
                             perl = TRUE)
    
    div_list  <- list()
    to_append <- list()
    
    if (isTRUE(format_info$group.only) ||
        ("by.first" %in% names(data_info) && 
         (!isTRUE(data_info$by.first) || !isTRUE(format_info$headings)))) {
      
      to_append <- add_head_element(list(c("Group", "Group"),
                                         c("N.Obs", "N")),
                                    h = 0)
    } else {
      
      if (isTRUE(format_info$headings)) {
        
        div_list[[1]] <- h3(title_sect[[1]])
        
        if (title_sect[[2]] != "") {
          div_list[[2]] <- h4(title_sect[[2]])
        }
        
        if ("Variable" %in% names(data_info)) {
          to_append <- add_head_element(list(c("Variable.label", "Label"),
                                             c("Weights", "Weights"),
                                             c("Group", "Group"),
                                             c("N.Obs", "N")),
                                        h = 0)
        } else {
          to_append <- add_head_element(list(c("Dataframe", "Data Frame"),
                                             c("Dataframe.label", "Label"),
                                             c("Weights", "Weights"),
                                             c("Group", "Group"),
                                             c("N.Obs", "N")),
                                        h = 0)
        }
      }
    }
    
    if (!identical(to_append, list())) {
      div_list[[length(div_list) + 1]] <- to_append
    }
    
    div_list[[length(div_list) + 1]] <- HTML(text = descr_table_html)
    
    if (parent.frame()$footnote != "") {
      div_list[[length(div_list) + 1]] <- HTML(text = parent.frame()$footnote)
    }
  }
  return(list(title_sect = title_sect, div_list = div_list))
}


#' @import htmltools
prep_dfs <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  title_sect  <- list()
  title_sect[[1]] <- "Data Frame Summary  "
  
  if (("Dataframe" %in% names(data_info)) && (!is.null(data_info$Dataframe))) {
    title_sect[[2]] <- data_info$Dataframe
  }  else {
    title_sect[[2]] <- ""
  }

  # Remove Var number ("No") column if specified in call to print/view
  if ("No" %in% names(x) && "varnumbers" %in% names(format_info) && 
      !isTRUE(format_info$varnumbers)) {
    x <- x[ ,-which(names(x) == 'No')]
  }
  
  # Remove Label column if specified in call to print/view
  if ('Label' %in% names(x) && "labels.col" %in% names(format_info) && 
      !isTRUE(format_info$labels.col)) {
    x <- x[ ,-which(names(x) == 'Label')]
  }
    
  # Remove Valid column if specified in call to print/view
  if ('Valid' %in% names(x) && "valid.col" %in% names(format_info) && 
      !isTRUE(format_info$valid.col)) {
    x <- x[ ,-which(names(x) == 'Valid')]
  }
  
  # Remove Missing column if specified in call to print/view
  if ('Missing' %in% names(x) && "na.col" %in% names(format_info) && 
      !isTRUE(format_info$na.col)) {
    x <- x[ ,-which(names(x) == 'Missing')]
  }
  
  if (method == "pander") {
    
    # remove html graphs
    if ("Graph" %in% names(x)) {
      x <- x[ ,-which(names(x) == "Graph")]
    }

    # Remove graph if specified in call to print/view
    if ('Text Graph' %in% names(x) && "graph.col" %in% names(format_info) &&
        !isTRUE(format_info$graph.col)) {
      x <- x[ ,-which(names(x) == 'Text Graph')]
    }
    
    # Check that style is not 'simple'
    if (isTRUE(format_info$style == 'simple')) {
      format_info$style <- 'multiline'
    }
    
    main_sect <- list()
    to_append <- list()
    
    if (!isTRUE(format_info$plain.ascii)) {
      # Escape symbols for words between <>'s to allow <NA> or factor
      # levels such as <ABC> to be rendered correctly
      if("Label" %in% names(x)) {
        x[["Label"]] <-
          gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
               x = x[["Label"]], perl=TRUE)
      }
      
      x[["Stats / Values"]] <-
        gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
             x = x[["Stats / Values"]], perl=TRUE)
      
      x[["Freqs (% of Valid)"]] <-
        gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
             x = x[["Freqs (% of Valid)"]], perl=TRUE)
      
      
      # Remove leading characters used for alignment in plain.ascii 
      x[["Freqs (% of Valid)"]] <-
        gsub(pattern = "^\\\\ *", replacement = "",
             x = x[["Freqs (% of Valid)"]], perl=TRUE)
      
      x[["Freqs (% of Valid)"]] <- 
        gsub(pattern = "\\n\\\\ *", replacement = "\n",
             x = x[["Freqs (% of Valid)"]], perl=TRUE)
      
      # Remove txt histograms b/c not supported in rmarkdown (for now)
      if ("Text Graph" %in% names(x)) {
        x[["Text Graph"]][which(grepl('[:.]', x[["Text Graph"]]))] <- ""
      }
      
    }
    
    if (isTRUE(format_info$headings)) {
      if (title_sect[[1]] != "") {
        main_sect[[1]] <- add_hash(title_sect[[1]], 3)
      }
      
      if (title_sect[[2]] != "") {
        if (isTRUE(format_info$plain.ascii)) {
          main_sect[[length(main_sect) + 1]] <- 
            paste0("\n", title_sect[[2]], "  ")
        } else {
          main_sect[[length(main_sect) + 1]] <- 
            paste0("\n**", title_sect[[2]], "**  ")
        }
      }
      
      to_append <- add_head_element(list(c("Dataframe.label", "Label"),
                                         c("Dimensions", "Dimensions"),
                                         c("Duplicates", "Duplicates")),
                                    h = 0)
    }
    
    if (!identical(to_append, list())) {
      main_sect <- append(main_sect, to_append)
    }
    
    pander_args <- append(list(style        = format_info$style,
                               plain.ascii  = format_info$plain.ascii,
                               justify      = format_info$justify,
                               split.cells  = format_info$split.cells,
                               split.tables = format_info$split.tables,
                               keep.line.breaks = TRUE),
                          attr(x, "user_fmt"))
    
    main_sect[[length(main_sect) + 1]] <-
      paste(
        capture.output(
          do.call(pander, append(pander_args, list(x = quote(x))))
        ),
        collapse = "\n")
    
    if (isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect)]])
    }
    
    return(list(title_sect = title_sect, main_sect = main_sect))
    
  } else {
    
    # dfSummary objects - method viewer / browser / render ---------------------
    
    # remove text graph
    if ("Text Graph" %in% names(x)) {
      x <- x[ ,-which(names(x) == "Text Graph")]
    }

    # Remove graph if specified in call to print/view
    if ('Graph' %in% names(x) && "graph.col" %in% names(format_info) &&
        !isTRUE(format_info$graph.col)) {
      x <- x[ ,-which(names(x) == 'Graph')]
    }
    
    table_head <- list()
    for(cn in colnames(x)) {
      if (cn %in% c("No", "Valid", "Missing")) {
        table_head[[length(table_head) + 1]] <- tags$th(tags$strong(cn),
                                                        align = "center")
      } else {
        table_head[[length(table_head) + 1]] <- tags$th(tags$strong(cn),
                                                        align = "center")
      }
    }
    
    table_rows <- list()
    for (ro in seq_len(nrow(x))) {
      table_row <- list()
      for (co in seq_len(ncol(x))) {
        cell <- x[ro,co]
        cell <- gsub('\\\\\n', '\n', cell)
        if (colnames(x)[co] %in% c("No", "Valid", "Missing")) {
          table_row[[length(table_row) + 1]] <- tags$td(cell, align = "center")
        } else if (colnames(x)[co] == "Label") {
          cell <- gsub('(\\d+)\\\\\\.', '\\1.', cell)
          cell <- paste(strwrap(cell,width = format_info$split.cells, 
                                simplify = TRUE), collapse = "\n")
          table_row[[length(table_row) + 1]] <- tags$td(cell, align = "left")
        } else if (colnames(x)[co] %in% 
                   c("Variable", "Properties", "Stats / Values")) {
          cell <- gsub('(\\d+)\\\\\\.', '\\1.', cell)
          table_row[[length(table_row) + 1]] <- tags$td(cell, align = "left")
        } else if (colnames(x)[co] == "Freqs (% of Valid)") {
          cell <- gsub("\\\\", " ", cell)
          cell <- gsub(" *(\\d|\\:)", "\\1", cell)
          cell <- gsub("\\:", " : ", cell)
          table_row[[length(table_row) + 1]] <- tags$td(cell, align = "left")
        } else if (colnames(x)[co] == "Graph") {
          table_row[[length(table_row) + 1]] <- 
            tags$td(HTML(cell), align = "center", border = "0")
        }
      }
      
      table_rows[[length(table_rows) + 1]] <- tags$tr(table_row)
    }
    
    
    dfs_table_html <-
      tags$table(
        tags$thead(tags$tr(table_head)),
        tags$tbody(table_rows),
        class = paste(
          "table table-striped table-bordered",
          "st-table st-table-striped st-table-bordered st-multiline",
          ifelse(is.na(parent.frame()$table.classes), 
                 "", parent.frame()$table.classes)
        )
      )
    
    dfs_table_html <-
      gsub(pattern = '(<th.*?>)\\s+(<strong>.*?</strong>)\\s+(</th>)',
           replacement = "\\1\\2\\3", x = dfs_table_html)
    div_list  <- list()
    to_append <- list()
    
    if (isTRUE(format_info$headings)) {
      
      if (title_sect[[1]] != "") {
        div_list[[1]] <- h3(title_sect[[1]])
      }
      
      if (title_sect[[2]] != "") {
        div_list[[length(div_list) + 1]] <- h4(title_sect[[2]])
      }
      
      to_append <- add_head_element(list(c("Dataframe.label", "Label"),
                                         c("Dimensions", "Dimensions"),
                                         c("Duplicates", "Duplicates")),
                                    h = 0)
    }
    
    if (!identical(to_append, list())) {
      div_list[[length(div_list) + 1]] <- to_append
    }
    
    div_list[[length(div_list) + 1]] <- HTML(text = dfs_table_html)
    
    if (parent.frame()$footnote != "") {
      div_list[[length(div_list) + 1]] <- HTML(text = parent.frame()$footnote)
    }
  }
  return(list(title_sect = title_sect, div_list = div_list))
}
