# Prepare descr objects for printing
#' @import htmltools
#' @keywords internal
prep_descr <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  if(!isTRUE(parent.frame()$silent) && !isTRUE(format_info$group.only) && 
     (!"by.first" %in% names(data_info) || 
      isTRUE(as.logical(data_info$by.last))) &&
     "ignored" %in% names(attributes(x))) {
    message("Non-numerical variable(s) ignored: ", attr(x, "ignored"))
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
