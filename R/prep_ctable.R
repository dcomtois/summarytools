# Prepare ctable objects for printing ------------------------------------------
#' @import htmltools
#' @keywords internal
prep_ctable <- function(x, method) {
  
  align_numbers <- function(counts, props) {
    res <- sapply(seq_len(ncol(counts)), function(colnum) {
      
      if ("Weights" %in% names(data_info)) {
        maxchar_cnt <- 
          nchar(as.character(round(max(counts[ ,colnum]), 
                                   digits = format_info$round.digits)))
        maxchar_pct <- 
          nchar(sprintf(paste0("%.", format_info$round.digits, "f"),
                        max(props[ ,colnum]*100)))

        return(paste(sprintf(paste0("%", maxchar_cnt, ".", 
                                    format_info$round.digits, "f"),
                             counts[ ,colnum]),
                     sprintf(paste0("(%", maxchar_pct, ".", 
                                    format_info$round.digits, "f%%)"),
                             props[ ,colnum]*100)))
      } else {
        maxchar_cnt <- nchar(as.character(max(counts[ ,colnum])))
        maxchar_pct <- nchar(sprintf(paste0("%.", format_info$round.digits,"f"), 
                                     max(props[ ,colnum]*100)))
        return(paste(sprintf(paste0("%", maxchar_cnt, "i"),
                             counts[ ,colnum]),
                     sprintf(paste0("(%", maxchar_pct, ".", 
                                    format_info$round.digits, "f%%)"),
                             props[ ,colnum]*100)))
      }
    })
    
    dim(res) <- dim(counts)
    dimnames(res) <- dimnames(counts)
    
    return(res)
  }
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  if (!isTRUE(format_info$totals)) {
    x$cross_table <- x$cross_table[which(rownames(x$cross_table) != 'Total'), 
                                   which(colnames(x$cross_table) != 'Total')]
    if (data_info$Proportions != "None") {
      x$proportions <- x$proportions[which(rownames(x$proportions) != 'Total'), 
                                     which(colnames(x$proportions) != 'Total')]
    }
  }
  
  if(data_info$Proportions %in% c("Row", "Column", "Total")) {
    cross_table <- align_numbers(x$cross_table, x$proportions)
  } else {
    cross_table <- x$cross_table
  }
  
  justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                    l = "left",
                    c = "center",
                    r = "right")
  
  # ctable -- pander section ---------------------------------------------------
  if(method == "pander") {
    
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
    
    main_sect <- build_heading_pander(format_info, data_info)
    
    main_sect[[length(main_sect) + 1]] <-
      paste(
        capture.output(
          do.call(pander, append(pander_args, 
                                 list(x = quote(ftable(cross_table)))))
        ),
        collapse = "\n")
    
    if (isTRUE(format_info$headings) && format_info$style != 'grid') {
      main_sect[[length(main_sect)]] <- sub("^\n", "\n\n", main_sect[[length(main_sect)]])
    } 
    # else {
    #   main_sect[[length(main_sect)]] <- sub("^\n", "", main_sect[[length(main_sect)]])
    # }
    
    
    if (isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect)]])
    }
    
    return(main_sect)
    
  } else {
    
    # ctable -- html section ---------------------------------------------------
    dnn <- names(dimnames(cross_table))
    
    table_head <- list()
    table_rows <- list()
    
    table_head[[1]] <- 
      list(tags$th(""), tags$th(dnn[2], colspan = ncol(cross_table) - 
                                  as.numeric(isTRUE(format_info$totals))))
    
    if (isTRUE(format_info$totals)) {
      table_head[[1]][[3]] <- tags$th("")
    }
    
    table_head[[2]] <-list(tags$td(tags$strong(dnn[1]), align = "center"))
    
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
        
        # No proportions
        if (length(x$proportions) == 0) {
          cell <- cross_table[ro,co]
          table_row[[length(table_row) + 1]] <- tags$td(tags$span(cell))
        } else {
          # With proportions
          cell <- sub("\\( *", "("     , cross_table[ro,co])
          cell <- sub(" *\\)", ")"     , cell)
          cell <- gsub(" "   , "&nbsp;", cell)
          cell <- sub("%"    , "&#37;" , cell, fixed = TRUE)
          
          table_row[[length(table_row) + 1]] <- tags$td(tags$span(HTML(cell)))
        }
        
        # On last col, insert row into list
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
    
    div_list <- build_heading_html(format_info, data_info, method)
    if (length(div_list) > 0 &&
        !("shiny.tag" %in% class(div_list[[length(div_list)]]))) {
      div_list[[length(div_list) + 1]] <- HTML(text = "<br/>")
    }
    div_list[[length(div_list) + 1]] <- cross_table_html
    
    
    if (parent.frame()$footnote != "") {
      div_list[[length(div_list) + 1]] <- HTML(text = parent.frame()$footnote)
    }
  }
  
  return(div_list)
}
