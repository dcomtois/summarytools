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
    
    # descr -- pander method ---------------------------------------------------
    # Format numbers (avoids inconsistencies with pander rounding digits)
    x <- format(round(x, format_info$round.digits),
                nsmall = format_info$round.digits)
    
    pander_args <- append(list(style        = format_info$style,
                               plain.ascii  = format_info$plain.ascii,
                               split.tables = format_info$split.tables,
                               justify      = justify),
                          attr("x", "user_fmt"))

    main_sect <- build_heading_pander(format_info, data_info)  
    
    main_sect %+=%
      paste(
        capture.output(
          do.call(pander, append(pander_args, list(x = quote(x))))
        ),
        collapse = "\n")
    
    if(isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect)]])
    }
    
    return(main_sect)
    
  } else {
    
    # descr -- html method -----------------------------------------------------
    
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
        table_head %+=% list(tags$th(HTML(cn), align = "center"))
      }
    }
    
    table_rows <- list()
    for (ro in seq_len(nrow(x))) {
      table_row <- list(tags$td(tags$strong(rownames(x)[ro])))
      for (co in seq_len(ncol(x))) {
        # cell is NA
        if (is.na(x[ro,co])) {
          table_row %+=% list(tags$td(format_info$missing))
        } else if ((rownames(x)[ro] == "N.Valid" || 
                    colnames(x)[co] == "N.Valid") && 
                   !"Weights" %in% names(data_info)) {
          table_row %+=% list(tags$td(tags$span(round(x[ro,co], 0))))
        } else {
          # When not NA, and not N.Valid row, format cell content
          cell <- sprintf(paste0("%.", format_info$round.digits, "f"), x[ro,co])
          table_row %+=% list(tags$td(tags$span(cell)))
        }
        # On last column, insert row to table_rows list
        if (co == ncol(x)) {
          table_rows %+=% list(tags$tr(table_row))
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
    
    # Prepare the main "div" for the html report
    div_list <- build_heading_html(format_info, data_info, method)
    if (length(div_list) > 0 &&
        !("shiny.tag" %in% class(div_list[[length(div_list)]]))) {
      div_list %+=% list(HTML(text = "<br/>"))
    }
    
    div_list %+=% list(HTML(text = descr_table_html))
   
    if (parent.frame()$footnote != "") {
      fn <- conv_non_ascii(parent.frame()[['footnote']])
      div_list %+=% list(HTML(text = fn))
    }
  }
  
  return(div_list)
}
