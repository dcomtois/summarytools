# Prepare freq objects for printing --------------------------------------------
#' @import htmltools
prep_freq <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  if (!isTRUE(format_info$report.nas)) {
    x[nrow(x), 1] <- x[nrow(x), 1] - x[nrow(x) -1, 1]
    x <- x[-(nrow(x)-1),1:3]
    colnames(x) <- c(trs('freq'), "%", trs('pct.cum'))
  }
  
  if (!isTRUE(format_info$totals)) {
    x <- x[-nrow(x),]
  }
  
  if(method=="pander") {
    
    # freq -- pander method ----------------------------------------------------
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
      freq_table[ ,1] <- sub("\\.0+", "", freq_table[ ,1])
    
    
    # Escape "<" and ">" when used in pairs in rownames
    if (!isTRUE(format_info$plain.ascii)) {
      row.names(freq_table) <- gsub(pattern = "\\<(.*)\\>", 
                                    replacement = "\\\\<\\1\\\\>",
                                    x = row.names(freq_table), perl = TRUE)
    }
    
    # set encoding to native to allow proper display of accentuated characters
    if (parent.frame()$file == "") {
      row.names(freq_table) <- enc2native(row.names(freq_table))
    }
    
    pander_args <- append(list(style        = format_info$style,
                               plain.ascii  = format_info$plain.ascii,
                               justify      = justify,
                               missing      = format_info$missing,
                               split.tables = Inf),
                          attr(x, "user_fmt"))
    
    main_sect <- build_heading_pander(format_info, data_info)
    
    main_sect %+=%
      paste(
        capture.output(
          do.call(pander, append(pander_args, list(x = quote(freq_table))))
        ),
        collapse = "\n")
    
    if (isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- gsub("\\|","\\\\|", 
                                             main_sect[[length(main_sect)]])
    }
    
    return(main_sect)
    
  } else {
    
    # freq -- html method ------------------------------------------------------
    justify <- switch(tolower(substring(format_info$justify, 1, 1)),
                      l = "left",
                      c = "center",
                      d = "center",
                      r = "right")
    
    table_head <- list()
    table_rows <- list()
    
    for (ro in seq_len(nrow(x))) {
      table_row <- list()
      for (co in seq_len(ncol(x))) {
        if (co == 1) {
          table_row %+=% list(tags$th(row.names(x)[ro]))
          if (!"Weights" %in% names(data_info)) {
            cell <- sub(pattern = "\\.0+", replacement = "", x[ro,co], 
                        perl = TRUE)
            table_row %+=% list(tags$td(cell, align = justify))
            next
          }
        }
        
        if (is.na(x[ro,co])) {
          table_row %+=% list(tags$td(format_info$missing, align = justify))
        } else {
          cell <- sprintf(paste0("%.", format_info$round.digits, "f"), x[ro,co])
          table_row %+=% list(tags$td(cell, align = justify))
        }
        
        if (co == ncol(x)) {
          table_rows %+=% list(tags$tr(table_row))
        }
      }
    }
    
    if (isTRUE(format_info$report.nas)) {
      table_head[[1]] <- list(tags$th("", colspan = 2),
                              tags$th(trs('valid'), colspan = 2),
                              tags$th(trs('total'), colspan = 2))
      table_head[[2]] <- list(tags$th(sub("^.*\\$(.+)$", "\\1", 
                                          data_info$Variable)),
                              tags$th(HTML(trs('freq'))),
                              tags$th("%"),
                              tags$th(HTML(trs('pct.cum'))),
                              tags$th("%"),
                              tags$th(HTML(trs('pct.cum'))))
      
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
                         tags$th(trs('freq')),
                         tags$th("%"),
                         tags$th(HTML(trs('pct.cum'))))
      
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
    freq_table_html <- gsub(pattern = "\\s*(\\d*)\\s*(<span|</td>)",
                            replacement = "\\1\\2", 
                            x = as.character(freq_table_html),
                            perl = TRUE)
    freq_table_html <- gsub(pattern = "</span>\\s*</span>",
                            replacement = "</span></span>",
                            x = freq_table_html,
                            perl = TRUE)
    
    # Prepare the main "div" for the html report
    div_list <- build_heading_html(format_info, data_info, method)
    
    if (length(div_list) > 0 &&
        !("shiny.tag" %in% class(div_list[[length(div_list)]]))) {
      div_list %+=% list(HTML(text = "<br/>"))
    }
    
    div_list %+=% list(HTML(text = conv_non_ascii(freq_table_html)))
    
    if (parent.frame()$footnote != "") {
      fn <- conv_non_ascii(parent.frame()[['footnote']])
      div_list %+=% list(HTML(text = fn))
    }
  }
  
  return(div_list)
}