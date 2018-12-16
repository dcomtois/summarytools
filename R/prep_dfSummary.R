#' @import htmltools
#' @keywords internal
prep_dfs <- function(x, method) {
  
  data_info   <- attr(x, "data_info")
  format_info <- attr(x, "formatting")
  
  # Remove Var number ("No") column if specified in call to print/view
  if (trs('no') %in% names(x) && 
      "varnumbers" %in% names(format_info) && 
      !isTRUE(format_info$varnumbers)) {
    x <- x[ ,-which(names(x) == trs('no'))]
  }
  
  # Remove Label column if specified in call to print/view
  if (trs("label") %in% names(x) && 
      "Labels.col" %in% names(format_info) && 
      !isTRUE(format_info$labels.col)) {
    x <- x[ ,-which(names(x) == trs('label'))]
  }
  
  # Remove Valid column if specified in call to print/view
  if (trs('valid') %in% names(x) && 
      "valid.col" %in% names(format_info) && 
      !isTRUE(format_info$valid.col)) {
    x <- x[ ,-which(names(x) == trs('valid'))]
  }
  
  # Remove Missing column if specified in call to print/view
  if (trs('missing') %in% names(x) && 
      "na.col" %in% names(format_info) && 
      !isTRUE(format_info$na.col)) {
    x <- x[ ,-which(names(x) == trs('missing'))]
  }

  # pander section -------------------------------------------------------------  
  if (method == "pander") {
    
    # remove html graphs
    if (trs("graph") %in% names(x)) {
      x <- x[ ,-which(names(x) == trs("graph"))]
    }
    
    # Remove graph if specified in call to print/view
    if (trs('text.graph') %in% names(x) && 
        "graph.col" %in% names(format_info) &&
        !isTRUE(format_info$graph.col)) {
      x <- x[ ,-which(names(x) == trs('text.graph'))]
    }
    
    # Check that style is not 'simple'
    if (isTRUE(format_info$style == 'simple')) {
      format_info$style <- 'multiline'
    }
    
    if (!isTRUE(format_info$plain.ascii)) {
      # Escape symbols for words between <>'s to allow <NA> or factor
      # levels such as <ABC> to be rendered correctly
      if(trs("label") %in% names(x)) {
        x[[trs("label")]] <-
          gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
               x = x[[trs("label")]], perl=TRUE)
      }
      
      x[[trs("stats.values")]] <-
        gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
             x = x[[trs("stats.values")]], perl=TRUE)
      
      x[[trs("freqs.pct.valid")]] <-
        gsub(pattern = "\\<(\\w*)\\>", replacement = "\\\\<\\1\\\\>",
             x = x[[trs("freqs.pct.valid")]], perl=TRUE)
      
      
      # Remove leading characters used for alignment in plain.ascii 
      x[[trs("freqs.pct.valid")]] <-
        gsub(pattern = "^\\\\ *", replacement = "",
             x = x[[trs("freqs.pct.valid")]], perl=TRUE)
      
      x[[trs("freqs.pct.valid")]] <- 
        gsub(pattern = "\\n\\\\ *", replacement = "\n",
             x = x[[trs("freqs.pct.valid")]], perl=TRUE)
      
      # Remove txt histograms b/c not supported in rmarkdown (for now)
      if (trs("text.graph") %in% names(x)) {
        x[[trs("text.graph")]][which(grepl('[:.]', 
                                           x[[trs("text.graph")]]))] <- ""
      }
    }
    
    pander_args <- append(list(style            = format_info$style,
                               plain.ascii      = format_info$plain.ascii,
                               justify          = format_info$justify,
                               split.cells      = format_info$split.cells,
                               split.tables     = format_info$split.tables,
                               keep.line.breaks = TRUE),
                          attr(x, "user_fmt"))
    
    main_sect <- build_heading_pander(format_info, data_info)
    
    main_sect %+=%
      paste(
        capture.output(
          do.call(pander, append(pander_args, list(x = quote(x))))
        ),
        collapse = "\n")
    
    if (isTRUE(parent.frame()$escape.pipe) && format_info$style == "grid") {
      main_sect[[length(main_sect)]] <- 
        gsub("\\|","\\\\|", main_sect[[length(main_sect)]])
    }
    
    return(main_sect)
    
  } else {
    
    # html section -------------------------------------------------------------
    
    # remove text graph
    if (trs("text.graph") %in% names(x)) {
      x <- x[ ,-which(names(x) == trs("text.graph"))]
    }
    
    # Remove graph if specified in call to print/view
    if (trs("graph") %in% names(x) && 
        "graph.col" %in% names(format_info) &&
        !isTRUE(format_info$graph.col)) {
      x <- x[ ,-which(names(x) == trs("graph"))]
    }
    
    table_head <- list()
    for(cn in colnames(x)) {
      if (cn %in% c(trs("no"), trs("valid"), trs("missing"))) {
        table_head %+=% list(tags$th(tags$strong(cn), align = "center"))
      } else {
        table_head %+=% list(tags$th(tags$strong(cn), align = "center"))
      }
    }
    
    table_rows <- list()
    for (ro in seq_len(nrow(x))) {
      table_row <- list()
      for (co in seq_len(ncol(x))) {
        cell <- x[ro,co]
        cell <- gsub('\\\\\n', '\n', cell)
        if (colnames(x)[co] %in% c(trs("no"), trs("valid"), trs("missing"))) {
          table_row %+=% list(tags$td(HTML(conv_non_ascii(cell)), 
                                      align = "center"))
        } else if (colnames(x)[co] == trs("label")) {
          cell <- gsub('(\\d+)\\\\\\.', '\\1.', cell)
          cell <- paste(strwrap(cell, width = format_info$split.cells, 
                                simplify = TRUE), collapse = "\n")
          table_row %+=% list(tags$td(HTML(conv_non_ascii(cell)), 
                                      align = "left"))
        } else if (colnames(x)[co] %in% 
                   c(trs("variable"), trs("stats.values"))) {
          cell <- gsub('(\\d+)\\\\\\.', '\\1.', cell)
          table_row %+=% list(tags$td(HTML(conv_non_ascii(cell)),
                                      align = "left"))
        } else if (colnames(x)[co] == trs("freqs.pct.valid")) {
          cell <- gsub("\\\\", " ", cell)
          cell <- gsub(" *(\\d|\\:)", "\\1", cell)
          cell <- gsub("\\:", " : ", cell)
          table_row %+=% list(tags$td(HTML(conv_non_ascii(cell)),
                                      align = "left"))
        } else if (colnames(x)[co] == trs("graph")) {
          table_row %+=% list(tags$td(HTML(conv_non_ascii(cell)), 
                                      align = "center", border = "0"))
        }
      }
      table_rows %+=% list(tags$tr(table_row))
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

    # Prepare the main "div" for the html report
    div_list <- build_heading_html(format_info, data_info, method)
    
    if (length(div_list) > 0 &&
        !("shiny.tag" %in% class(div_list[[length(div_list)]]))) {
      div_list %+=% list(HTML(text = "<br/>"))
    }
    
    div_list %+=% list(HTML(text = dfs_table_html))
    
    if (parent.frame()$footnote != "") {
      fn <- conv_non_ascii(parent.frame()[['footnote']])
      div_list %+=% list(HTML(text = fn))
    }
  }
  
  return(div_list)
}
