#' @import htmltools
#' @keywords internal
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
