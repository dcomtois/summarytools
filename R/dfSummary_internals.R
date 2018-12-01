# Utility functions ------------------------------------------------------------
align_numbers_dfs <- function(counts, props) {
  maxchar_cnt <- nchar(as.character(max(counts)))
  maxchar_pct <- nchar(sprintf(paste0("%.", 1, "f"), max(props*100)))
  paste(sprintf(paste0("%", maxchar_cnt, "i"), counts),
        sprintf(paste0("(%", maxchar_pct, ".", 1, "f%%)"), props*100))
}

#' @importFrom RCurl base64Encode
#' @importFrom graphics barplot hist par text plot.new
#' @importFrom grDevices dev.off nclass.Sturges png
encode_graph <- function(data, graph_type, graph.magnif = NA) {
  if (graph_type == "histogram") {
    png(img_png <- tempfile(fileext = ".png"), width = 150 * graph.magnif,
        height = 100 * graph.magnif, units = "px", bg = "transparent")
    par("mar" = c(0.03,0.01,0.07,0.01))
    data <- data[!is.na(data)]
    breaks_x <- pretty(range(data), n = min(nclass.Sturges(data), 250),
                       min.n = 1)
    hist_values <- suppressWarnings(hist(data, breaks = breaks_x, plot = FALSE))
    cl <- try(suppressWarnings(hist(data, freq = FALSE, breaks = breaks_x,
                                    axes = FALSE, xlab=NULL, ylab=NULL,
                                    main=NULL, col = "grey95",
                                    border = "grey65")),
              silent = TRUE)
    if (any(grepl('try-', class(cl)))) {
      plot.new()
      text("Graph Not Available", x = 0.5, y = 0.5, cex = 1)
    }
   
  } else if (graph_type == "barplot") {
  
    png(img_png <- tempfile(fileext = ".png"), width = 150 * graph.magnif,
        height = 26 * length(data) * graph.magnif, units = "px",
        bg = "transparent")
    par("mar" = c(0.03,0.01,0.05,0.01))
    data <- rev(data)
    bp_values <- barplot(data, names.arg = "", axes = FALSE, space = 0.2,
                         col = "grey97", border = "grey65", horiz = TRUE,
                         xlim = c(0, sum(data)))
  }
 
  dev.off()
  img_txt <- base64Encode(txt = readBin(con = img_png, what = "raw",
                                        n = file.info(img_png)[["size"]]),
                          mode = "character")
  return(sprintf('<img src="data:image/png;base64,%s">', img_txt))
}

txtbarplot <- function(props, maxwidth = 20) {
  #widths <- props / max(props) * maxwidth
  widths <- props * maxwidth
  outstr <- character(0)
  for (i in seq_along(widths)) {
    outstr <- paste(outstr, paste0(rep(x = 'I', times = widths[i]),
                                   collapse = ""),
                    sep = " \\ \n")
  }
  outstr <- sub("^ \\\\ \\n", "", outstr)
  return(outstr)
}

#' @importFrom grDevices nclass.Sturges
txthist <- function(data) {
  data <- data[!is.na(data)]
  breaks_x <- pretty(range(data), n = nclass.Sturges(data), min.n = 1)
  if (length(breaks_x) <= 10) {
    counts <- hist(data, breaks = breaks_x, plot = FALSE)$counts
  } else {
    counts <- as.vector(table(cut(data, breaks = 10)))
  }
 
  # make counts top at 10
  counts <- matrix(round(counts / max(counts) * 10), nrow = 1, byrow = TRUE)
  graph <- matrix(data = "", nrow = 6, ncol = length(counts))
  for (ro in 6:1) {
    for (co in seq_along(counts)) {
      if (counts[co] > 1) {
        graph[ro,co] <- ": "
      } else if (counts[co] > 0) {
        graph[ro,co] <- ". "
      } else {
        if (sum(counts[1, co:length(counts)] > 0)) {
          graph[ro,co] <- "\\ \\ "
        }
      }
    }
    counts <- matrix(apply(X = counts - 2, MARGIN = 2, FUN = max, 0),
                     nrow = 1, byrow = TRUE)
  }
 
  graphlines <- character()
  for (ro in seq_len(nrow(graph))) {
    graphlines[ro] <-  trimws(paste(graph[ro,], collapse = ""), "right")
  }
  return(paste(graphlines, collapse = "\\\n"))
}

#' @importFrom utils head
#' @importFrom stats na.omit
detect_barcode <- function(x) {
 
  x <- na.omit(x)
  if (length(x) < 10 || (len <- min(nchar(x))) != max(nchar(x)) ||
      !len %in% c(8,12,13,14)) {
    return(FALSE)
  }
 
  x <- head(x, 20)
 
  type <- switch(as.character(len),
                 "8"  = "EAN-8",
                 "12" = "UPC",
                 "13" = "EAN-13",
                 "14" = "ITF-14")
 
  x_pad      <- paste0(strrep("0", 14-len), x)
  vect_code  <- lapply(strsplit(x_pad,""), as.numeric)
  weighted   <- lapply(vect_code, FUN = function(x) x * c(3,1))
  sums       <- mapply(weighted, FUN = sum)
 
  if (any(sums %% 10 != 0)) {
    return(FALSE)
  }
 
  return(type)
}


crunch_factor <- function() {

  outlist <- list()

  max.string.width    <- parent.frame()$max.string.width
  max.distinct.values <- parent.frame()$max.distinct.values
  graph.magnif        <- parent.frame()$graph.magnif
  round.digits        <- parent.frame()$round.digits
  
  n_levels <- nlevels(parent.frame()$column_data)
  counts   <- table(parent.frame()$column_data, useNA = "no")
  props    <- prop.table(counts)
 
  if (parent.frame()$n_levels == 0 && parent.frame()$n_valid == 0) {
    ouplist[[1]] <- "No levels defined"
    outlist[[2]] <- "All NA's"
    outlist[[3]] <- ""
    outlist[[4]] <- ""
  
  } else if (parent.frame()$n_valid == 0) {
    outlist[[1]] <- paste0(1:n_levels,"\\. ",
                           levels(parent.frame()$column_data),
                           collapse = "\\\n")
    outlist[[2]] <- "All NA's"
    outlist[[3]] <- ""
    outlist[[4]] <- ""
  
  } else if (n_levels <= max.distinct.values) {
    outlist[[1]] <- paste0(1:n_levels,"\\. ",
                          substr(levels(parent.frame()$column_data), 1,
                                 max.string.width),
                          collapse = "\\\n")
    counts_props <- align_numbers_dfs(counts, round(props, round.digits + 2))
    outlist[[2]] <- paste0("\\", counts_props, collapse = "\\\n")
    if (isTRUE(parent.frame()$graph.col) &&
        any(!is.na(parent.frame()$column_data))) {
      outlist[[3]] <- encode_graph(counts, "barplot", graph.magnif)
      outlist[[4]] <- txtbarplot(prop.table(counts))
    }
  
  } else {
  
    # more levels than allowed by max.distinct.values
    n_extra_levels <- n_levels - max.distinct.values
  
    outlist[[1]] <-
      paste0(1:max.distinct.values,"\\. ",
             substr(levels(parent.frame()$column_data), 1,
                    max.string.width)[1:max.distinct.values],
             collapse="\\\n")
  
    outlist[[1]] <- paste(outlist[[1]],
                         paste("[", n_extra_levels, "others", "]"),
                         sep="\\\n")
  
    counts_props <- align_numbers_dfs(
      c(counts[1:max.distinct.values],
        sum(counts[(max.distinct.values + 1):length(counts)])),
      c(props[1:max.distinct.values],
        round(sum(props[(max.distinct.values + 1):length(props)]),
              round.digits + 2))
    )
  
    outlist[[2]] <- paste0("\\", counts_props, collapse = "\\\n")
  
    if (isTRUE(parent.frame()$graph.col) &&
        any(!is.na(parent.frame()$column_data))) {
      # prepare data for barplot
      tmp_data <- parent.frame()$column_data
      levels(tmp_data)[max.distinct.values + 1] <-
        paste("[", n_extra_levels, "others", "]")
      tmp_data[which(as.numeric(tmp_data) > max.distinct.values)] <-
        paste("[", n_extra_levels, "others", "]")
      levels(tmp_data)[(max.distinct.values + 2):n_levels] <- NA
      outlist[[3]] <- encode_graph(table(tmp_data), "barplot", graph.magnif)
      outlist[[4]] <- txtbarplot(prop.table(table(tmp_data)))
    }
  }
  return(outlist)
}

crunch_character <- function() {
 
  outlist <- list()

  max.string.width    <- parent.frame()$max.string.width
  max.distinct.values <- parent.frame()$max.distinct.values
  graph.magnif        <- parent.frame()$graph.magnif
  round.digits        <- parent.frame()$round.digits
  
  if (isTRUE(parent.frame()$trim.strings)) {
    column_data <- trimws(parent.frame()$column_data)
  }
 
  n_empty <- sum(parent.frame()$column_data == "", na.rm = TRUE)
 
  if (n_empty == parent.frame()$n_tot) {
    outlist[[1]] <- ""
    outlist[[2]] <- "All empty strings"
    outlist[[3]] <- ""
    outlist[[4]] <- ""
  
  } else if (parent.frame()$n_miss == parent.frame()$n_tot) {
    outlist[[1]] <- ""
    outlist[[2]] <- "All NA's"
    outlist[[3]] <- ""
    outlist[[4]] <- ""
  
  } else if (n_empty + parent.frame()$n_miss == parent.frame()$n_tot) {
    outlist[[1]] <- ""
    outlist[[2]] <- "All empty strings / NA's"
    outlist[[3]] <- ""
    outlist[[4]] <- ""
  
  } else {
  
    counts <- table(parent.frame()$column_data, useNA = "no")
    props <- prop.table(counts)      
  
    # Check if data fits UPC / EAN barcode numbers patterns
    if (!isFALSE(barcode_type <-
                 detect_barcode(trimmed <-
                                trimws(parent.frame()$column_data)))) {
    
      outlist[[1]] <- paste(barcode_type, "codes \\\n",
                           "min  :", min(trimmed, na.rm = TRUE), "\\\n",
                           "max  :", max(trimmed, na.rm = TRUE), "\\\n",
                           "mode :", names(counts)[which.max(counts)])
      outlist[[2]] <- paste0(counts_props, collapse = "\\\n")
    
      if (isTRUE(parent.frame()$graph.col)) {
        outlist[[3]] <- encode_graph(counts, "barplot", graph.magnif)
        outlist[[4]] <- txtbarplot(prop.table(counts))
      }
    
    } else if (length(counts) <= max.distinct.values + 1) {
    
      # Report all frequencies when allowed by max.distinct.values
      outlist[[1]] <- paste0(seq_along(counts), "\\. ",
                            substr(names(counts), 1, max.string.width),
                            collapse="\\\n")
      counts_props <- align_numbers_dfs(counts, round(props, round.digits + 2))
      outlist[[2]] <- paste0(counts_props, collapse = "\\\n")
    
      if (isTRUE(parent.frame()$graph.col)) {
        outlist[[3]] <- encode_graph(counts, "barplot", graph.magnif)
        outlist[[4]] <- txtbarplot(prop.table(counts))
      }
    
    } else {
    
      # Too many values - report most common strings
      counts <- sort(counts, decreasing = TRUE)
      props <- sort(props, decreasing = TRUE)
      n_extra_values <- length(counts) - max.distinct.values
      outlist[[1]] <- paste0(
        paste0(1:max.distinct.values,"\\. ",
               substr(names(counts), 1,
                      max.string.width)[1:max.distinct.values],
               collapse="\\\n"),
        paste("\\\n[", n_extra_values, "others", "]")
      )
      counts_props <- align_numbers_dfs(
        counts = c(counts[1:max.distinct.values],
                   sum(counts[(max.distinct.values + 1):length(counts)])),
        props = c(props[1:max.distinct.values],
                  round(sum(props[(max.distinct.values + 1):length(props)]),
                        round.digits + 2))
      )
      outlist[[2]] <- paste0(counts_props, collapse = "\\\n")
    
      if (isTRUE(parent.frame()$graph.col)) {
        # Prepare data for graph
        counts[max.distinct.values + 1] <-
          sum(counts[(max.distinct.values + 1):length(counts)])
        names(counts)[max.distinct.values + 1] <-
          paste0("[ ", n_extra_values, " others ]")
        counts <- counts[1:(max.distinct.values + 1)]
        outlist[[3]] <- encode_graph(counts, "barplot", graph.magnif)
        outlist[[4]] <- txtbarplot(prop.table(counts))
      }
    }
  }
  return(outlist)
}

#' @importFrom stats IQR median ftable sd
crunch_numeric <- function() {
 
  outlist <- list()
 
  max.string.width    <- parent.frame()$max.string.width
  max.distinct.values <- parent.frame()$max.distinct.values
  graph.magnif        <- parent.frame()$graph.magnif
  round.digits        <- parent.frame()$round.digits
  
  if (parent.frame()$n_miss == parent.frame()$n_tot) {
    outlist[[1]] <- ""
    outlist[[2]] <- "All NA's"
    outlist[[3]] <- ""
    outlist[[4]] <- ""
  
  } else {
  
    counts <- table(parent.frame()$column_data, useNA = "no")
  
    # Check if data fits UPC / EAN barcode numbers patterns
    if (!isFALSE(barcode_type <- detect_barcode(parent.frame()$column_data))) {
    
      outlist[[1]] <- paste(barcode_type, "codes \\\n",
                           "min  :", min(parent.frame()$column_data,
                                         na.rm = TRUE), "\\\n",
                           "max  :", max(parent.frame()$column_data, 
                                         na.rm = TRUE), "\\\n",
                           "mode :", names(counts)[which.max(counts)])
    
    } else if (length(counts) == 1) {
      outlist[[1]] <- "One distinct value"
    
    } else {
      outlist[[1]] <- paste(
        "mean (sd) : ", round(mean(parent.frame()$column_data, na.rm = TRUE),
                              round.digits),
        " (", round(sd(parent.frame()$column_data, na.rm = TRUE),
                    round.digits), ")\\\n",
        "min < med < max :\\\n", round(min(parent.frame()$column_data,
                                           na.rm = TRUE), round.digits),
        " < ", round(median(parent.frame()$column_data, na.rm = TRUE),
                     round.digits),
        " < ", round(max(parent.frame()$column_data, na.rm = TRUE),
                     round.digits), "\\\n",
        collapse="", sep="")
    
      # Data is binary: add mode
      if (length(counts) == 2 && counts[1] != counts[2]) {
        outlist[[1]] <- paste0(outlist[[1]], "mode: ",
                               names(counts)[parent.frame()$which.max(counts)])           
       
      } else if (length(counts) >= 3) {
        # Data has 3+ distinct values: add IQR and CV
        outlist[[1]] <-
          paste(outlist[[1]],
                "IQR (CV) : ", round(IQR(parent.frame()$column_data,
                                         na.rm = TRUE), round.digits),
                " (", round(sd(parent.frame()$column_data, na.rm = TRUE) /
                              mean(parent.frame()$column_data, na.rm = TRUE),
                            round.digits), ")", collapse="", sep="")
      }
    }
   
    extra_space <- FALSE
   
    # In specific circumstances, display most common values         
    if (length(counts) <= max.distinct.values &&
        (all(parent.frame()$column_data %% 1 == 0, na.rm = TRUE) ||
         identical(names(parent.frame()$column_data), "0") ||
         all(abs(as.numeric(names(counts[-which(names(counts) == "0")]))) >=
             10^-round.digits))) {
     
      props <- round(prop.table(counts), round.digits + 2)
      counts_props <- align_numbers_dfs(counts, props)
     
      outlist[[2]]  <-
        paste(
          paste0(rounded_names <-
                   format(round(as.numeric(names(counts)),
                                round.digits),
                          nsmall = round.digits *
                            !all(parent.frame()$column_data %% 1 == 0,
                                 na.rm = TRUE)),
                 ifelse(as.numeric(names(counts)) != as.numeric(rounded_names),
                        "!", " ")),
          counts_props, sep = ": ", collapse = "\\\n"
        )
     
      if (any(as.numeric(names(counts)) != as.numeric(rounded_names))) {
        extra_space <- TRUE
        outlist[[2]] <- paste(outlist[[2]], "! rounded", sep = "\\\n")
      }
     
    } else {
      # Do not display specific values - only the number of distinct values
      outlist[[2]] <- paste(length(counts), "distinct values")
      if (parent.frame()$n_miss == 0 &&
          (isTRUE(all.equal(parent.frame()$column_data,
                            min(parent.frame()$column_data):
                            max(parent.frame()$column_data))) ||
           isTRUE(all.equal(parent.frame()$column_data,
                            max(parent.frame()$column_data):
                            min(parent.frame()$column_data))))) {
        outlist[[2]] <- paste(outlist[[2]], "(Integer sequence)", sep = "\\\n")
      }
    }
   
    if (isTRUE(parent.frame()$graph.col)) {
      if (length(counts) <= max.distinct.values) {
        outlist[[3]] <- encode_graph(counts, "barplot", graph.magnif)
        outlist[[4]] <- txtbarplot(prop.table(counts))
       
        if (isTRUE(extra_space)) {
          outlist[[3]] <- paste0(outlist[[3]], "\n\n")
          outlist[[4]] <- paste0(outlist[[4]], " \\ \n \\")
        }
      } else {
        outlist[[3]] <- encode_graph(parent.frame()$column_data, "histogram",
                                     graph.magnif)
        outlist[[4]] <- txthist(parent.frame()$column_data)
      }
    }
  }
  return(outlist)
}

#' @importFrom lubridate as.period interval
crunch_time_date <- function() {
 
  outlist <- list()
 
  max.string.width    <- parent.frame()$max.string.width
  max.distinct.values <- parent.frame()$max.distinct.values
  graph.magnif        <- parent.frame()$graph.magnif
  round.digits        <- parent.frame()$round.digits
  
  if (parent.frame()$n_miss == parent.frame()$n_tot) {
    outlist[[1]] <- ""
    outlist[[2]] <- "All NA's"
    outlist[[3]] <- ""
    outlist[[4]] <- ""
   
  } else {
   
    counts <- table(parent.frame()$column_data, useNA = "no")
   
    # Report all frequencies when allowed by max.distinct.values
    if (length(counts) <= max.distinct.values) {
      outlist[[1]] <- paste0(seq_along(counts),". ", names(counts),
                             collapse="\\\n")
      props <- round(prop.table(counts), round.digits + 2)
      counts_props <- align_numbers_dfs(counts, props)
      outlist[[2]] <- paste(counts_props, collapse = "\\\n")
      outlist[[3]] <- encode_graph(counts, "barplot", graph.magnif)
      outlist[[4]] <- txtbarplot(prop.table(counts))
     
    } else {
     
      outlist[[1]] <- paste0(
        "min : ", tmin <- min(parent.frame()$column_data, na.rm = TRUE), "\\\n",
        "med : ", median(parent.frame()$column_data, na.rm = TRUE), "\\\n",
        "max : ", tmax <- max(parent.frame()$column_data, na.rm = TRUE), "\\\n",
        "range : ", sub(pattern = " 0H 0M 0S", replacement = "",
                        x = round(as.period(interval(tmin, tmax)),round.digits))
      )
     
      outlist[[2]] <- paste(length(counts), "distinct val.")
     
      if (isTRUE(parent.frame()$graph.col)) {
        tmp <- as.numeric(parent.frame()$column_data)[!is.na(
          parent.frame()$column_data)]
        outlist[[3]] <- encode_graph(tmp - mean(tmp), "histogram", graph.magnif)
        outlist[[4]] <- txthist(tmp - mean(tmp))
      }
    }
  }
  outlist 
}

crunch_other <- function() {
 
  outlist <- list()
 
  max.string.width    <- parent.frame()$max.string.width
  max.distinct.values <- parent.frame()$max.distinct.values
  graph.magnif        <- parent.frame()$graph.magnif
  round.digits        <- parent.frame()$round.digits
  
  counts <- table(parent.frame()$column_data, useNA = "no")
  
  if (parent.frame()$n_miss == parent.frame()$n_tot) {
    outlist[[1]] <- ""
    outlist[[2]] <- "All NA's"
    outlist[[3]] <- ""
    outlist[[4]] <- ""
   
  } else if (length(counts) <= max.distinct.values) {
    props <- round(prop.table(counts), parent.frame()$round.digits + 2)
    counts_props <- align_numbers_dfs(counts, props)
    outlist[[2]] <- paste0(counts_props, collapse = "\\\n")
   
  } else {
    outlist[[2]] <- paste(
      as.character(length(unique(parent.frame()$column_data))),
                          "distinct val."
      )
  }
 
  outlist[[3]] <- ""
  outlist[[4]] <- ""
 
  return(outlist)
}
