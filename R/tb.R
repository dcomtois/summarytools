#' @importFrom tibble as_tibble
#' @export
tb <- function(x) {
  if (attr(x, "st_type") == "freq") {
    
    output <- as_tibble(cbind(rownames(x), as.data.frame(x)))
    names(output) <- 
      c("values", "freq", "pct_valid", "pct_valid_cum", "pct_tot", "pct_cum")

    # remove totals row
    output <- output[1:(nrow(output) - 1), ]

    # remove na info when appropriate     
    if (!isTRUE(attr(x, "format_info")[["report.nas"]])) {
      output <- output[1:(nrow(output) - 1), 
                       -grep("^pct_(tot|cum)$", names(output))]
      
      names(output)[3:4] <- c("pct", "pct_cum")
      
    }
    
    # remove cumulative columns when appropriate
    if (!isTRUE(attr(x, "format_info")[["cumul"]])) {
      output <- output[ , -grep("_cum", names(output))]
    }

  } else if (attr(x, "st_type") == "descr") {
    
    if (!isTRUE(attr(x, "data_info")$transposed)) {
      output <- as_tibble(cbind(variable = colnames(x), t(as.data.frame(x))))
      names(output) <- c("variable", attr(x, "stats"))
    } else {
      output <- as_tibble(cbind(statistic = rownames(x), as.data.frame(x)))
      output$statistic <- attr(x, "stats")
    }
  }
    
  else if (attr(x, "st_type") == "ctable") {
    
    output <- data.frame(v1 = names(x[[1]][,1]))
    names(output) <- as.character(attr(x, "fn_call"))[2]
    class(x$cross_table) <- "matrix"
    output <- cbind(output, x$cross_table)
    names(output) <- sub("<(.+)>", "\\1", names(output))
    names(output)[2:(ncol(output) - 1)] <- 
      paste(as.character(attr(x, "fn_call"))[3], 
            names(output)[2:(ncol(output) - 1)], sep = "_")
  
    if (!is.null(x$proportions)) {
      class(x$proportions) <- "matrix"
      output <- cbind(output, x$proportions)
      names(output) <- sub("<(.+)>", "\\1", names(output))
      names(output)[(ncol(x$cross_table) + 2):(ncol(output) - 1)] <-
        paste(as.character(attr(x, "fn_call"))[3], 
              names(output)[(ncol(x$cross_table) + 2):(ncol(output) - 1)],
              paste0(tolower(substr(attr(x, "data_info")$Proportions, 1, 3)),
                     "pct"), 
              sep = "_")
      names(output)[ncol(output)] <-
        paste0("Total_", 
               tolower(substr(attr(x, "data_info")$Proportions, 1, 3)),
               "pct")
    }
    rownames(output) <- NULL
    output <- as_tibble(output)[1:(nrow(output) - 1),]
  }
  output
}
