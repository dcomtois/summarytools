#' Convert Summarytools Ojects to Tibbles
#'
#' Make a tidy dataset out of freq() or descr() outputs
#'
#' @param x a freq() or descr() output object
#' 
#' @return A \code{\link[tibble]{tibble} which is constructed following the 
#' \emph{tidy} principles.
#' 
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

    return(output)
    
  } else if (attr(x, "st_type") == "descr") {
    
    if (!isTRUE(attr(x, "data_info")$transposed)) {
      output <- as_tibble(cbind(variable = colnames(x), t(as.data.frame(x))))
      names(output) <- c("variable", attr(x, "stats"))
    } else {
      output <- as_tibble(cbind(statistic = rownames(x), as.data.frame(x)))
      output$statistic <- attr(x, "stats")
    }
    
    return(output)
    
  } else {
    stop("tb() supports summarytools freq() and descr() objects only")
  }
}
