#' Convert Summarytools Ojects to Tibbles
#'
#' Make a tidy dataset out of freq() or descr() outputs
#'
#' @param x a freq() or descr() output object
#' @param order Integer. Useful for grouped results (produced with `stby()`)
#'  only. When \code{1} (default), the levels of the grouping variable are used 
#'  to sort the table, followed by the values of the second column (data values
#'  for `freq()`, variable names for `descr()`. When \code{2}, the order is 
#'  based on the second column first. In `freq()` tables, the row for \code{NA} 
#'  counts will always appear last.
#' @return A \code{\link[tibble]{tibble}} which is constructed following the 
#' \emph{tidy} principles.
#' 
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows bind_cols
#' @export
tb <- function(x, order = 1) {
  
  if (inherits(x, "stby")) {
    grp_values <- attr(x, "dimnames")[[1]]
    grp_stats  <- lapply(x, tb)
    grp_size   <- nrow(grp_stats[[1]])
    out_c1     <- tibble(grp = factor(rep(grp_values, each = grp_size),
                                      levels = grp_values))
    output     <- bind_cols(out_c1, bind_rows(grp_stats))
    
    if (attr(x[[1]], "st_type") == "freq") {
      if ("pct_valid" %in% colnames(output)) {
        output$pct_valid <- output$pct_valid / length(grp_values)
        output$pct_tot   <- output$pct_tot / length(grp_values)
      } else {
        output$pct <- output$pct / length(grp_values)
      }
      
      if (identical(order, 2)) {
        if ("<NA>" %in% output[[2]]) {
          # change <NA> for true NA's for sorting, then put "<NA>" back
          output[[2]][output[[2]] == "<NA>"] <- NA
          output <- output[order(output[[2]], output[[1]], na.last = TRUE),]
          output[[2]][is.na(output[[2]])] <- "<NA>"
        } else {
          output <- output[order(output[[2]], output[[1]]),]
        }
      }
      
      if ("pct_valid_cum" %in% colnames(output)) {
        tmp_nomiss <- output$pct_valid
        tmp_nomiss[is.na(tmp_nomiss)] <- 0
        output$pct_valid_cum <- cumsum(tmp_nomiss)
        output$pct_tot_cum <- cumsum(output$pct_tot)
      }
    } else if (identical(order, 2)) {
      # st_type == "descr"
      output <- output[order(output[[2]], output[[1]], na.last = TRUE),]
    }
    
    colnames(output)[1] <- sub("(.+)\\$(.+)", "\\2", names(attr(x, "dimnames")))
    return(output)
  }      
  
  if (attr(x, "st_type") == "freq") {
    
    output <- as_tibble(cbind(rownames(x), as.data.frame(x)))
    varname <- na.omit(c(attr(x, "data_info")$Variable, "value"))[1]
    names(output) <- 
      c(varname, "freq", "pct_valid", "pct_valid_cum", "pct_tot", "pct_tot_cum")

    # remove totals row
    output <- output[1:(nrow(output) - 1), ]

    # remove na info when appropriate     
    if (!isTRUE(attr(x, "format_info")[["report.nas"]])) {
      output <- output[1:(nrow(output) - 1), 
                       -grep("^pct_(tot|tot_cum)$", names(output))]
      names(output)[3:4] <- c("pct", "pct_cum")
    }
    
    # remove cumulative columns when appropriate
    if (!isTRUE(attr(x, "format_info")[["cumul"]])) {
      output <- output[ , -grep("_cum", names(output))]
    }

    output[[varname]] <- factor(output[[varname]], levels = output[[varname]])
    return(output)
    
  } else if (attr(x, "st_type") == "descr") {
    
    if (!isTRUE(attr(x, "data_info")$transposed)) {
      output <- as_tibble(t(as.data.frame(x)), rownames = "variable")
      names(output) <- c("variable", attr(x, "stats"))
    } else {
      output <- as_tibble(as.data.frame(x), rownames = "variable")
      names(output) <- c("variable", attr(x, "stats"))
    }
    
    return(output)
    
  } else {
    stop("tb() supports summarytools freq() and descr() objects only")
  }
}
