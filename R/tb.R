#' Convert Summarytools Objects into Tibbles
#'
#' Make a tidy dataset out of freq() or descr() outputs
#'
#' @param x a freq() or descr() output object
#' @param order Integer. Useful for grouped results (produced with `stby()`)
#'  only. When \code{1} (default), the levels of the grouping variable are used 
#'  to sort the table, followed by the values of the other grouping variables if
#'  any, and variable names for `descr()`). When \code{2}, the order of the
#'  columns is reversed. In `freq()` tables, the row for \code{NA} counts will 
#'  always appear last for each group.
#' @return A \code{\link[tibble]{tibble}} which is constructed following the 
#' \emph{tidy} principles.
#' 
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows bind_cols
#' @export
tb <- function(x, order = 1) {
  
  if (inherits(x, "stby")) {
    
    grp_stats <- lapply(x, tb)
    
    if ("groups" %in% names(attributes(x))) {
      left_part <- as_tibble(merge(grp_stats[[1]][,1],
                                   attr(x, "groups"),
                                   all = TRUE)[,-1])
      grp_values <- attr(x, "groups")
    } else {
      null_grs     <- which(vapply(x, is.null, TRUE))
      non_null_grs <- setdiff(seq_along(x), null_grs)
      grp_values   <- as_tibble(expand.grid(attr(x, "dimnames")))[non_null_grs,]
      if (length(intersect(colnames(grp_values), 
                           colnames(grp_stats[[non_null_grs[1]]][,1])))) {
        stop(colnames(grp_stats[[non_null_grs[1]]][,1]), " is both a grouping ",
             "variable and an analysis variable; tidy table impossible to ",
             "generate")
      }
      left_part    <- as_tibble(merge(grp_stats[[non_null_grs[1]]][,1],
                                      grp_values, all = TRUE)[,-1])
    }
    
    nb_gr_var <- ncol(left_part)
    output    <- bind_cols(left_part, bind_rows(grp_stats))
    
    if (identical(order, 2)) {
      output <- output[do.call(what = "order", 
                               args = output[ ,(nb_gr_var + 1):1]), ]
    }
    
    colnames(output)[1:ncol(left_part)] <- 
      sub("(.+)\\$(.+)", "\\2", colnames(output)[1:ncol(left_part)])
    
    if (attr(x[[1]], "st_type") == "freq") {
      
      if ("pct_valid" %in% colnames(output)) {
        output$pct_valid <- output$pct_valid / nrow(grp_values)
        output$pct_tot   <- output$pct_tot   / nrow(grp_values)
      } else {
        output$pct <- output$pct / nrow(grp_values)
      }
      
      if ("pct_valid_cum" %in% colnames(output)) {
        tmp_nomiss <- output$pct_valid
        tmp_nomiss[is.na(tmp_nomiss)] <- 0
        output$pct_valid_cum <- cumsum(tmp_nomiss)
        output$pct_tot_cum <- cumsum(output$pct_tot)
      }

      if ("pct_cum" %in% colnames(output)) {
        output$pct_cum <- cumsum(output$pct)
      }
      
    }
    
    return(output)
  }      
  
  if (!is.null(x) && attr(x, "st_type") == "freq") {
    
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
    
  } else if (!is.null(x) && attr(x, "st_type") == "descr") {
    
    if (!isTRUE(attr(x, "data_info")$transposed)) {
      output <- as_tibble(t(as.data.frame(x)), rownames = "variable")
      names(output) <- c("variable", attr(x, "stats"))
    } else {
      output <- as_tibble(as.data.frame(x), rownames = "variable")
      names(output) <- c("variable", attr(x, "stats"))
    }
    
    return(output)
    
  } else if (is.null(x)) {
    return(list())
  } else {
    stop("tb() supports summarytools freq() and descr() objects only")
  }
}

      #by_levels <- expand.grid(attr(x, "dimnames"))
      #by_vars   <- names(attr(x, "dimnames"))
      #msg <- "Following group(s) had 0 observations:\n"
      #for (i in seq_along(null_ind)) {
      #  by_values <- as.character(unlist(by_levels[null_ind[i],]))
      #  msg <- paste0(msg, "  ", null_ind[i], ". ",
      #                paste(by_vars, by_values, collapse = ", ", sep = " = "),
      #                "\n")
