#' Convert Summarytools Objects into Tibbles
#'
#' Make a tidy dataset out of freq() or descr() outputs
#'
#' @param x a \code{freq()} or \code{descr()} output object.
#' @param order Integer. Useful for grouped results produced with
#'  \code{\link{stby}} or \code{dplyr::group_by}. When set to \code{1}
#'   (default), the ordering is done using the grouping variables first.
#'   When set to \code{2}, the ordering is done according to the analytical 
#'   (not grouping) variable. When set to \code{3}, the same ordering
#'   as with \code{2} is used, but the analytical variable is placed in
#'   first position. Depending on what function was used for grouping,
#'   the results will be different in subtle ways. See \emph{Details}.
#' @param drop.var.col Logical. For \code{\link{descr}} objects, drop the
#'   \code{variable} column. This is possible only when statistics are
#'   produced for a single variable; when multiple variables are present,
#'   this parameter is ignored. \code{FALSE} by default.
#' @param recalculate Logical. \strong{TRUE by default}. For grouped
#'   \code{\link{freq}} results, recalculate percentages to have total
#'   proportions sum up to 1. Defaults to \code{TRUE}.
#' @param fct.to.chr Logical. When grouped objects
#'   are created with \code{dplyr::\link[dplyr]{group_by}}, the resulting
#'   tibble will have factor columns when the grouping variable itself is
#'   a factor. To convert them to character, set this to TRUE. See
#'   \emph{Details}.
#' @param \dots For internal use only.
#'
#' @return A \code{\link[tibble]{tibble}} which is constructed following the
#' \emph{tidy} principles.
#'
#' @details 
#'   \code{stby}, which is based on and \code{by}, initially make the first
#'   variable vary, keeping the other(s) constant. On the other hand,
#'   \code{group_by} initially keeps the first grouping variable(s) constant,
#'   making the last one vary. This will impact the ordering of the rows (and
#'   as a result, the cumulative percent columns, if present).
#'   
#'   Also, keep in mind that while \code{group_by} shows \code{NA} groups by
#'   default, \code{useNA = TRUE} must be used to achieve the same
#'   results with \code{stby}.
#'
#' @examples
#'
#' tb(freq(iris$Species))
#' tb(descr(iris, stats = "common"))
#'
#' data("tobacco")
#' tb(stby(tobacco, tobacco$gender, descr, stats = "fivenum",check.nas = FALSE), 
#'    order=3)
#' tb(stby(tobacco, tobacco$gender, descr, stats = "common", useNA = TRUE))
#' 
#' # Compare stby() and group_by() groups' ordering
#' tb(with(tobacco, stby(diseased, list(gender, smoker), freq, useNA = TRUE)))
#' tobacco |> dplyr::group_by(gender, smoker) |> freq(diseased) |> tb()
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows bind_cols
#' @export
tb <- function(x, order = 1, drop.var.col = FALSE,
               recalculate = TRUE, fct.to.chr = FALSE, ...) {
  
  # For dispatched list elements having a NULL group
  if (is.null(x)) return(NULL)
  
  if (!inherits(x, c("summarytools", "stby")) &&
    !(inherits(x, "by") && inherits(x[[1]], "summarytools"))) {
    stop("x must be an object of class 'summarytools', or one of ",
         "'stby', 'by' and 'list' containing grouped summarytools results")
  }
  
  #mc <- match.call()
  #obj_names <- extract_names(mc, parent.env())
  #if (isTRUE(rc))
  #  on.exit(on.exit({cat("clearing .st_env$mc"); .st_env$mc <- list()}))
  
  errmsg <- check_args_tb(match.call())
  
  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "))
  }

  UseMethod("tb", x)
}

#' @exportS3Method tb default
tb.default <- function(x, order = 1, drop.var.col = FALSE,
                       recalculate = TRUE, fct.to.chr = FALSE, ...) {
  
  st_type <- attr(x, "st_type")
  method <- paste0("tb_", st_type)
  
  if (!exists(method, mode = "function")) {
    stop("tb() does not recognize or support this type of object")
  }
  
  do.call(
    method, 
    list(x = x, order = order, drop.var.col = drop.var.col,
         recalculate = recalculate, fct.to.chr = fct.to.chr, ...)
  )
}

#' @exportS3Method tb summarytools
tb.summarytools <- function(x, order = 1, drop.var.col = FALSE,
                            recalculate = TRUE, fct.to.chr = FALSE, ...) {
  
  st_type <- attr(x, "st_type")
  method <- paste0("tb_", st_type)
  
  if (!exists(method, mode = "function")) {
    stop("tb() does not recognize or support this type of object")
  }
  
  do.call(
    method, 
    list(x = x, order = order, drop.var.col = drop.var.col,
         recalculate = recalculate, fct.to.chr = fct.to.chr, ...)
  )
}


#' @exportS3Method tb by
tb.by <- function(x, order = 1, drop.var.col = FALSE,
                  recalculate = TRUE, fct.to.chr = FALSE, ...) {
  do.call(
    tb.stby, 
    list(x = x, order = order, drop.var.col = drop.var.col,
         recalculate = recalculate, fct.to.chr = fct.to.chr, ...)
  )
}

#' @exportS3Method tb stby
tb.stby <- function(x, order = 1, drop.var.col = FALSE,
                    recalculate = TRUE, fct.to.chr = FALSE, ...) {

  st_type <- attr(x[[1]], "st_type")
  method <- paste0("tb_stby_", st_type)
  
  if (!exists(method, mode = "function")) {
    stop("tb() does not support this type of object")
  }
  
  do.call(
    method, 
    list(x = x, order = order, drop.var.col = drop.var.col,
         recalculate = recalculate, fct.to.chr = fct.to.chr, ...)
  )
}

# Handles lists containing freq objects when freq() is called on a df 
#' @exportS3Method tb list
tb.list <- function(x, order = 1, drop.var.col = FALSE,
                    recalculate = FALSE, fct.to.chr = TRUE, ...) {

  if (isTRUE(recalculate))
    message("recalculate is only applicable to by-group results")
  
  if (!isTRUE(fct.to.chr))
    message("fct.to.char argument ignored")
  
  if (order != 1)
    message("order argument ignored")
  
  if (attr(x[[1]], "st_type") != "freq") 
    stop("tb() does not support this type of list")
  
  if (isTRUE(recalculate))
    message("recalculate argument ignored")
  
  # Get first set of results
  gr_res <- do.call(tb_freq,
                    list(x = x[[1]], order = 1,
                         recalculate = FALSE, fct.to.chr = TRUE))
  
  colnames(gr_res)[1] <- "value"
  
  output <- dplyr::bind_cols(
    variable = attr(x[[1]], "data_info")$Variable,
    gr_res
  )
  
  for (group in 2:length(x)) {
    gr_res <- do.call(tb_freq, 
                      list(x = x[[group]], order = 1,
                           recalculate = FALSE, fct.to.chr = TRUE))
    
    colnames(gr_res)[1] <- "value"
    
    output <- dplyr::bind_rows(
      output,
      dplyr::bind_cols(
        variable = attr(x[[group]], "data_info")$Variable,
        gr_res
      )
    )
  }

  return(output)
}

tb_stby_freq <- function(x, order = 1, drop.var.col = FALSE,
                         recalculate = TRUE, fct.to.chr = FALSE, ...)  {

  # initialise variables relevant only to stby() objects
  null_grps  <- integer() # relevant to by() objects only
  var_is_fct <- attr(x[[1]], "data_info")$Data.type == "Factor"
  
  if ("groups" %in% names(attributes(x))) {
    # produced through group_by - combinations of grouping vars 
    # is already given in x groups attributes
    gr_combs  <- attr(x, "groups")
    if (isTRUE(fct.to.chr)) {
      for (i in 1:ncol(gr_combs))
        if (is.factor(gr_combs[[i]]))
          gr_combs[[i]] <- as.character(gr_combs[[i]])
    }
  } else {
    # Build grouping combinations based on dimnames (for "by" class)
    gr_combs  <- as_tibble(
      expand.grid(
        attr(x, "dimnames"), stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE
      ))
    
    # Remove rows corresponding to NULL groups
    null_grps <- which(sapply(x, is.null))
    if (length(null_grps) > 0)
      gr_combs <- gr_combs[-null_grps,]
  }
  
  nb_gr_var <- ncol(gr_combs)
  
  # Dispatch to tb_freq
  grp_stats <- lapply(x, function(group) {
    tb(group, fct.to.chr = fct.to.chr)
  })
  
  # Eliminate null groups
  if (length(null_grps) > 0) {
    grp_stats <- grp_stats[-null_grps]
  }
  
  # Add grouping variables to each group (must be done *before* binding rows
  # otherwise we *could* have mismatch)
  for (g in seq_along(grp_stats)) {
    grp_stats[[g]] <- dplyr::bind_cols(gr_combs[g,], grp_stats[[g]])
  }

  output <- dplyr::bind_rows(grp_stats)
  var_name <- colnames(output)[nb_gr_var + 1]
  
  # Adjust ordering of rows and columns ('order' arg)
  if (order %in% c(2,3)) {
    output <- output[
      do.call(
        what = "order",
        args = unname(
          output[ ,c(nb_gr_var + 1, 1:nb_gr_var)])), ]
  }
  
  if (isTRUE(recalculate)) {
    
    output[,(nb_gr_var + 3):ncol(output)] <- NA
      
    cn <- colnames(output)
    if ("pct" %in% cn) {
      output$pct <- output$freq / sum(output$freq) * 100
    }

    if ("pct_cum" %in% cn) {
      output$pct_cum <- cumsum(output$pct)
    }

    if ("pct_tot" %in% cn) {
      output$pct_tot <- output$freq / sum(output$freq) * 100
    }

    if ("pct_tot_cum" %in% cn) {
      output$pct_tot_cum <- cumsum(output$pct_tot)
    }

    if ("pct_valid" %in% cn) {
      ind_valid <- which(!is.na(output[[nb_gr_var + 1]]))
      output$pct_valid[ind_valid] <- output$freq[ind_valid] /
        sum(output$freq[ind_valid]) * 100
    }

    if ("pct_valid_cum" %in% cn) {
        output$pct_valid_cum[ind_valid] <- cumsum(output$pct_valid[ind_valid])
    }
  }
  
  # Restore factor state of variable if applicable
  if (!isTRUE(fct.to.chr) && 
      attr(x[[1]], "data_info")$Data.type == trs("factor")) {
    
    na_val <- attr(x[[1]], "format_info")$na.val %||% NA
    lev <- head(dimnames(x[[1]])[[1]], -1) # leave out "Total"
    # if last category is "<NA>", we also leave it out, as it was 
    # (most probably) freq()'s value
    if (tail(lev, 1) == "<NA>")
      lev <- head(lev, -1)
    if (!is.na(na_val))
      output[[var_name]][which(is.na(output[[var_name]]))] <- na_val
    
    output[[var_name]] <- factor(output[[var_name]], levels = lev)
  }

  # Convert factors to character (grouping vars only)
  else if (isTRUE(fct.to.chr)) {
    for (i in 1:nb_gr_var) {
      if (is.factor(output[[i]])) {
        output[[i]] <- as.character(output[[i]])
        # Replace value "NA" with actual NA
        if (isTRUE(attr(x, "useNA")))
          output[[i]][which(output[[i]] == "NA")] <- NA
      }
    }
  }
  
  # Place main var first when order = 3
  if (order == 3) {
    output <- output[ ,c(nb_gr_var + 1,
                         1:nb_gr_var,
                         (nb_gr_var + 2):ncol(output))]
  }
  
  return(output)
}

tb_stby_descr <- function(x, order = 1, drop.var.col = FALSE,
                          recalculate = NA, fct.to.chr = FALSE, ...) {
  
  grp_stats <- lapply(x, function(group) {
    tb(group, drop.var.col = FALSE)
  })
  
  if ("groups" %in% names(attributes(x))) {
    left_part <- as_tibble(
      merge(grp_stats[[1]][,1], attr(x, "groups"), all = TRUE)[,-1]
    )
    
    # Special case of descr (one grouping variable)
    if (identical(colnames(left_part), "value")) {
      colnames(left_part) <- colnames(attr(x, "group"))
    }
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
    
    left_part <- as_tibble(merge(grp_stats[[non_null_grs[1]]][,1],
                                 grp_values, all = TRUE))[,-1]
  }
  
  nb_gr_var  <- ncol(left_part)
  right_part <- bind_rows(grp_stats)
  
  if (all(right_part$variable == "value") &&
      length(names(grp_values)) == 1) {
    right_part$variable <- attr(x[[1]], "data_info")$Variable
  }
  output <- bind_cols(left_part, right_part)
  
  colnames(output)[1:ncol(left_part)] <-
    sub("(.+)\\$(.+)", "\\2", colnames(output)[1:ncol(left_part)])
  
  if (order %in% 2:3) {
    output <- 
      output[do.call(what = "order",
                     args = unname(output[ ,c(nb_gr_var + 1, 1:nb_gr_var)])), ]
    
    if (order == 3) {
      output <- output[ ,c(nb_gr_var + 1,
                           1:(nb_gr_var),
                           (nb_gr_var + 2):ncol(output))]
    }
  }
  
  # Convert factors to character
  if (isTRUE(fct.to.chr)) {
    for (i in 1:(nb_gr_var + 1)) {
      if (is.factor(output[[i]]))
        output[[i]] <- as.character(output[[i]])
    }
  }
  
  return(output)
}

# Single freq object -------------------------------------------------------
tb_freq <- function(x, order = 1, drop.var.col = FALSE,
                    recalculate = FALSE, fct.to.chr = FALSE, ...) {

  # Flags for columns to keep in the output
  report.nas <- attr(x, "format_info")$report.nas
  cumul      <- attr(x, "format_info")$cumul

  na_val  <- attr(x, "format_info")$na.val %||% "<NA>"

  # Get right output columns based on parameters of freq() call 
  # 1) report.nas
  # 2) cumul 
  # TT = TRUE for 1 & 2; TF = TRUE for 1, FALSE for 2, etc.
  op <- paste0(substr(report.nas, 1, 1), 
               substr(cumul,1, 1))

  col_sets <- list(
    TT = list(
      columns = c(
        freq          = double(1),
        pct_valid     = double(1),
        pct_valid_cum = double(1),
        pct_tot     = double(1),
        pct_tot_cum = double(1)
      ),
      colnums = 1:5
    ),
    TF = list(  # no cumul
      columns = c(
        freq      = double(1),
        pct_valid = double(1),
        pct_tot   = double(1)
      ),
      colnumss = c(1L, 2L, 4L) # f + pct_valid + pct_tot
    ),
    FT = list(  # no nas
      columns = c(
        freq = double(1),
        pct = double(1),
        pct_cum = double(1)
      ),
      colnumss = c(1L, 2L, 3L) # f + pct_valid + pct_valid_cum
    ),
    FF = list(
      columns = c(
        freq = double(1),
        pct  = double(1)
      ),
      colnums = c(1L, 2L) # freq + pct_valid
    )
  )
  
  output <- tibble::tibble(
    dplyr::bind_cols(
      value = rownames(x)[-nrow(x)],
      x[-nrow(x), col_sets[[op]]$colnums]
    )
  )

  colnames(output)[-1] <- names(col_sets[[op]]$columns)
  
  # Change value "<NA>" to proper NA
  output$value[which(output$value == na_val)] <- NA

  # Discard NA rows for the main variable when report.nas = FALSE
  # in the freq() function call
  if (!isTRUE(report.nas)) {
    output <- output[-which(is.na(output$value)),]
  }
  
  # Restore factor state of variable 
  # (unless called by tb_stby_freq)
  if (!isTRUE(fct.to.chr) && 
      attr(x, "data_info")$Data.type == trs("factor") &&
      !"tb_stby_freq()" %in% lapply(sys.calls(), head, 1)) {
    if (na_val != "<NA>") {
      lev <- c(as.character(na.omit(output$value)),
               na_val)
      output$value[which(is.na(output$value))] <- na_val
    } else {
      lev <- output$value
    }
    output$value <- factor(output$value, levels = lev)
  }

  # Set value column's title to the main varname
  varname <- attr(x, "data_info")$Variable
  if (!is.null(varname) && make.names(varname) == varname) {
    colnames(output)[1] <- varname
  }
  
  return(output)
}

# Single descr object ------------------------------------------------------
tb_descr <- function(x, order = 1,  drop.var.col = FALSE,
                     recalculate = FALSE, fct.to.chr = FALSE, ...) {
  
  if (!isTRUE(attr(x, "data_info")$transposed)) {
    output <- as_tibble(t(as.data.frame(x)), rownames = "variable")
    names(output) <- c("variable", attr(x, "stats"))
  } else {
    output <- as_tibble(as.data.frame(x), rownames = "variable")
    names(output) <- c("variable", attr(x, "stats"))
  }
  
  if (isTRUE(drop.var.col)) {
    if (length(unique(output$variable)) == 1) {
      output$variable <- NULL
    } else {
      message("argument drop.var.col ignored")
    }
  }

  # Convert factors to character
  if (isTRUE(fct.to.chr) && is.factor(output[[1]])) {
    output[[1]] <- as.character(output[[1]])
  }

  return(output)
}
