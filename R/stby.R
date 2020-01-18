#' Obtain Grouped Statistics With summarytools
#'
#' This is essentially the base \code{\link{by}} function, except for the 
#' class of the returned object.
#'
#' @usage stby(data, INDICES, FUN, ..., simplify = TRUE)
#' 
#' @param data an R object, normally a data frame, possibly a matrix.
#' @param INDICES a grouping variable or a list of grouping variables,
#'  each of length nrow(data).
#' @param FUN a function to be applied to (usually data-frame) subsets of data.
#' @param \dots Further arguments to FUN.
#' @param simplify Logical. Essentially a placeholder to maintain full
#'  compatibility with base by. For more details, see 
#'  \code{\link[base]{tapply}}.
#'
#' @return An object having classes \dQuote{list} and \dQuote{summarytools}.
#'
#' @examples
#' data("tobacco")
#' with(tobacco, stby(BMI, gender, descr))
#' 
#' @seealso \code{\link[base]{by}}, \code{\link[base]{tapply}}
#' @keywords utilities
#' @export
stby <- function(data, INDICES, FUN, ..., simplify = TRUE) {
  
  # Check if variables are both in data and in INDICES
  gr_vars <- setdiff(as.character(substitute(INDICES)), "list")
  birole_var <- intersect(gr_vars, deparse(substitute(data)))
  if (length(birole_var) && (is.vector(data) || is.factor(data))) {
    warning(birole_var[1], " appears to be both a grouping as well as a data ",
            "variable; this could cause problems when generating results")
  }
  
  # Check for NA's in grouping variables
  na_counts <- rep(0, length(gr_vars))
  
  for (i in seq_along(gr_vars))
    na_counts[i] <- sum(is.na(INDICES[[i]]))
  
  if (sum(na_counts)) {
    message("NA", if (sum(na_counts) > 1) "'s", 
            " detected in the following INDICE variable",
            if (sum(na_counts > 0) > 1) "s",  ": ", appendLF = FALSE)
    for (i in seq_along(gr_vars)) {
      if (na_counts[i] > 0) {
        if (i == 1)
          message(gr_vars[i], appendLF = FALSE)
        else
          message(", ", gr_vars[i], appendLF = FALSE)
      }
    }
    message("\n  To include NA groups, consider using dplyr::group_by or",
            "  using forcats::fct_explicit_na")
  }
  UseMethod("stby", data)
}
  
#' @method stby data.frame
#' @keywords utilities
#' @export
stby.data.frame <- function (data, INDICES, FUN, ..., simplify = TRUE) {
  if (identical(FUN, summarytools::freq) && ncol(data) > 1) {
    stop("when using freq() with stby(), only one variable may be analysed")
  }
  if (!is.list(INDICES)) {
    IND <- vector("list", 1L)
    IND[[1L]] <- INDICES
    names(IND) <- deparse(substitute(INDICES))[1L]
  }
  else IND <- INDICES
  FUNx <- function(x) FUN(data[x, , drop = FALSE], ...)
  nd <- nrow(data)
  structure(eval(substitute(tapply(seq_len(nd), IND, FUNx, 
     simplify = simplify)), data), call = match.call(), class = "stby")
}


#' @method stby default
#' @export
stby.default <- function (data, INDICES, FUN, ..., simplify = TRUE) {
  dd <- as.data.frame(data)
  if (length(dim(data))) 
    by(dd, INDICES, FUN, ..., simplify = simplify)
  else {
    if (!is.list(INDICES)) {
      IND <- vector("list", 1L)
      IND[[1L]] <- INDICES
      names(IND) <- deparse(substitute(INDICES))[1L]
    }
    else IND <- INDICES
    FUNx <- function(x) FUN(dd[x, ], ...)
    nd <- nrow(dd)
    structure(eval(substitute(tapply(seq_len(nd), IND, FUNx, 
      simplify = simplify)), dd), call = match.call(), 
      class = "stby")
  }
}
