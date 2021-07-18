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
  UseMethod("stby", data)
}
  
#' @method stby data.frame
#' @keywords utilities
#' @export
stby.data.frame <- function(data, INDICES, FUN, ..., simplify = TRUE) {
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
stby.default <- function(data, INDICES, FUN, ..., simplify = TRUE) {
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
