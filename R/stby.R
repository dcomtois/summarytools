#' Obtain Grouped Statistics With summarytools
#'
#' An adaptation base R's \code{\link{by}} function, designed to
#' optimize the results' display.
#'
#' @usage stby(data, INDICES, FUN, ..., simplify = TRUE)
#' 
#' @param data an R object, normally a data frame, possibly a matrix.
#' @param INDICES a grouping variable or a list of grouping variables,
#'  each of length \code{nrow(data)}.
#' @param FUN a function to be applied to (usually data-frame) subsets of data.
#' @param \dots Further arguments to FUN.
#' @param simplify Logical. Essentially a placeholder to maintain full
#'  compatibility with base by. For more details, see 
#'  \code{\link[base]{tapply}}.
#'
#' @return An object of classes \dQuote{list} and \dQuote{summarytools},
#'   giving results for each subset.
#'
#' @details When the grouping variable(s) contain NA values, the
#'   corresponding rows of data will be ignored. To circumvent this
#'   potential drawback, consider using factors with an explicit NA
#'   level (for instance using
#'   \code{\link[forcats]{fct_na_value_to_level}}), or using
#'   \code{dplyr::\link[dplyr]{group_by}}, which creates additional groups
#'   for NA values.
#'
#' @examples
#' data("tobacco")
#' with(tobacco, stby(BMI, gender, descr))
#' 
#' @seealso \code{\link[base]{by}}, \code{\link[dplyr]{group_by}}
#' @keywords utilities
#' @export
stby <- function(data, INDICES, FUN, ..., simplify = TRUE) {
  if (sum(is.na(INDICES))) {
    mc <- pryr::standardise_call(match.call())
    caller_fun <- as.character(mc$FUN)
    if (!isTRUE(st_options(paste(caller_fun, "silent", sep = ".")))) {
      if (caller_fun %in% c("descr", "freq") && !("weights" %in% names(mc))) {
        message(paste("NA detected in grouping variable; consider recoding NAs",
                      "or using dplyr::group_by"))
      } else {
      message("NA detected in grouping variable; consider recoding NAs")
      }
    }
  }
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
