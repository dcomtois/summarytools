#' Obtain Grouped Statistics With summarytools
#'
#' An adaptation base R's \code{\link{by}} function, designed to
#' optimize the results' display.
#'
#' @param data an R object, normally a data frame, possibly a matrix.
#' @param INDICES a grouping variable or a list of grouping variables,
#'  each of length \code{nrow(data)}.
#' @param FUN a function to be applied to (usually data-frame) subsets of data.
#' @param \dots Further arguments to FUN.
#' @param check.nas Logical. Check for NA values in INDICES. \code{TRUE} by
#'   default. Ignored if explicit.nas is \code{TRUE}.
#' @param explicit.nas Make NA a valid grouping value in INDICES variable(s).
#'
#' @return An object of classes \dQuote{list} and \dQuote{summarytools},
#'   giving results for each subset.
#'
#' @details When the grouping variable(s) contain NA values, the
#'   \code{base::\link[base]{by}} function (as well as summarytools 
#'   versions prior to 1.1.0) ignores corresponding groups. Version 1.1.0
#'   allows setting \code{explicit.nas = TRUE} to make new groups using
#'   NA values on the grouping variable(s), just as
#'   \code{dplyr::\link[dplyr]{group_by}} does.
#'   
#'   When NA values are detected and `explicit.nas = FALSE`, a message is
#'   displayed; to disable this message, set `check.nas = FALSE`.
#'
#' @examples
#' data("tobacco")
#' with(tobacco, stby(BMI, gender, descr, check.nas = FALSE))
#' with(tobacco, stby(data = smoker, INDICES = gender, explicit.nas = TRUE))
#' with(tobacco, stby(data = list(x = smoker, y = diseased),
#'                    INDICES = gender, FUN = freq, explicit.nas = TRUE))
#'                    
#' @seealso \code{\link[base]{by}}, \code{\link[dplyr]{group_by}}
#' @keywords utilities
#' @export
stby <- function(data, INDICES, FUN, ..., 
                 explicit.nas = FALSE, check.nas = !explicit.nas) {
  UseMethod("stby", data)
}

#' @method stby default
#' @export
stby.default <- function(data, INDICES, FUN, ..., 
                         explicit.nas = FALSE, check.nas = !explicit.nas) {
  
  dd <- as.data.frame(data)

  if (length(dim(data))) {
    stby(dd, INDICES, FUN, ..., 
         explicit.as = explicit.nas, check.nas = check.nas)
  }
  
  else {
   
    if (!is.list(INDICES)) {
      IND <- vector("list", 1L)
      IND[[1L]] <- INDICES
      names(IND) <- deparse(substitute(INDICES))[1L]
    }
    
    else IND <- INDICES
    
    if (isTRUE(explicit.nas)) {
      for (i in seq_along(IND)) {
        if (anyNA(IND[[i]]))
          IND[[i]] <- forcats::fct_na_value_to_level(IND[[i]], "NA")
      }
    } else if (isTRUE(check.nas)) {
      check_nas(x = IND, mc = match.call())
    }
    
    FUNx <- function(x) FUN(dd[x, ], ...)
    nd <- nrow(dd)
    structure(eval(substitute(tapply(seq_len(nd), IND, FUNx, 
                                     simplify = TRUE)), dd),
              call = match.call(), 
              class = "stby")
  }
}
  
#' @method stby data.frame
#' @export
stby.data.frame <- function(data, INDICES, FUN, ..., 
                            explicit.nas = FALSE, check.nas = !explicit.nas) {
  
  if (identical(FUN, summarytools::freq) && ncol(data) > 1) {
    stop("when using freq() with stby(), only one variable may be analysed")
  }
  
  if (!is.list(INDICES)) {
    IND <- vector("list", 1L)
    IND[[1L]] <- INDICES
    names(IND) <- deparse(substitute(INDICES))[1L]
  }
  
  else IND <- INDICES
  
  if (isTRUE(explicit.nas)) {
    for (i in seq_along(IND)) {
      if (anyNA(IND[[i]]))
        IND[[i]] <- forcats::fct_na_value_to_level(IND[[i]], "NA")
    }
  } else if (isTRUE(check.nas)) { 
    check_nas(x = IND, mc = match.call())
  }

  FUNx <- function(x) FUN(data[x, , drop = FALSE], ...)
  nd <- nrow(data)
  structure(eval(substitute(tapply(seq_len(nd), IND, FUNx, 
     simplify = TRUE)), data), call = match.call(), class = "stby")
}


check_nas <- function(x, mc) {
  caller_fun <- as.character(mc$FUN)
  if (exists(caller_fun, mode = "function") && any(sapply(x, anyNA))) {
    message(paste("NA detected in grouping variable(s);",
                  "consider recoding ising stby's explicit.nas = TRUE",
                  "parameter"))
  }
}
