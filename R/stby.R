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
#' @param useNA Make NA a valid grouping value in INDICES variable(s).
#'   Set to \code{FALSE} explicitly to eliminate message.
#'
#' @return An object of classes \dQuote{list} and \dQuote{summarytools},
#'   giving results for each subset.
#'
#' @details When the grouping variable(s) contain NA values, the
#'   \code{base::\link[base]{by}} function (as well as summarytools 
#'   versions prior to 1.1.0) ignores corresponding groups. Version 1.1.0
#'   allows setting \code{useNA = TRUE} to make new groups using
#'   NA values on the grouping variable(s), just as
#'   \code{dplyr::\link[dplyr]{group_by}} does.
#'   
#'   When NA values are detected and \code{useNA = FALSE}, a message is
#'   displayed; to disable this message, set \code{check.nas = FALSE}.
#'
#' @examples
#' data("tobacco")
#' with(tobacco, stby(data = BMI, INDICES = gender, FUN = descr,
#'                    check.nas = FALSE))
#' with(tobacco, stby(data = smoker, INDICES = gender, freq, useNA = TRUE))
#' with(tobacco, stby(data = list(x = smoker, y = diseased),
#'                    INDICES = gender, FUN = ctable, useNA = TRUE))
#'                    
#' @seealso \code{\link[base]{by}}, \code{\link[dplyr]{group_by}}
#' @keywords utilities
#' @importFrom tibble as_tibble
#' @export
stby <- function(data, INDICES, FUN, ..., useNA = FALSE) {
  .p_reset()
  # Check that FUN is a summarytools function
  mc <- match.call()

  if (!"FUN" %in% names(mc))
    stop("FUN argument is missing in call to stby()")
  
  if (!is.function(FUN))
    stop(paste(mc$FUN, "is not a function"))
  
  #if (getNamespaceName(environment(FUN)) != "summarytools")
  #  stop("stby only supports summarytools functions")
  # getNamespaceName(environment(get(body(FUN)[[1]])))
  
  dd <- as.data.frame(data)

  if (identical(FUN, summarytools::freq) && ncol(dd) > 1) 
    stop("when using freq() with stby(), only one variable may be analysed;",
         "if only basic console output is needed, use by()")
  
  if (!is.list(INDICES)) {
    IND <- vector("list", 1L)
    IND[[1L]] <- INDICES
    names(IND) <- deparse(substitute(INDICES))[1L]
  } else {
    IND <- INDICES
  }
  
  # Retain classes & levels for the groups attribute
  IND_classes <- lapply(IND, class)
  if ("factor" %in% IND_classes) {
    IND_levels <- vector("list", length(IND_classes))
    for (i in seq_along(IND_classes)) {
      if (IND_classes[[i]] == "factor")
        IND_levels[[i]] <- levels(IND[[i]])
    }
  }
  
  if (isTRUE(useNA)) {
    for (i in seq_along(IND)) {
      if (anyNA(IND[[i]]))
        IND[[i]] <- forcats::fct_na_value_to_level(IND[[i]], "NA")
    }
  } else if (missing(useNA)) {
    if (any(sapply(IND, anyNA)))
      message(paste("NA detected in grouping variable(s); consider using",
                    "useNA = TRUE"))
  }
  
  FUNx <- function(x) FUN(dd[x, , drop = FALSE], ...)
  nd <- nrow(dd)
  res <- structure(eval(substitute(tapply(seq_len(nd), IND, FUNx, 
                                          simplify = TRUE)), dd),
                   call = match.call(), class = "stby")
  
  # add groups attribute
  groups <- as_tibble(
    expand.grid(
      attr(res, "dimnames"),
      stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE
    ), .name_repair = "minimal")
  
  # remove df names if present (zzz$abc --> abc)
  colnames(groups) <- sub(".+\\$", "", colnames(groups))
  
  # Replace "NA" with actual NA to restore original classes
  if (isTRUE(useNA))
    for (col in seq_along(groups))
      groups[[col]][groups[[col]] == "NA"] <- NA
  
  # Restore original classes
  for (i in seq_along(groups)) {
    if (class(groups[[i]]) != IND_classes[[i]]) {
      if (IND_classes[[i]] == "factor")
        groups[[i]] <- factor(groups[[i]], levels = IND_levels[[i]])
      else 
        try(
          groups[[i]] <- get(paste0("as.", IND_classes[[i]][1]))(groups[[i]]),
          silent = TRUE
        )
    }
  }
  
  # remove NULL elements (has side-effect of removing dim and dimnames)
  non_null_ind <- which(!vapply(res, is.null, logical(1)))
  if (length(non_null_ind)) {
    atr <- attributes(res)
    res <- res[non_null_ind]
    attributes(res) <- atr[c("call", "class")]
    groups <- groups[non_null_ind,]
  }
  
  # Set useNA as attribute; to be used by tb()
  attr(res, "useNA")  <- useNA
  attr(res, "groups") <- groups
  
  # set names
  if (ncol(groups) == 1 && length(res) == length(groups[[1]])) {
    names(res) <- groups[[1]]
  } else {
    names(res) <- vapply(res, function(gr) attr(gr, "data_info")$Group,
                         character(1))
  }
  #.e_reset()
  return(res)
}
