#' Get Variable or Data Frame Label
#'
#' This function returns character value previously stored in variable or data
#' frame's \code{label} attribute, or \code{NA} if none found.
#' @param x An R object to extract labels from
#' @param all Logical. When x is a data frame, setting this argument to \code{TRUE} will
#'   make the function return all variable labels. By default, its value is \code{FALSE},
#'   so that if x is a data frame, it is the data frame's label that will be returned.
#' @param fallback a logical value indicating if labels should fallback to object name(s).
#'   Defaults to \code{FALSE}.
#' @param simplify When x is a data frame and \code{all = TRUE}, coerce results to a vector
#'   when \code{TRUE}, otherwise (default) return a \code{named list} containing only
#'   non-NULL/non-NA elements.
#' @seealso \code{\link[rapportools]{label}}, \code{\link[Hmisc]{label}}
#' @author
#' Dominic Comtois, \email{dominic.comtois@@gmail.com},
#' Gergely Daróczi, \email{daroczig@@rapporter.net}
#' @note Loosely based on Gergely Daróczi's \code{\link[rapportools]{label}} function.
#' @references \url{https://github.com/Rapporter/rapportools}
#' @export
label <- function(x, all = FALSE, fallback = FALSE, simplify = FALSE) {

  if (base::missing(x))
    stop('No variable / data frame provided.')

  if (is.null(x))
    stop("x can not be NULL")

  if (is.atomic(x)) {
    lbl <- attr(x, which = 'label', exact = TRUE)
    if (is.null(lbl)){
      if (isTRUE(fallback)) {
        lbl <- tail(as.character(substitute(x)), 1)
      } else {
        lbl <- NA
      }
    }
  } else if (is.data.frame(x)) {
    if (isTRUE(all)) {
      lbl <- sapply(x, attr, which = 'label', exact = TRUE)
      lbl[which(sapply(lbl, is.null))] <- NA
      if (isTRUE(fallback)) {
        lbl[which(is.na(lbl))] <- colnames(x)[which(is.na(lbl))]
      }
      if (isTRUE(simplify)) {
        lbl <- as.character(lbl)
      } else {
        lbl <- lbl[which(!is.na(lbl))]
      }
    } else {
      lbl <- attr(x, which = 'label', exact = TRUE)
      if (is.null(lbl)) {
        if (isTRUE(fallback)) {
          lbl <- tail(as.character(substitute(x)), 1)
        } else {
          lbl <- NA
        }
      }
    }
  }
  return(lbl)
}

#' Set Variable or Data Frame Label
#'
#' Defines a data frame or variable label by using the \code{label} attribute.
#'
#' @param x The data frame or variable to be labelled.
#' @param value String to be used as label.
#' @export
"label<-" <- function(x, value) {

  if (base::missing(x) || base::missing(value))
    stop('Both x and value arguments must be provided')

  if (is.data.frame(x)) {
    if (length(value) > 1 && length(value) < ncol(x))
      stop("Number of labels does not match number of columns in x")

    if (is.na(value)) {
      attr(x, 'label') <- NULL
    } else if (length(value) == 1) {
      attr(x, 'label') <- value
    } else if (length(value) == ncol(x)) {
      for (i in seq_along(value)) {
        attr(x[[i]], 'label') <- value[i]
      }
    } else {
      stop(paste("provide a single string to label the data frame, or a vector of characters",
                 "having length = ncol(x)"))
    }
  } else if (is.atomic(x)) {
    if (is.na(value)) {
      attr(x, 'label') <- NA
    } else if (length(value) > 1) {
      stop("A variable label must be a character vector of length 1")
    } else {
      attr(x, 'label') <- value
    }
  }
  return(x)
}
