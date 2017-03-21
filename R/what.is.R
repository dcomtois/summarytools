#' Obtain Extended Properties of Objects
#'
#' Combination of most common \dQuote{macro-level} functions that describe an object.
#'
#' Instead of calling, in turn, \code{\link{class}}, \code{\link{typeof}},
#' \code{\link{dim}}, and so on, a call to this function will readily give all this
#' information at once.
#'
#' @param x Any object.
#' @param show.all Logical. When \code{TRUE}, all logical results from the
#'   \dQuote{is.} \emph{identifier functions} will be displayed, with a warning
#'   message when the result applies only to the first element in the structure.
#'   \code{FALSE} by default.
#' @param ignore.size.warn Set to \code{TRUE} to force execution of the function
#'   for large (> 20 K-bytes) objects. Defaults to \code{FALSE}.
#'
#' @return A list with following elements:
#' \describe{
#'   \item{properties}{A dataframe with the class(es), type, mode and storage
#'     mode of the object as well as the dim, length and object.size.}
#'   \item{attributes.lengths}{A named character vector giving all attributes
#'     (\emph{c.f.} \dQuote{names}, \dQuote{row.names}, \dQuote{class},
#'     \dQuote{dim}, and so forth) along with their length.}
#'   \item{extensive.is}{A character vector of all the \emph{identifier functions.}
#'     (starting with \dQuote{is.}) that yield \code{TRUE} when used with \code{x}
#'     as argument.}
#'   \item{function.type}{When x is a function, results of \code{\link[pryr]{ftype}}
#'     are added.}
#' }
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#'
#' @examples
#' what.is(1)
#' what.is(NaN)
#' what.is(iris3)
#' what.is(print)
#' what.is(what.is)
#'
#' @seealso \code{\link{class}}, \code{\link{typeof}}, \code{\link{mode}},
#' \code{\link{storage.mode}}, \code{\link{dim}}, \code{\link{length}},
#' \code{\link{is.object}}, \code{\link[pryr]{otype}},
#' \code{\link{object.size}}, \code{\link[pryr]{ftype}}
#'
#' @keywords attribute classes utilities
#'
#' @export
what.is <- function(x, show.all=FALSE, ignore.size.warn=FALSE) {

  if(!is.function(x) && object.size(x) > 20000 && ignore.size.warn == FALSE) {
    stop("object.size(x) is greater than 10K; computing time might be long. Set argument ignore.size.warn to TRUE to force execution anyway")
  }

  # set the warn option to -1 to temporarily ignore warnings
  op <- options("warn")
  options(warn = -1)
  on.exit(options(op))

  # Part 1. Data Properties - class, typeof, mode, storage.mode
  properties <- data.frame(property=c("class", "typeof", "mode", "storage.mode", "dim", "length",
                                      "is.object","object.type","object.size"),
                           value = c(paste(class(x),collapse=" "), typeof(x), mode(x), storage.mode(x),
                                     paste(dim(x), collapse = " x "), length(x),
                                     base::is.object(x), pryr::otype(x), paste(object.size(x), "Bytes")))


  # Part 2. Make a list of all x's attribute and their length
  attributes.lengths <- sapply(attributes(x),length)
  if(length(attributes.lengths)==0)
    attributes.lengths <- NULL


  # Part 3. Test object against all "is.[...]" functions
  # Look for all relevant functions
  list.id.fun <- grep(methods(is), pattern = "<-", invert = TRUE, value = TRUE)

  # remove is.R which is not relevant and can take a lot of time
  list.id.fun <- setdiff(list.id.fun, "is.R")

  # loop over all "is" functions with x as argument, and store the results
  if(!show.all) {
    extensive.is <- c()
    for(fun in list.id.fun) {
      res <- try(eval(call(fun,x)),silent=TRUE)
      if(isTRUE(res))
        extensive.is <- append(extensive.is, fun)
    }
  } else {

    # Generate table of all identifier tests
    extensive.is <- data.frame(test=character(), value=character(),
                                   warnings=character(), stringsAsFactors = FALSE)

    # loop over all functions with x as argument, and store the results
    for(fun in list.id.fun) {
      value <- try(eval(call(fun,x)),silent=TRUE)
      if(class(value)=="try-error") {
        next() # ignore tests that yield an error
      } else if (length(value)>1) {
        warn <- "!!! Logical value applies only to the first element of the provided object !!!"
        value <- paste(value,sep="")
      } else {
        warn <- ""
      }
      extensive.is[nrow(extensive.is)+1,] <- list(fun, value, warn)
    }

    # sort the results according to the results and warnings if any
    extensive.is <- extensive.is[order(extensive.is$value, extensive.is$warnings=="", decreasing = TRUE),]
  }

  # Part 4. Get info on the type of object - S3, S4, attributes / slots

  #obj.otype <- pryr::otype(x)

  if(is.function(x)) {
  	function.type <- pryr::ftype(x)
  } else {
    function.type <- NULL
  }

  output <- list()
  output$properties <- properties
  output$attributes.lengths <- attributes.lengths
  output$extensive.is <- extensive.is
  output$function.type <- function.type

  return(output)
}
