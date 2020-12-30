#' Delete Temporary Html Files
#'
#' Delete temporary files created when using generic print method with 
#' \code{method='browser'} or \code{method='viewer'}, or when calling
#' \code{view()} function.
#'
#' @param all Logical. When \code{TRUE} (default), all temporary summarytools 
#'   files are deleted. When \code{FALSE}, only the latest file is.
#' @param silent Logical. Hide confirmation messages (\code{FALSE} by default).
#' @param verbose Logical. Display a message for every file that is deleted.
#'   \code{FALSE} by default.
#'
#' @return NULL
#'
#' @note Given that all temporary files are deleted automatically when an R
#' session is ended, this function is an overkill in most circumstances. It 
#' could however be useful in server-type setups.
#'
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com>}
#'
#' @keywords IO
#'
#' @export
#' @importFrom utils tail
cleartmp <- function(all=TRUE, silent = FALSE, verbose = FALSE) {
  if (length(.st_env$tmpfiles) == 0) {
    if (!silent)
      message("No temporary files to delete.")
  } else if (isTRUE(all) || all == 1 || all == "all") {
    nfiles <- 0
    for (tmpfile in .st_env$tmpfiles) {
      nfiles <- nfiles + 1
      if (isTRUE(verbose))
        message(paste("Deleting", tmpfile))
      unlink(tmpfile)
    }
    .st_env$tmpfiles <- c()
    if (!isTRUE(silent))
      message(paste(nfiles, "file(s) deleted"))
  } else {
    tmpfile <- tail(.st_env$tmpfiles, 1)
    if (!isTRUE(silent))
      message(paste("Deleting", tmpfile))
    unlink(tmpfile)
    length(.st_env$tmpfiles) <- length(.st_env$tmpfiles) - 1
  }
}
