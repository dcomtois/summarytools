#' Delete Temporary Html Files
#'
#' Delete temporary files created when using \code{method='browser'},
#' \code{method='viewer'}, or \code{view()} function.
#'
#' All temporary files are deleted automatically when R session is ended. This
#' function is thus an overkill for most circumstances.
#'
#' @param all Logical. When \code{TRUE}, all temporary summarytools are deleted.
#'   When \code{FALSE} (default), only the latest is.
#' @param silent Hide confirmation messages (\code{FALSE} by default).
#'
#' @return NULL
#'
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com>}
#'
#' @keywords IO
#'
#' @export
cleartmp <- function(all=FALSE, silent=FALSE) {
  if(length(.st_env$tmpfiles) == 0) {
    if (!silent)
      message("No temporary files to delete.")
  } else if(isTRUE(all) || all == 1 || all == "all") {
    nfiles <- 0
    for(tmpfile in .st_env$tmpfiles) {
      nfiles <- nfiles + 1
      if(!silent)
        message(paste("Deleting", tmpfile))
      unlink(tmpfile)
    }
    .st_env$tmpfiles <- c()
    if(!silent)
      message(paste(nfiles, "file(s) deleted"))
  } else {
    tmpfile <- tail(.st_env$tmpfiles, 1)
    if(!silent)
      message(paste("Deleting", tmpfile))
    unlink(tmpfile)
    .st_env$tmpfiles <- .st_env$tmpfiles[-length(.st_env$tmpfiles)]
  }
}
