# Initialise vector containing paths to temporary html files generated when viewing in browser or
# in RStudio visualisation pane. Will be updated whenever print.summarytools() / cleartmp() are called.
.st_env <- new.env(parent = emptyenv())
.st_env$tmpfiles <- c()
.st_env$byInfo <- list()

# cleartmp() ----------------------------------------------------------

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
