# Initialize vector containing paths to temporary html files generated when viewing
# in browser or in RStudio visualization pane. Will be updated whenever
# print.summarytools() / cleartmp() are called.
.st_env <- new.env(parent = emptyenv())
.st_env$tmpfiles <- c()

# Initialize list used by view() when printing an object of class "by"
.st_env$byInfo <- list()

#.onAttach <- function(libname, pkgname) {
#  packageStartupMessage(
#    paste("Temporary html files will automatically be deleted when R Session is terminated.",
#          "If you need to delete temporary files before ending R session, see ?cleartmp.", sep = "\n")
#    )
#}

