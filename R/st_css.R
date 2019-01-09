#' Include \bold{summarytools}' \emph{css} Into Active Document
#'
#' Generates the \emph{css} needed by \bold{summarytools} in \emph{Rmarkdown} 
#' documents.
#'
#' @param main Logical. Includes \emph{summarytools.css} file. \code{TRUE} by
#'   default.
#' @param bootstrap Logical. Includes \emph{bootstrap.min.css}. \code{FALSE}
#'   by default.
#' @param style.tag Logical. Includes the opening and closing \code{<style>}
#'   tags. \code{TRUE} by default.
#' @param \dots Character. Path to additional \emph{css} file(s) to include.
#'
#' @details Typically the function is called in the first R chunk of an 
#'   \emph{Rmarkdown} document. For instance:
#'   \code{
#'   ```{r, include=FALSE}
#'   library(knitr)
#'   opts_chunk$set(results = "asis")
#'   library(summarytools)
#'   st_options(plain.ascii = FALSE, style = "rmarkdown")
#'   include.css()
#'   ```
#'   }
#'   
#' @keywords utilities
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#' @export
st_css <- function(main = TRUE, bootstrap = FALSE, 
                          style.tag = TRUE, ...) {

  output <- character()
  
  if (isTRUE(style.tag)) {
    output %+=% '<style type="text/css">\n'
  }
  
  if (isTRUE(main)) {
    output %+=% readLines(system.file(package = "summarytools",
                                      "includes/stylesheets/summarytools.css"))
  }
  
  if (isTRUE(bootstrap)) {
    output %+=% readLines(system.file(package = "summarytools",
                                      "includes/stylesheets/bootstrap.min.css"))
  }
  
  dotArgs <- list(...)
  for (f in dotArgs) {
    output %+=% 
      readLines(f)
  }
  
  if (isTRUE(style.tag)) {
    output %+=% '</style>\n'
  }
  
  output <- paste(output, sep = "\n")
  cat(output)
  return(invisible(output))
}