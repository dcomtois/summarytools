#' Import and use a custom language
#'
#' If your language is not available or if you wish to customize the outputs'
#' language to suit your preference, you can set up a translations file (see
#' details) and import it with this function.
#'
#' @param file Character. The path to the translations file.
#'
#' @details To build the translations file, download
#' \href{https://raw.githubusercontent.com/dcomtois/summarytools/master/translations/language_template.csv}{this template},
#' and fill out the \sQuote{custom} column, leaving \sQuote{custom} on the first
#' line. The file must have \emph{UTF-8} encoding.
#'
#' @keywords utilities
#' @importFrom utils read.csv
#' @export
useTranslations <- function(file) {
  browser()
  tr <- read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE,
                 encoding = "UTF-8")
  items <- tr$item
  tr <- as.data.frame(t(tr$custom), stringsAsFactors = FALSE)
  colnames(tr) <- items
  rownames(tr) <- "custom"

  .st_env$custom_lang <- tr
  st_options(lang = "custom")
}
