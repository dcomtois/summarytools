#' Import and use a custom language
#'
#' If your language is not available or if you wish to customize the outputs'
#' language to suit your preference, you can set up a translations file (see
#' details) and import it with this function.
#'
#' @param file Character. The path to the translations file.
#'
#' @details To build the translations file, you can get
#' \href{https://raw.githubusercontent.com/dcomtois/summarytools/dev-current/translations/custom_lang_template.csv}{this template},
#' or use the \href{https://raw.githubusercontent.com/dcomtois/summarytools/dev-current/translations/translations.csv}{current translations definition file}
#' and fill out a new column \strong{having \sQuote{custom} on the first line}.
#' The file must have \emph{UTF-8} encoding.
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

  # testing this part:
  # for (cname in colnames(tr)) {
  #   tr[,cname] <- enc2utf8(tr[,cname])
  # }
  # end test

  .st_env$custom_lang <- tr
  st_options(lang = "custom")
}
