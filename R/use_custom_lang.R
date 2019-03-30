#' Import and use a custom language
#'
#' If your language is not available or if you wish to customize the outputs'
#' language to suit your preference, you can set up a translations file (see
#' details) and import it with this function.
#' @aliases use_custom_lang useTranslations
#' @param file Character. The path to the translations file.
#'
#' @details To build the translations file, download
#' \href{https://raw.githubusercontent.com/dcomtois/summarytools/master/translations/language_template.csv}{this template},
#' or copy the \emph{language_template.csv} file located in the installed 
#' package's \emph{includes} directory and fill out the \sQuote{custom} column
#' using a text editor, leaving column titles unchanged. The file must also
#' retain its \emph{UTF-8} encoding.
#'
#' @keywords utilities
#' @importFrom utils read.csv
#' @importFrom tcltk tclvalue tkgetOpenFile
#' @export
use_custom_lang <- function(file) {
  if (!"file" %in% names(match.call())) {
    file <- character()
    file <- tclvalue(tkgetOpenFile(initialdir = "~",
                                   filetypes = "{{csv files} {*.csv}}"))
    if (file == "") {
      stop("operation cancelled")
    }
  } 
  
  if (is.character(file)) {
    tr <- read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE,
                   encoding = "UTF-8")
  } else {
    tr <- file
  }
  
  items <- tr$item
  tr <- as.data.frame(t(tr$custom), stringsAsFactors = FALSE)
  colnames(tr) <- items
  rownames(tr) <- "custom"
  
  .st_env$custom_lang <- tr
  st_options(lang = "custom")
}

#' @export
useTranslations <- function(file) {
  message("useTranlations() is deprecated; please use use_custom_lang() ",
          "instead")
}
