#' Import and use a custom language
#'
#' If your language is not available or if you wish to customize the outputs'
#' language to suit your preference, you can set up a translations file (see
#' details) and import it with this function.
#' @aliases use_custom_lang
#' 
#' @param file Character. The path to the translations file.
#'
#' @details To build the translations file, copy the 
#' \emph{language_template.csv} file located in the installed 
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
    if (interactive() && isTRUE(capabilities("tcltk"))) {
      file <- character()
      file <- try(tclvalue(tkgetOpenFile(initialdir = "~",
                                         filetypes = "{{csv files} {*.csv}}")),
                  silent = TRUE)
      if (class(file) == "try-error") {
        stop("Window dialog not permitted; 'file' argument must be specified")
      }
      if (file == "") {
        stop("operation cancelled")
      }
    } else {
      stop("Window dialog not permitted; 'file' argument must be specified")
    }
  }
  
  if (is.character(file)) {
    tr <- read.csv(file = file, strip.white = TRUE, stringsAsFactors = FALSE,
                   encoding = "UTF-8")
  } else if (is.data.frame(file)) {
    # used when called from define_keywords()
    tr <- file
  } else {
    stop("invalid 'file' argument class: ", class(file))
  }
  
  if (nrow(tr) == ncol(.translations)) {
    items <- tr$item
    tr <- as.data.frame(t(tr$custom), stringsAsFactors = FALSE)
    colnames(tr) <- items
    rownames(tr) <- "custom"
  } 
  
  # Check contents
  expected <- ncol(.translations)
  tmp <- tr[,intersect(colnames(.translations), colnames(tr))]
  tmp <- tmp[,!is.na(tmp[1,])]
  nb_in   <- ncol(tmp)
  nb_blank <- sum(as.character(tmp[1,]) == "")
  cols_missing <- setdiff(colnames(.translations), colnames(tmp))
  pct_actual <- round((nb_in - nb_blank) * 100 / expected)
  
  if (pct_actual < 60) {
    warning("input file contains only ", pct_actual, "% of translated terms; ",
            "English will be used for missing terms")
  } else if (pct_actual < 100) {
    message("no translation found for the following term(s) ; English will be ",
    "used as fall-back values:\n    ", 
            paste(cols_missing, sQuote(.translations[1,cols_missing]),
                                 sep = " : ", collapse = "\n    "))
  }
  
  if (nb_in < expected) {
    tr <- .translations["en",]
    tr[,colnames(tmp)] <- tmp[1,]
    rownames(tr) <- "custom"
  }
  
  .st_env$custom_lang <- tr
  st_options(lang = "custom")
  # message("Operation successful")
}
