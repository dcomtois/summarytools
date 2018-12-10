# Fn to smartly split variable names that are too long
# ref: https://tinyurl.com/y7qv48z9
#' @keywords internal
smart_split <- function(str, maxlen) {
  re <- paste0("(?=.{1,", maxlen, "}(.*))",
               "(?=.*?[^\\W._].*?[\\W._].*?\\1)",
               ".{1,", maxlen, "}(?<=_|\\b|\\Z)",
               "|.{1,", maxlen, "}")
  matchinfo <- gregexpr(pattern = re,
                        text = str, perl = TRUE)
  groups <- regmatches(x = str, m = matchinfo)[[1]]
  paste(groups, collapse = "<br/>")
}

# infix to simplify append()ing 
#' @keywords internal
`%+=%` <- function(x, y) {
  assign(x = deparse(substitute(x)), value = append(x, y), 
         envir = parent.frame())
}

# Remove quotation marks inside a string
#' @keywords internal
unquote <- function(x) {
  x <- sub("^\\'(.+)\\'$", '\\1', x)
  x <- sub('^\\"(.+)\\"$', '\\1', x)
  x
}
