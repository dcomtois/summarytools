# smart_split ------------------------------------------------------------------
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

# %+=% -------------------------------------------------------------------------
# infix to simplify append()ing
#' @keywords internal
`%+=%`<- function(x, value) {
  eval.parent(substitute(x <- append(x, value)))
}

# unquote ----------------------------------------------------------------------
# Remove quotation marks inside a string
#' @keywords internal
unquote <- function(x) {
  x <- sub("^\\'(.+)\\'$", "\\1", x)
  x <- sub('^\\"(.+)\\"$', "\\1", x)
  x
}

# conv_non_ascii ---------------------------------------------------------------
# Replace accentuated characters by their html decimal entity
#' @keywords internal
conv_non_ascii <- function(...) {
  out <- character()
  for (s in list(...)) {
    splitted <- unlist(strsplit(s, ""))
    intvalues <- utf8ToInt(enc2utf8(s))
    pos_to_modify_lat <- which(intvalues >=161 & intvalues <= 255)
    pos_to_modify_cyr <- which(intvalues >= 1024 & intvalues <=1279)
    pos_to_modify <- c(pos_to_modify_lat, pos_to_modify_cyr)
    splitted[pos_to_modify] <- paste0("&#0",  intvalues[pos_to_modify], ";")
    out <- c(out, paste0(splitted, collapse = ""))
  }
  out
}

# ws_to_symbol -----------------------------------------------------------------
# Replace leading and trailing white space in character vectors and factor
# levels by the special character intToUtf8(183)
#' @keywords internal
ws_to_symbol <- function(x) {
  #intchar <- c(729, 1632, 183, 765, 11825, 9251, 9141, 11825)
  ws_char <- intToUtf8(183)
  ws_char <- enc2native(ws_char)
  if (is.character(x)) {
    x <- enc2native(x)
    left_ws <- nchar(x) - nchar(sub("^ +", "", x))
    right_ws <- nchar(x) - nchar(sub(" +$", "", x))
    right_ws[nchar(x) == left_ws] <- 0
    outstr <- paste0(strrep(ws_char, times = left_ws), trimws(x),
                     strrep(ws_char, times = right_ws))
    outstr[is.na(x)] <- NA
    return(outstr)
  }
  
  if (is.factor(x)) {
    levx <- levels(x)
    levx <- enc2native(levx)
    left_ws  <- nchar(levx) - nchar(sub("^ +", "", levx))
    right_ws <- nchar(levx) - nchar(sub(" +$", "", levx))
    right_ws[left_ws == nchar(levx)] <- 0
    newlev <- paste0(strrep(ws_char, times = left_ws), trimws(levx),
                     strrep(ws_char, times = right_ws))
    newlev[is.na(levx)] <- NA              
    levels(x) <- newlev
    return(x)
  }
}

# Shorcut function to get translation strings
#' @keywords internal
trs <- function(item, l = st_options("lang")) {
  l <- force(l)
  if (l != "custom") {
    .translations[l,item]
  } else {
    .st_env$custom_lang["custom", item]
  }
}

# Shortcut function to get the item name of a translated element
#' @keywords internal
inv_trs <- function(name, l = st_options("lang")) {
  l <- force(l)
  if(l != "custom") {
    colnames(.translations)[which(.translations["en",] == name)]
  } else {
    colnames(.st_env$custom_lang)[which(.st_env$custom_lang["custom",] == name)]
  }
}

# Count "empty" elements (NA's / vectors of size 0)
#' @keywords internal
count_empty <- function(x, count.nas = TRUE) {
  n <- 0
  for (item in x) {
    if(length(item) == 0) {
      n <- n + 1
    } else if (isTRUE(count.nas)) {
      n <- n + sum(is.na(item))
    }
  }
  n
}

# Redefine htmltools's includeCSS but use collapse = "\n"
#' @importFrom htmltools tags HTML
#' @keywords internal
includeCss <- function(path, ...) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  args <- list(...)
  if (is.null(args$type)) 
    args$type <- "text/css"
  return(do.call(tags$style, c(list(HTML(paste8(lines, collapse = "\n"))), 
                               args)))
}

# Clone of htmltools:::paste8
#' @keywords internal
paste8 <- function (..., sep = " ", collapse = NULL) {
  args <- c(lapply(list(...), enc2utf8), 
            list(sep = if (is.null(sep)) sep else enc2utf8(sep), 
                 collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)))
  do.call(paste, args)
}

# Map columns / values of dplyr's group_by objects based on group_keys
#' @keywords internal
map_groups <- function(gk) {
  grs <- c()
  for (i in seq_len(nrow(gk))) {
    gr <- paste(colnames(gk), as.character(unlist(gk[i,])), 
                sep = " = ", collapse = ", ")
    grs <- c(grs, gr)
  }
  grs
}
