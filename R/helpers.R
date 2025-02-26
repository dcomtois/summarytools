# smart_split ------------------------------------------------------------------
# Smartly split variable names that are too long
# ref: https://tinyurl.com/y7qv48z9
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
# infix to simplify append'ing
`%+=%` <- function(.Variable_Name_, .New_Value_) {
  eval.parent(substitute(
    .Variable_Name_ <- append(.Variable_Name_, .New_Value_)
    ))
}

# unquote ----------------------------------------------------------------------
# Remove quotation marks inside a string
unquote <- function(x) {
  x <- sub("^\\'(.+)\\'$", "\\1", x)
  x <- sub('^\\"(.+)\\"$', "\\1", x)
  x
}

# conv_non_ascii ---------------------------------------------------------------
# Replace accentuated characters by their html decimal entity
conv_non_ascii <- function(...) {
  out <- character()
  for (s in list(...)) {
    if (is.null(s) || length(s) == 0) next
    splitted <- unlist(strsplit(s, ""))
    intvalues <- utf8ToInt(enc2utf8(s))
    pos_to_modify_lat <- which(intvalues >=  161 & intvalues <=  255)
    pos_to_modify_cyr <- which(intvalues >= 1024 & intvalues <= 1279)
    pos_to_modify_no  <- which(intvalues == 8470)
    pos_to_modify <- c(pos_to_modify_lat, pos_to_modify_cyr, pos_to_modify_no)
    splitted[pos_to_modify] <- paste0("&#0",  intvalues[pos_to_modify], ";")
    out <- c(out, paste0(splitted, collapse = ""))
  }
  out
}

# ws_to_symbol -----------------------------------------------------------------
# Replace leading and trailing white space in character vectors and factor
# levels by the special character intToUtf8(183)
ws_to_symbol <- function(x) {
  
  ws_symbol <- intToUtf8(183)
  x_is_char <- is.character(x)
  if (isTRUE(x_is_char)) {
    x <- as.factor(x)
  }
  
  xx <- levels(x)
  xx_encod <- Encoding(xx)
  
  for (enc in setdiff(Encoding(xx), "unknown")) {
    xx[xx_encod == enc] <- iconv(xx[xx_encod == enc], 
                                 from = enc, to = "UTF-8")
  }
  
  left_ws_count  <- nchar(xx) - nchar(sub("^ +", "", xx))
  right_ws_count <- nchar(xx) - nchar(sub(" +$", "", xx))
  
  # Correction to avoid doubling the length of whitespace-only strings
  right_ws_count[right_ws_count == nchar(xx)] <- 0
  
  xx <- gsub("((?:\\A|\\G) )|(?&rec)( )+(?'rec')$", ws_symbol, xx, perl = TRUE)
  
  
  levels(x) <- xx
  
  if (isTRUE(x_is_char)) {
    return(as.character(x))
  } else {
    return(x)
  }
}

# trs --------------------------------------------------------------------------
# Shorcut function to get translation strings
trs <- function(item, l = st_options("lang")) {
  l <- force(l)
  if (l != "custom") {
    val <- .translations[l,item]
  } else {
    val <- .st_env$custom_lang["custom", item]
  }
  ifelse(is.na(val), "", val)
}

# Checks if individual, vector, or list items are empty/NULL, NA, or ""
empty_na <- function(x) {
  if (is.null(x) || 
      length(x) == 0 ||
      identical(as.character(x), NA_character_) ||
      identical(as.character(x), "NaN") ||
      (length(x) == 1 && nchar(as.character(x)) == 0))
    return(TRUE)
  
  # matrix / array / df
  else if (inherits(x, c("matrix, data.frame")))
    return(lapply(x, function(z) all(empty_na(z))))

  else if (length(x) > 1) {
    if (is.list(x)) {
      # If there are lists within the list, we treat those separately
      long_lists <- lengths(x) > 1
      if (any(long_lists)) {
        out <- logical(length(x))
        out[which(long_lists)] <- FALSE
        if (!all(long_lists)) {
          out[which(!long_lists)] <- vapply(x[which(!long_lists)], empty_na,
                                            FUN.VALUE = logical(1),
                                            USE.NAMES = FALSE)
        }
        return(out)
      } else {
        return(vapply(x, empty_na, FUN.VALUE = logical(1), USE.NAMES = FALSE))
      }
    } else {
      return(vapply(x, empty_na, FUN.VALUE = logical(1), USE.NAMES = FALSE))
    }
  }
  
  else {
    return(FALSE)
  }
}

# empty_na(list(NA, "", NaN, list("", "", " ")))
# empty_na(list("", " ", character(), NULL))
# empty_na("aaa")
# empty_na("")
# empty_na(character())
# empty_na(c(NA, NaN))
# empty_na(data.frame(a=c("", " ", "123"), b = c(character(3))))

# Turn labelled / haven_labelled into factor
lbl_to_factor <- function(x, num_pos = "after") {
  label <- attr(x, "label", exact = TRUE)
  labels <- attr(x, "labels")
  
  # check and remove tagged na's (should be supported in the future)
  if (length(na_lev_ind <- which(is.na(labels))) >= 1) {
    labels <- labels[-na_lev_ind]
    if (any(haven::is_tagged_na(x))) {
      assign("flag_tagged_na", value = parent.frame()$flag_tagged_na + 1,
             envir = parent.frame())
      x[is.na(x)] <- NA
    }
  }
  
  if (num_pos == "after")
    names(labels) <- paste0(names(labels), " [", labels, "]")
  else if (num_pos == "before")
    names(labels) <- paste0("[", labels, "] ", names(labels))
  x_vec <- as.vector(x)
  extra_vals <- sort(setdiff(x_vec, labels))
  names(extra_vals) <- sub("^(\\d+)$", "[\\1]", as.character(extra_vals))
  labels2 <- c(labels, extra_vals)
  res <- factor(x, levels = unname(labels2), labels = names(labels2))
  structure(res, label = label)
}

#' Remove Attributes to Get a Simplified Object
#'
#' Get rid of summarytools-specific attributes to get a simple data structure
#' (matrix, array, ...), which can be easily manipulated.
#'
#' @param x An object with attributes
#' @param except Character. A vector of attribute names to preserve. By default,
#'   \dQuote{dim} and \dQuote{dimnames} are preserved.
#' 
#' @details
#' If the object contains grouped results:
#' \itemize{
#'  \item The inner objects will lose their attributes
#'  \item The \dQuote{stby} class will be replaced with \dQuote{by}
#'  \item The \dQuote{dim} and \dQuote{dimnames} attributes will be set to
#'    available relevant values, but expect slight differences between objects
#'    created with \code{stby()} \emph{vs} \code{group_by()}.
#' }
#' 
#' @examples
#' data(tobacco)
#' zap_attr(descr(tobacco))
#' zap_attr(freq(tobacco$gender))
#' @export
zap_attr <- function(x, except = c("dim", "dimnames")) {
  if (inherits(x, c("by", "stby"))) {
    for (i in seq_along(x)) {
      attributes(x[[i]]) <- attributes(x[[i]])[except]
    }
    class(x) <- "by"
    
    if (!"dim" %in% names(attributes(x)))
      attr(x, "dim") <- length(x)
    
    if (!"dimnames" %in% names(attributes(x)))
      dimnames(x) <- list(names(x))
    
  } else {
    attributes(x) <- attributes(x)[except]
  }
  x
}

# Count_empty  (NA's / vectors of size 0) --------------------------------------
#' @keywords internal
count_empty <- function(x, count.nas = TRUE) {
  if (count.nas)
    sum(sapply(x, function(z) length(z) == 0 || identical(as.character(z),
                                                          NA_character_)))
  else
    sum(sapply(x, function(z) length(z) == 0))
}

# Clone of htmltools:::paste8 --------------------------------------------------
#' @keywords internal
paste8 <- function(..., sep = " ", collapse = NULL) {
  args <- c(
    lapply(list(...), enc2utf8), 
    list(
      sep = if (is.null(sep)) sep else enc2utf8(sep), 
      collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)
    )
  )
  do.call(paste, args)
}

# map_groups : Map columns / values of group_by obj based on group_keys --------
#' @keywords internal
map_groups <- function(gk) {
  grs <- c()
  for (i in seq_len(nrow(gk))) {
    # Ensure factors are treated as characters
    gr <- paste(
      colnames(gk), 
      sapply(gk[i, ], as.character),
      sep = " = ", 
      collapse = ", "
    )
    grs <- c(grs, gr)
  }
  grs
}

# pad (used in print.summarytools) ---------------------------------------------
pad <- function(string, width) {
  paste0(strrep(" ", max(0, width - nchar(string))), string)
}

# pctvalid (used in dfSummary) -------------------------------------------------
pctvalid <- function(x, round.digits = 1) {
  round(sum(!is.na(x)) / length(x) * 100, round.digits)
}

classes <- function(x) {
    vapply(x, class, "", USE.NAMES = FALSE)
}

# Standardize call, possibly nested (based on pryr::standardise_call)
standardize <- function(call, env = parent.frame()) {
  
  if (!is.call(call)) {
    return(call)
  }
  
  # Evaluate the function/symbol in the environment
  f <- eval(call[[1]], env)

  if (is.primitive(f)) {
    # if primitive, we can only std any subcalls
    for (i in seq_along(call)) {
      if (i == 1) next  # skip the function
      call[[i]] <- standardize(call[[i]], env)
    }
    return(call)
  } else {
    # if non-primitive, use match.call()
    call_std <- match.call(f, call)
    # recurse through each argument
    for (i in seq_along(call_std)) {
      if (i == 1) next  # skip symbol
      call_std[[i]] <- standardize(call_std[[i]], env)
    }
    return(call_std)
  }
}

.p_reset <- function() {
  rm(list = ls(envir = .p), envir = .p)
}
