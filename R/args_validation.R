# Arguments validation for freq, ctable, descr and dfSummary functions.
# Another function for validating st_options arguments follows.
#' @importFrom checkmate test_int test_logical test_choice test_string test_number
#' @importFrom dplyr n_distinct
#' @importFrom stats na.omit
#' @keywords internal
check_args <- function(mc, dotArgs, caller) {
  
  pf          <- parent.frame()
  errmsg      <- character()
  
  if (mc[[1]] == "FUN" || mc$x == "dd[x, , drop = FALSE]") {
    pf$flag_by <- TRUE
  } else {
    pf$flag_by <- FALSE
  }
  
  # Deprecated arguments -------------------------------------------------------
  if ("file" %in% names(dotArgs)) {
    message(paste("'file' argument is deprecated; use with print() or view(),",
                  "e.g. print(x, file=", dotArgs$file))
  }
  
  if ("omit.headings" %in% names(dotArgs)) {
    errmsg %+=% "'omit.headings' is deprecated; use 'headings' instead"
  }
  
  # Arguments common to all functions ------------------------------------------
  if (is.null(pf$x)) {
    errmsg %+=% "x is either NULL or does not exist"
  }
  
  if ("round.digits" %in% names(mc) && 
      !isTRUE(test_int(pf$round.digits))) {
    errmsg %+=% "'round.digits' must be a whole number"
  }
  
  if ("style" %in% names(mc)) {
    if (caller %in% c("freq", "descr", "ctable")) {
      if (!isTRUE(test_choice(pf$style, 
                              c("simple", "grid", "rmarkdown", "jira")))) {
        errmsg %+=% "'style' must be one of 'simple', 'grid', 'rmarkdown', or 'jira'"
      }
    }
  }
  
  if ("plain.ascii" %in% names(mc) && 
      !isTRUE(test_logical(pf$plain.ascii, 
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'plain.ascii' must be either TRUE or FALSE"
  }
  
  if ("justify" %in% names(mc)) {
    if (caller == "freq") {
      if (!isTRUE(test_string(pf$justify, min.chars = 1)) ||
          !isTRUE(test_choice(tolower(substr(pf$justify,1,1)), 
                              c("l", "c", "m", "r", "d")))) {
        errmsg %+=% "'justify' must be one of 'l', 'c', 'r', or 'd' (default)"
      }
    } else if (!isTRUE(test_choice(tolower(substr(pf$justify,1,1)), 
                                   c("l", "c", "m", "r")))) {
      errmsg %+=% "'justify' must be one of 'l', 'c', or 'r'"
    }
    
    pf$justify <- switch(tolower(substr(pf$justify, 1, 1)),
                         l = "left",
                         c = "center",
                         m = "center",
                         d = "default",
                         r = "right")
  }
  
  if ("display.labels" %in% names(mc) &&
      !isTRUE(test_logical(pf$display.labels, 
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'display.labels' must be either TRUE or FALSE"
  }
  
  if ("headings" %in% names(mc) &&
      !isTRUE(test_logical(pf$headings, 
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'headings' must be either TRUE or FALSE"
  }
  
  # freq-specific arguments ----------------------------------------------------
  if (caller == "freq") {
    
    if ("report.nas" %in% names(mc) && 
        !isTRUE(test_logical(pf$report.nas, 
                             len = 1, any.missing = FALSE))) {
      errmsg %+=% "'report.nas' must be either TRUE or FALSE"
    }
    
    if ("display.type" %in% names(mc) &&
        !isTRUE(test_logical(pf$display.type, 
                             len = 1, any.missing = FALSE))) {
      errmsg %+=% "'display.type' must be either TRUE or FALSE"
    }
    
    if ("cumul" %in% names(mc) && 
        !isTRUE(test_logical(pf$cumul, len = 1, any.missing = FALSE))) {
      errmsg %+=% "'cumul' must be either TRUE or FALSE"
    }
    
    if ("order" %in% names(mc)) {
      order <- switch(tolower(substr(sub("[+-]", "", pf$order), 1, 1)),
                      d = "default",
                      l = "level",
                      f = "freq",
                      n = "name")
      
      if (!isTRUE(test_choice(order, 
                              c("default", "level", "freq", "name")))) {
        errmsg %+=% paste("'order' must be one of 'default', 'level',",
                          "'freq', or 'name'")
      } else if (order == "level" && !is.factor(pf$x)) {
        errmsg %+=% paste("'order' can be set to 'level' only for factors.",
                          "Use 'name' or 'freq', or convert object to factor",
                          "prior to calling freq()")
      }
      
      order_sign <- sub("^.*([-+]).*$", "\\1", pf$order)
      order_sign <- ifelse(order_sign %in% c("+", "-"), order_sign, "+")
      
      pf$order <- order
      pf$order_sign <- order_sign
      
    } else {
      pf$order_sign <- "+"
    }
    
    if ("rows" %in% names(mc)) {
      
      if (NA %in% pf$rows) {
        errmsg %+=% paste("'rows' cannot contain NA; NA's are always displayed",
                          "last; use 'report.nas' to turn off na reporting")
      }
      
      if (is.character(pf$rows) && length(pf$rows) > 1 &&
          !all(pf$rows %in% unique(pf$x))) {
        errmsg %+=% paste("the following items used in the 'rows' argument",
                          "are not found in the data:", 
                          paste(setdiff(pf$rows, unique(pf$x)), sep = ","))
      }
      
      if (is.numeric(pf$rows) && length(pf$rows) > 0) {
        if (0 %in% pf$rows || length(unique(sign(pf$rows))) > 1 ||
            (sign(pf$rows[1]) == -1 && 
             length(pf$rows) >= n_distinct(pf$x, na.rm = TRUE))) {
          errmsg %+=% "Invalid 'rows' argument"
        } else if (!is.null(pf$x) && 
                   max(abs(pf$rows)) > n_distinct(pf$x, na.rm = TRUE)) {
          nmax <- n_distinct(pf$x, na.rm = TRUE)
          wrong_ind <- which(abs(pf$rows) > nmax)
          if (length(wrong_ind)) {
            message("There are only ", nmax, " rows to show; higher ",
                    "numbers will be ignored")
            pf$rows <- pf$rows[-wrong_ind]
          }
        } 
      } else if (length(pf$rows) > 0 && max(table(pf$rows)) > 1) {
        warning("one or more elements in the rows argument appears more than once")
      }
    }
    
    if (!is.null(pf$na.val)) {
      if (!isTRUE(test_character(pf$na.val, any.missing = FALSE, len = 1))) {
        errmsg %+=% "invalid na.val value; must be character of length 1"
      }
      
      # If na.val is specified and one of two conditions is TRUE, we set it
      # back to its default (NULL) value:
      # - There are <NA> values in the vector/factor or
      # - na.val is not in a factor's levels
      if ((nas_found <- anyNA(pf$x)) ||
          (is.factor(pf$x) && !pf$na.val %in% levels(pf$x))) {
        if (nas_found && !isTRUE(st_options("freq.silent")))
          message("NA values different from na.val detected; ",
                  "na.val will be ignored")
        pf$na.val <- NULL
      }
    }
  }
  
  # freq & ctable arguments ----------------------------------------------------
  if (caller %in% c("freq", "ctable")) {
    if ("totals" %in% names(mc) && 
        !isTRUE(test_logical(pf$totals, 
                             len = 1, any.missing = FALSE))) {
      errmsg %+=% "'totals' must be either TRUE or FALSE"
    }
    
    if (!identical(pf$weights, NA)) {
      if (is.null(pf$weights)) {
        errmsg %+=% "weights vector not found"
      } else if (isFALSE(pf$flag_by) && 
                 length(pf$weights) != nrow(as.data.frame(pf$x))) {
        errmsg %+=% "weights vector must have same length as 'x'"      
      }
    }
  }
  
  # freq & descr arguments -----------------------------------------------------
  if (caller %in% c("freq", "descr")) {
    if ("rescale.weights" %in% names(mc) &&
        !isTRUE(test_logical(pf$rescale.weights, 
                             len = 1, any.missing = FALSE))) {
      errmsg %+=% "'rescale.weights' must be either TRUE or FALSE"
    }
  }
  
  # ctable-specific arguments --------------------------------------------------
  if (caller == "ctable") {
    if (is.null(pf$y)) {
      errmsg %+=% "y is either NULL or does not exist"
    }
    
    if ("prop" %in% names(mc)) {
      prop <- tolower(substr(pf$prop, 1, 1))
      if (!isTRUE(test_choice(prop, c("t", "r", "c", "n")))) {
        errmsg %+=% "'prop' must be one of 't', 'r', 'c', or 'n'"
      }
      if (nchar(prop > 1)) {
        pf$prop <- prop
      }
    }
    
    if ("useNA" %in% names(mc) &&
        !isTRUE(test_choice(pf$useNA, 
                            c("ifany", "always", "no")))) {
      errmsg %+=% "'useNA' must be one of 'ifany', 'always', or 'no'"
    }
    
    if ("dnn" %in% names(mc) &&
        !isTRUE(test_character(pf$dnn, any.missing = FALSE, 
                               len = 2, unique = TRUE))) {
      errmsg %+=% "'dnn' must be a character vector of 2 distinct values"
    }
    
    if ("OR" %in% names(mc) && !isFALSE(pf$OR)) {
      if (isTRUE(pf$OR)) {
        pf$OR <- .95
      } else {
        if (!test_number(pf$OR, na.ok = FALSE, lower = .5, upper = .999)) {
          errmsg %+=% "'OR' must be TRUE, FALSE, or a number between .5 and .999"
        }
        if (length(as.numeric(na.omit(unique(pf$x)))) != 2 ||
            length(as.numeric(na.omit(unique(pf$y)))) != 2) {
          errmsg %+=% "'OR' can only be used with 2 x 2 tables"
        }
      }
    }
    
    if ("RR" %in% names(mc) && !isFALSE(pf$RR)) {
      if (isTRUE(pf$RR)) {
        pf$RR <- .95
      } else {
        if (!test_number(pf$RR, na.ok = FALSE, lower = .5, upper = .999)) {
          errmsg %+=% "'RR' must be TRUE/FALSE or a number between .5 and .999"
        }
        if (length(as.numeric(na.omit(unique(pf$x)))) != 2 ||
            length(as.numeric(na.omit(unique(pf$y)))) != 2) {
          errmsg %+=% "'RR' can only be used with 2 x 2 tables"
        }
      }
    }
    
    if (!is.null(pf$na.val)) {
      if (!isTRUE(test_character(pf$na.val, any.missing = FALSE, len = 1))) {
        errmsg %+=% "invalid na.val value; must be character of length 1"
      }
      if (anyNA(pf$x) ||
          (is.factor(pf$x) && !pf$na.val %in% levels(pf$x))) {
          pf$na.val.x <- NULL
      }
      if (anyNA(pf$y) ||
          (is.factor(pf$y) && !pf$na.val %in% levels(pf$y))) {
        pf$na.val.y <- NULL
      }
    }
  }
  
  # descr arguments ------------------------------------------------------------
  if (caller == "descr") {
    if ("na.rm" %in% names(mc) &&
        !isTRUE(test_logical(pf$na.rm, 
                             len = 1, any.missing = FALSE))) {
      errmsg %+=% "'na.rm' must be either TRUE or FALSE"
    }
    
    if ("transpose" %in% names(mc) &&
        !isTRUE(test_logical(pf$transpose))) {
      errmsg %+=% "'transpose' must be either TRUE or FALSE"
    }
    
    if ("order" %in% names(mc)) {
      if (length(pf$order) == 1) {
        if (tolower(pf$order) == "s") {
          pf$order <- "sort"
        } else if (tolower(pf$order) == "p") {
          pf$order <- "preserve"
        }
        
        if (!pf$order %in% c("sort", "preserve")) {
          errmsg %+=% paste0("'order' must be one of 'sort', ",
                             "'preserve', or a vector of variable names")
        }
      } else {
        # order has length > 1 -- all elements must correspond to column names
        if (length(ind <- which(!pf$order %in% colnames(pf$xx))) > 0) {
          errmsg %+=% paste("Following ordering element(s) not recognized:", 
                            paste(pf$order[ind], sep = ", "),
                            collapse = " ")
        }
      }
    }
    
    if (!is.null(pf$weights)) {
      if (isFALSE(pf$flag_by) && (length(pf$weights) != nrow(pf$xx))) {
        errmsg %+=% "weights vector must have same length as 'x'"      
      }
    }
  }
  
  # dfSummary arguments --------------------------------------------------------
  if (caller == "dfSummary") {

    if (!isTRUE(test_choice(pf$style, c("grid", "multiline")))) {
      errmsg %+=% "'style' must be either 'grid' or 'multiline'"
    }
    
    if ("varnumbers" %in% names(mc) &&
        !isTRUE(test_logical(pf$varnumbers, 
                             len = 1, any.missing = FALSE))) {
      errmsg %+=% "'varnumbers' must be either TRUE or FALSE"
    }
    
    if ("labels.col" %in% names(mc) &&
        !isTRUE(test_logical(pf$labels.col, 
                             len = 1, any.missing = FALSE))) {
      errmsg %+=% "'labels.col' must be either TRUE or FALSE"
    }
    
    if ("valid.col" %in% names(mc) &&
        !isTRUE(test_logical(pf$valid.col, 
                             len = 1, any.missing = FALSE))) {
      errmsg %+=% "'valid.col' must be either TRUE or FALSE"
    }
    
    if ("na.col" %in% names(mc) &&
        !isTRUE(test_logical(pf$na.col, 
                             len = 1, any.missing = FALSE))) {
      errmsg %+=% "'na.col' must be either TRUE or FALSE"
    }
    
    if ("graph.col" %in% names(mc) &&
        !isTRUE(test_logical(pf$graph.col, 
                             len = 1, any.missing = FALSE))) {
      errmsg %+=% "'graph.col' must be either TRUE or FALSE"
    }
    
    if ("graph.magnif" %in% names(mc) && pf$graph.magnif <= 0) {
      errmsg %+=% "'graph.magnif' must be > 0"
    }
    
    if ("style" %in% names(mc) && pf$style == "rmarkdown") {
      message("'rmarkdown' style not supported - using 'multiline' instead")
      pf$style <- "multiline"
    }
    
    if ("trim.strings" %in% names(mc) &&
        !isTRUE(test_logical(pf$trim.strings, 
                             len = 1, any.missing = FALSE))) {
      errmsg %+=% "'trim.strings' must be either TRUE or FALSE"
    }
    
    if ("silent" %in% names(mc) &&
        !isTRUE(test_logical(pf$silent, len = 1, any.missing = FALSE))) {
      errmsg %+=% "'silent' must be either TRUE or FALSE"
    }
    
    if ("tmp.img.dir" %in% names(mc) && !is.na(pf$tmp.img.dir) &&
        (!isTRUE(test_character(pf$tmp.img.dir, min.chars = 1, len = 1, )) ||
         nchar(pf$tmp.img.dir) > 5)) {
      errmsg %+=% "'tmp.img.dir' must have at least 1 and at most 5 characters"
    }
    
    if ("tmp.img.dir" %in% names(mc) && !is.na(pf$tmp.img.dir) &&
        isFALSE(st_options("use.x11"))) {
      message("'tmp.img.dir' will be ignored since use.x11 option is set to ",
              "FALSE")
    }
  }
  return(errmsg)
}

check_args_tb <- function(mc) {
  
  pf <- parent.frame()
  errmsg <- character()

  if ("order" %in% names(mc) &&
      !isTRUE(test_choice(pf$order, c(1, 2, 3)))) {
    errmsg %+=% "'order' must be one of 1, 2, or 3"
  }

  if ("drop.val.col" %in% names(mc) && 
      !isTRUE(test_logical(pf$drop.val.col, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'drop.val.col' must be either TRUE or FALSE"
  }
  
  if ("na.rm" %in% names(mc)) {
    message("parameter na.rm is deprecated; use ",
            "freq(..., report.nas = FALSE) instead")
  }
  
  return(errmsg)
}

check_args_print <- function(mc) {
  
  pf <- parent.frame()
  errmsg <- character()
  
  if (!isTRUE(test_choice(pf$method, 
                          c("pander", "browser", "viewer", "render")))) {
    errmsg %+=% paste("'method' must be one of 'pander', 'browser', 'viewer',",
                      "or 'render'")
  }

  if (!isTRUE(test_int(pf$max.tbl.height, lower = 100, na.ok = FALSE)) &&
      !is.infinite(pf$max.tbl.height)) {
    errmsg %+=% "'max.tbl.height' must be an integer between 100 and Inf"
  } 
  
  if (pf$file == "" && isTRUE(pf$append)) {
    errmsg %+=% "'append' is set to TRUE but no file name has been specified"
  }

  if (pf$file != "" && isTRUE(pf$append) && !file.exists(pf$file)) {
    errmsg %+=% "'append' is set to TRUE but specified file does not exist"
  }

  if (pf$file != "" && isTRUE(pf$append) && !is.na(pf$report.title)) {
    errmsg %+=% "Appending existing file -- 'report.title' arg. will be ignored"
  }

  if (!isTRUE(test_string(pf$report.title, na.ok = TRUE))) {
    errmsg %+=% "'report.title' must either be NA or a character string"
  }

  if (!isTRUE(test_logical(pf$escape.pipe, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'escape.pipe' must be either TRUE or FALSE"
  }

  if (!is.na(pf$custom.css) && 
      !isTRUE(check_file_exists(pf$custom.css, access = "r"))) {
    errmsg %+=% "'custom.css' must point to an existing file."
  }

  if (!isTRUE(test_logical(pf$silent, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'silent' must be either TRUE or FALSE"
  }

  if (pf$file != "" && !isTRUE(test_path_for_output(pf$file, overwrite = TRUE))) {
     errmsg %+=% "'file' path is not valid - check that directory exists"
  }
  
  if (pf$method == "pander" && !is.na(pf$table.classes)) {
    errmsg %+=% "'table.classes' option does not apply to method 'pander'"
  }
  
  if (pf$method == "pander" && !is.na(pf$custom.css)) {
    errmsg %+=% "'custom.css' option does not apply to method 'pander'"
  }
  
  # Set plain.ascii to false and adjust style when file extension is .md or .Rmd
  if (grepl("\\.r?md$", pf$file, ignore.case = TRUE, perl = TRUE) 
      && !"style" %in% names(pf$dotArgs)) {
    if (isTRUE(attr(pf$x, "format_info")$plain.ascii) && 
        !"plain.ascii" %in% names(pf$dotArgs)) {
      tmp_msg_flag <- TRUE
      pf$dotArgs %+=% list(plain.ascii = FALSE)
    } else {
      tmp_msg_flag <- FALSE
    }

    newstyle = switch(attr(pf$x, "st_type"),
                      freq      = "rmarkdown",
                      ctable    = "grid",
                      descr     = "rmarkdown",
                      dfSummary = "grid")
    
    if (attr(pf$x, "format_info")$style %in% c("simple", "multiline")) {
      pf$dotArgs %+=% list(style = newstyle)
      if (isTRUE(tmp_msg_flag)) {
        message("Setting 'plain.ascii' to FALSE and changing style to '",
                newstyle, "' for improved markdown compatibility")
      } else {
        message("Changing style to '", newstyle, 
                "' for improved markdown compatibility")
      }
    } else if (isTRUE(tmp_msg_flag)) {
      message("Setting 'plain.ascii' to FALSE for improved markdown ",
              "compatibility")
    }
  }
  
  if (pf$file != "" && identical(pf$method, "render")) {
    message('To write content to a file, use method="pander", or ',
            'method="browser", or leave method unspecified')
  }
  
  if (is.na(pf$footnote)) {
    pf$footnote <- ""
  }

  if (!"silent" %in% names(mc)) {
    if (attr(pf$x, "st_type") == "descr") {
      pf$silent <- st_options("descr.silent")
    } else if (attr(pf$x, "st_type") == "dfSummary") {
      pf$silent <- st_options("dfSummary.silent")
    }
  }
 return(errmsg)
}
  
# check_args_st_options ---------------------------------------------------
#' @importFrom checkmate test_int test_logical test_choice 
#' test_file_exists test_character
check_args_st_options <- function(mc) {
  
  pf <- parent.frame()
  errmsg <- character()
  
  if ("omit.headings" %in% names(mc)) {
    errmsg %+=% "'omit.headings' is deprecated; use 'headings' instead"
  }
  
  if ("style" %in% names(mc)) {
    if (!isTRUE(test_choice(pf$style, 
                            c("simple", "grid", "rmarkdown", "jira")))) {
      errmsg %+=% paste("'style' must be one of 'simple', 'grid', 'markdown', ",
                        "or 'jira'; See documentation for details")
    }
  }
  
  if ("plain.ascii" %in% names(mc) && 
      !isTRUE(test_logical(pf$plain.ascii, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'plain.ascii' must be either TRUE or FALSE"
  }
  
  if ("round.digits" %in% names(mc) && !isTRUE(test_int(pf$round.digits))) {
    errmsg %+=% "'round.digits' must be a whole number"
  }
  
  if ("headings" %in% names(mc) &&
      !isTRUE(test_logical(pf$headings, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'headings' must be either TRUE or FALSE"
  }
  
  if ("footnote" %in% names(mc) &&
      !isTRUE(test_character(pf$footnote)) && !is.na(pf$footnote)) {
    errmsg %+=% "'footnote' must be either a string or NA"
  }
  
  if ("display.labels" %in% names(mc) &&
      !isTRUE(test_logical(pf$display.labels, 
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'display.labels' must be either TRUE or FALSE"
  }

  if ("na.val" %in% names(mc) && !is.null(pf$na.val) &&
      isFALSE(test_character(pf$na.val, any.missing = FALSE, len = 1))) {
        errmsg %+=% "invalid na.val; must be character vector of length 1"
  }
  
  if ("bootstrap.css" %in% names(mc) &&
      !isTRUE(test_logical(pf$bootstrap.css, 
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'bootstrap.css' must be either TRUE or FALSE"
  }
  
  if ("custom.css" %in% names(mc) && !is.na(pf$custom.css) &&
      !isTRUE(test_file_exists(pf$custom.css, access = "r"))) {
    errmsg %+=% "'custom.css' file not found"
  }
  
  if ("escape.pipe" %in% names(mc) &&
      !isTRUE(test_logical(pf$escape.pipe, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'escape.pipe' must be either TRUE or FALSE"
  }
  
  if ("freq.cumul" %in% names(mc) &&
      !isTRUE(test_logical(pf$freq.cumul, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'freq.cumul' must be either TRUE or FALSE"
  }
  
  if ("freq.totals" %in% names(mc) &&
      !isTRUE(test_logical(pf$freq.totals, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'freq.totals' must be either TRUE or FALSE"
  }
  
  if ("freq.report.nas" %in% names(mc) &&
      !isTRUE(test_logical(pf$freq.report.nas, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'freq.report.nas' must be either TRUE or FALSE"
  }
  
  if ("freq.ignore.threshold" %in% names(mc) &&
      !isTRUE(test_int(pf$freq.ignore.threshold, lower = 0)) &&
      !is.infinite(pf$freq.ignore.threshold)) {
    errmsg %+=% "'freq.ignore.threshold' must be an integer greater than 0"
  }
  
  if ("freq.silent" %in% names(mc) &&
      !isTRUE(test_logical(pf$freq.silent, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'freq.silent' must be either TRUE or FALSE"
  }
  
  if ("ctable.prop" %in% names(mc) &&
      !isTRUE(test_choice(pf$ctable.prop, c("r", "c", "t", "n")))) {
    errmsg %+=% "'ctable.prop' must be one of \"r\", \"c\", \"t\", or \"n\""
  }
  
  if ("ctable.totals" %in% names(mc) &&
      !isTRUE(test_logical(pf$ctable.totals, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'ctable.totals' must be either TRUE or FALSE"
  }
  
  if ("descr.stats" %in% names(mc)) {
    
    # Check for invalid items
    stats <- tolower(pf$descr.stats)
    invalid_stats <- setdiff(
      stats, c(.st_env$descr.stats.valid$no_wgts,
               paste0("-", .st_env$descr.stats.valid$no_wgts),
               "all", "common", "fivenum"))
    
    if (length(invalid_stats) > 0) {
      errmsg %+=%
        paste("descr.stats: values", 
              paste(dQuote(invalid_stats), collapse = ", "),
              "not recognized; see ?descr")
    }
  }

  if ("descr.transpose" %in% names(mc) &&
      !isTRUE(test_logical(pf$descr.transpose, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'descr.transpose' must be either TRUE or FALSE"
  }
  
  if ("descr.silent" %in% names(mc) &&
      !isTRUE(test_logical(pf$descr.silent, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'descr.silent' must be either TRUE or FALSE"
  }

  if ("dfSummary.style" %in% names(mc) &&
      !isTRUE(test_choice(pf$dfSummary.style, 
                          c("grid", "multiline")))) {
    errmsg %+=% "'dfSummary.style' must be either 'grid' or 'multiline'"
  }
    
  if ("dfSummary.varnumbers" %in% names(mc) &&
      !isTRUE(test_logical(pf$dfSummary.varnumbers, 
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'dfSummary.varnumbers' must be either TRUE or FALSE"
  }
  
  if ("dfSummary.labels.col" %in% names(mc) &&
      !isTRUE(test_logical(pf$dfSummary.labels.col,
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'dfSummary.labels.col' must be either TRUE or FALSE"
  }
  
  if ("dfSummary.valid.col" %in% names(mc) &&
      !isTRUE(test_logical(pf$dfSummary.valid.col,
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'dfSummary.valid.col' must be either TRUE or FALSE"
  }
  
  if ("dfSummary.na.col" %in% names(mc) &&
      !isTRUE(test_logical(pf$dfSummary.na.col, 
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'dfSummary.na.col' must be either TRUE or FALSE"
  }
  
  if ("dfSummary.graph.col" %in% names(mc) &&
      !isTRUE(test_logical(pf$dfSummary.graph.col,
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'dfSummary.graph.col' must be either TRUE or FALSE"
  }
  
  if ("dfSummary.graph.magnif" %in% names(mc) && 
      pf$dfSummary.graph.magnif <= 0) {
    errmsg %+=% "'dfSummary.graph.magnif' must be > 0"
  }

  if ("dfSummary.silent" %in% names(mc) &&
      !isTRUE(test_logical(pf$dfSummary.silent,
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'dfSummary.silent' must be either TRUE or FALSE"
  }
  
  if ("dfSummary.custom.1" %in% names(mc) &&
      !isTRUE(is.expression(pf$dfSummary.custom.1)) &&
      !is.na(pf$dfSummary.custom.1) && 
      !pf$dfSummary.custom.1 %in% c("default", "reset")) {
    errmsg %+=% "'dfSummary.custom.1' must be an expression, NA, or 'default'"
  }

  if ("dfSummary.custom.2" %in% names(mc) &&
      !isTRUE(is.expression(pf$dfSummary.custom.2)) &&
      !is.na(pf$dfSummary.custom.2))  {
    errmsg %+=% "'dfSummary.custom.2' must be an expression, or NA"
  }
  
  if ("tmp.img.dir" %in% names(mc) && !is.na(pf$tmp.img.dir) &&
       (!isTRUE(test_character(pf$tmp.img.dir, min.chars = 1, len = 1)) ||
       nchar(pf$tmp.img.dir) > 5)) {
    errmsg %+=% "'tmp.img.dir' must have at least 1 and at most 5 characters"
  }

  if ("subtitle.emphasis" %in% names(mc) &&
      !isTRUE(test_logical(pf$subtitle.emphasis, 
                           len = 1, any.missing = FALSE))) {
    errmsg %+=% "'subtitle.emphasis' must be either TRUE or FALSE"
  }
    
  if ("lang" %in% names(mc) && !pf$lang %in% 
      c(rownames(.translations), "custom")) {
    errmsg %+=% paste0("'lang' can take the following values only: ",
                       paste(rownames(.translations), collapse = ", "))
  }

  if ("option" %in% names(mc) && 
      any(grepl("dfSummary.custom", pf$option)) && "value" %in% names(mc)) {
    errmsg %+=% paste0("'dfSummary.custom expressions must be defined using ",
                       "syntax st_options(dfSummary.custom.1 = expression(...)")
  }
  
  if ("use.x11" %in% names(mc) &&
      !isTRUE(test_logical(pf$use.x11, len = 1, any.missing = FALSE))) {
    errmsg %+=% "'use.x11' must be either TRUE or FALSE"
  }
  
  return(errmsg)
}
