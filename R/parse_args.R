#' Extract Data Information From Arguments Passed to Functions
#'
#' Using sys.calls(), sys.frames() and match.call(), this utility function
#' extracts and/or deducts information about the data being processed.
#' Data frame name, variable names and labels if any, subsetting information,
#' grouping information (when by() is used) are returned by the function which
#' tries various methods to get this information.
#'
#' @param sys_calls Object created using \code{sys.calls()}.
#' @param sys_frames Object created using \code{sys.frames()}.
#' @param match_call Object created using \code{match.call()}.
#' @param var Character. \dQuote{x} (default) and/or \dQuote{y} (the latter 
#'   being used only in \code{\link{ctable}}).
#' @param silent Logical. Hide console messages. \code{TRUE} by default.
#' 
#' @return A list consisting of one or many of the following items
#' \itemize{
#'   \item df_name The data frame name
#'   \item df_label The data frame label
#'   \item var_name The variable name(s)
#'   \item var_label The variable label
#'   \item by_var The variable used in \code{by()}, when in the call stack
#'   \item by_group The group, when \code{by()} was used
#'   \item by_first Binary indicator used when \code{by()} is in the call stack
#'   \item by_last Binary indicator}
#'
#' @keywords internal misc
#'
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com>}
#'
#' @importFrom pryr standardise_call where
#' @importFrom utils head
parse_args <- function(sys_calls, sys_frames, match_call, 
                       var = "x", silent = FALSE, 
                       df_name = TRUE, df_label = TRUE,
                       var_name = TRUE, var_label = TRUE,
                       caller = "") {
  
  upd_output <- function(item, value, force = FALSE) {
    output <- get("output", envir = fn.env)
    if (isTRUE(force) || 
        ((length(output[[item]]) == 0 || is.na(output[[item]])) &&
          length(value) == 1 && class(value) == class(output[[item]]))) {
      names(value) <- NULL
      if (!is.na(value) && value == ".") value <- NA_character_
      output[[item]] <- value
      
      # Check if output is ready to be returned
      if (count_empty(output, count.nas = FALSE) == 0) {
        # Cleanup
        for (item in intersect(c("var_name", "by_var", "by_group"),
                                 names(output))) {
          if (length(output$df_name) == 1 && !is.na(output$df_name)) {
            
            re <- paste0("^",output$df_name,  # starts with df_name
                         "[$[]*['\"]?",       # subsetting chars
                         "([\\w_.]+)",        # var_name (group 1)
                         "['\"]?\\]{0,2}",    # closing subsetting char(s)
                         "(.*)$")             # remainder of expression (gr. 2)
            output[[item]] <- sub(pattern = re,
                                  replacement = "\\1\\2", 
                                  x = output[[item]], perl = TRUE)
            
            if (item == "by_group") {
              output$by_group <- gsub(paste0(output$df_name, "\\$"), "", output$by_group)
            }
            
          }
          output[[item]] <- gsub("['\"]", "", output[[item]])
        }
      
        empty_elements <- as.numeric(
          which(vapply(output, function(x) {identical(x, NA_character_) || 
              length(x) == 0}, TRUE))
          )
        
        if (length(empty_elements) > 0) {
          output <- output[-empty_elements]
        }
        
        assign("do_return", envir = fn.env, value = TRUE)        
      }
    }
    assign("output", output, envir = fn.env)
  }

  populate_by_info <- function() {
    
    by_var  <- deparse(calls$by$INDICES)

    # Normalize variable name
    if (grepl(re4, by_var, perl = TRUE)) {
      df_nm   <- sub(re1, "\\1", by_var, perl = TRUE)
      df_       <- get_object(df_nm, "data.frame")
      if (!identical(df_, NA)) {
        v_name <- sub(re1, "\\4", by_var, perl = TRUE)
        if (v_name %in% colnames(df_)) {
          by_var <- paste(df_nm, v_name, sep = "$")
        }
      }
    }
      
    if (grepl(re2, by_var, perl = TRUE)) {
      df_nm <- sub(re2, "\\1", by_var, perl = TRUE)
      df_   <- get_object(df_nm, "data.frame")
      if (!identical(df_, NA)) {
        var_number <- as.numeric(sub(re2, "\\4", by_var, perl = TRUE))
        v_name <- colnames(df_)[var_number]
        by_var <- paste(df_nm, v_name, sep = "$")
      }
    }
    
    # On first iteration, generate levels based on IND variables, and store
    if (length(.st_env$byInfo) == 0) {
      if (is.null(names(sys_frames[[pos$tapply]]$namelist)) ||
          is.na(names(sys_frames[[pos$tapply]]$namelist)[1])) {
        names(sys_frames[[pos$tapply]]$namelist) <- 
          as.character(calls$by$INDICES)[-1]
      }
      by_levels <- sys_frames[[pos$tapply]]$namelist
      .st_env$byInfo$by_levels <- 
        expand.grid(by_levels, stringsAsFactors = FALSE)
      colnames(.st_env$byInfo$by_levels) <- by_var
      .st_env$byInfo$iter <- 1
    }
    
    # Populate by_group item
    by_group <- 
      paste(colnames(.st_env$byInfo$by_levels),
            as.character(.st_env$byInfo$by_levels[.st_env$byInfo$iter, ]),
            sep=" = ", collapse = ", ")
    
    # by_first and by_last are used by print.summarytools when printing objects
    # passed by the by() function
    if (.st_env$byInfo$iter == 1 && nrow(.st_env$byInfo$by_levels) == 1) {
      by_first <- TRUE
      by_last  <- TRUE
      .st_env$byInfo <- list()
    } else if (.st_env$byInfo$iter == 1) {
      by_first <- TRUE
      by_last  <- FALSE
      .st_env$byInfo$iter <- .st_env$byInfo$iter + 1
    } else if (.st_env$byInfo$iter == nrow(.st_env$byInfo$by_levels)) {
      by_first <- FALSE
      by_last  <- TRUE
      .st_env$byInfo <- list()
    } else {
      by_first <- FALSE
      by_last <- FALSE
      .st_env$byInfo$iter <- .st_env$byInfo$iter + 1
    }
    
    upd_output("by_var",   by_var)
    upd_output("by_group", by_group)
    upd_output("by_first", by_first)
    upd_output("by_last",  by_last)
    TRUE
  }
  
  by_ctable_case <- function() {
    
    v_name <- c(deparse(calls$by$data$x), deparse(calls$by$data$y))
    
    if (!"with" %in% names(calls)) {
      if (any(grepl("\\$", v_name))) {
        df_nm    <- sub("^([\\w._]+)\\$.+$", "\\1", v_name[1], perl = TRUE)
        df_nm[2] <- sub("^([\\w._]+)\\$.+$", "\\1", v_name[2], perl = TRUE)
      }
      
      if (isTRUE(df_nm[1] == df_nm[2])) {
        df_nm <- df_nm[1]
        df_ <- get_object(df_nm, "data.frame")
        if (!identical(df_, NA)) {
          upd_output("df_name", df_nm)
          upd_output("df_label", label(df_))
        }
      } else {
        upd_output("df_name", NA_character_)
        upd_output("df_label", NA_character_)
      }
    }
    upd_output("var_name", v_name, force = TRUE)
  }

  get_object <- function(name, class) {
    for (i in seq_along(sys_frames)) {
      if (name %in% ls(sys_frames[[i]])) {
        if (inherits(sys_frames[[i]][[name]], class)) {
          return(sys_frames[[i]][[name]])
        }
      }
    }
    # fallback method
    env <- where(name = name)
    return(get(name, env))
  }

  parse_data_str <- function(str) {
    
    if (grepl(re1, str, perl = TRUE)) {
      df_nm <- sub(re1, "\\1", str, perl = TRUE)
      df_   <- get_object(df_nm, "data.frame")
      if (!identical(df_, NA)) {
        upd_output("df_name", df_nm)
        upd_output("df_label", label(df_))
        v_name <- sub(re1, "\\4", str, perl = TRUE)
        if (v_name %in% colnames(df_)) {
          upd_output("var_name", v_name)
          upd_output("var_label", label(df_[[v_name]]))
          return(TRUE)
        }
      }
    } else if (grepl(re2, str, perl = TRUE)) {
      df_nm   <- sub(re2, "\\1", str, perl = TRUE)
      df_       <- get_object(df_nm, "data.frame")
      if (!identical(df_, NA)) {
        upd_output("df_name",  df_nm)
        upd_output("df_label", label(df_))
        var_number <- as.numeric(sub(re2, "\\4", str, perl = TRUE))
        upd_output("var_name",  colnames(df_)[var_number])
        upd_output("var_label", label(df_[[var_number]]))
        return(TRUE)
      }
    } else if (grepl(re3, str, perl = TRUE)) {
      obj_name <- sub(re3, "\\1", str, perl = TRUE)
      obj_env <- try(where(obj_name))
      if (!inherits(obj_env, "try-error")) {
        obj <- get(obj_name, envir = obj_env)
        if (is.data.frame(obj)) {
          upd_output("df_name", obj_name)
          upd_output("df_label", label(obj))
          if(isTRUE(var_name)) {
            obj2_name <- sub(re3, "\\2", str, perl = TRUE)
            obj2      <- try(eval(parse(text = obj2_name), envir = obj), 
                             silent = TRUE)
            if (!inherits(obj2, "try-error") && is.atomic(obj2)) {
              upd_output("var_name",  obj2_name)
              upd_output("var_label", NA_character_)
            }
          }
        } else if (is.atomic(obj)) {
          upd_output("var_name", obj_name)
          upd_output("var_label", label(obj))
        }
        return(TRUE)
      }
    }
    upd_output("df_name",   NA_character_)
    upd_output("var_name",  NA_character_)
    upd_output("df_label",  NA_character_)
    upd_output("var_label", NA_character_)
    return(FALSE)
  }
  
  # When pipe is used, this recursive function gets the "deepest" lhs
  # that constitues something other than a function call
  get_lhs <- function(x) {
    if ("lhs" %in% names(x) && is.call(x$lhs)) {
      x$lhs <- pryr::standardise_call(x$lhs)
      return(get_lhs(x$lhs))
    } else {
      return(x$lhs)
    }
  }
  
  # Declare a few "constant" ---------------------------------------------------
  oper <- c("$", "[", "[[", "<", ">", "<=", ">=", "==", ":", "%>%")
  fn.env <- environment()
  do_return <- FALSE
  # regex 1 ; both names are there (df$name, df['name']), etc.
  re1 <- paste0("^([\\w.]+)(\\$|\\[{1,2})(.*\\,\\s)?['\"]?",
                "([a-zA-Z._][\\w._]+)['\"]?\\]{0,2}(\\[.+)?$")
  # regex 2 ; there is numeric indexing (df[[2]], df[ ,2], df[2])
  re2 <- "^([\\w.]+)(\\$|\\[{1,2})(.*\\,\\s)?(\\d+)\\]{1,2}(\\[.+)?$"
  # regex 3 : fallback solution when only 1 name can be found / second group
  #           can also be further decomposed if needed
  re3 <- "^([a-zA-Z._][\\w.]*)[$[]*(.*?)$"
  # re4 is like re1 but doesn't match df$name
  re4 <- paste0("^([\\w.]+)(\\[{1,2})(.*\\,\\s)?['\"]?",
                "([a-zA-Z._][\\w._]+)['\"]?\\]{0,2}(\\[.+)?$")
  
  # Initialize output object
  output <- list()
  if (isTRUE(df_name))
    output %+=% list(df_name   = character())
  if (isTRUE(df_label))
    output %+=% list(df_label  = character())
  if (isTRUE(var_name))
    output %+=% list(var_name  = character())
  if (isTRUE(var_label))
    output %+=% list(var_label = character())
  
  #  Make a list of all data items contained in the environments
  ls_sys_frames <- lapply(sys_frames, ls)
  funs_stack    <- lapply(sys_calls, head, 1)
  names(ls_sys_frames) <- sub("summarytools::", "",
                              as.character(unlist(funs_stack)),
                              fixed = TRUE)
  
  # Look for position of by() + tapply(), with() lapply() and %>% in sys.calls()
  pos         <- list()
  pos$by      <- which(funs_stack %in% c("by()", "stby()"))
  pos$with    <- which(funs_stack == "with()")
  pos$pipe    <- which(funs_stack == "`%>%`()")
  pos$dollar  <- which(funs_stack == "`%$%`()")
  pos$lapply  <- which(funs_stack == "lapply()")
  pos$tapply  <- which(funs_stack == "tapply()")
  pos$fun     <- which(grepl(paste0(caller, "()"), funs_stack))
  
  pos <- pos[-which(unlist(lapply(pos, length)) == 0)]
  
  if ("by" %in% names(pos)) {
    output %+=% list(by_var   = character(),
                     by_group = character(),
                     by_first = logical(),
                     by_last  = logical())
  }
  
  # Generate standardized calls
  calls <- list()
  for (i in seq_along(pos)) {
    calls[[names(pos)[i]]] <- sys_calls[[pos[[i]]]]
  }

  # in the call stack: by() ----------------------------------------------------  
  if ("by" %in% names(calls)) {
    calls$by <- standardise_call(calls$by)
    try(calls$lapply <- standardise_call(calls$lapply[-4]), silent = TRUE)
    try(calls$tapply <- standardise_call(calls$tapply), silent = TRUE)

    populate_by_info()
    # treat special case of by() called on ctable, with ou without "with()"
    if (length(calls$by$data) > 1 && deparse(calls$by$data[[1]]) == "list"
        && identical(names(calls$by$data), c("", "x", "y"))) {
      by_ctable_case()
      if (isTRUE(do_return)) {
        return(output)
      }
    } else {
      x <- sys_frames[[pos$by]]$data
      if (is.data.frame(x)) {
        if (length(calls$by$data) == 1) {
          upd_output("df_name", deparse(calls$by$data))
          upd_output("df_label",
                     label(get_object(deparse(calls$by$data), "data.frame")))
        } else {
          parse_data_str(deparse(calls$by$data))
        }
      } else if (is.atomic(x)) {
        if (length(calls$by$data) == 1) {
          upd_output("var_name", deparse(calls$by$data))
          upd_output("var_label", label(x))
          if (!"with" %in% names(calls)) {
            upd_output("df_name", NA_character_)
            upd_output("df_label", NA_character_)
          }
        } else {
          x_str <- deparse(calls$by$data)
          parse_data_str(x_str)
        }
      }
    }
    if (isTRUE(do_return)) {
      return(output)
    }
  }
  
  # in the call stack: with() --------------------------------------------------  
  if ("with" %in% names(calls)) {
    x <- sys_frames[[pos$with]]$data
    calls$with <- standardise_call(calls$with)
    calls$with$expr <- standardise_call(calls$with$expr)
    if (is.data.frame(x)) {
      if (length(calls$with$data) == 1) {
        tmp_name <- deparse(calls$with$data)
        if (tmp_name == "." && "dollar" %in% names(calls)) {
          calls$dollar <- standardise_call(calls$dollar)
          if (is.call(calls$dollar$rhs) && 
              identical(standardise_call(calls$dollar$rhs), calls$with$expr)) {
            tmp_name <- calls$dollar$lhs
            if (length(tmp_name) == 1) {
              upd_output("df_name",  deparse(tmp_name))
            } else {
              upd_output("df_name",  NA_character_)
            }
            upd_output("df_label", label(x))
          }
        } else if (tmp_name == "." && "pipe" %in% names(calls)) {
          calls$pipe <- standardise_call(calls$pipe)
          calls$pipe$lhs <- standardise_call(calls$pipe$lhs)
          tmp_name <- get_lhs(calls$pipe$lhs)
          if (length(tmp_name) == 1) {
            upd_output("df_name",  deparse(tmp_name))
          } else if (is.null(tmp_name)) {
            upd_output("df_name", deparse(calls$pipe$lhs))
          }
        } else {
          upd_output("df_name", deparse(calls$with$data))
        }
      } else {
        upd_output("df_name", setdiff(as.character(calls$with$data), oper)[1])
      }
      upd_output("df_label", label(x))
      if (isTRUE(do_return)) {
        return(output)
      }
      
      if ("x" %in% names(calls$with$expr)) {
        if (is.call(calls$with$expr$x)) {
          v_name <- c(x = deparse(standardise_call(calls$with$expr$x)$x),
                      y = deparse(standardise_call(calls$with$expr$x)$y))
        } else {
          v_name <- c(x = deparse(calls$with$expr$x),
                      y = deparse(calls$with$expr$y))
        }
        if (length(var) == 1) {
          upd_output("var_name",  v_name[[var]])
          upd_output("var_label", label(x[[v_name[[var]]]]))
        } else {
          upd_output("var_name", v_name, force = TRUE)
          upd_output("var_label", NA_character_)
        }
      } else if ("data" %in% names(calls$with$expr)) {
        calls$with$expr$data <- standardise_call(calls$with$expr$data)
        if ("x" %in% names(calls$with$expr$data)) {
          v_name <- c(x = deparse(calls$with$expr$data$x),
                      y = deparse(calls$with$expr$data$y))
          if (length(var) == 1) {
            upd_output("var_name",  v_name[[var]])
            upd_output("var_label", label(x[[v_name[[var]]]]))
          } else {
            upd_output("var_name", v_name, force = TRUE)
            upd_output("var_label", NA_character_)
          }
        }
      }
      if (isTRUE(do_return)) {
        return(output)
      }
    }
  }

  # in the call stack: %>% -----------------------------------------------------
  if ("pipe" %in% names(calls)) {
    calls$pipe <- standardise_call(calls$pipe)
    obj_name <- deparse(calls$pipe$lhs)
    obj_name <- sub(paste0(caller, "\\((.+)\\)"), "\\1", obj_name)
    obj <- eval(sys_frames[[pos$pipe]][[obj_name]], 
                envir = sys_frames[[pos$pipe]]$parent)
    if (is.data.frame(obj)) {
      if (length(calls$pipe$lhs) == 1) {
        upd_output("df_name", obj_name)
      } else {
        calls$pipe$lhs <- standardise_call(calls$pipe$lhs)
        tmp_name <- get_lhs(calls$pipe$lhs)
        if (length(tmp_name) == 1) {
          upd_output("df_name", deparse(tmp_name))
        } else if (is.null(tmp_name)) {
          upd_output("df_name", setdiff(as.character(calls$pipe$lhs), oper)[1])
        }
      }
      upd_output("df_label", label(obj))
      v_name <- setdiff(as.character(calls$pipe$rhs), c(caller, oper))
      if (length(v_name) == 1 && v_name %in% colnames(obj)) {
        upd_output("var_name", v_name)
        upd_output("var_label", label(obj[[v_name]]))
      } else {
        if (ncol(obj) == 1) {
          upd_output("var_name", names(obj))
          upd_output("var_label", label(obj[[1]]))
        } else {
          upd_output("var_name", NA_character_)
          upd_output("var_label", NA_character_)
        }
      }
    } else if (is.atomic(obj)) {
      if(length(calls$pipe$lhs) == 1) {
        upd_output("var_name", obj_name)
        upd_output("var_label", label(obj))
      } else {
        parse_data_str(obj_name)
      }
    }
    if (isTRUE(do_return)) {
      return(output)
    }
  }

  # in the call stack: lapply() ------------------------------------------------
  if ("lapply" %in% names(calls)) {
    try(calls$lapply <- standardise_call(calls$lapply[-4]))
        
    iter <- sys_frames[[pos$lapply]]$i
    obj  <- sys_frames[[pos$lapply]]$X[iter]
    
    if (is.atomic(obj[[1]])) {
      v_name <- names(obj)
      
      # Find the data frame
      df_nm <- setdiff(all.names(calls$lapply$X), oper)[1]
      df_     <- get_object(df_nm, "data.frame")
      if (identical(df_, NA)) {
        env <- try(where(df_nm))
        if (!inherits(env, "try-error")) {
          df_ <- get(df_nm, envir = env)
        }
      }
      if (is.data.frame(df_) && v_name %in% colnames(df_)) {
        upd_output("var_name",  paste(df_nm, v_name, sep = "$"))
        upd_output("var_label", label(df_[[v_name]]))
        upd_output("df_name",   NA_character_)
        upd_output("df_label",  NA_character_)
      }
    } else if (is.data.frame(obj[[1]])) {
      df_nm <- names(obj)
      upd_output("df_name",   df_nm)
      upd_output("df_label",  label(obj))
      upd_output("var_name",  NA_character_)
      upd_output("var_label", NA_character_)
    }
    
    if (isTRUE(do_return)) {
      return(output)
    }
  }  
      
  if ("fun" %in% names(calls)) {
    calls$fun <- standardise_call(calls$fun)
    obj <- sys_frames[[pos$fun]][[var]]
    if (is.data.frame(obj)) {
      if (length(calls$fun[[var]]) == 1) {
        upd_output("df_name",   deparse(calls$fun[[var]]))
        upd_output("df_label",  label(obj))
        upd_output("var_name",  NA_character_)
        upd_output("var_label", NA_character_)
      } else {
        parse_data_str(deparse(calls$fun[[var]]))
      }
    } else if (is.atomic(obj)) {
      if (length(calls$fun[[var]]) == 1) {
        upd_output("var_name",  deparse(calls$fun[[var]]))
        upd_output("var_label", label(obj))
        upd_output("df_name",   NA_character_)
        upd_output("df_label",  NA_character_)
      } else {
        parse_data_str(deparse(calls$fun[[var]]))
      }
    }
    if (isTRUE(do_return)) {
      return(output)
    }
  }
}
