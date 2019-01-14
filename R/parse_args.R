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
    if (isTRUE(force) || 
        ((length(output[[item]]) == 0 || is.na(output[[item]])) &&
          length(value) == 1 && class(value) == class(output[[item]]))) {
      output[[item]] <<- value
      
      # Check if output is ready to be returned
      if (count.empty(output, count.nas = FALSE) == 0) {
        # Cleanup
        if (length(output$df_name) == 1 && !is.na(output$df_name)) {
          for (item in intersect(c("var_name", "by_var", "by_group"),
                                 names(output))) {
            re <- paste0("^",output$df_name,  # starts with df_name
                         "[$[]*['\"]?",       # subsetting chars
                         "([\\w_.]+)",        # var_name (group 1)
                         "['\"]?\\]{0,2}",    # closing subsetting char(s)
                         "(.*)$")             # remainder of expression (gr. 2)
            output[[item]] <<- sub(pattern = re,
                                   replacement = "\\1\\2", 
                                   x = output[[item]], perl = TRUE)
          }
        }
        
        empty_elements <- as.numeric(
          which(vapply(output, function(x) {is.na(x) || !length(x)}, 
                       TRUE))
        )
        
        if (length(empty_elements) > 0) {
          output <<- output[-empty_elements]
        }
        
        assign("do_return", envir = fn.env, value = TRUE)        
      }
    }
  }

  populate_by_info <- function() {
    
    by_var  <- deparse(calls$by$INDICES)

    # Normalize variable name
    if (grepl(re4, by_var, perl = TRUE)) {
      df_name   <- sub(re1, "\\1", by_var, perl = TRUE)
      df_       <- get_object(df_name, "data.frame")
      if (!identical(df_, NA)) {
        var_name <- sub(re1, "\\4", by_var, perl = TRUE)
        if (var_name %in% colnames(df_)) {
          by_var <- paste(df_name, var_name, sep = "$")
        }
      }
    }
      
    if (grepl(re2, by_var, perl = TRUE)) {
      df_name   <- sub(re2, "\\1", by_var, perl = TRUE)
      df_       <- get_object(df_name, "data.frame")
      if (!identical(df_, NA)) {
        var_number <- as.numeric(sub(re2, "\\4", by_var, perl = TRUE))
        var_name <- colnames(df_)[var_number]
        by_var <- paste(df_name, var_name, sep = "$")
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
    
    var_name <- c(deparse(calls$by$data$x), deparse(calls$by$data$y))
    
    if (!"with" %in% names(calls)) {
      if (any(grepl("\\$", var_name))) {
        df_name    <- sub("^([\\w._]+)\\$.+$", "\\1", var_name[1], perl = TRUE)
        df_name[2] <- sub("^([\\w._]+)\\$.+$", "\\1", var_name[2], perl = TRUE)
      }
      
      if (isTRUE(df_name[1] == df_name[2])) {
        df_name <- df_name[1]
        df_ <- get_object(df_name, "data.frame")
        if (!identical(df_, NA)) {
          upd_output("df_name", df_name)
          upd_output("df_label", label(df_))
        }
      } else {
        upd_output("df_name", NA_character_)
        upd_output("df_label", NA_character_)
      }
    }
    upd_output("var_name", var_name, force = TRUE)
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
      df_name   <- sub(re1, "\\1", str, perl = TRUE)
      df_       <- get_object(df_name, "data.frame")
      if (!identical(df_, NA)) {
        upd_output("df_name", df_name)
        upd_output("df_label", label(df_))
        var_name <- sub(re1, "\\4", str, perl = TRUE)
        if (var_name %in% colnames(df_)) {
          upd_output("var_name", var_name)
          upd_output("var_label", label(df_[[var_name]]))
          return(TRUE)
        }
      }
    } else if (grepl(re2, str, perl = TRUE)) {
      df_name   <- sub(re2, "\\1", str, perl = TRUE)
      df_       <- get_object(df_name, "data.frame")
      if (!identical(df_, NA)) {
        upd_output("df_name", df_name)
        upd_output("df_label", label(df_))
        var_number <- as.numeric(sub(re2, "\\4", str, perl = TRUE))
        upd_output("var_name", colnames(df_)[var_number])
        upd_output("var_label", label(df_[[var_number]]))
        return(TRUE)
      }
    } else if (grepl(re3, str, perl = TRUE)) {
      obj_name <- sub(re3, str, perl = TRUE)
      obj_env <- try(where(obj_name))
      if (!inherits(obj_env, "try-error")) {
        obj <- get(obj_name, envir = obj_env)
        if (is.data.frame(obj)) {
          upd_output("df_name", obj_name)
          upd_output("df_label", label(obj))
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
    
  # Declare a few "constant" ---------------------------------------------------
  oper <- c("$", "[", "[[", "<", ">", "<=", ">=", "==", ":", "%>%")
  fn.env <- environment()
  do_return <- FALSE
  # regex ; both names are there (df$name, df['name']), etc.
  re1 <- paste0("^([\\w.]+)(\\$|\\[{1,2})(.*\\,\\s)?['\"]?",
                "([a-zA-Z._][\\w._]+)['\"]?\\]{0,2}(\\[.+)?$")
  # regex ; there is numeric indexing (df[[2]], df[ ,2], df[2])
  re2 <- "^([\\w.]+)(\\$|\\[{1,2})(.*\\,\\s)?(\\d+)\\]{1,2}(\\[.+)?$"
  # regex : When we can only get one name (fallback solution)
  re3 <- "^([a-zA-Z._][\\w.]*)(.*?)$"
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
  names(ls_sys_frames) = as.character(unlist(funs_stack))
  
  # Look for position of by() + tapply(), with() lapply() and %>% in sys.calls()
  pos <- list()
  pos$by      <- which(funs_stack == "by()")
  pos$with    <- which(funs_stack == "with()")
  pos$pipe    <- which(funs_stack == "`%>%`()")
  pos$lapply  <- which(funs_stack == "lapply()")
  pos$tapply  <- which(funs_stack == "tapply()")
  pos$fun     <- which(funs_stack == paste0(caller, "()"))
  
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
        upd_output("df_name", deparse(calls$with$data))
      } else {
        upd_output("df_name", setdiff(as.character(calls$with$data), oper)[1])
      }
      upd_output("df_label", label(x))
      if (do_return) {
        return(output)
      }
      if (length(var) == 2) {
        var_name <- deparse(calls$with$expr$data$x)
        var_name %+=% deparse(calls$with$expr$data$y)
        upd_output("var_name", var_name, force = TRUE)
        upd_output("var_label", NA_character_)
      } else {
        var_name <- deparse(calls$with$expr[[var]])
        upd_output("var_name",  var_name)
        upd_output("var_label", label(x[[var_name]]))
      }
    }
    if (isTRUE(do_return)) {
      return(output)
    }
  }

    # in the call stack: %>% -----------------------------------------------------
  if ("pipe" %in% names(calls)) {
    calls$pipe <- standardise_call(calls$pipe)
    obj_name <- deparse(calls$pipe$lhs)
    obj <- eval(sys_frames[[pos$pipe]]$lhs, 
                envir = sys_frames[[pos$pipe]]$parent)
    if (is.data.frame(obj)) {
      if (length(calls$pipe$lhs) == 1) {
        upd_output("df_name", obj_name)
      } else {
        upd_output("df_name", setdiff(as.character(calls$pipe$lhs), oper)[1])
      }
      upd_output("df_label", label(obj))
      var_name <- setdiff(as.character(calls$pipe$rhs), c(caller, oper))
      if (var_name %in% colnames(obj)) {
        upd_output("var_name", var_name)
        upd_output("var_label", label(obj[[var_name]]))
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
      var_name <- names(obj)
      
      # Find the data frame
      df_name <- setdiff(all.names(calls$lapply$X), oper)[1]
      df_     <- get_object(df_name, "data.frame")
      if (identical(df_, NA)) {
        env <- try(where(df_name))
        if (!inherits(env, "try-error")) {
          df_ <- get(df_name, envir = env)
        }
      }
      if (is.data.frame(df_) && var_name %in% colnames(df_)) {
        upd_output("var_name",  paste(df_name, var_name, sep = "$"))
        upd_output("var_label", label(obj))
        upd_output("df_name",   NA_character_)
        upd_output("df_label",  NA_character_)
      }
    } else if (is.data.frame(obj[[1]])) {
      df_name <- names(obj)
      upd_output("df_name",   df_name)
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
    obj <- sys_frames[[pos$fun]]$x
    if (is.data.frame(obj)) {
      if (length(calls$fun$x) == 1) {
        upd_output("df_name",   deparse(calls$fun$x))
        upd_output("df_label",  label(obj))
        upd_output("var_name",  NA_character_)
        upd_output("var_label", NA_character_)
      }
    } else if (is.atomic(obj)) {
      if (length(calls$fun$x) == 1) {
        upd_output("var_name",  deparse(calls$fun$x))
        upd_output("var_label", label(obj))
        upd_output("df_name",   NA_character_)
        upd_output("df_label",  NA_character_)
      } else {
        parse_data_str(deparse(calls$fun$x))
      }
    }
    if (isTRUE(do_return)) {
      return(output)
    }
  }
}

  #   #pos_fn      <- which(grepl("^by\\(", as.character(sys_calls))) # attn! pas bon avec by() (et lapply?)
  #   # Extract, if possible:
  #   #  - string describing the data passed to the caller function
  #   #  - position in the call stack where the data (with its labels) can be found
  # #  - 
  # 
  # if (length(pos_with) == 1) {
  #   std_call <- list()
  #   std_call <- as.list(standardise_call(sys_calls[[pos_with]]))
  #   std_call %+=% as.list(standardise_call(std_call$expr))
  # } else if (length(pos_by) == 1) {
  #   pos_fn   <- which(grepl("^by\\(", as.character(sys_calls)))
  #   data_str <- deparse(sys_calls[[pos_fn]]$data)
  # } else {
  #   pos_fn   <- which(grepl(paste0("^", caller), as.character(sys_calls)))
  #   std_call <- as.list(standardise_call(sys_calls[[pos_fn]]))
  #   data_str <- deparse(std_call$x)
  #   #data_str <- setdiff(as.character(sys_calls[[pos_fn]]), caller)
  # }
  # 
  # 
  # 
  # # operators to remove from vectors of all.names()
  # oper <- c("$", "[", "[[", "<", ">", "<=", ">=", "==", ":", "%>%")
  # # Declare functions
  # # Find df_name ---------------------------------------------------------------
  # get_df <- function() {
  # 
  #   # We get the name and create 2 instances of df__ ; depending on where df
  #   # was created, and how the function is called the labels 
  #   if (length(pos_with) == 1) {
  #     std_call <- as.list(standardise_call(sys_calls[[pos_with]]))
  #     if (is.data.frame(eval(std_call$data))) {
  #       df_name <- deparse(std_call$data)
  #       for (i in seq_along(sys_frames)) {
  #         if (df_name %in% ls(sys_frames[[i]])) {
  #           if (is.data.frame(sys_frames[[i]][[df_name]])) {
  #             df__ <- sys_frames[[i]][[df_name]]
  #             break
  #           }
  #         }
  #       }
  #       if (!exists("df__", where = -1, inherits = FALSE)) {
  #         df_env <- where(df_name)
  #         df__   <- get(df_name, envir = df_env)
  #       }
  #       return(list(name = df_name, df = df__, 
  #                   call = std_call, method = "with"))
  #     }
  #     return(list(name = NA, df = NA, call = std_call, method = "with"))
  #   }
  #   
  #   else if (length(pos_by) == 1) {
  #     std_call <- as.list(standardise_call(sys_calls[[pos_by]]))
  #     nn <- all.names(by_call$data)
  #     if (length(nn) > 1) {
  #       nn <- setdiff(nn, oper)[1]
  #     }
  #     df_env <- where(nn)
  #     if (is.data.frame(get(nn, envir = df_env))) {
  #       df_name <- nn
  #       for (i in seq_along(sys_frames)) {
  #         if (df_name %in% ls(sys_frames[[i]])) {
  #           if (is.data.frame(sys_frames[[i]][[df_name]])) {
  #             df__ <- sys_frames[[i]][[df_name]]
  #             break
  #           }
  #         }
  #       }
  #       if (!exists("df__", where = -1, inherits = FALSE)) {
  #         df_env <- where(df_name)
  #         df__   <- get(df_name, envir = df_env)
  #       }
  #       return(list(name = df_name, df = df__, 
  #                   call = std_call, method = "by"))
  #     }
  #     return(list(name = NA, df = NA, call = std_call, method = "by"))
  #   }
  #   #df__  <- get(nn, envir = df_env)
  #   #df__2   <- sys_frames[[pos_by]]$data
  #   
  #   else if (length(pos_pipe) == 1) {
  #     std_call <- as.list(standardise_call(sys_calls[[pos_pipe]]))
  #     if (is.data.frame(eval(std_call$lhs))) {
  #       nn <- as.character(std_call$lhs)
  #       if (length(nn) > 1) {
  #         nn <- setdiff(nn, oper)[1]
  #       }
  #       # Find the envir containing the potential df
  #       df_env <- where(nn)
  #       if (is.data.frame(get(nn, envir = df_env))) {
  #         df_name <- nn
  #         for (i in seq_along(sys_frames)) {
  #           if (df_name %in% ls(sys_frames[[i]])) {
  #             if (is.data.frame(sys_frames[[i]][[df_name]])) {
  #               df__ <- sys_frames[[i]][[df_name]]
  #               break
  #             }
  #           }
  #         }
  #         if (!exists("df__", where = -1, inherits = FALSE)) {
  #           df_env <- where(df_name)
  #           df__   <- get(df_name, envir = df_env)
  #         }
  #       } else {
  #         df_name <- std_call$lhs
  #         df__    <- eval(std_call$lhs)
  #       }
  #       return(list(name = df_name, df = df__, 
  #                   call = std_call, method = "pipe"))
  #       
  #     }
  #     return(name = NA, df = NA, call = std_call, method = "pipe")
  #   }
  # 
  #   else if (length(pos_lapply) == 1) {
  #     std_call <- as.list(standardise_call(sys_calls[[pos_lapply]]))
  #     nn <- all.names(lapply_call$X)
  #     df_env <- where(nn)
  #     if (is.data.frame(get(nn, envir = df_env))) {
  #       df_name <- setdiff(nn, oper)[1]
  #       for (i in seq_along(sys_frames)) {
  #         if (df_name %in% ls(sys_frames[[i]])) {
  #           if (is.data.frame(sys_frames[[i]][[df_name]])) {
  #             df__ <- sys_frames[[i]][[df_name]]
  #             break
  #           }
  #         }
  #       }
  #       if (!exists("df__", where = -1, inherits = FALSE)) {
  #         df_env <- where(df_name)
  #         df__   <- get(df_name, envir = df_env)
  #       }
  #       return(list(name = df_name, df = df__, 
  #                   call = std_call, method = "lapply"))
  #     }
  #     return(list(name = NA, df = NA, call = std_call, method = "lapply"))
  #   }
  # 
  #   # function was called directly - find position in sys_calls
  #   else {
  #     nn <- all.names(sys_calls[[pos_fn]])
  #     nn <- setdiff(nn, c(caller, oper))[1]
  #     df_env <- where(nn)
  #     if (is.data.frame(get(nn, envir = df_env))) {
  #       df_name <- nn
  #       for (i in seq_along(sys_frames)) {
  #         if (df_name %in% ls(sys_frames[[i]])) {
  #           df__ <- sys_frames[[i]][[df_name]]
  #           break
  #         }
  #       }
  #       if (!exists("df__", where = -1, inherits = FALSE)) {
  #         df_env <- where(df_name)
  #         df__   <- get(df_name, envir = df_env)
  #       } 
  #       return(list(name = df_name, df = df__, method = "none"))
  #     }
  #     return(list(name = NA, df = NA, method = "none"))
  #   }
  # }
  # 
  # # find_var_name --------------------------------------------------------------
  # find_var_name <- function() {
  #   # Recalculate max.varnames in case it's too high
  #   
  #   if (df$method == "with") {
  #     nn <- as.character(df$call$expr)
  #     var_name <- setdiff(nn, c(oper, df$name, caller))[1]
  #     return(var_name)
  #   }
  # 
  #   else if (df$method == "by") {
  #     nn <- as.character(df$call$data)
  #     var_name <- setdiff(nn, c(oper, df$name))[1]
  #     return(var_name)
  #   }
  # 
  #   else if (df$method == "pipe") {
  #     nn <- all.names(df$call$lhs)
  #     var_name <- setdiff(nn, c(oper, df$name, caller))
  #     return(var_name)
  #   }
  #   
  #   else if (df$method == "lapply") {
  #     nn <- colnames(eval(df$call$X))
  #     # Find position in sys_frames that corresponds to the lapply envir.
  #     # to get "i"
  #     pos <- unlist(lapply(ls_sys_frames,
  #                          function(x) all.equal(x, c("FUN", "i", "X"))))
  #     pos <- which(pos == "TRUE")
  #     i <- sys_frames[[pos]]$i
  #     return(nn[i])
  #   }
  #   
  #   else if (df$method == "none") {
  #     nn <- all.names(sys_calls[[pos_fn]])
  #     if (!is.na(df$name)) {
  #       var_name <- setdiff(nn, c(caller, oper, df$name))[1:max.varnames]
  #       if (all(var_name %in% colnames(df$df))) {
  #         return(var_name)
  #       }
  #     } else {
  #       var_name <- setdiff(nn, c(caller, oper))[1:max.varnames]
  #       if (all(exists("var_name"))) {
  #         return(var_name)
  #       }
  #     }
  #     if (length(var_name) > 0 && !is.na(var_name)) {
  #       return(var_name)[1:max.varnames]
  #     } else {
  #       if (is.data.frame(sys_frames[[pos_fn]]$x)) {
  #         return(colnames(sys_frames[[pos_fn]]$x))
  #       } else {
  #         if (is.atomic(sys_frames[[pos_fn]]$x) && !is.na(df$name)) {
  #           # See if indexing of type [,X] was used
  #           # Declare regular expressions for matching "df[,9]" column indexing
  #           re <- 
  #             paste0(
  #               "^([\\w\\.\\_]+)\\s*",        # data frame name          (1)
  #               "\\[(.+)?",                   # row indexing             (2)
  #               "(\\,\\s)",                   # comma followed by space  (3)
  #               '(\\d+|\\".+\\"|\\\'.+\\\')', # column indexing          (4)
  #               "\\]$")                       # end of indexing  
  #           if (grepl(re, data_str, perl = TRUE)) {
  #             ind <- sub(re, "\\4", data_str, perl = TRUE)
  #             if (grepl("\\d+", ind)) {
  #               ind <- as.numeric(ind)
  #             }
  #             var_name <- colnames(df$df[ind])
  #             return(var_name)
  #           } else {
  #             re <-
  #               paste0("^([\\w\\.\\_]+)",                        # df name   (1)
  #                      '\\[{2}(\\d+|\\".+\\"|\\\'.+\\\')\\]{2}', # indexing  (2)
  #                      "(\\s*\\[\\s*(.*)\\s*\\])?")              # row ind.  (4)
  #             if (grepl(re, data_str, perl = TRUE)) {
  #               ind <- sub(re, "\\2", data_str, perl = TRUE)
  #               if (grepl("\\d+", ind)) {
  #                 ind <- as.numeric(ind)
  #               }
  #               var_name <- colnames(df$df[ind])
  #               return(var_name)
  #             } else {
  #               return(NA)
  #             }
  #           }
  #         }
  #       }
  #     }
  #   }
  #   return(data_str)
  # }
  #     
  # 
  # 
  # 
  # df <- get_df()
  # 
  # if ("df_label" %in% what && !identical(df$df, NA)) {
  #   df_label  <- label(df$df)
  # }
  # 
  # if ("var_name" %in% what) {
  #   var_name  <- find_var_name()
  #   if (!is.na(var_name) && !is.na(df$name)) {
  #     var_name <- sub(paste0("^", df$name, "\\$"), "", var_name)
  #   }
  # }
  # 
  # if ("var_label" %in% what && length(var_name) == 1) {
  #   if (!identical(df$df, NA)) {
  #     var_label <- label(df$df[[var_name]])
  #   } else {
  #     for (i in seq_along(sys_frames)) {
  #       if (var_name %in% ls(sys_frames[[i]])) {
  #         if (is.atomic(sys_frames[[i]][[var_name]])) {
  #           var_label <- label(sys_frames[[i]][[var_name]])
  #           break
  #         }
  #       }
  #     }
  #     if (length(var_label) == 0) {
  #       var_env   <- where(var_name)
  #       var_label <- label(get(var_name, envir = var_env))
  #     }
  #   }
  # }
  # 
  # output <- prep_return()
  # return(output)
  # }
  # 
# output$return <- function() {
#   # Remove dataframe name from items having form df_name$var_name 
#   # by_group string and the same
#   if (length(output$df_name) == 1 && !is.na(output$df_name)) {
#     for (item in c("var_name", "by_var", "by_group")) {
#       re <- paste0("^",df_name,      # starts with df_name
#                    "[$[]*['\"]?",    # subsetting chars
#                    "([\\w_.]+)",     # var_name (group 1)
#                    "['\"]?\\]{0,2}", # closing subsetting char(s)
#                    "(.*)$")          # remainder of expression (group 2)
#       output[[item]] <- sub(pattern = ,
#                             replacement = "\\1\\2", 
#                             x = output[[item]])
#     }
#   }
#   
#   empty_elements <- as.numeric(
#     which(vapply(output[-1:-3], function(x) {is.na(x) || !length(x)}, TRUE))
#   )
#   
#   if (length(empty_elements) > 0) {
#     output <<- output[-(empty_elements + 3)]
#   }
#   
#   ret(output[-1:-3])
#   #stop()
#   #eval(return(output[-1:-3]), envir = env.1)
# }
