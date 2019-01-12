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
#' @param var One of \dQuote{x} (default) or \dQuote{y} (the latter being used
#'   only in \code{\link{ctable}}).
#' @param  max.varnames Numeric. Allows limiting the number of expected variable
#'   names. Defaults to \code{1}
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
                       var = "x", max.varnames = 1, silent = FALSE, 
                       caller = NA, what = c("df_name", "df_label", 
                                             "var_name", "var_label")) {

  # Look for position of by() + tapply(), with() lapply() and %>% in sys.calls()
  funs_stack    <- lapply(sys_calls, head, 1)
  pos_by        <- which(funs_stack == "by()")
  pos_tapply    <- which(funs_stack == "tapply()")
  pos_lapply    <- which(funs_stack == "lapply()")
  pos_with      <- which(funs_stack == "with()")
  pos_pipe      <- which(funs_stack == "`%>%`()")
  
  ls_sys_frames <- lapply(sys_frames, ls)

  # Extract the string describing the data passed to the caller function
  
  if (length(pos_by) == 0) {
    pos_fn   <- which(grepl(paste0("^", caller), as.character(sys_calls)))
    std_call <- as.list(standardise_call(sys_calls[[pos_fn]]))
    data_str <- deparse(std_call$x)
    #data_str <- setdiff(as.character(sys_calls[[pos_fn]]), caller)
  } else {
    pos_fn   <- which(grepl("^by\\(", as.character(sys_calls)))
    data_str <- deparse(sys_calls[[pos_fn]]$data)
  }
  
  # initialise output elements
  df_label  <- character()
  var_name  <- character()
  var_label <- character()
  by_var    <- character()
  by_group  <- character()
  by_first  <- logical()
  by_last   <- logical()
  
  # operators to remove from vectors of all.names()
  oper <- c("$", "[", "[[", "<", ">", "<=", ">=", "==", ":", "%>%")
  # Declare functions
  # Find df_name ---------------------------------------------------------------
  get_df <- function() {

    # We get the name and create 2 instances of df__ ; depending on where df
    # was created, and how the function is called the labels 
    if (length(pos_with) == 1) {
      std_call <- as.list(standardise_call(sys_calls[[pos_with]]))
      if (is.data.frame(eval(std_call$data))) {
        df_name <- deparse(std_call$data)
        for (i in seq_along(sys_frames)) {
          if (df_name %in% ls(sys_frames[[i]])) {
            if (is.data.frame(sys_frames[[i]][[df_name]])) {
              df__ <- sys_frames[[i]][[df_name]]
              break
            }
          }
        }
        if (!exists("df__", where = -1, inherits = FALSE)) {
          df_env <- where(df_name)
          df__   <- get(df_name, envir = df_env)
        }
        return(list(name = df_name, df = df__, 
                    call = std_call, method = "with"))
      }
      return(list(name = NA, df = NA, call = std_call, method = "with"))
    }
    
    else if (length(pos_by) == 1) {
      std_call <- as.list(standardise_call(sys_calls[[pos_by]]))
      nn <- all.names(by_call$data)
      if (length(nn) > 1) {
        nn <- setdiff(nn, oper)[1]
      }
      df_env <- where(nn)
      if (is.data.frame(get(nn, envir = df_env))) {
        df_name <- nn
        for (i in seq_along(sys_frames)) {
          if (df_name %in% ls(sys_frames[[i]])) {
            if (is.data.frame(sys_frames[[i]][[df_name]])) {
              df__ <- sys_frames[[i]][[df_name]]
              break
            }
          }
        }
        if (!exists("df__", where = -1, inherits = FALSE)) {
          df_env <- where(df_name)
          df__   <- get(df_name, envir = df_env)
        }
        return(list(name = df_name, df = df__, 
                    call = std_call, method = "by"))
      }
      return(list(name = NA, df = NA, call = std_call, method = "by"))
    }
    #df__  <- get(nn, envir = df_env)
    #df__2   <- sys_frames[[pos_by]]$data
    
    else if (length(pos_pipe) == 1) {
      std_call <- as.list(standardise_call(sys_calls[[pos_pipe]]))
      if (is.data.frame(eval(std_call$lhs))) {
        nn <- as.character(std_call$lhs)
        if (length(nn) > 1) {
          nn <- setdiff(nn, oper)[1]
        }
        # Find the envir containing the potential df
        df_env <- where(nn)
        if (is.data.frame(get(nn, envir = df_env))) {
          df_name <- nn
          for (i in seq_along(sys_frames)) {
            if (df_name %in% ls(sys_frames[[i]])) {
              if (is.data.frame(sys_frames[[i]][[df_name]])) {
                df__ <- sys_frames[[i]][[df_name]]
                break
              }
            }
          }
          if (!exists("df__", where = -1, inherits = FALSE)) {
            df_env <- where(df_name)
            df__   <- get(df_name, envir = df_env)
          }
        } else {
          df_name <- std_call$lhs
          df__    <- eval(std_call$lhs)
        }
        return(list(name = df_name, df = df__, 
                    call = std_call, method = "pipe"))
        
      }
      return(name = NA, df = NA, call = std_call, method = "pipe")
    }

    else if (length(pos_lapply) == 1) {
      std_call <- as.list(standardise_call(sys_calls[[pos_lapply]]))
      nn <- all.names(lapply_call$X)
      df_env <- where(nn)
      if (is.data.frame(get(nn, envir = df_env))) {
        df_name <- setdiff(nn, oper)[1]
        for (i in seq_along(sys_frames)) {
          if (df_name %in% ls(sys_frames[[i]])) {
            if (is.data.frame(sys_frames[[i]][[df_name]])) {
              df__ <- sys_frames[[i]][[df_name]]
              break
            }
          }
        }
        if (!exists("df__", where = -1, inherits = FALSE)) {
          df_env <- where(df_name)
          df__   <- get(df_name, envir = df_env)
        }
        return(list(name = df_name, df = df__, 
                    call = std_call, method = "lapply"))
      }
      return(list(name = NA, df = NA, call = std_call, method = "lapply"))
    }
  
    # function was called directly - find position in sys_calls
    else {
      nn <- all.names(sys_calls[[pos_fn]])
      nn <- setdiff(nn, c(caller, oper))[1]
      df_env <- where(nn)
      if (is.data.frame(get(nn, envir = df_env))) {
        df_name <- nn
        for (i in seq_along(sys_frames)) {
          if (df_name %in% ls(sys_frames[[i]])) {
            df__ <- sys_frames[[i]][[df_name]]
            break
          }
        }
        if (!exists("df__", where = -1, inherits = FALSE)) {
          df_env <- where(df_name)
          df__   <- get(df_name, envir = df_env)
        } 
        return(list(name = df_name, df = df__, method = "none"))
      }
      return(list(name = NA, df = NA, method = "none"))
    }
  }

  # find_var_name --------------------------------------------------------------
  find_var_name <- function() {
    # Recalculate max.varnames in case it's too high
    
    if (df$method == "with") {
      nn <- as.character(df$call$expr)
      var_name <- setdiff(nn, c(oper, df$name, caller))[1]
      return(var_name)
    }

    else if (df$method == "by") {
      nn <- as.character(df$call$data)
      var_name <- setdiff(nn, c(oper, df$name))[1]
      return(var_name)
    }

    else if (df$method == "pipe") {
      nn <- all.names(df$call$lhs)
      var_name <- setdiff(nn, c(oper, df$name, caller))
      return(var_name)
    }
    
    else if (df$method == "lapply") {
      nn <- colnames(eval(df$call$X))
      # Find position in sys_frames that corresponds to the lapply envir.
      # to get "i"
      pos <- unlist(lapply(ls_sys_frames,
                           function(x) all.equal(x, c("FUN", "i", "X"))))
      pos <- which(pos == "TRUE")
      i <- sys_frames[[pos]]$i
      return(nn[i])
    }
    
    else if (df$method == "none") {
      nn <- all.names(sys_calls[[pos_fn]])
      if (!is.na(df$name)) {
        var_name <- setdiff(nn, c(caller, oper, df$name))[1:max.varnames]
        if (all(var_name %in% colnames(df$df))) {
          return(var_name)
        }
      } else {
        var_name <- setdiff(nn, c(caller, oper))[1:max.varnames]
        if (all(exists("var_name"))) {
          return(var_name)
        }
      }
      if (length(var_name) > 0 && !is.na(var_name)) {
        return(var_name)[1:max.varnames]
      } else {
        if (is.data.frame(sys_frames[[pos_fn]]$x)) {
          return(colnames(sys_frames[[pos_fn]]$x))
        } else {
          if (is.atomic(sys_frames[[pos_fn]]$x) && !is.na(df$name)) {
            # See if indexing of type [,X] was used
            # Declare regular expressions for matching "df[,9]" column indexing
            re <- 
              paste0(
                "^([\\w\\.\\_]+)\\s*",        # data frame name          (1)
                "\\[(.+)?",                   # row indexing             (2)
                "(\\,\\s)",                   # comma followed by space  (3)
                '(\\d+|\\".+\\"|\\\'.+\\\')', # column indexing          (4)
                "\\]$")                       # end of indexing  
            if (grepl(re, data_str, perl = TRUE)) {
              ind <- sub(re, "\\4", data_str, perl = TRUE)
              if (grepl("\\d+", ind)) {
                ind <- as.numeric(ind)
              }
              var_name <- colnames(df$df[ind])
              return(var_name)
            } else {
              re <-
                paste0("^([\\w\\.\\_]+)",                        # df name   (1)
                       '\\[{2}(\\d+|\\".+\\"|\\\'.+\\\')\\]{2}', # indexing  (2)
                       "(\\s*\\[\\s*(.*)\\s*\\])?")              # row ind.  (4)
              if (grepl(re, data_str, perl = TRUE)) {
                ind <- sub(re, "\\2", data_str, perl = TRUE)
                if (grepl("\\d+", ind)) {
                  ind <- as.numeric(ind)
                }
                var_name <- colnames(df$df[ind])
                return(var_name)
              } else {
                return(NA)
              }
            }
          }
        }
      }
    }
    return(data_str)
  }
      
  # prep_return ----------------------------------------------------------------
  prep_return <- function() {
    if (length(var_name) > max.varnames) {
      length(var_name) <- max.varnames
    }
    
    # Remove dataframe name from items having form df_name$var_name 
    # by_group string and the same
    if (length(by_group) > 0 && !is.na(df$name)) {
      by_group <- sub(pattern = paste0(df$name,"$"), replacement = "", 
                      x = by_group, fixed = TRUE)
    }
    
    output <- list(df_name    = df$name,
                   df_label   = df_label,
                   var_name   = var_name,
                   var_label  = var_label,
                   by_var     = by_var,
                   by_group   = by_group,
                   by_first   = by_first,
                   by_last    = by_last)
    
    output <- output[which(mapply(length, output, SIMPLIFY = TRUE) > 0)]
    
    return(output[!is.na(output)])
  }
  
  
  # by() is in the call stack --------------------------------------------------
  # We use the .st_env environment to store the group-info at each iteration
  # and find df_name and var_names, then return
  if (length(pos_by) == 1) {

    by_call <- as.list(standardise_call(sys_calls[[pos_by]]))
    by_var  <- deparse(by_call$INDICES)
    
    # On first iteration, generate levels based on IND variables, and store
    if (length(.st_env$byInfo) == 0) {
      if (is.null(names(sys_frames[[pos_tapply]]$namelist)) ||
          is.na(names(sys_frames[[pos_tapply]]$namelist)[1])) {
        names(sys_frames[[pos_tapply]]$namelist) <- 
          as.character(by_call$INDICES)[-1]
      }
      by_levels <- sys_frames[[pos_tapply]]$namelist
      .st_env$byInfo$by_levels <- 
        expand.grid(by_levels, stringsAsFactors = FALSE)
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
    
    # by() was called on ctable (both "x" and "y" and in by_call$data)
    if (length(by_call$data) > 1 && deparse(by_call$data[[1]]) == "list" && 
        identical(names(by_call$data), c("", "x", "y"))) {
      df <- list()
      var_name <- c(deparse(by_call$data$x), deparse(by_call$data$y))
      if (any(grepl("$", var_name, fixed = TRUE))) {
        df_name    <- sub("^([\\w._]+)\\$.+$", "\\1", var_name[1])
        df_name[2] <- sub("^([\\w._]+)\\$.+$", "\\1", var_name[2])
      }
      if (isTRUE(df_name[1] == df_name[2])) {
        df$name <- df_name[1]
        var_name[1] <- sub(paste0(df_name, "$"), "", var_name[1])
        var_name[2] <- sub(paste0(df_name, "$"), "", var_name[2])
      } else {
        if (length(pos_with) == 1) {
          with_call <- as.list(standardise_call(sys_calls[[pos_with]]))
          if (length(with_call$data) == 1) {
            df$name <- as.character(with_call$data)
          }
        }
      }
      output <- prep_return()
      return(output)
    }
  }

  df <- get_df()
  
  if ("df_label" %in% what && !identical(df$df, NA)) {
    df_label  <- label(df$df)
  }
  
  if ("var_name" %in% what) {
    var_name  <- find_var_name()
    if (!is.na(var_name) && !is.na(df$name)) {
      var_name <- sub(paste0("^", df$name, "\\$"), "", var_name)
    }
  }
  
  if ("var_label" %in% what && length(var_name) == 1) {
    if (!identical(df$df, NA)) {
      var_label <- label(df$df[[var_name]])
    } else {
      for (i in seq_along(sys_frames)) {
        if (var_name %in% ls(sys_frames[[i]])) {
          if (is.atomic(sys_frames[[i]][[var_name]])) {
            var_label <- label(sys_frames[[i]][[var_name]])
            break
          }
        }
      }
      if (length(var_label) == 0) {
        var_env   <- where(var_name)
        var_label <- label(get(var_name, envir = var_env))
      }
    }
  }
  
  output <- prep_return()
  return(output)
}