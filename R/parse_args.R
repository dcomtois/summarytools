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
#' @param var One of \dQuote{x} or \dQuote{y} (the latter being used only
#'   in \code{\link{ctable}}).
#' @param  max.varnames Numeric. Allows limiting the number of expected variable
#'   names. Defaults to \code{Inf}
#' @param silent Logical. Hide console messages. \code{FALSE} by default.
#' 
#' @return A list consisting of:
#' \itemize{
#'   \item df_name The data frame name when applicable.
#'   \item var_names The variable names when applicable.
#'   \item by_group The group, when functions are called through
#'     \code{by()}
#'   \item by_first A binary indicator used when function was called
#'     through \code{by()}
#'   \item by_last A binary indicator used when function was called
#'     through \code{by()}}
#'
#' @keywords internal misc
#'
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com>}
#'
#' @importFrom pryr standardise_call
#' @importFrom utils head
parse_args <- function(sys_calls, sys_frames, match_call, 
                       var = "x", max.varnames = Inf, silent = FALSE) {

  df_name     <- character()
  df_label    <- character()
  var_names   <- character()
  var_label   <- character()
  data_str    <- character()
  by_var      <- character()
  by_group    <- character()
  by_first    <- logical()
  by_last     <- logical()

  # Look for position of by(), tapply(), with() and lapply() in sys.calls()
  by_pos     <- which(as.character(lapply(sys_calls, head, 1)) == "by()")
  tapply_pos <- which(as.character(lapply(sys_calls, head, 1)) == "tapply()")
  lapply_pos <- which(as.character(lapply(sys_calls, head, 1)) == "lapply()")

  # List of classes accepted as "data frames"
  # classes <- c("data.frame", "data.table", "tbl")

  # by() was invoked -----------------------------------------------------------
  # We use the .st_env environment to store the group-info at each iteration
  if (length(by_pos) == 1) {

    by_call <- as.list(standardise_call(sys_calls[[by_pos]]))

    by_var <- deparse(by_call$INDICES)
    
    # On first iteration, generate levels based on IND variables, and store
    if (length(.st_env$byInfo) == 0) {
      if (is.null(names(sys_frames[[tapply_pos]]$namelist)) ||
          is.na(names(sys_frames[[tapply_pos]]$namelist)[1])) {
        names(sys_frames[[tapply_pos]]$namelist) <- 
          as.character(by_call$INDICES)[-1]
      }
      by_levels <- sys_frames[[tapply_pos]]$namelist
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
    
    if (length(by_call$data) > 1 && deparse(by_call$data[[1]]) == "list" && 
        identical(names(by_call$data), c("", "x", "y"))) {
      var_names <- c(deparse(by_call$data$x), deparse(by_call$data$y))
      df_name    <- sub("^([a-zA-Z0-9._]+)\\$.+$", "\\1", var_names[1])
      df_name[2] <- sub("^([a-zA-Z0-9._]+)\\$.+$", "\\1", var_names[2])
      if (df_name[1] == df_name[2]) {
        df_name <- df_name[1]
        var_names[1] <- sub(paste0(df_name, "$"), "", var_names[1])
        var_names[2] <- sub(paste0(df_name, "$"), "", var_names[2])
      } else {
        df_name <- NA
      }
    }
  }
  # End of "by"

  # No by() but rather lapply() ------------------------------------------------
  else if (length(lapply_pos) == 1) {
    lapply_call <- as.list(standardise_call(sys_calls[[lapply_pos]]))
    df_name <- setdiff(all.names(lapply_call$X), c("[", "[[", "$"))[1]
    #df_ <- eval(expr = lapply_call$X, envir = sys_frames[[lapply_pos]])
    df_ <- eval(expr = sys_frames[[lapply_pos]]$X)
    df_label <- label(df_)
    for (i_ in seq_along(sys_frames)) {
      if (identical(names(sys_frames[[i_]])[1:3], c("i", "X", "FUN"))) {
        var_names  <- names(df_[sys_frames[[i_]]$i])
        var_label <- label(df_[[sys_frames[[i_]]$i]])
        break
      }
    }
  }
  # End of special (by/lapply) calls
  
  
  # From here code applies if df_name was not found yet ------------------------
  # Look for the data frame name (df_name)
  if (length(df_name) == 0) {
    if (exists("by_call")) {
      data_str <- deparse(by_call$data)
      allnames <- all.vars(by_call$data)
    } else {
      data_str <- deparse(match_call[[var]])
      allnames <- all.vars(match_call[[var]])
    }
    
    # Loop over sys_frames looking for a dataframe whose name is in allnames
    cont <- TRUE
    for (no.frame in seq_along(sys_frames)) {
      if (!cont) 
        break
      for (obj.name in allnames) {
        if (exists(obj.name, envir = sys_frames[[no.frame]], mode = "list")) {
          df_ <- get(obj.name, envir = sys_frames[[no.frame]])
          if (is.data.frame(df_)) {
            # df found - look for label and column names / labels
            df_name   <- obj.name
            df_label  <- label(df_)

            if (max.varnames > 0 && 
                (length(var_names) == 0 || length(var_label) == 0)) {
              col_names <- colnames(df_)
              if (length(intersect(allnames, col_names)) > 0) {
                # Check that the varname is not used in an expression, 
                # sur as is "age > 50"
                if (exists("by_call")) {
                  check_expr <- grep(intersect(allnames, col_names)[1], 
                                     by_call, value = TRUE)
                } else {
                  check_expr <- grep(intersect(allnames, col_names)[1], match_call, 
                                     value = TRUE)
                }
                if (!grepl("[!><=()]", check_expr)) {
                  var_names <- intersect(allnames, col_names)[1]
                  var_label <- label(df_[[var_names]])
                } else {
                  var_names <- sub("^(.+?)\\[.+$", "\\1", check_expr)
                  var_names <- sub(paste0("^", df_name, "\\$"), "", var_names)
                  if (var_names %in% col_names) {
                    var_label <- label(df_[[var_names]])
                  } else if (!exists("dnn", envir = parent.frame()) ||
                                     is.list(parent.frame()$dnn)) {
                    message("Variable name '", var_names, "' was guessed but ",
                            "could not be confirmed")
                  }
                }
              }
            }
            cont <- FALSE
            break
          }
        }
      }
    }
  }
  
  # Found df_name but not var_names --------------------------------------------
  if (max.varnames > 0 && length(data_str) > 0) {
    #  && length(setdiff(df_name, allnames)) > 0) {
    if (length(df_name) == 1 && length(var_names) == 0) {
      # Declare regular expressions for matching with indexing
      re_singlBrackets <-
        paste0(
          "^([\\w\\.\\_]+)\\s*", # data frame name (most commonly)         (1)
          "\\[(.+)?",            # rows indexing                           (2)
          "\\s*(\\,)\\s*",       # comma surrounded (or not) by spaces     (3)
          "(.*?)",               # column indexing                         (4)
          "\\s*\\]$")            # end of indexing
      
      
      # dfname[[col]][rows]
      re_dblBrackets <-
        paste0("([\\w\\.\\_]+)",                 # data frame               (1)
               "\\s*\\[\\[\\s*(.*)\\s*\\]\\]",   # variable number or name  (2)
               "(\\s*\\[\\s*(.*)\\s*\\])?")      # rows indexing (opt)      (4)
      
      
      if (grepl(re_singlBrackets, data_str, perl = TRUE)) {
        # Isolate column indexing
        col_indexing <- 
          trimws(sub(re_singlBrackets, "\\4", 
                     x = data_str, perl = TRUE))
        
        # Try to extract var_names based on numeric indexing (if indexing
        # is not numeric, normally we would have captures colnames by
        # now, unless it's an iterator. We'll deal with that possibility
        # further below.
        
        if (grepl("\\d+", col_indexing)) {
          col_num <- as.numeric(col_indexing)
          if (col_num <= ncol(df_)) {
            var_names  <- colnames(df_[col_num])
            var_label <- label(df_[[col_num]])
          }
          
        } else if (grepl("\\d+:\\d+", col_indexing)) {
          col_nums <- do.call(":", as.list(unlist(strsplit("1:3",":"))))
          if (all(col_nums <= ncol(df_))) {
            var_names <- names(df_[col_nums])
            var_label <- label(df_[[col_nums]])
          }
        }
      } else if (grepl(re_dblBrackets, data_str, perl = TRUE)) {
        # single brackets didn't succeed - try double brackets
        col_indexing <- trimws(sub(re_dblBrackets, "\\2",
                                   x = data_str,  perl = TRUE))
        if (grepl("\\d+", col_indexing)) {
          col_num <- as.numeric(col_indexing)
          if (col_num <= ncol(df_)) {
            var_names <- names(df_[col_num])
            var_label <- label(df_[[col_num]])
          }
        }
      }
    }
    
    if (length(df_name) ==1 && length(var_names) == 0) {
      # At this stage if we haven't succeeded in identifying the variable(s),
      # it's very likely that the index is an iterator coming from lapply() 
      # or another looping function
      potential.iterator <- setdiff(allnames, df_name)
      if (length(potential.iterator) > 0) {
        cont <- TRUE
        for (no.frame in seq_along(sys_frames)) {
          if (!cont) 
            break
          for (iter in potential.iterator) {
            if (exists(iter, envir = sys_frames[[no.frame]], mode = "numeric")) {
              it <- get(iter, envir = sys_frames[[no.frame]])
              var_names <- head(names(df_[it]), max.varnames)
              var_label <- label(df_[[it]])[1]
              cont <- FALSE
              break
            }
          }
        }
      }
    }
  }  
  
  # Look for a "stand-alone" variable
  # look for numeric if function is descr()
  if (length(df_name) == 0 && length(var_names) == 0) {
    cont <- TRUE
    if (grepl("descr", deparse(match_call))) {
      for (no.frame in seq_along(sys_frames)) {
        if (!cont) 
          break
        for (obj.name in allnames) {
          if (exists(obj.name, envir = sys_frames[[no.frame]], 
                     mode = "numeric")) {
            var_names <- obj.name
            var_label <- label(get(obj.name, envir = sys_frames[[no.frame]]))
            cont <- FALSE
            break
          }
        }
      }
    } else {
      # Look for numeric or character if function is freq or ctable
      for (no.frame in seq_along(sys_frames)) {
        if (!cont) 
          break
        for (obj.name in allnames) {
          if (exists(obj.name, envir = sys_frames[[no.frame]], 
                     mode = "numeric") ||
              exists(obj.name, envir = sys_frames[[no.frame]],
                     mode = "character")) {
            var_names <- obj.name
            var_label <- label(get(obj.name, envir = sys_frames[[no.frame]]))
            cont <- FALSE
            break
          }
        }
      }
    }
  }
  
  # All methods failed, so we try to distribute the names from allnames
  # in a logical manner
  if (length(var_names) == 0 && length(df_name) == 0) {
    if (length(allnames) == 1) {
      if (grepl("(dfSummary)", deparse(match_call))) {
        df_name <- allnames    
      } else if (grepl("(freq|ctable)", deparse(match_call))) {
        var_names <- allnames
      }
    } else if (length(allnames) == 2) {
      if (grepl("freq|descr|ctable")) {
        df_name <- allnames[1]
        var_names <- allnames[2]
      }
    }
  }
  
  if (length(var_names) > max.varnames) {
    length(var_names) <- max.varnames
  }
  
  if (length(var_label) > 1) {
    length(var_label) <- 1
  }
  
  # Remove dataframe name from by_group if df_name is present in the 
  # by_group string and the same
  if (length(by_group) == 1 && length(df_name) > 0) {
    by_group <- sub(pattern = paste0(df_name,"$"), replacement = "", 
                    x = by_group, fixed = TRUE)
    var_names <- sub(paste0("^", df_name, "\\$"), "", var_names)
  }
  
  output <- list(df_name    = df_name,
                 df_label   = df_label,
                 var_names  = var_names,
                 var_label  = var_label,
                 by_var     = by_var,
                 by_group   = by_group,
                 by_first   = by_first,
                 by_last    = by_last)
  
  output <- output[which(mapply(length, output, SIMPLIFY = TRUE) > 0)]
  
  return(output[!is.na(output)])
}
