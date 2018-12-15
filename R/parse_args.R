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
  var_labels  <- character()
  #rows_subset <- character()
  by_group <- character()
  by_first <- logical()
  by_last  <- logical()

  # Look for position of by(), tapply(), with() and lapply() in sys.calls()
  by_pos     <- which(as.character(lapply(sys_calls, head, 1)) == "by()")
  tapply_pos <- which(as.character(lapply(sys_calls, head, 1)) == "tapply()")
  with_pos   <- which(as.character(lapply(sys_calls, head, 1)) == "with()")
  lapply_pos <- which(as.character(lapply(sys_calls, head, 1)) == "lapply()")

  #lapply_first <- logical()
  #lapply_last  <- logical()


  # List of classes accepted as "data frames"
  # classes <- c("data.frame", "data.table", "tbl")

  # Function was called through with() -----------------------------------------
  # We should be able to extract:
  #  - df_name
  #  - var_names
  #  - rows_subset
  if (length(with_pos) == 1) {
    with_call <- as.list(standardise_call(sys_calls[[with_pos]]))
    with_objects <- ls(sys_frames[[with_pos + 3]])

    # Get names when with() is not combined with by()
    if (length(by_pos) == 0) {
      tmp_ <- eval(with_call$data)
      if (is.data.frame(tmp_)) {
        df_ <- tmp_
        df_name <- deparse(with_call$data)
        allnames <- all.vars(with_call$expr)
        if (length(allnames) > 0) {
          if (var == "x") {
            var_names <- intersect(allnames, with_objects)[1]
          } else {
            var_names <- intersect(allnames, with_objects)[2]
          }
        } else {
          var_names <- with_objects
        }
      } else if (is.list(tmp_)) {
        if (length(with_objects) == 1 &&
            is.data.frame(get(with_objects, 
                              envir = as.environment(eval(with_call$data))))) {
          df_ <- get(with_objects, envir = as.environment(eval(with_call$data)))
          df_name <- with_objects
        }
      }
    } else {
      # with is combined with by
      by_call <- as.list(standardise_call(sys_calls[[by_pos]]))
      if (is.data.frame(eval(with_call$data))) {
        df_name <- deparse(with_call$data)
        var_names <- deparse(by_call$data)
      } else if (is.list(eval(with_call$data))) {
        if (length(with_objects) == 1 &&
            is.data.frame(get(with_objects, 
                              envir = as.environment(eval(with_call$data))))) {
          df_name <- with_objects
          if (length(all.vars(by_call$data)) == 1) {
            var_names <- colnames(eval(with_call$data)[[df_name]])
          } else {
            var_names <- setdiff(all.vars(by_call$data), df_name)
          }
        }
      }
    }
  }

  # by() is involved -----------------------------------------------------------
  # We use the .st_env environment to store the group-info at each iteration
  if (length(by_pos) == 1) {

    by_call <- as.list(standardise_call(sys_calls[[by_pos]]))

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
  }
  # End of "by" without handling -----------------------------------------------

  # No by() but rather lapply() ------------------------------------------------
  else if (length(lapply_pos) == 1) {
    lapply_call <- as.list(standardise_call(sys_calls[[lapply_pos]]))
    df_name <- setdiff(all.names(lapply_call$X), c("[", "[[", "$"))[1]
    df_ <- get(df_name, envir = sys_frames[[lapply_pos]])
    df_label <- label(df_)
    for (i_ in seq_along(sys_frames)) {
      if (identical(names(sys_frames[[i_]])[1:3], c("i", "X", "FUN"))) {
        var_names  <- names(df_[sys_frames[[i_]]$i])
        var_labels <- label(df_[sys_frames[[i_]]$i])
      }
      break
    }
  }
  
  # End of special (with/by/lapply) calls handling------------------------------
  
  # From here code applies no matter what if df_name and/or var_names are
  # not found yet --------------------------------------------------------------
  
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
            df_pos    <- no.frame
            df_label  <- label(df_)

            if (max.varnames > 0) {
              col_names <- colnames(df_)
              if (length(intersect(allnames, col_names)) > 0) {
                var_names <- intersect(allnames, col_names)
                var_labels <- label(df_[var_names])
              }
              cont <- FALSE
              break
            }
          }
        }
      }
    }
  }
  
  # Found df_name but not var_names --------------------------------------------
  if (max.varnames > 0 && length(setdiff(df_name, allnames)) > 0) {
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
            var_names <- colnames(df_[col_num])
            val_label <- label(df_[col_num])
          }
          
        } else if (grepl("\\d+:\\d+", col_indexing)) {
          col_nums <- do.call(":", as.list(unlist(strsplit("1:3",":"))))
          if (all(col_nums <= ncol(df_))) {
            var_names <- names(df_[col_nums])
            var_labels <- label(df_[col_nums])
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
            var_labels <- label(df_[col_num])
          }
        }
      } 
    }
    
    if (length(df_name) ==1 && length(var_names) == 0) {
      # At this stage if we haven't succeeded in identifying the variable(s),
      # it's very likely that the index is an iterator coming from lapply() 
      # or another looping function
      potential.iterator <- setdiff(allnames, df_name)
      cont <- TRUE
      for (no.frame in seq_along(sys_frames)) {
        if (!cont) 
          break
        for (iter in possible.iterator) {
          if (exists(iter, envir = sys_frames[[no.frame]], mode = "numeric")) {
            it <- get(iter, envir = sys_frames[[no.frame]])
            var_names <- head(names(df_[it]), max.varnames)
            var_labels <- label(df_[it])[1]
            cont <- FALSE
            break
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
              var_labels <- label(get(obj.name, envir = sys_frames[[no.frame]]))
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
              var_labels <- label(get(obj.name, envir = sys_frames[[no.frame]]))
              cont <- FALSE
              break
            }
          }
        }
      }
    }
  }
  # All methods failed, so we try to distribute the names from allnames
  # in a logical manner
  if (length(var_names) == 0 && length(df_name) == 0) {
    if (length(allnames) == 1) {
      if (grepl("(dfSummary|descr)", deparse(match_call))) {
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
  
  if (length(var_labels) > 1) {
    length(var_labels) <- 1
  }
  
  # Remove dataframe name from by_group if df_name is present in the 
  # by_group string and the same
  if (length(by_group) == 1 && length(df_name) > 0) {
    by_group <- sub(pattern = paste0(df_name,"$"), replacement = "", 
                    x = by_group, fixed = TRUE)
  }
  
  output <- list(df_name    = df_name,
                 df_label   = df_label,
                 var_names  = var_names,
                 var_labels = var_labels,
                 by_group   = by_group,
                 by_first   = by_first,
                 by_last    = by_last)
  
  output <- output[which(mapply(length, output, SIMPLIFY = TRUE) > 0)]
  
  return(output[!is.na(output)])
}
