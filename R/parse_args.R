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

  found     <- character()
  df_name   <- character()
  df_label  <- character()
  df_call   <- expression()
  var_name  <- character()
  var_label <- character()
  by_var    <- character()
  by_group  <- character()
  by_first  <- logical()
  by_last   <- logical()

  # Declare functions   
  # Find df_name ---------------------------------------------------------------
  find_df_name <- function() {

    if (length(pos_with) == 1) {
      assign("with_call", envir = parent.frame(), 
             value = as.list(standardise_call(sys_calls[[pos_with]])))
      if (length(with_call$data) == 1) {
        if (is.data.frame(eval(with_call$data))) {
          assign("found", envir = parent.frame(),
                 value = append(found, "df_name"))
          assign("df_call", envir = parent.frame(), value = with_call$data)
          assign("method", envir = parent.frame(), value = "with")
          return(as.character(with_call$data))
        }
      }
    }
    
    if (length(pos_by) == 1) {
      assign("by_call", envir = parent.frame(), 
             value = as.list(standardise_call(sys_calls[[pos_by]])))
      if (length(by_call$data) == 1) {
        if (is.data.frame(eval(by_call$data))) {
          assign("found", envir = parent.frame(), 
                 value = append(found, df_name))
          assign("df_call", envir = parent.frame(),
                 value = by_call$data)
          assign("method", envir = parent.frame(), value = "by")
          return(as.character(by_call$data))
        }
      } else {
        nn <- all.names(by_call$data)
        df_name <- setdiff(nn, c("$", "[", "[["))[1]
        if (length(df_name) == 1) {
          # find in what envir is df
          df_env <- where(df_name)
          assign("found", envir = parent.frame(),
                 value = append(found, df_name))
          assign("df_env", envir = parent.frame(),
                 value = df_env)
          assign("df_call", envir = parent.frame(),
                 value = expression(get(df_name, envir = df_env)))
          assign("method", envir = parent.frame(), value = "by")
          return(df_name)
        }
      }
    }
    
    if (length(pos_pipe) == 1) {
      assign("pipe_call", envir = parent.frame(),
             value = as.list(standardise_call(sys_calls[[pos_pipe]])))
      if (is.data.frame(eval(pipe_call$lhs))) {
        df_name <- as.character(pipe_call$lhs)
        if (length(df_name) > 1) {
          pos <- which(!df_name %in% c("%>%", "[", "[[", "$"))[1]
          df_name <- df_name[pos]
        }
        # find in what envir is df
        df_env <- where(df_name)
        assign("found", envir = parent.frame(),
               value = append(found, df_name))
        assign("df_env", envir = parent.frame(),
               value = df_env)
        assign("df_call", envir = parent.frame(),
               value = expression(get(df_name, envir = df_env)))
        assign("method", envir = parent.frame(), value = "pipe")
        return(df_name)
      } else {
        nn <- all.names(sys_calls[[pos_pipe]])
        df_name <- setdiff(nn, c("%>%", "$", "[", "[["))[1]
        # Find the envir containing the df
        df_env <- where(df_name)
        if (is.data.frame(df_env[[df_name]])) {
          assign("found", envir = parent.frame(),
                 value = append(found, "df_name"))
          assign("df_env", envir = parent.frame(),
                 value = df_env)
          assign("df_call", envir = parent.frame(),
                 value = expression(get(df_name, envir = df_env)))
          assign("method", envir = parent.frame(), value = "pipe")
          return(df_name)
        }
      }
    }

    if (length(pos_lapply) == 1) {
      assign("lapply_call", envir = parent.frame(),
             value = as.list(standardise_call(sys_calls[[pos_lapply]])))
      df_name <- setdiff(all.names(lapply_call$X), c("[", "[[", "$"))[1]
      assign("df_call", envir = parent.frame(),
             value = lapply_call$X)
      assign("found", envir = parent.frame(),
             value = append(found, "df_name"))
      assign("method", envir = parent.frame(), value = "lapply")
      return(df_name)
    }

    # function was called directly - find position in sys_calls
    pos_fn <- which(grepl(caller, as.character(sys_calls)))
    nn <- all.names(sys_calls[[pos_fn]])
    df_name <- setdiff(nn, c("$", "[", "[[", ":", caller))[1]
    df_env <- where(df_name)
    if (is.data.frame(df_env[[df_name]])) {
      assign("found", envir = parent.frame(),
             value = append(found, "df_name"))
      assign("df_env", envir = parent.frame(),
             value = df_env)
      assign("df_call", envir = parent.frame(),
             value = expression(get(df_name, envir = df_env)))
      assign("method", envir = parent.frame(), value = "none")
      return(df_name)
    }
    
    # No df_name found
    assign("method", envir = parent.frame(), value = "none")
    return(NA)
  }

  # find_df_label --------------------------------------------------------------
  find_df_label <- function() {
    if (length(df_call) > 0) {
      return(label(eval(df_call)))
    } else {
      return(NA)
    }
  }
  
  # find_var_name --------------------------------------------------------------
  find_var_name <- function() {
    if (method == "with") {
      nn <- as.character(with_call$data)
      var_name <- setdiff(nn, c("%>%", "$", "[", "[[", ":", df_name, caller))
      return(var_name)
    }
      
    if (method == "by") {
      return(colnames(eval(by_call$data)))
    }
    
    if (method == "pipe") {
      nn <- all.names(sys_calls[[pos_pipe]])
      var_name <- setdiff(nn, c("%>%", "$", "[", "[[", ":", df_name, caller))
      return(var_name)
    }
    
    if (method == "lapply") {
      nn <- colnames(eval(lapply_call$X))
      # Find position in sys_frames that corresponds to the lapply envir.
      # to get "i"
      pos <- unlist(lapply(ls_sys_frames,
                           function(x) all.equal(x, c("FUN", "i", "X"))))
      pos <- which(pos == "TRUE")
      i <- sys_frames[[pos]]$i
      return(nn[i])
    }
    
    if (method == "none") {
      pos_fn <- which(grepl(caller, as.character(sys_calls)))
      nn <- as.character(sys_calls[[pos_fn]])
      if (!is.na(df_name)) {
        var_name <- setdiff(nn, c(caller, df_name))
      } else {
        var_name <- setdiff(nn, caller)
      }
      if (length(var_name) > 0) {
        return(var_name)
      } else {
        return(NA)
      }
    }
    
    return(NA)
  }
  
  # find_var_label -------------------------------------------------------------
  find_var_label <- function() {
    if(!is.na(df_name) && !is.na(var_name) && length(var_name) == 1) {
      return(label(eval(df_call)[[var_name]]))
    } else if (is.na(df_name) && length(var_name) == 1) {
      return(label(get(var_name, envir = where(var_name))))
    } else {
      return(NA)
    }
  }
  
  # prep_return ----------------------------------------------------------------
  prep_return <- function() {
    if (length(var_name) > max.varnames) {
      length(var_name) <- max.varnames
    }
    
    # Remove dataframe name from items having form df_name$var_name 
    # by_group string and the same
    if (length(by_group) > 0 && !is.na(df_name)) {
      by_group <- sub(pattern = paste0(df_name,"$"), replacement = "", 
                      x = by_group, fixed = TRUE)
    }
    
    output <- list(df_name    = df_name,
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
      var_name <- c(deparse(by_call$data$x), deparse(by_call$data$y))
      if (any(grepl("$", var_name, fixed = TRUE))) {
        df_name    <- sub("^([a-zA-Z0-9._]+)\\$.+$", "\\1", var_name[1])
        df_name[2] <- sub("^([a-zA-Z0-9._]+)\\$.+$", "\\1", var_name[2])
      }
      if (isTRUE(df_name[1] == df_name[2])) {
        df_name <- df_name[1]
        var_name[1] <- sub(paste0(df_name, "$"), "", var_name[1])
        var_name[2] <- sub(paste0(df_name, "$"), "", var_name[2])
      } else {
        if (length(pos_with) == 1) {
          with_call <- as.list(standardise_call(sys_calls[[pos_with]]))
          if (length(with_call$data) == 1) {
            df_name <- as.character(with_call$data)
          }
        }
      }
      output <- prep_return()
      return(output)
    }
  }
  
  df_name <- find_df_name()
  
  if ("df_label" %in% what && !is.na(df_name)) {
    df_label  <- find_df_label()
  }
  
  if ("var_name" %in% what) {
    var_name  <- find_var_name()
    if (!is.na(var_name) && !is.na(df_name)) {
      var_name <- sub(paste0("^", df_name, "\\$"), "", var_name)
    } 
  }
  
  if ("var_label" %in% what) {
    var_label <- find_var_label()
  }
  
  output <- prep_return()
  return(output)
}