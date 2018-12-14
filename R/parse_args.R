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
      if (is.data.frame(eval(with_call$data))) {
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
      } else if (is.list(eval(with_call$data))) {
        if (length(with_objects) == 1 &&
            is.data.frame(get(with_objects, 
                              envir = as.environment(eval(with_call$data))))) {
          df_name <- with_objects
        }
      }
    } else if (length(by_pos) == 1) {
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

  # Function was called through by() -------------------------------------------
  
  # This part will ensure the group-info is made part of the summarytools object
  if (length(by_pos) == 1) {

    by_call <- as.list(standardise_call(sys_calls[[by_pos]]))

    # On first iteration, generate levels based on IND variables
    # and store in package envir
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

  # Function was called through lapply() ---------------------------------------

  if (length(lapply_pos) == 1 && lapply_pos == 1) {
    lapply_call <- as.list(standardise_call(sys_calls[[1]]))
    df_name <- setdiff(all.names(lapply_call$X), c("[", "[[", "$"))[1]
    var_names <- names(sys_frames[[1]]$X)[sys_frames[[1]]$i]
    
    df_label <- label(eval(lapply_call$X))
    var_label <- label(eval(lapply_call$X)[[var_names]])
  }
  
  # End of special calls -------------------------------------------------------
  
  if (length(df_name) == 0) {
    # Look for the data frame
    if (exists("by_call")) {
      data_str <- deparse(by_call$data)
      allnames <- all.vars(by_call$data)
    } else {
      data_str <- deparse(match_call[[var]])
      allnames <- all.vars(match_call[[var]])
    }

    # Find df_name, and col_names if required
    cont <- TRUE
    for (no.frame in seq_along(sys_frames)) {
      if (!cont) 
        break
      for (obj.name in allnames) {
        if (exists(obj.name, envir = sys_frames[[no.frame]], mode = "list")) {
          df_ <- get(obj.name, envir = sys_frames[[no.frame]])
          if (is.data.frame(df_)) {
            df_name   <- obj.name
            df_pos    <- no.frame
            df_label  <- label(df_)
            # Find colnames
            if (max.varnames > 0) {
              col_names <- colnames(df_)
              if (length(intersect(allnames, col_names)) > 0) {
                var_names <- head(intersect(allnames, col_names), max.varnames)
                var_label <- label(df_[var_names[1]])
              } else {
                possible.iterator <- setdiff(allnames, df_name)
              }
              cont = FALSE
              break
            }
          }
        }
      }
    }
    
    if (exists("df_") && exists("possible.iterator")) {
      cont <- TRUE
      for (no.frame in seq_along(sys_frames)) {
        if (!cont) 
          break
        for (iter in possible.iterator) {
          if (exists(iter, envir = sys_frames[[no.frame]], mode = "numeric")) {
            it <- get(iter, envir = sys_frames[[no.frame]])
            var_names <- head(names(df_[it]), max.varnames)
            var_label <- label(df_[it])[1]
            cont <- FALSE
            break
          }
        }
      }
    }
    
    # Look for a "stand-alone" variable
    # look for numeric if function is descr()
    if (length(var_names) == 0 && grepl("descr", deparse(match_call))) {
      cont <- TRUE
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
      cont <- TRUE
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
    
    if (length(var_names) == 0 && length(allnames) == 1) {
      var_names <- allnames
    }
  }
  
  output <- list(df_name = df_name,
                 df_label = df_label,
                 var_names = var_names,
                 var_label = var_label,
                 by_group = by_group,
                 by_first = by_first,
                 by_last = by_last)

  output <- output[which(mapply(length, output, SIMPLIFY = TRUE) > 0)]

  return(output[!is.na(output)])
}
