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

  #lapply_first <- logical()
  #lapply_last  <- logical()
  lapply_pos <- which(as.character(lapply(sys_calls, head, 1)) == "lapply()")


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
    df_name <- as.character(lapply_call$X)
    df_name <- grep(re5, df_name, value = TRUE)[1]
    var_names <- names(sys_frames[[1]]$X)[sys_frames[[1]]$i]
  }
  
  # End of special calls -------------------------------------------------------
  
  # regex to try and parse indexing
  re_indexing <- 
    paste0(
      "^([\\w\\.\\_]+)\\s*", # normally, data frame name (1)
      "\\[(.+)?",           # rows indexing  (2)
      "\\s*(\\,)\\s*",        # comma surrounded (or not) by spaces     (3)
      "(.*?)",              # column indexing                         (4)
      "\\s*\\]$")            # end of indexing
  
  
  # re2: dfname[[col]] (+ optionnal rows indexing)
  re_dbl_brackets <- 
    paste0("([\\w\\.\\_]+)",                 # data frame               (1)
           "\\s*\\[\\[\\s*(.*)\\s*\\]\\]",   # variable number or name  (2)
           "(\\s*\\[\\s*(.*)\\s*\\])?")      # rows indexing (opt)      (4)
  
  if (length(df_name) == 0 ||
      (length(var_names) == 0 && max.varnames > 0)) {
        # From here code applies no matter how function was called -------------------
        skipvars <- FALSE
        no_df    <- FALSE
        
        # Extract call as a string
        if (exists("by_call")) {
          data_str <- deparse(by_call$data)
          allnames <- all.vars(by_call$data)
        } else {
          data_str <- deparse(match_call[[var]])
          allnames <- all.vars(match_call[[var]])
        }
        
        # Look for a data frame; go up the chain of environments
        cont <- TRUE
        for (i in seq_along(allnames)) {
          if (!cont)
            break
          for (j in seq_along(sys.frames())) {
            check <- exists(x = allnames[i], where = j, mode = 'list')
            if (isTRUE(check)) {
              obj <- get(allnames[i], pos = j)
              if (is.data.frame(obj)) {
                df_name <- allnames[i]
                df_label <- label(obj)
                if (length(allnames == 1)) {
                  # if there is column indexing, simplify it so to get df[col]
                  if (grepl(re_indexing, data_str, perl = TRUE)) {
                    var_names <- 
                      colnames(obj[as.numeric(sub(re_indexing, "\\4", 
                                                  x = data_str, perl = TRUE))])
                    var_label <- 
                      label(obj[eval(sub(re_indexing, "\\4", 
                                         x = data_str, perl = TRUE))])
                    cont <- FALSE
                    break
                  } else if (grepl(re_dbl_brackets, data_str, perl = TRUE)) {
                    var_names <- 
                      colnames(obj[as.numeric(sub(re_dbl_brackets, "\\2",
                                                   x = data_str, perl = TRUE))])
                    var_label <- 
                      label(obj[as.numeric(sub(re_dbl_brackets, "\\2",
                                                x = data_str, perl = TRUE))])
                    cont <- FALSE
                    break
                  }
                } else {
                  tmp <- intersect(colnames(obj), allnames)
                  if (length(tmp) > 0) {
                    # truncate the vector var_names to the max expected
                    length(tmp) <- min(length(tmp), max.varnames) 
                    var_names <- tmp
                  }
                  var_label <- label(obj[,var_names])
                  cont <- FALSE
                  break
                }
              }
            }
          }
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
  
  return(output)
}
