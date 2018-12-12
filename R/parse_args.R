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
          df <- get(obj.name, envir = sys_frames[[no.frame]])
          if (is.data.frame(df)) {
            df_name   <- obj.name
            df_pos    <- no.frame
            df_label  <- label(df)
            # Find colnames
            if (max.varnames > 0) {
              col_names <- colnames(df)
              if (length(intersect(allnames, col_names)) > 0) {
                var_names <- head(intersect(allnames, col_names), max.varnames)
                var_label <- label(df[var_names[1]])
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
    
    if (exists("df") && exists("possible.iterator")) {
      cont <- TRUE
      for (no.frame in seq_along(sys_frames)) {
        if (!cont) 
          break
        for (iter in possible.iterator) {
          if (exists(iter, envir = sys_frames[[no.frame]], mode = "numeric")) {
            it <- get(iter, envir = sys_frames[[no.frame]])
            var_names <- head(names(df[it]), max.varnames)
            var_label <- label(df[it])[1]
            cont <- FALSE
            break
          }
        }
      }
    }
  }
  
  # From here code applies no matter how function was called -----------------
  # We have missing information
  # Extract call as a string
  
  # get names of objects in all available frames
  # objects_in_frames <- list()
  # for (no.frame in seq_along(sys_frames)) {
  #   objects_in_frame <- character()
  #   for (obj.number in seq_along(ls.str(sys.frames()[[no.frame]]))) {
  #     objects_in_frame <- c(objects_in_frame, ls.str(sys.frames()[[no.frame]])[obj.number])
  #   }
  #   objects_in_frames[[no.frame]] <- objects_in_frame
  # }
  
  # # regex to try and parse indexing
  # re_indexing <- 
  #   paste0(
  #     "^([\\w\\.\\_]+)\\s*", # normally, data frame name (1)
  #     "\\[(.+)?",            # rows indexing  (2)
  #     "\\s*(\\,)\\s*",       # comma surrounded (or not) by spaces     (3)
  #     "(.*?)",               # column indexing                         (4)
  #     "\\s*\\]$")            # end of indexing
  # 
  # 
  # # re2: dfname[[col]] (+ optionnal rows indexing)
  # re_dbl_brackets <- 
  #   paste0("([\\w\\.\\_]+)",                 # data frame               (1)
  #          "\\s*\\[\\[\\s*(.*)\\s*\\]\\]",   # variable number or name  (2)
  #          "(\\s*\\[\\s*(.*)\\s*\\])?")      # rows indexing (opt)      (4)
  # 
  
  #     # Look for a data frame; go up the chain of environments
  #     cont <- TRUE
  #     for (i.name in seq_along(allnames)) {
  #       if (!cont)
  #         break
  #       for (j.frame in seq_along(sys.frames())) {
  #         check <- exists(x = allnames[i.name], where = j.frame, mode = 'list')
  #         if (isTRUE(check)) {
  #           obj <- get(allnames[i.name], pos = j.frame)
  #           if (is.data.frame(obj)) {
  #             df_name <- allnames[i.name]
  #             df_label <- label(obj)
  #             df_frame_pos <- j.frame
  #             if (length(allnames) == 1 && max.varnames != Inf) {
  #               # if there is column indexing, simplify it so to get df[col]
  #               if (grepl(re_indexing, data_str, perl = TRUE)) {
  #                 var_names <- 
  #                   colnames(obj[as.numeric(sub(re_indexing, "\\4", 
  #                                               x = data_str, perl = TRUE))])
  #                 var_label <- 
  #                   label(obj[eval(sub(re_indexing, "\\4", 
  #                                      x = data_str, perl = TRUE))])
  #                 cont <- FALSE
  #                 break
  #               } else if (grepl(re_dbl_brackets, data_str, perl = TRUE)) {
  #                 # try to convert to numeric what's inside the dbl brackets
  #                 tmp_content <- unquote(sub(re_dbl_brackets, "\\2", 
  #                                            x = data_str, perl = TRUE))
  #                 varnum <- suppressWarnings(as.numeric(tmp_content))
  #                 if (is.na(varnum)) {
  #                   var_names <- colnames(obj[tmp_content])
  #                   var_label <- label(obj[tmp_content])
  #                 } else {
  #                   var_names <- colnames(obj[as.numeric(tmp_content)])
  #                   var_label <- label(obj[as.numeric(tmp_content)])
  #                 }
  #                 cont <- FALSE
  #                 break
  #               }
  #             } else {
  #               # more than 1 name in allnames - see if these are variable names
  #               tmp <- intersect(colnames(obj), allnames)
  #               if (length(tmp) > 0) {
  #                 # truncate the vector var_names to the max expected
  #                 length(tmp) <- min(length(tmp), max.varnames) 
  #                 var_names <- tmp
  #                 var_label <- label(obj[,var_names])
  #               }
  #               cont <- FALSE
  #               break
  #             }
  #           }
  #         }
  #       }
  #     }
  #     
  #     # A dataframe was found, but not a variable;
  #     # There might be an iterator accessible in the same frame as
  #     # the dataframe (df_frame_pos)
  #     if (length(df_name) == 1 && length(allnames) == 2) {
  #       name = setdiff(allnames, df_name)
  #       check <- exists(x = name, where = df_frame_pos, mode = 'numeric')
  #       if (isTRUE(check)) {
  #         # we have the value of the iterator; get the name of the variable
  #         col_num <- get(name, pos = j.frame)
  #         var_names <- colnames(obj[col_num])
  #         var_label <- colnames(obj[col_num])
  #       }
  #     }
  #     
  #     # No dataframe found;
  #     if (length(df_name) == 0) {
  #       # Look for a variable; go up the chain of environments
  #       for (.i in seq_along(allnames)) {
  #         if (!cont)
  #           break
  #         for (.j in seq_along(sys.frames())) {
  #           check <- exists(x = allnames[.i], where = .j, mode = 'character') ||
  #             exists(x = allnames[.i], where = .j, mode = 'numeric')
  #           if (isTRUE(check)) {
  #             obj <- get(allnames[.i], pos = .j)
  #             if (is.atomic(obj)) {
  #               var_names <- allnames[.i]
  #               var_label <- label(obj)
  #               cont <- FALSE
  #               break
  #             }
  #           }
  #         }
  #       }
  #     }
  #   }
  #   
  
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
