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
#'
#' @return A list consisting of:
#' \itemize{
#'   \item df_name The data frame name when applicable.
#'   \item var_names The variable names when applicable.
#'   \item rows_subset The subsetting condition when applicable.
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
#' @export
parse_args <- function(sys_calls, sys_frames, match_call, var = "x") {

  # Define regular expressions -- those will be used only when other means
  # of identifying structures have failed.

  # re1: dfname[rows,columns]
  re1 <- paste0("([\\w\\.\\_]+)\\s*", # normally, data frame name (1)
                "\\[(.+)?",           # rows indexing  (2)
                "\\s*(,)\\s*",        # comma surrounded (or not) by spaces     (3)
                "((c\\(.*\\))|",      # column indexing in the form [ , c(...)] (4) (5)
                "(\\d+)|",            # column indexing in the form [ , 9     ] (6)
                "(\\d+\\:\\d+)|",     # column indexing in the form [ , 9:99  ] (7)
                "(\'.*\')|",          # column indexing in the form [ , 'var.name' ] (8)
                "(\".+\")|",          # column indexing in the form [ , "var.name" ] (9)
                "(\\\".+\\\"))?",     # column indexing in the form [ , "var.name" ] (10)
                "\\s*\\]")            # end of indexing

  # re2: dfname[[col]] (+ optionnal rows indexing)
  re2 <- paste0("([\\w\\.\\_]+)",                 # data frame               (1)
                "\\s*\\[\\[\\s*(.*)\\s*\\]\\]",   # variable number or name  (2)
                "(\\s*\\[\\s*(.*)\\s*\\])?")      # rows indexing (opt)      (4)


  # re3: dfname$column[rows] or dfname['column'][rows]
  re3 <- paste0("^([\\w\\.]+)\\s*",           # data.frame name                        (1)
                "(\\$|\\[\\'|\\[\\\")\\s*",   # column indexing chars ($ or [' or [")  (2)
                "([\\w\\.\\_]+)",             # variable name                          (3)
                "(\\s|\\'\\]|\\\"\\])*",      # optional closing indexing chars        (4)
                "(\\[\\s*(.+)\\s*\\])?")      # optional row indexing             (5)  (6)

  # re4: variable[rows]
  re4 <- paste0("\\w+",                       # variable name       (1)
                "(\\[\\s*(.+)\\s*\\])")       # rows indexing       (2)

  # valid names for dataframes
  re5 <- "^[_a-zA-Z0-9](\\w|\\d|\\.)*$"
  
  df_name     <- character()
  var_names   <- character()
  rows_subset <- character()

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

  # Function was called through with() ----------------------------------------------------
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
            is.data.frame(get(with_objects, envir = as.environment(eval(with_call$data))))) {
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
            is.data.frame(get(with_objects, envir = as.environment(eval(with_call$data))))) {
          df_name <- with_objects
          if (length(all.vars(by_call$data)) == 1) {
            var_names <- colnames(eval(with_call$data)[[df_name]])
          } else {
            var_names <- setdiff(all.vars(by_call$data), df_name)
          }
        }
      }
    }

    if (grepl(".+\\[.+\\]$", as.character(with_call$expr[2]), perl = TRUE)) {
      rows_subset <- sub("(.+)\\[(.+)\\]$","\\2",
                         as.character(with_call$expr[2]), perl = TRUE)
    } else {
      rows_subset <- "NULL"
    }
  }

  # Function was called through by() ---------------------------------------------------
  
  # This part will ensure the group-info is made part of the summarytools object
  if (length(by_pos) == 1) {

    by_call <- as.list(standardise_call(sys_calls[[by_pos]]))

    # On first iteration, generate levels based on IND variables
    # and store in package envir
    if (length(.st_env$byInfo) == 0) {
      if (is.null(names(sys_frames[[tapply_pos]]$namelist)) ||
          is.na(names(sys_frames[[tapply_pos]]$namelist)[1])) {
        names(sys_frames[[tapply_pos]]$namelist) <- as.character(by_call$INDICES)[-1]
      }
      by_levels <- sys_frames[[tapply_pos]]$namelist
      .st_env$byInfo$by_levels <- expand.grid(by_levels, stringsAsFactors = FALSE)
      .st_env$byInfo$iter <- 1
    }

    # Populate by_group item
    by_group <- paste(colnames(.st_env$byInfo$by_levels),
                      as.character(.st_env$byInfo$by_levels[.st_env$byInfo$iter, ]),
                      sep=" = ", collapse = ", ")

    # by_first and by_last are used by print.summarytools when printing objects
    # passed by the by() function
    if (.st_env$byInfo$iter == 1 && nrow(.st_env$byInfo$by_levels) == 1) {
      by_first <- TRUE
      by_last  <- TRUE
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


  # Function was called through lapply() -------------------------------------------------

  if (length(lapply_pos) == 1 && lapply_pos == 1) {
    lapply_call <- as.list(standardise_call(sys_calls[[1]]))
    df_name <- as.character(lapply_call$X)
    df_name <- grep(re5, df_name, value = TRUE)[1]
    var_names <- names(sys_frames[[1]]$X)[sys_frames[[1]]$i]
  }


  # From here code applies no matter how function was called -----------------------------
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

  if (length(allnames) == 0 && length(var_names) == 0) {
    var_names <- data_str
    if (length(df_name) == 0) {
      no_df <- TRUE
    }
  } else {
    allnames_exist <- allnames[which(sapply(allnames, exists, USE.NAMES = FALSE))]
    if (length(df_name) == 0 && length(allnames_exist) > 0) {
      if (is.atomic(get(allnames_exist[1]))) {
        no_df <- TRUE
        var_names <- allnames_exist[1]
      } else if (is.data.frame(get(allnames_exist[1]))) {
        df_name <- allnames_exist[1]
      }
    }
  }

  # Extract the dataset name (or unique variable name) if not already done
  if (length(df_name) == 0 && !no_df) {
    if (length(allnames_exist) > 0) {
      if (length(allnames_exist) == 1 && is.data.frame(get(allnames_exist)))
        df_name <- allnames_exist
      else if (length(allnames_exist) > 1) {
        df_index <- which(isTRUE(sapply(sapply(allnames_exist, get), is.data.frame)))
        if (length(df_index) == 1 && is.data.frame(get(allnames_exist[df_index])))
          df_name <- allnames_exist[df_index]
      }
    }

    # If it fails, search for df_name using regex's
    if (length(df_name) == 0) {

      # remove what follows last '['
      data_str_df <- sub("(.+)\\[(.*)", "\\1", data_str, perl = TRUE)

      # check that structure exists and has proper class
      if (exists(data_str_df) && any(class(get(data_str_df)) == "data.frame")) {
        df_name <- data_str_df
      } else {
        getobj <- try(eval(parse(text=data_str_df)), silent = TRUE)
        if (inherits(getobj, "try-error")) {
          skipvars <- TRUE
        } else {
          if (is.data.frame(getobj)) {
            df_name <- data_str_df
            var_names_tmp <- colnames(getobj)
          }
        }
      }
    }
  }

  # Remove dataframe name from by_group if df_name is present in the 
  # by_group string and the same

  if (length(by_group) == 1 && length(df_name) > 0) {
    by_group <- sub(pattern = paste0(df_name,"$"), replacement = "", x = by_group, fixed = TRUE)
  }

  # Extract data frame label if any
  if (!no_df && length(df_name) > 0 && exists(df_name) && !is.na(label(get(df_name)))) {
  #if (!no_df && length(df_name) > 0) {
  #} && exists(df_name) && !is.na(label(get(df_name)))) {
    df_label <- label(get(df_name))
  } else {
    df_label <- character()
  }

  # Determine variables name(s) if not already done
  if (!no_df && length(var_names) == 0 && !skipvars) {
    if (exists("var_names_tmp")) {
      var_names <- var_names_tmp
    } else if (grepl("^\\w+$", data_str, perl = TRUE)) {
      var_names <- colnames(eval(parse(text=data_str)))
    } else if (grepl("^\\w+\\[.*,\\s*\\-.+\\]", data_str, perl = TRUE)) {
      var_names <- colnames(eval(parse(text=data_str)))
    } else if (grepl(re1, data_str, perl = TRUE)) {
      # set aside row subsetting for now
      # replacing for instance "dat[1:29,4]" with "dat[4]"
      data_str_var <- sub(re1,"\\1[\\4]", data_str, perl = TRUE)
      # remove brackets if subsetting is now null
      data_str_var <- sub("\\[\\s*\\,?\\s*\\]","", data_str_var, perl = TRUE)
      var_names <- colnames(eval(parse(text=data_str_var)))
    } else if (grepl(re2, data_str, perl = TRUE)) {
      # set aside row subsetting for now
      data_str_var <- sub(re2, "\\1[\\2]", data_str, perl = TRUE)
      var_names <- colnames(eval(parse(text=data_str_var)))
    } else if (grepl(re3, data_str, perl = TRUE)) {
      data_str_var <- sub(re3, '\\1["\\3"]', data_str, perl = TRUE)
      var_names <- colnames(eval(parse(text=data_str_var)))
    } else if (exists(data_str) && is.atomic(get(data_str))) {
      var_names <- data_str
    }

    if (length(var_names) == 0) {
      message("unable to identify var names: ", data_str)
    }
  }

  # Rows subset
  if (length(rows_subset) == 0) {
    if (grepl(re1, data_str, perl = TRUE)) {
      rows_subset <- sub(re1, "\\2", data_str, perl = TRUE)
    } else if (grepl(re2, data_str, perl = TRUE)) {
      rows_subset <- sub(re2, "\\4", data_str, perl = TRUE)
    } else if (grepl(re3, data_str, perl = TRUE)) {
      rows_subset <- sub(re3, "\\6", data_str, perl = TRUE)
    } else if (grepl(re4, data_str, perl = TRUE)) {
      rows_subset <- sub(re4, "\\2", data_str, perl = TRUE)
    }
  }

  if (!no_df && length(df_name) > 0 && length(rows_subset) == 1) {
    rows_subset <- sub(pattern = paste0(df_name, "$"),
                       replacement = "", x = rows_subset, fixed = TRUE)
    rows_subset <- sub(pattern = "==", replacement = "=", x = rows_subset, fixed = TRUE)
    rows_subset <- sub(pattern = '"(.+)"', replacement = "\\1", x = rows_subset, perl = TRUE)
  }

  if (length(rows_subset) == 0 || nchar(rows_subset) == 0 || rows_subset == "NULL") {
    rows_subset <- character()
  } else {
    if (exists(get("rows_subset"))) {
      tmp <- deparse(get(rows_subset))
      if (length(tmp) == 1 && grepl("\\d+:\\d+", tmp, perl = TRUE)) {
        rows_subset <- tmp
      }
    }
  }

  # remove column negative indexing
  if (!identical(rows_subset, character())) {
    # scenario 1: by itself, ex: ", -4"
    if (grepl("^\\s*,\\s*-.+$", rows_subset, perl = TRUE)) {
      rows_subset <- character()
    } else if (grepl("^.*,\\s*-.+$", rows_subset, perl = TRUE)) {
      # scenario 2: with row indexing, ex: "1:10, -4"
      rows_subset <- sub(",\\s*-.+$", "", rows_subset, perl = TRUE)
    }
  }
  
  output <- list(df_name = df_name,
                 df_label = df_label,
                 var_names = var_names,
                 rows_subset = rows_subset,
                 by_group = by_group,
                 by_first = by_first,
                 by_last = by_last)

  output <- output[which(sapply(output, length) > 0)]
  return(output)
}

