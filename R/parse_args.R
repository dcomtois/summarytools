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
#'
#' @return A list comprised of:
#' \itemize{
#'   \item df_name The data frame name when applicable.
#'   \item var_names The variable names when applicable.
#'   \item rows_subset The subsetting condition when applicable.
#'   \item by_group The group, when functions are called through
#'     \code{by()}}A frequency table of class \code{matrix} with added
#'     attributes used by \code{\link{print.summarytools}} method.
#'
#' @details The default \code{plain.ascii = TRUE} option is there to make
#'   results appear cleaner in the console. To avoid markdown rendering
#'   problems, the option is automatically set to \code{FALSE} whenever
#'   \code{style = 'rmarkdown'}, unless \code{plain.ascii = TRUE} is made
#'   explicit. If the intent is to produce \emph{rmarkdown} text for further
#'   processing while using \code{style = 'simple'}, set \code{plain.ascii} to
#'   \code{FALSE}.
#'
#' @keywords internal misc
#'
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com>}
#'
#' @export
parse_args <- function(sys_calls, sys_frames, match_call, y = FALSE) {

  if (!isTRUE(y)) {
    df_name = character()
    var_names = character()
    rows_subset = character()
    by_group = character()
    by_first = logical()
    by_last = logical()

    # Look for position of by.default(), tapply() and with.default() in sys.calls()
    by_pos <- which(as.character(lapply(sys_calls, head, 1)) == "by()")
    tapply_pos <- which(as.character(lapply(sys_calls, head, 1)) == "tapply()")
    with_pos <- which(as.character(lapply(sys_calls, head, 1)) == "with()")
    fn_pos <- which(as.character(lapply(sys_calls, head, 1)) == deparse(match_call[1]))

    # List of classes accepted as "data frames"
    # classes <- c("data.frame", "data.table", "tbl")

    # Function was called through with() ----------------------------------------------------
    # We should be able to extract:
    #  - df_name
    #  - var_names
    #  - rows_subset
    if (length(with_pos) == 1) {
      with_call <- as.list(pryr::standardise_call(sys_calls[[with_pos]]))
      with_objects <- ls(sys_frames[[with_pos + 3]])

      # Get properties when with() is not combined with by()
      if (length(by_pos) == 0) {
        if(is.data.frame(eval(with_call$data))) {
          df_name <- deparse(with_call$data)
          allnames <- all.vars(with_call$expr)
          if (length(allnames) > 0)
            var_names <- head(intersect(allnames,with_objects),1)
          else
            var_names <- with_objects
        } else if (is.list(eval(with_call$data))) {
          if (length(with_objects) == 1 &&
              is.data.frame(get(with_objects, envir = as.environment(eval(with_call$data))))) {
            df_name <- with_objects
          }
        }
      } else if (length(by_pos) == 1) {
        by_call <- as.list(pryr::standardise_call(sys_calls[[by_pos]]))
        if(is.data.frame(eval(with_call$data))) {
          df_name <- deparse(with_call$data)
          var_names <- deparse(by_call$data)
        } else if (is.list(eval(with_call$data))) {
          if (length(with_objects) == 1 &&
              is.data.frame(get(with_objects, envir = as.environment(eval(with_call$data))))) {
            df_name <- with_objects
            if (length(all.vars(by_call$data)) == 1)
              var_names <- colnames(eval(with_call$data)[[df_name]])
            else
              var_names <- setdiff(all.vars(by_call$data), df_name)
          }
        }
      }

      if (grepl(".+\\[.+\\]$", as.character(with_call$expr[2]), perl = TRUE))
        rows_subset <- sub("(.+)\\[(.+)\\]$","\\2",
                                  as.character(with_call$expr[2]), perl = TRUE)
      else
        rows_subset <- "NULL"
    }

    # Function was called through by() -------------------------------------------------------
    # This part will ensure the group-info is made part of the summarytools object
    if (length(by_pos) == 1) {

      by_call <- as.list(pryr::standardise_call(sys_calls[[by_pos]]))

      # On first iteration, generate levels based on IND variables
      # and store in package envir
      if (length(.st_env$byInfo) == 0) {
        if (is.null(names(sys_frames[[tapply_pos]]$namelist)) ||
            is.na(names(sys_frames[[tapply_pos]]$namelist)[1]))
          names(sys_frames[[tapply_pos]]$namelist) <- as.character(by_call$INDICES)[-1]
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
      if (.st_env$byInfo$iter == 1) {
        by_first <- TRUE
        by_last <- FALSE
        .st_env$byInfo$iter = .st_env$byInfo$iter + 1
      } else if (.st_env$byInfo$iter == nrow(.st_env$byInfo$by_levels)) {
        by_first <- FALSE
        by_last <- TRUE
        .st_env$byInfo <- list()
      } else {
        by_first <- FALSE
        by_last <- FALSE
       .st_env$byInfo$iter = .st_env$byInfo$iter + 1
      }
    }

    # From here code applies no matter how function was called ---------------------------------
    skipvars <- FALSE # will be changed to TRUE if can't determine df_name

    # Following regular expressions allow to split data frame name, row indexes and column indexes

    # re1: when form dfname[rows,columns] is used
    re1 <- paste0("([\\w\\.\\_]+)\\s*", # normally, data frame name (1)
                  "\\[(.+)?",           # rows indexing  (2)
                  "\\s*(,)\\s*",        # comma surrounded (or not) by spaces (3)
                  "((c\\(.*\\))|",      # column indexing in the form [ , c(...)]  (4) (5)
                  "(\\d+)|",            # column indexing in the form [ , 9     ] (6)
                  "(\\d+\\:\\d+)|",     # column indexing in the form [ , 9:99  ] (7)
                  "(\'.*\')|",          # column indexing in the form [ , 'var.name' ] (8)
                  "(\".+\")|",          # column indexing in the form [ , "var.name" ] (9)
                  "(\\\".+\\\"))?",     # column indexing in the form [ , "var.name" ] (10)
                  "\\s*\\]")            # end of indexing

    # re2: form dfname[[col]] is used
    re2 <- paste0("([\\w\\.\\_]+)",                 # data frame
                  "\\s*\\[\\[\\s*(.*)\\s*\\]\\]")   # variable number or name

    # re3: form listname$dfname$column[rows]
    re3 <- paste0("([\\w\\.\\_]+)\\s*\\$",       # list-type structure (1)
                  "\\s*([\\w\\.\\_]+)\\s*\\$",   # data.frame name     (2)
                  "\\s*([\\w\\.\\_]+)\\s*",      # variable name       (3)
                  "(\\[\\s*(.+)\\s*\\])?")       # rows indexing (opt) (4)

    # re4: form dfname$column[rows] or dfname['column'][rows]
    re4 <- paste0("^([\\w\\.]+)\\s*",           # data.frame name                        (1)
                  "(\\$|\\[\\'|\\[\\\")\\s*",   # column indexing chars ($ or [' or [")  (2)
                  "([\\w\\.\\_]+)",             # variable name                          (3)
                  "(\\s|\\'\\]|\\\"\\])*",      # optional closing indexing chars        (4)
                  "(\\[\\s*(.+)\\s*\\])?")      # optional row indexing             (5)  (6)

    #  re4 <- paste0("([\\w\\.]+)\\s*\\$",         # data.frame name         (1)
    #                "\\s*([\\w\\.\\_]+)\\s*",     # variable name           (2)
    #                "(\\[\\s*(.+)\\s*\\])?")      # rows indexing (opt) (3) (4)
    #


    # re5: form variable[rows]
    re5 <- paste0("\\w+",                       # variable name       (1)
                  "(\\[\\s*(.+)\\s*\\])")       # rows indexing       (2)

    # Extract call as a string
    if (exists("by_call")) {
      data_str <- deparse(by_call$data)
      allnames <- all.vars(by_call$data)
    } else {
      data_str <- deparse(match_call$x)
      allnames <- all.vars(match_call$x)
    }

    if (length(allnames) == 0 && length(var_names) == 0) {
      var_names <- data_str
      if (length(df_name) == 0) {
        no_df <- TRUE
      }
    } else {

      allnames_exist <- allnames[which(sapply(allnames, exists, USE.NAMES = FALSE))]
      if (length(df_name) == 0 && length(allnames_exist) == 1 && is.atomic(get(allnames_exist))) {
        no_df <- TRUE
        var_names <- allnames_exist
      } else {
        no_df <- FALSE
      }
    }
    # Extract the dataset name (or unique variable name) if not already done
    if (!no_df && length(df_name) == 0) {
      if (length(allnames_exist) > 0) {
        if (length(allnames_exist) == 1 && is.data.frame(get(allnames_exist)))
          df_name <- allnames_exist
        else if (length(allnames_exist) > 1) {
          df_index <- which(isTRUE(sapply(sapply(allnames_exist, get), is.data.frame)))
          if (length(df_index) == 1 && is.data.frame(get(allnames_exist[df_index])))
            df_name <- allnames_exist[df_index]
        }
      }

      # If it fails, go the regex way
      if (length(df_name) == 0) {

        # remove what follows last '['
        data_str_df <- sub("(.+)\\[(.*)", "\\1", data_str, perl = TRUE)

        # check that structure exists and has proper class
        if (exists(data_str_df) && any(class(get(data_str_df)) == "data.frame"))
          df_name <- data_str_df
        else {
          getobj <- try(eval(parse(text=data_str_df)), silent = TRUE)
          if (inherits(getobj, "try-error"))
            skipvars <- TRUE
          else {
            if(is.data.frame(getobj)) {
              df_name <- data_str_df
              var_names_tmp <- colnames(getobj)
            } else if (grepl(re4, data_str)) {              # mismatch between re's
              data_str_df <- sub(re3, "\\1$\\2", data_str)  # mismatch between re's
              getobj <- try(eval(parse(text=data_str_df)), silent = TRUE)
              if (inherits(getobj, "try-error"))
                skipvars <- TRUE
              else {
                if(is.data.frame(getobj)) {
                  df_name <- data_str_df
                  var_names_tmp <- sub(re4, data_str, "\\3") # change re4 to re3 ?? 2017-03-10
                }
              }
            }
          }
        }
      }
    }

    # Extract data frame label if any
    if (!no_df && length(df_name) > 0 && exists(df_name) && !is.na(label(get(df_name)))) {
      df_label <- label(get(df_name))
    } else {
      df_label <- character()
    }

    # Determine variables name(s) if not already done
    if (!no_df && length(var_names) == 0 && !skipvars) {
      if (exists("var_names_tmp") && !grepl(re3, data_str, perl = TRUE)) {
        var_names <- var_names_tmp
      } else if (grepl("^\\w+$", data_str, perl = TRUE)) {
        var_names <- colnames(eval(parse(text=data_str)))
      } else if (grepl(re1, data_str, perl = TRUE)) {
        # set aside row subsetting - it will be handled further down
        # (replacing for instance "dat[1:29,4]" with "dat[4]")
        data_str_var <- sub(re1,"\\1[\\4]", data_str, perl = TRUE)
        # remove brackets if subsetting is now null
        data_str_var <- sub("\\[\\s*\\,?\\s*\\]","", data_str_var, perl = TRUE)
        var_names <- colnames(eval(parse(text=data_str_var)))
      } else if (grepl(re2, data_str, perl = TRUE)) {
        # replace double by single brackets
        data_str_var <- sub("[[", "[", data_str, fixed = TRUE)
        data_str_var <- sub("]]", "]", data_str_var, fixed = TRUE)
        var_names <- colnames(eval(parse(text=data_str_var)))
      } else if (grepl(re3, data_str, perl = TRUE)) {
        var_names <- sub(re3, "\\3", data_str, perl = TRUE)
      } else if (grepl(re4, data_str, perl = TRUE)) {
        var_names <- sub(re4, "\\3", data_str, perl = TRUE)
      } else if (exists(data_str) && is.atomic(get(data_str))) {
        var_names <- data_str
      }

      if (length(var_names) == 0) {
        message("unable to identify var names: ", data_str)
      }
    }

    # Rows subset
    if (length(rows_subset) == 0) {
      if(grepl(re1, data_str, perl = TRUE))
        rows_subset <- sub(re1, "\\2", data_str, perl = TRUE)
      else if (grepl(re2, data_str, perl = TRUE))
        rows_subset <- ""
      else if (grepl(re3, data_str, perl = TRUE))
        rows_subset <- sub(re3, "\\4", data_str, perl = TRUE)
      else if (grepl(re4, data_str, perl = TRUE))
        rows_subset <- sub(re4, "\\6", data_str, perl = TRUE)
      else if (grepl(re5, data_str, perl = TRUE))
        rows_subset <- sub(re5, "\\2", data_str, perl = TRUE)
    }

    if (!no_df && length(df_name) > 0 && length(rows_subset) == 1) {
      rows_subset <- sub(pattern = paste0(df_name, "$"),
                                replacement = "", x = rows_subset, fixed = TRUE)
      rows_subset <- sub(pattern = "==", replacement = "=", x = rows_subset, fixed = TRUE)
      rows_subset <- sub(pattern = '"(.+)"', replacement = "\\1", x = rows_subset, perl = TRUE)
    }

    if (length(rows_subset) == 0 || nchar(rows_subset) == 0 ||
        rows_subset == "NULL")
      rows_subset <- character()

    output <- list(df_name = df_name,
                   df_label = df_label,
                   var_names = var_names,
                   rows_subset = rows_subset,
                   by_group = by_group,
                   by_first = by_first,
                   by_last = by_last)

    output <- output[which(sapply(output, length) > 0)]

  } else {
    # Look for position of by.default(), tapply() and with.default() in sys.calls()
    with_pos <- which(as.character(lapply(sys_calls, head, 1)) == "with()")
    fn_pos <- which(as.character(lapply(sys_calls, head, 1)) == deparse(match_call[1]))

    # List of classes accepted as "data frames"
    # classes <- c("data.frame", "data.table", "tbl")

    # Initiate all objects to store data about x (pos. 1) and y (pos. 2)
    allnames       <- rep(list(NULL), 2)
    allnames_exist <- rep(list(NULL), 2)
    data_str       <- rep(list(NULL), 2)
    data_str_df    <- rep(list(NULL), 2)
    df_index       <- rep(list(NULL), 2)
    df_label       <- rep(list(NULL), 2)
    df_name        <- rep(list(NULL), 2)
    getobj         <- rep(list(NULL), 2)
    no_df          <- rep(list(NULL), 2)
    rows_subset    <- rep(list(NULL), 2)
    skipvars       <- rep(list(NULL), 2)
    var_names      <- rep(list(NULL), 2)
    var_names_tmp  <- rep(list(NULL), 2)

    # Function was called through with() ----------------------------------------------------
    # We should be able to extract:
    #  - df_name
    #  - var_names
    #  - rows_subset
    if (length(with_pos) == 1) {
      with_call <- as.list(pryr::standardise_call(sys_calls[[with_pos]]))
      with_objects <- ls(sys_frames[[with_pos + 3]])

      if(is.data.frame(eval(with_call$data))) {
        df_name[[1]] <- df_name[[2]] <- deparse(with_call$data)
        with_allnames <- all.vars(with_call$expr)
        if (length(with_allnames) == 2) {
          var_names[[1]] <- with_allnames[1]
          var_names[[2]] <- with_allnames[2]
        }
      } else if (is.list(eval(with_call$data))) {
        if (length(with_objects) == 1 &&
            is.data.frame(get(with_objects, envir = as.environment(eval(with_call$data))))) {
          df_name[[1]] <- df_name[[2]] <- with_objects
        }
      }
    }


    # Following regular expressions allow to split data frame name, row indexes and column indexes

    # re1: when form dfname[rows,columns] is used
    re1 <- paste0("([\\w\\.\\_]+)\\s*", # normally, data frame name (1)
                  "\\[(.+)?",           # rows indexing  (2)
                  "\\s*(,)\\s*",        # comma surrounded (or not) by spaces (3)
                  "((c\\(.*\\))|",      # column indexing in the form [ , c(...)]  (4) (5)
                  "(\\d+)|",            # column indexing in the form [ , 9     ] (6)
                  "(\\d+\\:\\d+)|",     # column indexing in the form [ , 9:99  ] (7)
                  "(\'.*\')|",          # column indexing in the form [ , 'var.name' ] (8)
                  "(\".+\")|",          # column indexing in the form [ , "var.name" ] (9)
                  "(\\\".+\\\"))?",     # column indexing in the form [ , "var.name" ] (10)
                  "\\s*\\]")            # end of indexing

    # re2: form dfname[[col]] is used
    re2 <- paste0("([\\w\\.\\_]+)",                 # data frame
                  "\\s*\\[\\[\\s*(.*)\\s*\\]\\]")   # variable number or name

    # re3: form listname$dfname$column[rows]
    re3 <- paste0("([\\w\\.\\_]+)\\s*\\$",       # list-type structure (1)
                  "\\s*([\\w\\.\\_]+)\\s*\\$",   # data.frame name     (2)
                  "\\s*([\\w\\.\\_]+)\\s*",      # variable name       (3)
                  "(\\[\\s*(.+)\\s*\\])?")       # rows indexing (opt) (4)

    # re4: form dfname$column[rows] or dfname['column'][rows]
    re4 <- paste0("^([\\w\\.]+)\\s*",           # data.frame name                        (1)
                  "(\\$|\\[\\'|\\[\\\")\\s*",   # column indexing chars ($ or [' or [")  (2)
                  "([\\w\\.\\_]+)",             # variable name                          (3)
                  "(\\s|\\'\\]|\\\"\\])*",      # optional closing indexing chars        (4)
                  "(\\[\\s*(.+)\\s*\\])?")      # optional row indexing             (5)  (6)


    # re5: form variable[rows]
    re5 <- paste0("\\w+",                       # variable name       (1)
                  "(\\[\\s*(.+)\\s*\\])")       # rows indexing       (2)

    # From here code applies no matter how function was called ---------------------------------

    for (XY in 1:2) {


      skipvars[[XY]] <- FALSE # will be changed to TRUE if can't determine df_name
      # Extract call as a string

      if (XY == 1) {
        data_str[[XY]] <- deparse(match_call[["x"]])
        allnames[[XY]] <- all.vars(match_call[["x"]])
      } else {
        data_str[[XY]] <- deparse(match_call[["y"]])
        allnames[[XY]] <- all.vars(match_call[["y"]])
      }

      allnames_exist[[XY]] <- allnames[[XY]][which(sapply(allnames[[XY]], exists, USE.NAMES = FALSE))]

      if (length(df_name[[XY]]) == 0 && length(allnames_exist[[XY]]) == 1 && is.atomic(get(allnames_exist[[XY]]))) {
        no_df[[XY]] <- TRUE
        var_names[[XY]] <- allnames_exist[[XY]]
      } else {
        no_df[[XY]] <- FALSE
      }

      # Extract the dataset name (or unique variable name) if not already done
      if (!no_df[[XY]] && length(df_name[[XY]]) == 0) {
        if (length(allnames_exist[[XY]]) > 0) {
          if (length(allnames_exist[[XY]]) == 1 && is.data.frame(get(allnames_exist[[XY]])))
            df_name[[XY]] <- allnames_exist[[XY]]
          else if (length(allnames_exist[[XY]]) > 1) {
            df_index[[XY]] <- which(isTRUE(sapply(sapply(allnames_exist[[XY]], get), is.data.frame)))
            if (length(df_index[[XY]]) == 1 && is.data.frame(get(allnames_exist[[df_index[[XY]]]])))
              df_name[[XY]] <- allnames_exist[[XY]][[df_index[[XY]]]]
          }
        }

        # If it fails, go the regex way
        if (length(df_name[[XY]]) == 0) {

          # remove what follows last '['
          data_str_df[[XY]] <- sub("(.+)\\[(.*)", "\\1", data_str[[XY]], perl = TRUE)

          # check that structure exists and has proper class
          if (exists(data_str_df[[XY]]) && any(class(get(data_str_df[[XY]])) == "data.frame"))
            df_name[[XY]] <- data_str_df[[XY]]
          else {
            getobj[[XY]] <- try(eval(parse(text=data_str_df[[XY]])), silent = TRUE)
            if (inherits(getobj[[XY]], "try-error"))
              skipvars[[XY]] <- TRUE
            else {
              if(is.data.frame(getobj[[XY]])) {
                df_name[[XY]] <- data_str_df[[XY]]
                var_names_tmp[[XY]] <- colnames(getobj)
              } else if (grepl(re4, data_str[[XY]])) {                    # mismatch between re's
                data_str_df[[XY]] <- sub(re3, "\\1$\\2", data_str[[XY]])  # mismatch between re's
                getobj[[XY]] <- try(eval(parse(text=data_str_df[[XY]])), silent = TRUE)
                if (inherits(getobj[[XY]], "try-error"))
                  skipvars[[XY]] <- TRUE
                else {
                  if(is.data.frame(getobj[[XY]])) {
                    df_name[[XY]] <- data_str_df[[XY]]
                    var_names_tmp[[XY]] <- sub(re4, data_str[[XY]], "\\3") # change re4 to re3 ?? 2017-03-10
                  }
                }
              }
            }
          }
        }
      }

      # Extract data frame label if any
      if (!no_df[[XY]] && df_name[[XY]] != "" && exists(df_name[[XY]]) &&
          !is.na(label(get(df_name[[XY]]))))
        df_label[[XY]] <- label(get(df_name[[XY]]))

      # Determine variables name(s) if not already done
      if (!no_df[[XY]] && length(var_names[[XY]]) == 0 && !skipvars[[XY]]) {
        if (length(var_names_tmp[[XY]]) == 1 && !grepl(re3, data_str[[XY]], perl = TRUE)) {
          var_names[[XY]] <- var_names_tmp[[XY]]
        } else if (grepl("^\\w+$", data_str[[XY]], perl = TRUE)) {
          var_names[[XY]] <- colnames(eval(parse(text=data_str[[XY]])))
        } else if (grepl(re1, data_str[[XY]], perl = TRUE)) {
          # set aside row subsetting - it will be handled further down
          # (replacing for instance "dat[1:29,4]" with "dat[4]")
          data_str_var[[XY]] <- sub(re1,"\\1[\\4]", data_str[[XY]], perl = TRUE)
          # remove brackets if subsetting is now null
          data_str_var[[XY]] <- sub("\\[\\s*\\,?\\s*\\]","", data_str_var[[XY]], perl = TRUE)
          var_names[[XY]] <- colnames(eval(parse(text=data_str_var[[XY]])))
        } else if (grepl(re2, data_str[[XY]], perl = TRUE)) {
          # replace double by single brackets
          data_str_var[[XY]] <- sub("[[", "[", data_str[[XY]], fixed = TRUE)
          data_str_var[[XY]] <- sub("]]", "]", data_str_var[[XY]], fixed = TRUE)
          var_names[[XY]] <- colnames(eval(parse(text=data_str_var[[XY]])))
        } else if (grepl(re3, data_str[[XY]], perl = TRUE)) {
          var_names[[XY]] <- sub(re3, "\\3", data_str[[XY]], perl = TRUE)
        } else if (grepl(re4, data_str[[XY]], perl = TRUE)) {
          var_names[[XY]] <- sub(re4, "\\3", data_str[[XY]], perl = TRUE)
        } else if (length(data_str[[XY]]) == 1 && is.atomic(get(data_str[[XY]]))) {
          var_names[[XY]] <- data_str[[XY]]
        }

        if (length(var_names[[XY]]) == 0) {
          message("unable to identify var names: ", data_str[[XY]])
        }
      }

      # Rows subset
      if (length(rows_subset[[XY]]) == 0) {
        if(grepl(re1, data_str[[XY]], perl = TRUE))
          rows_subset[[XY]] <- sub(re1, "\\2", data_str[[XY]], perl = TRUE)
        else if (grepl(re2, data_str[[XY]], perl = TRUE))
          rows_subset[[XY]] <- ""
        else if (grepl(re3, data_str[[XY]], perl = TRUE))
          rows_subset[[XY]] <- sub(re3, "\\4", data_str[[XY]], perl = TRUE)
        else if (grepl(re4, data_str[[XY]], perl = TRUE))
          rows_subset[[XY]] <- sub(re4, "\\6", data_str[[XY]], perl = TRUE)
        else if (grepl(re5, data_str[[XY]], perl = TRUE))
          rows_subset[[XY]] <- sub(re5, "\\2", data_str[[XY]], perl = TRUE)
      }

      if (length(rows_subset[[XY]]) == 0 || nchar(rows_subset[[XY]]) == 0 ||
          rows_subset[[XY]] == "NULL")
        rows_subset[[XY]] <- character()
    }
    output <- list(df_name, var_names, rows_subset)
    names(output) <- c("df_name", "var_names", "rows_subset")
    names(output[[1]]) <- c("x", "y")
    names(output[[2]]) <- c("x", "y")
    names(output[[3]]) <- c("x", "y")

  }
  return(output)
}

