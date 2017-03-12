# Parsing function, version used with ctable() only
.parse_arg_xy <- function(sys_calls, sys_frames, match_call) {

  # Look for position of by.default(), tapply() and with.default() in sys.calls()
  with_pos <- which(as.character(lapply(sys_calls, head, 1)) == "with()")
  fn_pos <- which(as.character(lapply(sys_calls, head, 1)) == deparse(match_call[1]))

  # List of classes accepted as "dataframes"
  # classes <- c("data.frame", "data.table", "tbl")

  # Initiate all objects to store data about x (pos. 1) and y (pos. 2)
  allnames <- rep(list(NULL), 2)
  allnames_exist <- rep(list(NULL), 2)
  data_str <- rep(list(NULL), 2)
  data_str_df <- rep(list(NULL), 2)
  df_index <- rep(list(NULL), 2)
  df_name <- rep(list(NULL), 2)
  getobj <- rep(list(NULL), 2)
  no_df <- rep(list(NULL), 2)
  rows_subset <- rep(list(NULL), 2)
  skipvars <- rep(list(NULL), 2)
  var_names <- rep(list(NULL), 2)
  var_names_tmp <- rep(list(NULL), 2)

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


  # Following regular expressions allow to split dataframe name, row indexes and column indexes

  # re1: when form dfname[rows,columns] is used
  re1 <- paste0("([\\w\\.\\_]+)\\s*", # normally, dataframe name (1)
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
  re2 <- paste0("([\\w\\.\\_]+)",                 # dataframe
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
  return(output)
}
