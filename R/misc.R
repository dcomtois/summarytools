# Initialise vector containing paths to temporary html files generated when viewing in browser or
# in RStudio visualisation pane. Will be updated whenever print.summarytools() / cleartmp() are called.
.st_env <- new.env(parent = emptyenv())
.st_env$tmpfiles <- c()
.st_env$byInfo <- list()


# .parse_arg() -----------------------------------------------------------

.parse_arg <- function(scalls, sframes, mcall) {

  output <- list(df_name = character(),
                 var_names = character(),
                 rows_subset = character(),
                 by_group = character())
  
  # Look for position of by.default(), tapply() and with.default() in sys.calls() 
  by_pos <- which(as.character(lapply(scalls, head, 1)) == "by()")
  tapply_pos <- which(as.character(lapply(scalls, head, 1)) == "tapply()")
  with_pos <- which(as.character(lapply(scalls, head, 1)) == "with()")
  fn_pos <- which(as.character(lapply(scalls, head, 1)) == deparse(mcall[1]))
  
  # List of classes accepted as "dataframes"
  # classes <- c("data.frame", "data.table", "tbl")
  
  # Function was called through with() ----------------------------------------------------
  # We should be able to extract:
  #  - df_name
  #  - var_names
  #  - rows_subset
  if (length(with_pos) == 1) {
    with_call <- as.list(pryr::standardise_call(scalls[[with_pos]]))
    with_objects <- ls(sframes[[with_pos + 3]])
    
    # Get properties when with() is not combined with by()
    if (length(by_pos) == 0) {
      if(is.data.frame(eval(with_call$data))) {
        output$df_name <- deparse(with_call$data)
        allnames <- all.vars(with_call$expr)
        if (length(allnames) > 0)
          output$var_names <- head(intersect(allnames,with_objects),1)
        else
          output$var_names <- with_objects
      } else if (is.list(eval(with_call$data))) {
        if (length(with_objects) == 1 && 
            is.data.frame(get(with_objects, envir = as.environment(eval(with_call$data))))) {
          output$df_name <- with_objects
        }
      }
    } else if (length(by_pos) == 1) {
      by_call <- as.list(pryr::standardise_call(scalls[[by_pos]]))
      if(is.data.frame(eval(with_call$data))) {
        output$df_name <- deparse(with_call$data)
        output$var_names <- deparse(by_call$data)
      } else if (is.list(eval(with_call$data))) {
        if (length(with_objects) == 1 && 
            is.data.frame(get(with_objects, envir = as.environment(eval(with_call$data))))) {
          output$df_name <- with_objects
          if (length(all.vars(by_call$data)) == 1)
            output$var_names <- colnames(eval(with_call$data)[[output$df_name]])
          else
            output$var_names <- setdiff(all.vars(by_call$data), output$df_name)
        }
      }
    } 
    
    if (grepl(".+\\[.+\\]$", as.character(with_call$expr[2]), perl = TRUE))
      output$rows_subset <- sub("(.+)\\[(.+)\\]$","\\2", 
                                as.character(with_call$expr[2]), perl = TRUE)
    else
      output$rows_subset <- "NULL"
  }
  
  # Function was called through by() -------------------------------------------------------
  # This part will ensure the group-info is made part of the summarytools object
  if (length(by_pos) == 1) {
    
    by_call <- as.list(pryr::standardise_call(scalls[[by_pos]]))
    
    # On first iteration, generate levels based on IND variables 
    # and store in package envir
    if (length(.st_env$byInfo) == 0) {
      if (is.null(names(sframes[[tapply_pos]]$namelist)) || is.na(names(sframes[[tapply_pos]]$namelist)[1]))
        names(sframes[[tapply_pos]]$namelist) <- as.character(by_call$INDICES)[-1]
      by_levels <- sframes[[tapply_pos]]$namelist
      .st_env$byInfo$by_levels <- expand.grid(by_levels, stringsAsFactors = FALSE)
      .st_env$byInfo$iter <- 1
    }
    
    # Populate by_group item
    output$by_group <- paste(colnames(.st_env$byInfo$by_levels), 
                             as.character(.st_env$byInfo$by_levels[.st_env$byInfo$iter,]), 
                             sep=" = ", collapse = ", ")
    
    # On last iteration, clear .st_env$byIndo
    if (.st_env$byInfo$iter == nrow(.st_env$byInfo$by_levels))
      .st_env$byInfo <- list()
    else 
      .st_env$byInfo$iter = .st_env$byInfo$iter + 1
  }
  
  # From here code applies no matter how function was called ---------------------------------
  skipvars <- FALSE # will be changed to TRUE if can't determine df_name
  
  # Following regular expressions allow to split dataframe name, row indexes and column indexes

  # re1: when form dfname[rows,columns] is used
  re1 <- paste0("([\\w\\.\\_]+)\\s*", # normally, dataframe name (1)
                "\\[(.+)?",           # rows indexing  (2)
                "\\s*(,)\\s*",        # comma surrounded (or not) by spaces (3)
                "((c\\(.*\\))|",      # column indexing in the form [ , c(...)]  (4) (5)
                "(\\d+)|",            # column indexing in the form [ , 9     ] (6)
                "(\\d+\\:\\d+)|",     # column indexing in the form [ , 9:99  ] (7)
                "(\'.*\')|",          # column indexing in the form [ , 'var.name' ] (8)
                "(\".+\")|",          # column indexing in the form [ , 'var.name' ] (9)
                "(\\\".+\\\"))?",     # column indexing in the form [ , "var.name" ] (10)
                "\\s*\\]")            # end of indexing
  
  # re2: form dfname[[col]] is used
  re2 <- paste0("([\\w\\.\\_]+)",                # dataframe
                "\\s*\\[\\[\\s*(.*)\\s*\\]\\]")   # variable number or name 
  
  # re3: form listname$dfname$column[rows]
  re3 <- paste0("([\\w\\.\\_]+)\\s*\\$",      # list-type structure (1)
                "\\s*([\\w\\.\\_]+)\\s*\\$",  # data.frame name     (2)
                "\\s*([\\w\\.\\_]+)\\s*",     # variable name       (3)
                "(\\[\\s*(.+)\\s*\\])?")      # rows indexing (opt) (4)  
  
  # re4: form dfname$column[rows]
  re4 <- paste0("([\\w\\.]+)\\s*\\$",         # data.frame name     (1)
                "\\s*([\\w\\.\\_]+)\\s*",     # variable name       (2)
                "(\\[\\s*(.+)\\s*\\])?")      # rows indexing (opt) (3) (4)

  # re5: form variable[rows]
  re5 <- paste0("\\w+",                       # variable name       (1)
                "(\\[\\s*(.+)\\s*\\])")       # rows indexing       (2)
  
  # Extract call as a string
  if (exists("by_call")) {
    data_str <- deparse(by_call$data)
    allnames <- all.vars(by_call$data)
  } else {
    data_str <- deparse(mcall$x)
    allnames <- all.vars(mcall$x)
  }

  allnames_exist <- allnames[which(sapply(allnames, exists, USE.NAMES = FALSE))]
  if (length(output$df_name) == 0 && length(allnames_exist) == 1 && is.atomic(get(allnames_exist))) {
    no_df <- TRUE
    output$var_names <- allnames_exist
  } else {
    no_df <- FALSE
  }

  # Extract the dataset name (or unique variable name) if not already done
  if (!no_df && length(output$df_name) == 0) {
    if (length(allnames_exist) > 0) {
      if (length(allnames_exist) == 1 && is.data.frame(get(allnames_exist)))
        output$df_name <- allnames_exist
      else if (length(allnames_exist) > 1) {
        df_index <- which(isTRUE(sapply(sapply(allnames_exist, get), is.data.frame)))
        if (length(df_index) == 1 && is.data.frame(get(allnames_exist[df_index])))
          output$df_name <- allnames_exist[df_index]
      }
    }
    
    # If it fails, go the regex way
    if (length(output$df_name) == 0) {
      
      # remove what follows last '['
      data_str_df <- sub("(.+)\\[(.*)", "\\1", data_str, perl = TRUE)
      
      # check that structure exists and has proper class
      if (exists(data_str_df) && any(class(get(data_str_df)) == "data.frame"))
        output$df_name <- data_str_df
      else {
        getobj <- try(eval(parse(text=data_str_df)), silent = TRUE)
        if (inherits(getobj, "try-error"))
          skipvars <- TRUE
        else {
          if(is.data.frame(getobj)) {
            output$df_name <- data_str_df
            var_names_tmp <- colnames(getobj)
          } else if (grepl(re4, data_str)) {
            data_str_df <- sub(re3, "\\1$\\2", data_str)
            getobj <- try(eval(parse(text=data_str_df)), silent = TRUE)
            if (inherits(getobj, "try-error"))
              skipvars <- TRUE
            else {
              if(is.data.frame(getobj)) {
                output$df_name <- data_str_df
                var_names_tmp <- sub(re4, data_str, "\\3")
              }
            }
          }
        }
      }
    }
  }
  
  # Determine variables name(s) if not already done 
  if (!no_df && length(output$var_names) == 0 && !skipvars) {
    if (exists("var_names_tmp") && !grepl(re3, data_str, perl = TRUE)) {
      output$var_names <- var_names_tmp
    } else if (grepl("^\\w+$", data_str, perl = TRUE)) {
      output$var_names <- colnames(eval(parse(text=data_str)))
    } else if (grepl(re1, data_str, perl = TRUE)) {
      # set aside row subsetting - it will be handled further down
      # (replacing for instance "dat[1:29,4]" with "dat[4]")
      data_str_var <- sub(re1,"\\1[\\4]", data_str, perl = TRUE)
      # remove brackets if subsetting is now null
      data_str_var <- sub("\\[\\s*\\,?\\s*\\]","", data_str_var, perl = TRUE)
      output$var_names <- colnames(eval(parse(text=data_str_var)))
    } else if (grepl(re2, data_str, perl = TRUE)) {
      # replace double by single brackets
      data_str_var <- sub("[[", "[", data_str, fixed = TRUE)
      data_str_var <- sub("]]", "]", data_str_var, fixed = TRUE)
      output$var_names <- colnames(eval(parse(text=data_str_var)))
    } else if (grepl(re3, data_str, perl = TRUE)) {
      output$var_names <- sub(re3, "\\3", data_str, perl = TRUE)
    } else if (grepl(re4, data_str, perl = TRUE)) {
      output$var_names <- sub(re4, "\\2", data_str, perl = TRUE)
    } else if (exists(data_str) && is.atomic(get(data_str))) {
      output$var_names <- data_str
    }
    
    if (length(output$var_names) == 0) {
      message("unable to identify var names: ", data_str)
    }
  }

  # Rows subset
  if (length(output$rows_subset) == 0) {
    if(grepl(re1, data_str, perl = TRUE))
      output$rows_subset <- sub(re1, "\\2", data_str, perl = TRUE)
    else if (grepl(re2, data_str, perl = TRUE))
      output$rows_subset <- ""
    else if (grepl(re3, data_str, perl = TRUE))
      output$rows_subset <- sub(re3, "\\4", data_str, perl = TRUE)
    else if (grepl(re4, data_str, perl = TRUE))
      output$rows_subset <- sub(re4, "\\4", data_str, perl = TRUE)
    else if (grepl(re5, data_str, perl = TRUE))
      output$rows_subset <- sub(re5, "\\2", data_str, perl = TRUE)
  }
  
  if (length(output$rows_subset) == 0 || nchar(output$rows_subset) == 0 || 
      output$rows_subset == "NULL")
    output$rows_subset <- character()
  
  return(output[which(sapply(output, length) > 0)])
}

# cleartmp() ----------------------------------------------------------

cleartmp <- function(all=FALSE, silent=FALSE) {
  if(length(.st_env$tmpfiles) == 0) {
    if (!silent)
      message("No temporary files to delete.")
  } else if(isTRUE(all) || all == 1 || all == "all") {
    nfiles <- 0
    for(tmpfile in .st_env$tmpfiles) {
      nfiles <- nfiles + 1
      if(!silent)
        message(paste('Deleting', tmpfile))
      unlink(tmpfile)
    }
    .st_env$tmpfiles <- c()
    if(!silent)
      message(paste(nfiles, "file(s) deleted"))
  } else {
    tmpfile <- tail(.st_env$tmpfiles, 1)
    if(!silent)
      message(paste('Deleting', tmpfile))
    unlink(tmpfile)
    .st_env$tmpfiles <- .st_env$tmpfiles[-length(.st_env$tmpfiles)]
  }
}
