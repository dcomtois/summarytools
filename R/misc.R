# Initialise vector containing paths to temporary html files generated when viewing in browser or
# in RStudio visualisation pane. Will be updated whenever print.summarytools() / cleartmp() are called.
.st.env <- new.env(parent = emptyenv())
.st.env$tmpfiles <- c()
.st.env$byInfo <- list()


# .parse_arg ####################################################################
#
# This function takes a string referring to existing data and parses it
# to get information on the data structure.
#
# info returned: df.name, var.names, rows.subset

#scalls <- sys.calls()
#sframes <- sys.frames()
#mcall <- match.call()

.parse_arg <- function(scalls, sframes, mcall) {

    output <- list(df.name = character(),
                   var.names = character(),
                   rows.subset = character(),
                   by.group = character())
    
    # Look for position of by.default(), tapply() and with.default() in sys.calls() 
    by_pos <- which(as.character(lapply(scalls, head, 1)) == "by()")
    tapply_pos <- which(as.character(lapply(scalls, head, 1)) == "tapply()")
    with_pos <- which(as.character(lapply(scalls, head, 1)) == "with()")
    fn_pos <- which(as.character(lapply(scalls, head, 1)) == deparse(mcall[1]))
    
    # List of classes accepted as "dataframes"
    # classes <- c("data.frame", "data.table", "tbl")
    
    
    # Function was called through with() ----------------------------------------------------
    # We should be able to extract:
    #  - df.name
    #  - var.names
    #  - rows.subset
    if (length(with_pos) == 1) {
      with_call <- as.list(pryr::standardise_call(scalls[[with_pos]]))
      with_objects <- ls(sframes[[with_pos + 3]])
      
      # Get properties when with() is not combined with by()
      if (length(by_pos) == 0) {
        if(is.data.frame(eval(with_call$data))) {
          output$df.name <- deparse(with_call$data)
          allnames <- all.vars(with_call$expr)
          if (length(allnames) > 0)
            output$var.names <- head(intersect(allnames,with_objects),1)
          else
            output$var.names <- with_objects
        } else if (is.list(eval(with_call$data))) {
          if (length(with_objects) == 1 && 
              is.data.frame(get(with_objects, envir = as.environment(eval(with_call$data))))) {
            output$df.name <- with_objects
          }
        }
      } else if (length(by_pos) == 1) {
        by_call <- as.list(pryr::standardise_call(scalls[[by_pos]]))
        if(is.data.frame(eval(with_call$data))) {
          output$df.name <- deparse(with_call$data)
          output$var.names <- deparse(by_call$data)
        } else if (is.list(eval(with_call$data))) {
          if (length(with_objects) == 1 && 
              is.data.frame(get(with_objects, envir = as.environment(eval(with_call$data))))) {
            output$df.name <- with_objects
            if (length(all.vars(by_call$data)) == 1)
              output$var.names <- colnames(eval(with_call$data)[[output$df.name]])
            else
              output$var.names <- setdiff(all.vars(by_call$data), output$df.name)
          }
        }
      } 
      
      if (grepl("(.+)\\([.+\\])$", as.character(with_call$expr[2])))
        output$rows.subset <- sub("(.+)\\([.+\\])$","\\2", as.character(with_call$expr[2]))
      else
        output$rows.subset <- "NULL"
    }
    
    # Function was called through by() -------------------------------------------------------
    # This part will ensure the group-info is made part of the summarytools object
    if (length(by_pos) == 1) {
      
      by_call <- as.list(pryr::standardise_call(scalls[[by_pos]]))
      
      # On first iteration, generate levels based on IND variables 
      # and store in package envir
      if (length(.st.env$byInfo) == 0) {
        if (is.null(names(sframes[[tapply_pos]]$namelist)) || is.na(names(sframes[[tapply_pos]]$namelist)[1]))
          names(sframes[[tapply_pos]]$namelist) <- as.character(by_call$INDICES)[-1]
        by_levels <- sframes[[tapply_pos]]$namelist
        .st.env$byInfo$by_levels <- expand.grid(by_levels, stringsAsFactors = FALSE)
        .st.env$byInfo$iter <- 1
      }
      
      # Populate by.group item
      output$by.group <- paste(colnames(.st.env$byInfo$by_levels), 
                               as.character(.st.env$byInfo$by_levels[.st.env$byInfo$iter,]), 
                               sep=" = ", collapse = ", ")
      
      # On last iteration, clear .st.env$byIndo
      if (.st.env$byInfo$iter == nrow(.st.env$byInfo$by_levels))
        .st.env$byInfo <- list()
      else 
        .st.env$byInfo$iter = .st.env$byInfo$iter + 1
    }
    
    # From here code applies no matter how function was called ---------------------------------
    skipvars <- FALSE # will be changed to TRUE if can't determine df.name
    
    # Following regular expressions allow to split dataframe name, row indexes and column indexes
    # re1: when form dfname[rows,columns] is used
    re1 <- paste0("(\\w+)\\s*",      # normally, dataframe name (1)
                  "\\[(.+)?",        # rows indexing  (2)
                  "\\s*(,)\\s*",     # comma surrounded (or not) by spaces (3)
                  "((c\\(.*\\))|",   # column indexing in the form [ , c(...)]  (4) (5)
                  "(\\d+)|",         # column indexing in the form [ , 9     ] (6)
                  "(\\d+\\:\\d+)|",  # column indexing in the form [ , 9:99  ] (7)
                  "(\'.*\')|",       # column indexing in the form [ , 'var.name' ] (8)
                  "(\".+\")|",       # column indexing in the form [ , 'var.name' ] (9)
                  "(\\\".+\\\"))?",  # column indexing in the form [ , "var.name" ] (10)
                  "\\s*\\]")         # end of indexing
    
    # re2: form dfname[[col]] is used
    re2 <- paste0("(\\w+)",                         # dataframe
                  "\\s*\\[\\[\\s*(.*)\\s*\\]\\]")   # variable number or name 
    
    # re3: form listname$dfname$column[rows]
    re3 <- paste0("(\\w+)\\s*\\$",       # list-type structure (1)
                  "\\s*(\\w+)\\s*\\$",     # data.frame name     (2)
                  "\\s*(\\w+)\\s*",        # variable name       (3)
                  "(\\[\\s*(.+)\\s*\\])?") # rows indexing (opt) (4)  
    
    # re4: form dfname$column[rows]
    re4 <- paste0("(\\w+)\\s*\\$",         # data.frame name     (1)
                  "\\s*(\\w+)\\s*",        # variable name       (2)
                  "(\\[\\s*(.+)\\s*\\])?") # rows indexing (opt) (3)  
    
    # Extract call as a string)
    if (exists("by_call")) {
      tmpstr <- deparse(by_call$data)
      allnames <- all.vars(by_call$data)
    } else {
      tmpstr <- deparse(mcall$x)
      allnames <- all.vars(mcall$x)
    }
    
    # Extract the dataset name if not already done
    if (length(output$df.name) == 0) {
      
      allnames.exist <- allnames[which(sapply(allnames, exists, USE.NAMES = FALSE))]
      
      if (length(allnames.exist) > 0) {
        if (length(allnames.exist) == 1 && is.data.frame(get(allnames.exist)))
          output$df.name <- allnames.exist
        else if (length(allnames.exist) > 1) {
          dfindex <- which(isTRUE(sapply(sapply(allnames.exist, get),is.data.frame)))
          if (length(dfindex) == 1 && is.data.frame(get(allnames.exist[dfindex])))
            output$df.name <- allnames.exist[dfindex]
        }
      }
      
      # If it fails, go the regex way
      if (length(output$df.name) == 0) {

        # remove what follows last '['
        tmpstr.df <- sub("(.+)\\[(.*)", "\\1", tmpstr, perl = TRUE)
        
        # check that structure exists and has proper class
        if (exists(tmpstr.df) && any(class(get(tmpstr.df)) == "data.frame"))
          output$df.name <- tmpstr.df
        else {
          getobj <- try(eval(parse(text=tmpstr.df)), silent = TRUE)
          if (inherits(getobj, "try-error"))
            skipvars <- TRUE
          else {
            if(is.data.frame(getobj)) {
              output$df.name <- tmpstr.df
              tmpvarnames <- colnames(getobj)
            } else if (grepl(re4, tmpstr)) {
              tmpstr.df <- sub(re3, "\\1$\\2", tmpstr)
              getobj <- try(eval(parse(text=tmpstr.df)), silent = TRUE)
              if (inherits(getobj, "try-error"))
                skipvars <- TRUE
              else {
                if(is.data.frame(getobj)) {
                  output$df.name <- tmpstr.df
                  tmpvarnames <- sub(re4, tmpstr, "\\3")
                }
              }
            }
          }
        }
      }
    }
    
    # Determine variables name(s) if not already done 
    if (length(output$var.names) == 0 && !skipvars) {
      if (exists("tmpvarnames") && !grepl(re3, tmpstr)) {
        output$var.names <- tmpvarnames
      } else if (grepl("^\\w+$", tmpstr)) {
        output$var.names <- colnames(eval(parse(text=tmpstr)))
      } else if (grepl(re1, tmpstr)) {
        # set aside row subsetting - it will be handled further down
        # (replacing for instance "dat[1:29,4]" with "dat[4]")
        tmpstr.var <- sub(re1,"\\1[\\4]", tmpstr, perl = TRUE)
        # remove brackets if subsetting is now null
        tmpstr.var <- sub("\\[\\s*\\,?\\s*\\]","", tmpstr.var)
        output$var.names <- colnames(eval(parse(text=tmpstr.var)))
      } else if (grepl(re2, tmpstr)) {
        # replace double by single brackets
        tmpstr.var <- sub("[[", "[", tmpstr, fixed = TRUE)
        tmpstr.var <- sub("]]", "]", tmpstr.var, fixed = TRUE)
        output$var.names <- colnames(eval(parse(text=tmpstr.var)))
      } else if (grepl(re3, tmpstr)) {
        output$var.names <- sub(re3, "\\3", tmpstr)
      } else if (grepl(re4, tmpstr)) {
        output$var.names <- sub(re4, "\\2", tmpstr)
      } 
      
      if (length(output$var.names) == 0)
        message("unable to identify var names: ", tmpstr)
    }
    
    # Rows subset
    if (length(output$rows.subset) == 0) {
      if(grepl(re1, tmpstr))
        output$rows.subset <- sub(re1, "\\2", tmpstr, perl = TRUE)
      else if (grepl(re2, tmpstr))
        output$rows.subset <- ""
      else if (grepl(re3, tmpstr))
        output$rows.subset <- sub(re3, "\\4", tmpstr, perl = TRUE)
      else if (grepl(re4, tmpstr))
        output$rows.subset <- sub(re4, "\\3", tmpstr, perl = TRUE)
    }
    
    if (length(output$rows.subset) == 0 || nchar(output$rows.subset) == 0 || 
        output$rows.subset == "NULL")
      output$rows.subset <- character()
    
    return(output[which(sapply(output, length) > 0)])
  }




# view is a wrapper function for print(x, "view"). Allows alternate "browser" or "pander" methods as well.
view <- function(x, method="viewer", include.footer=TRUE, silent=FALSE, ...) {
  print.summarytools(x, method=method, silent=silent, ...)
}


cleartmp <- function(what="last", silent=FALSE) {
  if(length(.st.env$tmpfiles) == 0) {
    message("No temporary files to delete.")
  } else if(what == "all") {
    nfiles <- 0
    for(tmpfile in .st.env$tmpfiles) {
      nfiles <- nfiles + 1
      if(!silent)
        message(paste('Deleting', tmpfile))
      unlink(tmpfile)
    }
    .st.env$tmpfiles <- c()
    if(!silent)
      message(paste(nfiles, "file(s) deleted"))
  } else if(what == "last") {
    tmpfile <- tail(.st.env$tmpfiles, 1)
    if(!silent)
      message(paste('Deleting', tmpfile))
    unlink(tmpfile)
    .st.env$tmpfiles <- .st.env$tmpfiles[-length(.st.env$tmpfiles)]
  } else {
    message("The 'what' argument can only take values 'last' (default) or 'all'")
  }
}
