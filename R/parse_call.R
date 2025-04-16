#' Extract Data Information From Arguments Passed to Functions (internal)
#'
#' Using sys.calls(), sys.frames() and match.call(), this utility function
#' extracts and/or infers information about the data being processed.
#' Data frame name, variable names and labels if any, subsetting information,
#' grouping information (when by() is used) are returned by the function which
#' tries various methods to get this information.
#'
#' @param var Character. \dQuote{x} (default) or \dQuote{y} (the latter 
#'   being used only in \code{\link{ctable}}).
#' @param df_name Logical.
#' @param df_label Logical.
#' @param var_name Logical.
#' @param var_label Logical.
# @param retain Logical. Flag to keep .p active (for iterative processing
#   managed by a st function.)
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
#' @author Dominic Comtois, \email{dominic.comtois@@gmail.com}
#'
#' @importFrom pryr standardise_call where
#' @importFrom utils head
parse_call <- function(mc,
                       var       = "x",
                       df_name   = TRUE,
                       df_label  = TRUE,
                       var_name  = TRUE,
                       var_label = TRUE,
                       caller    = "",
                       silent    = TRUE) {
  
  sc <- sys.calls()
  
  on.exit(expr = {
    if (.p$do_return) {
      rc <- is_recyclable()
      if (!rc) {
        rm(list = ls(envir = .p), envir = .p)
      }
    } else {
      warning(paste("metadata extraction terminated unexpectedly;",
                    "inspect results carefully"))
      cleanup_output()
      output <- c(.p$output, .p$by_info)
      if (!is_recyclable()) {
        rm(list = ls(envir = .p), envir = .p)
      }
      return(output)
    }
  })

  # Check if previous call's .p can be recycled (iteration)
  if (is_recyclable()) {
    incr_grp_iter()
    return(c(.p$output, .p$by_info))
  } else {
    # Rebuild the .p environment
    if (length(ls(envir = .p)) > 0) {
      rm(list = ls(envir = .p), envir = .p)
    }
    .p$call       <- sc[[1]] # As a basis of comparison for iterated calls
    .p$caller     <- caller
    .p$calls      <- list() # Will be updated later
    .p$do_return  <- FALSE  # Flag to see if there is premature exit
    .p$output     <- list() # Output list
    .p$pos        <- list() # Positions in the call stack
    .p$silent     <- silent # Set to TRUE for debugging / try() calls)
    .p$sf         <- sys.frames()
    .p$var        <- var    # value of 'var' argument ("x", "y"...)
    out_elem <- list(df_name = character(), df_label = character(),
                     var_name = character(), var_label = character())
    .p$output <- out_elem[as.logical(mget(names(out_elem)))]
  }
    
  # Get position of relevant calls in the stack
  pos         <- list()
  sc_head     <- lapply(sc, head, 1)
  
  pos$by      <- grep("^(st)?by\\(",       sc_head)
  pos$with    <- grep("^(base::)?with\\(", sc_head)
  pos$pipe    <- grep("^`%>%`\\(",  sc_head)
  pos$piper   <- grep("^`%>>%`\\(", sc_head)
  pos$expos   <- grep("^`%\\$%`\\(",sc_head)
  pos$lapply  <- grep("^lapply",    sc_head)
  pos$tapply  <- grep("^tapply",    sc_head)
  pos$fun     <- grep(paste0("^(summarytools::)?", caller), sc_head)[1]
  
  if (is.na(pos$fun)) {
    for (i in seq_along(sc)) {
      if (grepl(caller, paste0(deparse(sc[[i]]), collapse = ""))) {
        pos$fun <- i
        break
      }
    }
  }
  
  pos <- pos[which(lengths(pos) > 0)]
  
  # Keep only relevant calls
  calls <- list()
  for (i in seq_along(pos)) {
    calls[[names(pos)[i]]] <- sc[[pos[[i]]]]
  }
  
  # store pos and calls to .p
  .p$pos <- pos
  .p$calls <- calls
  
  # Call "subroutines"
  done <- FALSE
  for (p in names(pos)) {
    if (exists(f <- paste0("parse_", p), mode = "function"))
      done <- do.call(f, list())
    if (done) {
      cleanup_output()
      output <- c(.p$output, .p$by_info)
      return(output)
    }
  }
  
  # When all else fails, use deduction / elimination
  done <- deduce_names()
  if (!done)
    message("Some object names could not be found")
  cleanup_output()
  return(c(.p$output, .p$by_info))
}

# parse_fun() ---------------------------------------
# Call to the summarytools fn (freq, descr, ...)
parse_fun <- function()  {
  
  done <- FALSE
  call <- standardize(.p$calls$fun)
  
  if (length(.p$var) > 1) {
    done <- parse_data_str(deparse(call))
    return(done)
  }
  
  # case of get(...)  
  if (grepl("^get\\(", deparse(call[[.p$var]]))) {
    obj_ <- try(eval(call[[.p$var]]), silent = TRUE)
    if (is.data.frame(obj_)) {
      df_name <- dynGet(x = deparse(call[[.p$var]][[2]]),
                        ifnotfound = NULL, inherits = TRUE)
      if (is.character(df_name) && length(df_name) == 1L) { 
        done <- upd_output("df_name",  df_name)
        done <- upd_output("df_label", label(obj_) %||% NA_character_)
        if (done)  return(TRUE)
      }
    }
  }
  
  # Identify names in the fn call
  nms <- unique(c(all.names(call[[.p$var]]), as.character(call[[.p$var]])))
  nms <- setdiff(
    nms,
    c(.st_env$oper, "", "summarytools", "::", "TRUE", "FALSE", .p$caller)
    )
  nms <- grep(pattern = "::", x = nms, fixed = TRUE, invert = TRUE, value = TRUE)
  len <- length(nms)
  
  obj <- try(.p$sf[[.p$pos$fun]][[.p$var]], silent = TRUE)
  
  if (is.data.frame(obj)) {
    if (len == 1) {
      if (identical(call[[.p$var]], as.name("."))) {
        done <- upd_output("var_name", colnames(obj))
        if (ncol(obj) == 1)
          upd_output("var_label", label(obj[[1]]))
      } else {
        done <- upd_output("df_name",   deparse(call[[.p$var]]))
        done <- upd_output("df_label",  label(obj))
        done <- upd_output("var_name",  NA_character_)
        done <- upd_output("var_label", NA_character_)
        if (done)
          return(TRUE)
      }
    } else {
      # x is data.frame, and call[[.p$var]] > 1
      # possibly a native-pipe command.
      # iterate over names of the call and extract objects
      var_pos <- which(names(call) == .p$var) 
      obj_components <- list()
      var_candidates <- c()
      
      if (empty_na(.p$output$df_name)) {
        for (nm in nms) {
          obj_ <- dynGet(x = nm, ifnotfound = NULL, inherits = TRUE)
          
          if (inherits(obj_, c("data.frame", "list")))
            obj_components[[nm]] <- list(class   = class(obj_),
                                         content = ls(obj_),
                                         classes = lapply(obj_, class),
                                         label   = label(obj_),
                                         labels  = label(obj_, all = TRUE))
          
          else if (inherits(obj_, "matrix"))
            obj_components[[nm]] <- list(class   = class(obj_),
                                         content = colnames(obj_),
                                         classes = mode(obj_))
          
          else if (grepl("\\w", nm))
            var_candidates %+=% nm
        }
        
        if (length(var_candidates) > 0) {
          if (any(var_candidates %in% obj_components[[1]]$content)) {
            # Check whether the class of the obj_components[[1]] is
            # data.frame -- if not, the df might be in a list
            if ("data.frame" %in% obj_components[[1]]$class) {
              done <- upd_output("df_name",  names(obj_components)[1])
              done <- upd_output("df_label", 
                                 obj_components[[nm]]$label %||%
                                   NA_character_)
            } else if ("list" %in% obj_components[[1]]$class) {
              ind1 <- which(var_candidates %in% obj_components[[1]]$content)
              #ind2 <- which(obj_components[[1]]$content == var_candidates[ind1])
              obj_cl <- obj_components[[1]]$classes[[var_candidates[ind1]]]
              if ("data.frame" %in% obj_cl) {
                dfnm <- paste(names(obj_components)[[1]], var_candidates[ind1],
                              sep = "$")
                done <- upd_output("df_name", dfnm)
                if (is.data.frame(obj))
                  done <- upd_output("df_label", label(obj))
              }
            }
            
            if (done)  return(TRUE)
          }
            
          for (v in var_candidates) {
            if (v %in% obj_components[[1]]$content) {
              done <- upd_output("var_name", v)
              done <- upd_output("var_label",
                                 obj_components[[nm]]$labels[[v]] %||%
                                   NA_character_)
              if (done)  return(TRUE)
            }
          }
        }
      }
    }
    
    if (isTRUE(.p$do_return))
      return(TRUE)
    
  } else if (is.atomic(obj)) {
    
    if (len == 1) {
      if (all(c("x", "var") %in% names(call))) {
        if (deparse(call[[.p$var]]) != ".") {
          done <- upd_output("df_name", deparse(call[[.p$var]]))
        }
        try(upd_output("df_label", label(eval(call[[.p$var]]))),
            silent = .p$silent)
        if (length(call$var) == 1) {
          done <- upd_output("var_name", deparse(call$var))
          done <- upd_output("var_label", label(obj))
        }
      } else {
        if ("x" %in% names(standardize(call)))
        done <- upd_output("var_name",  deparse(call[[.p$var]]))
        done <- upd_output("var_label", label(obj))
        done <- upd_output("df_name",   NA_character_)
        done <- upd_output("df_label",  NA_character_)
      }
    } else if (len == 2L && !"var" %in% names(call)) {
      
      # Check whether 2nd name is a string var containing a column name
      obj <- try(get(nms[[1]]), silent = TRUE)
      if (inherits(x = obj, what = c("list", "data.frame"))) {
        done <- upd_output("df_name", nms[[1]])
        done <- upd_output("df_label", label(obj))
        
        if (!done && exists(nms[[2]])) {
          tmp_name <- get(nms[[2]])
          if (tmp_name %in% colnames(obj)) {
            done <- upd_output("var_name", tmp_name)
            done <- upd_output("var_label", label(obj[[tmp_name]]))
          }
        }
      }
    }
  }
  
  if (done)
    return(TRUE)
  done <- parse_data_str(deparse(call[[.p$var]]))
  return(done)
}
  
# parse_by() ----------------------------------------------
parse_by <- function() {
  done <- FALSE
  call <- standardize(.p$calls$by)
  populate_by_info(call)
  
  try(.p$calls$lapply <- standardize(.p$calls$lapply[-4]), silent = .p$silent)
  try(.p$calls$tapply <- standardize(.p$calls$tapply), silent = .p$silent)
  
  # treat special case of by() called on ctable, with ou without "with()"
  if (length(call$data) > 1 && deparse(call$data[[1]]) == "list" &&
      .p$caller == "ctable") {
    done <- by_ctable_case(call)
    if (done)  return(TRUE)
  } else {
    x <- .p$sf[[.p$pos$by]]$data
    if (is.data.frame(x)) {
      if (length(call$data) == 1) {
        done <- upd_output("df_name", deparse(call$data))
        done <- upd_output("df_label",
                           label(get_object(deparse(call$data), "data.frame")))
        if (done)  return(TRUE)
      } else {
        if (parse_data_str(deparse(call$data)))  return(TRUE)
      }
    } else if (is.atomic(x)) {
      if (length(call$data) == 1) {
        done <- upd_output("var_name", deparse(call$data))
        done <- upd_output("var_label", label(x))
        if (done)  return(TRUE)
        if (!"with" %in% names(.p$calls)) {
          done <- upd_output("df_name", NA_character_)
          done <- upd_output("df_label", NA_character_)
        }
      } else {
        x_str <- deparse(call$data)
        done <- parse_data_str(x_str)
      }
    }
  }
  return(done)
}

# parse_with() --------------------------------------------
parse_with <- function() {
  done <- FALSE
  call <- standardize(.p$calls$with)
  call$expr <- standardize(call$expr)
  
  x <- .p$sf[[.p$pos$with]]$data
  
  if (is.data.frame(x)) {
    if (length(call$data) == 1) {
      df_name <- deparse(call$data)
      if (df_name == "." && "expos" %in% names(.p$calls)) {
        .p$calls$expos <- standardize(.p$calls$expos)
        if (is.call(.p$calls$expos$rhs) && 
            identical(standardize(.p$calls$expos$rhs),
                      call$expr)) {
          df_name <- .p$calls$expos$lhs
          if (length(df_name) == 1) {
            done <- upd_output("df_name",  deparse(df_name))
          } else {
            done <- upd_output("df_name",  NA_character_)
          }
        }
      } else if (df_name == "." && "pipe" %in% names(.p$calls)) {
        call <- standardize(.p$calls$pipe)
        call$lhs <- standardize(call$lhs)
        df_name <- get_lhs(call$lhs)
        if (length(df_name) == 1) {
          done <- upd_output("df_name",  deparse(df_name))
        } else if (is.null(df_name)) {
          done <- upd_output("df_name", deparse(call$lhs))
        }
      } else {
        done <- upd_output("df_name", deparse(call$data))
      }
    } else {
      done <- upd_output("df_name", 
                         setdiff(as.character(call$data), .st_env$oper)[1])
    }
    done <- upd_output("df_label", label(x))
    if (done)  return(TRUE)
  }
  
  if ("x" %in% names(call$expr)) {
    if (is.call(call$expr$x)) {
      v_name <- c(x = deparse(standardize(call$expr$x)$x),
                  y = deparse(standardize(call$expr$x)$y))
    } else {
      v_name <- c(x = deparse(call$expr$x),
                  y = deparse(call$expr$y))
    }
    if (length(.p$var) == 1) {
      done <- upd_output("var_name",  v_name[[.p$var]])
      done <- upd_output("var_label", label(x[[v_name[[.p$var]]]]))
    } else {
      done <- upd_output("var_name", v_name, force = TRUE)
      done <- upd_output("var_label", NA_character_)
    }
  } else if ("data" %in% names(call$expr)) {
    call$expr$data <- standardize(call$expr$data)
    if ("x" %in% names(call$expr$data)) {
      v_name <- c(x = deparse(call$expr$data$x),
                  y = deparse(call$expr$data$y))
      if (length(.p$var) == 1) {
        done <- upd_output("var_name",  v_name[[.p$var]])
        done <- upd_output("var_label", label(x[[v_name[[.p$var]]]]))
      } else {
        done <- upd_output("var_name", v_name, force = TRUE)
        done <- upd_output("var_label", NA_character_)
      }
    }
  }
  return(done)
}

# parse_pipe() (magrittr's) --------------------------------
parse_pipe <- function() {
  
  done <- FALSE
  call <- standardize(.p$calls$pipe)
  
  obj_expr <- get_lhs(call)
  obj_name <- deparse(obj_expr)
  obj_str  <- as.character(obj_expr)
  
  # We must avoid entering in an infinite loop here if we have
  # e.g. freq(iris$Species) %>% print() ; in this case we dispatch
  # directly to parse_data_str, returning FALSE if unsuccessful
  if (length(obj_expr) > 1 && obj_expr[[1]] == .p$caller) {
    if (length(obj_expr$x) > 1 && deparse(obj_expr$x) != ".") {
      if (parse_data_str(deparse(obj_expr$x)))  return(TRUE)
    }
    return(FALSE)
  }
  
  obj <- eval(obj_expr,
              envir = .p$sf[[.p$pos$pipe]]$parent)
  
  if (is.data.frame(obj)) {
    obj_df <- obj
    if ("var_name" %in% names(.p$output) && ncol(obj_df) == 1) {
      done <- upd_output("var_name", colnames(obj_df))
      done <- upd_output("var_label", label(obj_df[[1]]))
      if (done)  return(TRUE)
    }
    
    done <- upd_output("df_label", as.character(label(obj_df)))
    if (length(obj_str) == 1) {
      done <- upd_output("df_name", obj_name)
      if (done)  return(TRUE)
    } else if (length(setdiff(obj_str, c(.p$caller, .st_env$oper))) == 1) {
      done <- upd_output("df_name", 
                         setdiff(obj_str, c(.p$caller, .st_env$oper)))
    } else if (length(setdiff(obj_str, c(.p$caller, .st_env$oper))) == 2) {
      done <- upd_output("df_name", setdiff(obj_str, .st_env$oper)[1])
      done <- upd_output("var_name", setdiff(obj_str, .st_env$oper)[2])
    }
  } else {
    done <- parse_data_str(obj_name)
    obj_df <- NULL
  }
  if (done)  return(TRUE)
  
  # Move focus to rhs
  if ("var_name" %in% names(.p$output)) {
    rhs <- call$rhs
    if (is.call(rhs))  
      rhs <- standardize(rhs)
    
    rhs_nms <- all.names(rhs)
    if (.p$caller %in% rhs_nms && length(rhs_nms) > 1) {
      rhs_args <- setdiff(rhs_nms, .p$caller)
      if (length(rhs_args) == 1) {
        if (rhs_args %in% colnames(obj_df)) {
          done <- upd_output("var_name", rhs_args)
          done <- upd_output("var_label", label(obj_df[[rhs_args]]))
          if (done)  return(TRUE)
        }
      } else {
        if (length(var_ind <- which(rhs_args %in% colnames(obj_df))) == 1) {
          var_name <- rhs_args[[var_ind]]
          done <- upd_output("var_name", var_name)
          done <- upd_output("var_label", label(obj_df[[var_name]]))
          if (done)  return(TRUE)
        }
      }
    }
    
    if (is.call(rhs)) {
      if (any(.p$var %in% names(rhs))) {
        vname <- c()
        labl <- c()
        for (i in seq_along(.p$var)) {
          if (.p$var[i] %in% names(rhs)) {
            vname <- c(vname, deparse(rhs[[.p$var[i]]]))
          }
        }
        done <- upd_output("var_name", vname)
        if (done)  return(TRUE)
      }
    }
  }
  return(FALSE)
}

# parse_piper() -------------------------------------------
parse_piper <- function() {
  done <- FALSE
  # Call recursive fn to hopefully get df_name
  call     <- standardize(.p$calls$piper)
  obj_expr <- get_last_x(call)
  obj_str  <- as.character(obj_expr)
  
  obj <- try(eval(obj_expr,
                  envir = .p$sf[[.p$pos$piper]]$envir),
             silent = .p$silent)

  if (is.data.frame(obj)) {
    done <- upd_output("df_label", label(obj))
    if (ncol(obj) == 1) {
      done <- upd_output("var_name", colnames(obj))
      done <- upd_output("var_label", label(obj[[1]]))
      if (done)  return(TRUE)
    }
    
    if (length(obj_str <- setdiff(obj_str, c(.p$caller, .st_env$oper))) == 1) {
      done <- upd_output("df_name", obj_str)
    } else if (length(obj_str) == 2) {
      done <- upd_output("df_name", obj_str[1])
      done <- upd_output("var_name", obj_str[2])
    }
    if (done)  return(TRUE)
  } else {
    # obj is not a df
    obj_str <- setdiff(obj_str, c(.p$caller, .st_env$oper, ""))
    if (length(obj_str) == 1) {
      done <- upd_output("var_name", obj_str)
      done <- try(upd_output("var_label", label(obj)), silent = TRUE)
    } else if (length(obj_str) == 2) {
      obj_df <- try(get_object(obj_str[1], "data.frame"),
                    silent = TRUE)
      if (inherits(obj_df, "try-error"))
        return(FALSE)
      done <- upd_output("df_name", obj_str[1])
      done <- upd_output("df_label", label(obj_df))
      if (done)  return(TRUE)
      if (grepl("[a-zA-Z]", obj_str[2])) {
        done <- upd_output("var_name", obj_str[2])
        done <- upd_output("var_label", label(obj_df[[obj_str[2]]]))
        if (done)  return(TRUE)
      }
      if (grepl("^\\d+L?$", obj_str[2])) {
        vnum <- as.integer(obj_str[2])
        if (vnum <= ncol(obj_df)) {
          done <- upd_output("var_name", colnames(obj_df)[vnum])
          done <- upd_output("var_label", label(obj_df[[vnum]]))
        }
      }
    }
  }
  return(done)
}

# parse_lapply() ------------------------------------------
parse_lapply <- function() {
  done <- FALSE
  call <- standardize(.p$calls$lapply)
  
  iter <- .p$sf[[.p$pos$lapply]]$i
  obj  <- .p$sf[[.p$pos$lapply]]$X[iter]
  
  if (is.atomic(obj[[1]])) {
    v_name <- names(obj)
    
    # Find the data frame
    df_nm <- setdiff(all.names(call$X), .st_env$oper)[1]
    df_     <- get_object(df_nm, "data.frame")
    if (identical(df_, NA)) {
      env <- try(pryr::where(df_nm))
      if (!inherits(env, "try-error")) {
        df_ <- get(df_nm, envir = env)
      }
    }
    if (is.data.frame(df_) && v_name %in% colnames(df_)) {
      done <- upd_output("var_name",  paste(df_nm, v_name, sep = "$"))
      done <- upd_output("var_label", label(df_[[v_name]]))
      done <- upd_output("df_name",   NA_character_)
      done <- upd_output("df_label",  NA_character_)
    }
  } else if (is.data.frame(obj[[1]])) {
    df_nm <- names(obj)
    done <- upd_output("df_name",   df_nm)
    done <- upd_output("df_label",  label(obj))
    done <- upd_output("var_name",  NA_character_)
    done <- upd_output("var_label", NA_character_)
  }
  return(done)
}
  
# deduce_names() ------------------------------------------
deduce_names <- function() {
  # Last strategy -- proceed by elimination from parent's match call
  # - eliminate objects that are functions
  # - hope to find a df or a variable that exists
  # - if there is a df, hope there is only one other object left
  nms <- setdiff(all.names(sys.calls()[[1]]), .p$caller)
  call <- standardize(sys.calls()[[1]])
  
  if (length(.p$var) == 1) {
    nms <- unique(c(nms, as.character(call[[.p$var]])))
  }
  
  nnames <- length(nms)
  df_found <- !empty_na(.p$output$df_name)
  
  if (df_found)
    obj_df <- get(.p$output$df_name)

  candidates <- c()
  cand_class <- c()
  
  for (nm in nms) {
    obj_ <- try(get(nm), silent = .p$silent)
    if (inherits(obj_, "try-error")) {
      if (df_found) {
        obj_ <- try(obj_df[[nm]], silent = .p$silent)
        if (!inherits(obj_, "try-error") && !is.null(obj_)) {
          candidates %+=% c(tested = nm)
          cand_class %+=% class(obj_)[1]
          names(cand_class)[length(cand_class)] <- nm
        }
      } else candidates %+=% c(untested = nm)
    } else if (is.data.frame(obj_)) {
      if (isFALSE(df_found)) {
        df_found <- TRUE
        obj_df <- obj_
        done <- upd_output("df_name", nm)
        done <- upd_output("df_label", label(obj_df))
        if (done)  return(TRUE)
        nnames <- nnames - 1
      } else {
        # We had already found df_name, so we'll simply ignore it
        nnames <- nnames - 1
      }
    } else if (inherits(obj_, "function")) {
      nnames <- nnames - 1
    } else if (is.atomic(obj_)) {
      candidates %+=% c(tested = nm)
      cand_class %+=% class(obj_)[1]
      names(cand_class)[length(cand_class)] <- nm
    } else if (is.list(obj_)) {
      candidates %+=% c(tested = nm)
      cand_class %+=% class(obj_)[1]
    }
  }
  
  # Work with what's left (tested and/or untested)
  if (length(candidates) == 1 && names(candidates) == "tested") {
    if (cand_class[[1]] != "data.frame") {
      done <- upd_output("var_name", candidates[[1]])
      if (df_found)
        done <- upd_output("var_label", label(obj_df[[candidates[[1]]]]))
      else
        done <- upd_output("var_label", label(get(candidates[[1]])))
      if (done)  return(TRUE)
    }
  } else {
    if ("untested" %in% names(candidates)) {
      for (i in which(names(candidates) == 'untested')) {
        obj_ <- try(obj_df[[candidates[[i]]]], silent = .p$silent)
        if (inherits(obj_, "try-error") || is.null(obj_))
          names(candidates)[i] <- "discarded"
        else
          names(candidates)[i] <- "tested"
      }
    }
    
    # If there is only 1 tested, we keep it
    n_tested <- table(names(candidates))[['tested']]
    if (n_tested == 1) {
      var_name <- candidates[['tested']]
      done <- upd_output("var_name", var_name)
      done <- upd_output("var_label", label(obj_df[[var_name]]))
      return(TRUE)
    } else {
      # More than one variable -- hopefully ctable
      if (n_tested == 2 && length(.p$var) == 2) {
        done <- upd_output(
          "var_name",
          unname(candidates[names(candidates) == "tested"]),
          force = TRUE
        )
      }
    }
  }
  if (done)  return(TRUE)
  
  # Set .p$do_return to TRUE to avoid warning (although there will be a msg)
  .p$do_return <- TRUE
  return(FALSE)
}

## by_ctable_case() ---------------------------------------
by_ctable_case <- function(call) {
  done <- FALSE
  if (!is.null(names(call$data))) {
    v_name <- c(deparse(call$data$x), deparse(call$data$y))
  } else {
    v_name <- as.character(call$data[2:3])
  }
  
  if (!"with" %in% names(.p$pos)) {
    if (any(grepl("\\$", v_name))) {
      df_nm    <- sub("^([\\w._]+)\\$.+$", "\\1", v_name[1], perl = TRUE)
      df_nm[2] <- sub("^([\\w._]+)\\$.+$", "\\1", v_name[2], perl = TRUE)
    }
    
    if (isTRUE(df_nm[1] == df_nm[2])) {
      df_nm <- df_nm[1]
      df_ <- get_object(df_nm, "data.frame")
      if (!identical(df_, NA)) {
        done <- upd_output("df_name", df_nm)
        done <- upd_output("df_label", label(df_))
      }
    } else {
      done <- upd_output("df_name", NA_character_)
      done <- upd_output("df_label", NA_character_)
    }
  }
  done <- upd_output("var_name", v_name, force = TRUE)
  return(done)
}

# Utility functions ---------------------------------------
## upd_output() -------------------------------------------
upd_output <- function(item, value, force = FALSE) {
  
  if (isTRUE(force) || 
      ( (length(.p$output[[item]]) == 0 || is.na(.p$output[[item]])) &&
         length(value) == 1 && class(value) == class(.p$output[[item]])) ) {
    
    names(value) <- NULL
    if (sum(value %in% c(".", NA)) == length(value))
      value <- NA_character_
    
    .p$output[[item]] <- value
    
    # Check if output is ready to be returned
    if (count_empty(.p$output, count.nas = FALSE) == 0)
      .p$do_return <- TRUE
  }
  return(.p$do_return)
}

# parse_data_str() ----------------------------------------
parse_data_str <- function(str) {
  if (grepl(.st_env$re$two_names, str, perl = TRUE)) {
    df_nm <- sub(.st_env$re$two_names, "\\1", str, perl = TRUE)
    df_   <- get_object(df_nm, "data.frame")
    if (!identical(df_, NA)) {
      done <- upd_output("df_name", df_nm)
      done <- upd_output("df_label", label(df_))
      if (done)  return(TRUE)
      
      v_name <- sub(.st_env$re$two_names, "\\4", str, perl = TRUE)
      if (v_name %in% colnames(df_)) {
        done <- upd_output("var_name", v_name)
        done <- upd_output("var_label", label(df_[[v_name]]))
        if (done)  return(TRUE)
      }
    }
  } else if (grepl(.st_env$re$num_index, str, perl = TRUE)) {
    df_nm <- sub(.st_env$re$num_index, "\\1", str, perl = TRUE)
    df_   <- get_object(df_nm, "data.frame")
    if (!identical(df_, NA)) {
      done <- upd_output("df_name",  df_nm)
      done <- upd_output("df_label", label(df_))
      if (done)  return(TRUE)
      
      var_number <- as.numeric(sub(.st_env$re$num_index, "\\4", str,
                                   perl = TRUE))
      done <- upd_output("var_name",  colnames(df_)[var_number])
      done <- upd_output("var_label", label(df_[[var_number]]))
      if (done)  return(TRUE)
    } else {
      # Possibly a list containing data frame(s)
      # Try to get item name
      obj_ <- get_object(df_nm, "list")
      if (is.list(obj_)) {
        list_nm <- df_nm
        # get the index (number)
        ind <- sub(.st_env$re$num_index, "\\4", str, perl = TRUE)
        df_nm <- eval(parse(text = paste0("names(", list_nm, "[", ind, "])")))
        if (df_nm != "") {
          done <- upd_output("df_name",  paste(list_nm, df_nm, sep = "$"))
          done <- upd_output("df_label", label(obj_[as.integer(ind)]))
        } else {
          # No name for the list element
          done <- upd_output("df_name", str)
          done <- upd_output("df_label", label(obj_[as.integer(ind)]))
        }
        if (done)  return(TRUE)
      }
    }
  } else if (grepl(.st_env$re$neg_num_index, str, perl = TRUE)) {
    df_nm <- sub(.st_env$re$neg_num_index, "\\1", str, perl = TRUE)
    df_   <- get_object(df_nm, "data.frame")
    if (!identical(df_, NA)) {
      done <- upd_output("df_name", df_nm)
      done <- upd_output("df_label", label(df_))
      if (done)  return(TRUE)
    }
    
    neg_index <- as.numeric(sub(.st_env$re$neg_num_index, "\\4", str,
                                perl = TRUE))
    # Check whether there is only 1 other variable
    if (ncol(df_) == 2) {
      upd_output("var_name", colnames(df_)[neg_index])
      upd_output("var_label", label(df_[,neg_index]))
    } else {
      upd_output("var_name", NA_character_)
      upd_output("var_label", NA_character_)
    }
    return(TRUE)
    
  } else if (grepl(.st_env$re$fallback_1name, str, perl = TRUE)) {
    obj_name <- sub(.st_env$re$fallback_1name, "\\1", str, perl = TRUE)
    obj_env  <- try(pryr::where(obj_name), silent = .p$silent)
    if (!inherits(obj_env, "try-error")) {
      obj <- get(obj_name, envir = obj_env)
      if (is.data.frame(obj)) {
        done <- upd_output("df_name", obj_name)
        done <- upd_output("df_label", label(obj))
        if (done)  return(TRUE)
        
        if ("var_name" %in% names(.p$output)) {
          obj2_name <- sub(.st_env$re$fallback_1name, "\\2", str,
                           perl = TRUE)
          obj2      <- try(eval(parse(text = obj2_name), envir = obj), 
                           silent = .p$silent)
          if (!inherits(obj2, "try-error") && is.atomic(obj2)) {
            done <- upd_output("var_name",  obj2_name)
            done <- upd_output("var_label", NA_character_)
            if (done)  return(TRUE)
          }
        }
      } else if (is.atomic(obj)) {
        done <- upd_output("var_name", obj_name)
        done <- upd_output("var_label", label(obj))
        if (done)  return(TRUE)
      } else {
        if (is.function(obj)) {
          # Most probably something like descr(rnorm(10)) 
          # First, confirm that function is a summarytools fn
          if (!grepl("summarytools", 
                     capture.output(pryr::where(obj_name))[1])) {
            # See if only one of var_name & df_name is required, and
            # use that slot and return
            name_slots <- grep("_name", names(.p$output), value = TRUE)
            if (length(name_slots) == 1) {
              upd_output(name_slots, str, force = TRUE)
              .p$do_return <- TRUE
              return(TRUE)
            } else {
              # Get first element of evaluated str to determine which
              # slot to use
              if (is.data.frame(eval(str2expression(str))[1]))
                upd_output("df_name", str, force = TRUE)
              else
                upd_output("var_name", str, force = TRUE)
              .p$do_return <- TRUE
              return(TRUE)
            }
          }
        }
      }
    }
  }
  return(FALSE)
}

## populate_by_info() -------------------------------------
populate_by_info <- function(call) {
  # on avait by_levels et iter dans .st_env$byInfo, le reste dans output
  # by_levels est devenu groups (+/- ... groups est une nouveauté pour les
  # stby / by), et iter est pour parse_call seul
  .p$by_info <- list(by_var   = character(),
                     by_group = character(),
                     by_first = logical(),
                     by_last  = logical(),
                     groups   = data.frame())
  
  .p$by_iter <- 0L # will not be part of output, keep it separate
  
  # More than one group var
  if ("list" %in% all.names(call$INDICES)) {
    by_var <- character()
    for (i in seq_len(length(call$INDICES) - 1)) {
      by_var[i]  <- deparse(call$INDICES[[i + 1]])
    }
  } else {
    by_var <- deparse(call$INDICES)
  }
  
  .p$by_info$by_var <- by_var
  
  # Normalize / validate variable name  
  for (i in seq_along(by_var)) {
    if (grepl(.st_env$re$two_names, by_var[i], perl = TRUE)) {
      df_nm <- sub(.st_env$re$two_names, "\\1", by_var[i], perl = TRUE)
      df_   <- get_object(df_nm, "data.frame")
      if (!identical(df_, NA)) {
        v_name <- sub(.st_env$re$two_names, "\\4", by_var[i], perl = TRUE)
        if (v_name %in% colnames(df_)) {
          .p$by_info$by_var[i] <- paste(df_nm, v_name, sep = "$")
        }
      }
    } else if (grepl(.st_env$re$num_index, by_var[i], perl = TRUE)) {
      df_nm <- sub(.st_env$re$num_index, "\\1", by_var[i], perl = TRUE)
      df_   <- get_object(df_nm, "data.frame")
      if (!identical(df_, NA)) {
        var_number <- as.numeric(sub(.st_env$re$num_index, "\\4", by_var[i], 
                                     perl = TRUE))
        v_name <- colnames(df_)[var_number]
        .p$by_info$by_var[i] <- paste(df_nm, v_name, sep = "$")
      }
    }
  }
  
  # On first iteration, generate levels based on IND variables, and store
  # groups df in output
  if (.p$by_iter == 0) {
      names(.p$sf[[.p$pos$tapply]]$namelist) <- .p$by_info$by_var
    by_levels <- .p$sf[[.p$pos$tapply]]$namelist
    groups <- expand.grid(by_levels, stringsAsFactors = FALSE)
    colnames(groups) <- .p$by_info$by_var
    
    # update directly to avoid adding a layer of complexity to checks 
    .p$by_info$groups <- groups
    
    return(incr_grp_iter())
  } else {
    warning(paste("problem encountered while extracting metadata;",
                  "heading or group information could be missing or incorrect;",
                  "inspect results carefully"))
    return(incr_grp_iter())
  }
}

## incr_grp_iter() ----------------------------------------
incr_grp_iter <- function() {
  
  # st "internal" iteration in progress (e.g. descr.grouped_df)
  # we only keep track of the number of retains left
  # if (!is.null(.p$n_retain)) {
  #   .p$n_retain <- .p$n_retain - 1L
  #   return(.p$n_retain == 0L) # ret val not used for now
  # }
  
  .p$by_iter <- .p$by_iter + 1L
  
  # Populate by_group item
  .p$by_info$by_group <-
    paste(colnames(.p$by_info$groups),
          as.character(.p$by_info$groups[.p$by_iter, ]),
          sep = " = ", collapse = ", ")
  
  .p$by_info$by_first <- .p$by_iter == 1L
  .p$by_info$by_last  <- .p$by_iter == nrow(.p$by_info$groups)
  
  if (isTRUE(.p$by_info$by_last)) {
    #.p$by_iter <- integer()
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## cleanup_output() ---------------------------------------
# remove redundant df_name and remove NA / empty elements
cleanup_output <- function() {
  
  # re to remove redundant df_name
  re <- paste0("^",.p$output$df_name, # starts with df_name
               "[$[]*['\"]?",         # subsetting chars
               "([\\w_.]+)",          # var_name (group 1)
               "['\"]?\\]{0,2}",      # closing subsetting char(s)
               "(.*)$")               # remainder of expression (gr. 2)
  
  # Remove (redundant) df_name
  if (length(.p$output$df_name) == 1 && !is.na(.p$output$df_name)) {
    
    if ("var_name" %in% names(.p$output))
      .p$output$var_name <- sub(pattern = re,
                                replacement = "\\1\\2", 
                                x = .p$output$var_name, perl = TRUE)
    
    if ("by_info" %in% names(.p)) {
      .p$by_info$by_var   <- gsub(paste0(.p$output$df_name, "\\$"), "", 
                                  .p$by_info$by_var)
      .p$by_info$by_group <- gsub(paste0(.p$output$df_name, "\\$"), "", 
                                  .p$by_info$by_group)
      colnames(.p$by_info$groups) <- .p$by_info$by_var
    }
  }
  
  if ("by_info" %in% names(.p)) {
    # Remove quotation marks
    .p$by_info$by_var   <- gsub("['\"]", "", .p$by_info$by_var)
    .p$by_info$by_group <- gsub("['\"]", "", .p$by_info$by_group)
    colnames(.p$by_info$groups) <- .p$by_info$by_var
    
    # Special case where by_var has a function call, e.g.
    # x > mean(x) -- we want to remove "x = "
    # For now, we only deal with 1 grouping variable
    #TODO: Add support for more grouping variables
    re_fn <- "(.+\\b([\\w.]+)\\()(.+?) = (.+)$"
    if (length(.p$by_info$by_var) == 1 &&
        grepl(re_fn, .p$by_info$by_var, perl = TRUE)) {
      # check that it is indeed a function call
      f_nm <- sub(re_fn, "\\2", .p$by_info$by_var, perl = TRUE)
      arg_nm <- sub(re_fn, "\\3", .p$by_info$by_var, perl = TRUE)
      # check that arg_nm is the first parameter in the fn
      if (exists(f_nm, mode = "function") &&
          names(formals(f_nm))[1] == arg_nm) {
        .p$by_info$by_var <- 
          sub(re_fn, "\\1\\4", .p$by_info$by_var, perl = TRUE)
        .p$by_info$by_group <- 
          sub(re_fn, "\\1\\4", .p$by_info$by_group, perl = TRUE)
        colnames(.p$by_info$groups) <- .p$by_info$by_var
      }
    }
  }

  # Remove quotation marks from df/var name
  for (item in grep("_name", names(.p$output), value = TRUE))
    .p$output[[item]] <- gsub("['\"]", "", .p$output[[item]])
  
  .p$output <- .p$output[which(!empty_na(.p$output))]
}

# Miscellaneous -------------------------------------------
is_recyclable <- function() {
  #isTRUE(.p$n_retain > 0) ||
    ("call" %in% names(.p) &&
    identical(.p$call, sys.calls()[[1]]) &&
    any(c("by", "lapply", "tapply") %in% names(.p$pos)) &&
    length(.p$output) > 0 &&
    all(c("by_var", "by_first", "by_last", "by_group")
        %in% names(.p$by_info)) &&
    #!empty_na(.p$by_iter) &&
    .p$by_iter < nrow(.p$by_info$groups))
}

# When pipe is used, this recursive function gets the "deepest" lhs
# that constitutes something other than a function call
get_lhs <- function(x) {
  if (!is.null(names(x)) && "lhs" %in% names(x) && is.call(x$lhs)) {
    x$lhs <- standardize(x$lhs)
    return(get_lhs(x$lhs))
  } else if (!is.null(names(x)) && "lhs" %in% names(x)) {
    return(x$lhs)
  } else {
    return(x)
  }
}

# Recursively seek "x" (piped calls)
get_last_x <- function(expr) {
  if (is.call(expr) && "x" %in% names(standardize(expr))) {
    get_last_x(standardize(expr)$x)
  } else {
    return(expr)
  }
}

get_object <- function(name, class) {
  for (i in seq_along(.p$sf)) {
    if (name %in% ls(.p$sf[[i]])) {
      if (inherits(.p$sf[[i]][[name]], class)) {
        return(.p$sf[[i]][[name]])
      }
    }
  }
  
  # fallback method 1
  env <- try(pryr::where(name = name), silent = TRUE)
  if (!inherits(env, "try-error")) {
    obj <- get(name, env, mode = "list")
    if (inherits(obj, class))
      return(obj)
  }
  
  # fallback method 2
  obj <- dynGet(x = name, inherits = TRUE, ifnotfound = NA)
  if (inherits(obj, class))
    return(obj)
  
  return(NA)
}
