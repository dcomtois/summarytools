#' Modify Keywords Used In Outputs
#'
#' As an alternative to \code{\link{use_custom_lang}}, this function allows
#' temporarily modifying the pre-defined terms in the outputs.
#'
#' @param \dots One or more pairs of keywords and their new values see 
#' \emph{Details} for the complete list of existing keywords.
#' @param ask Logical. When `TRUE` (default), a dialog box comes up to ask
#'   whether to save the edited values in a csv file for later use.
#' @param file Character. Path and name of custom language file to be saved.
#'   This comma delimited file can be reused by calling
#'   \code{\link{use_custom_lang}}.
#'
#' @details On systems with GUI capabilities, a window will pop-up when calling 
#' \code{define_keywords()} without any parameters, allowing the modification 
#' of the \emph{custom} column. The changes will be active as long as the
#' package is loaded. When the edit window is closed, a dialog will pop up,
#' prompting the user to save the modified set of keywords in a custom csv
#' language file that can later be used with \code{\link{use_custom_lang}}.
#' 
#' Here is the full list of modifiable keywords.
#'   
#' \describe{
#'   \item{title.freq}{main heading for \code{freq()}}
#'   \item{title.freq.weighted}{main heading for \code{freq()} (weighted)}
#'   \item{title.ctable}{main heading for \code{ctable()}}
#'   \item{title.ctable.weighted}{main heading \code{ctable()} (weighted)}
#'   \item{title.ctable.row}{indicates what proportions are displayed}
#'   \item{title.ctable.col}{indicates what proportions are displayed}
#'   \item{title.ctable.tot}{indicates what proportions are displayed}
#'   \item{title.descr}{main heading for \code{descr()}}
#'   \item{title.descr.weighted}{main heading for \code{descr()} (weighted)}
#'   \item{title.dfSummary}{main heading for \code{dfSummary()}}
#'   \item{n}{heading item used in \code{descr()}}
#'   \item{dimensions}{heading item used in \code{dfSummary()}}
#'   \item{duplicates}{heading item used in \code{dfSummary()}}
#'   \item{data.frame}{heading item (all functions)}
#'   \item{label}{heading item (all functions) & column name in \code{dfSummary()}}
#'   \item{variable}{heading item (all functions) & column name in \code{dfSummary()}}
#'   \item{group}{heading item (all functions when used with \code{stby()}}
#'   \item{by}{heading item for \code{descr()} when used with stby()}
#'   \item{weights}{heading item - \code{descr()} & \code{freq()}}
#'   \item{type}{heading item for \code{freq()}}
#'   \item{logical}{heading item - type in \code{freq()}}
#'   \item{character}{heading item - type in \code{freq()}}
#'   \item{numeric}{heading item - type in \code{freq()}}
#'   \item{factor}{heading item - type in \code{freq()}}
#'   \item{factor.ordered}{heading item - type in \code{freq()}}
#'   \item{date}{heading item - type in \code{freq()}}
#'   \item{datetime}{heading item - type in \code{freq()}}
#'   \item{freq}{column name in \code{freq()}}
#'   \item{pct}{column name in \code{freq()} when \code{report.nas=FALSE}}
#'   \item{pct.valid.f}{column name in \code{freq()}}
#'   \item{pct.valid.cum}{column name in \code{freq()}}
#'   \item{pct.total}{column name in \code{freq()}}
#'   \item{pct.total.cum}{column name in \code{freq()}}
#'   \item{pct.cum}{column name in \code{freq()}}
#'   \item{valid}{column name in \code{freq()} and \code{dfSummary()} & column content in \code{dfSummary()}}
#'   \item{invalid}{column content in \code{dfSummary()} (emails)}
#'   \item{total}{column grouping in \code{freq()}, html version}
#'   \item{mean}{row name in \code{descr()}}
#'   \item{sd.long}{row name in \code{descr()}}
#'   \item{sd}{cell content (dfSummary)}
#'   \item{min}{row name in \code{descr()}}
#'   \item{q1}{row name in \code{descr()} - 1st quartile}
#'   \item{med}{row name in \code{descr()}}
#'   \item{q3}{row name in \code{descr()} - 3rd quartile}
#'   \item{max}{row name in \code{descr()}}
#'   \item{mad}{row name in \code{descr()} - Median Absolute Deviation}
#'   \item{iqr}{row name in \code{descr()} - Inter-Quartile Range}
#'   \item{cv}{row name in \code{descr()} - Coefficient of Variation}
#'   \item{skewness}{row name in \code{descr()}}
#'   \item{se.skewness}{row name in \code{descr()} - Std. Error for Skewness}
#'   \item{kurtosis}{row name in \code{descr()}}
#'   \item{n.valid}{row name in \code{descr()} - Count of non-missing values}
#'   \item{pct.valid}{row name in \code{descr()} - pct. of non-missing values}
#'   \item{no}{column name in \code{dfSummary()} - position of column in the data frame}
#'   \item{stats.values}{column name in \code{dfSummary()}}
#'   \item{freqs.pct.valid}{column name in \code{dfSummary()}}
#'   \item{graph}{column name in \code{dfSummary()}}
#'   \item{missing}{column name in \code{dfSummary()}}
#'   \item{distinct.value}{cell content in \code{dfSummary()} - singular form}
#'   \item{distinct.values}{cell content in \code{dfSummary()} - plural form}
#'   \item{all.nas}{cell content in \code{dfSummary()} - column has only NAs}
#'   \item{all.empty.str}{cell content in \code{dfSummary()} - column has only empty strings}
#'   \item{all.empty.str.nas}{cell content in \code{dfSummary()} - col. has only NAs and empty strings}
#'   \item{no.levels.defined}{cell content in \code{dfSummary()} - factor has no levels defined}
#'   \item{int.sequence}{cell content in \code{dfSummary()}}
#'   \item{rounded}{cell content in \code{dfSummary()} - note appearing in Stats/Values}
#'   \item{others}{cell content in \code{dfSummary()} - nbr of values not displayed}
#'   \item{codes}{cell content in \code{dfSummary()} - When UPC codes are detected}
#'   \item{mode}{cell content in \code{dfSummary()} - mode = most frequent value}
#'   \item{med.short}{cell content in \code{dfSummary()} - median (shortened term)}
#'   \item{start}{cell content in \code{dfSummary()} - earliest date for date-type cols}
#'   \item{end}{cell content in \code{dfSummary()} - latest date for data-type cols}
#'   \item{emails}{cell content in \code{dfSummary()}}
#'   \item{generated.by}{footnote content}
#'   \item{version}{footnote content}
#'   \item{date.fmt}{footnote - date format (see \code{\link{strptime}})}
#' }
#' 
#' @note Setting a keyword starting with \dQuote{title.} to NA or to empty
#' string causes the main title to disappear altogether, which might be
#' desired in some circumstances (when generating a table of contents, for
#' instance).
#' 
#' @examples 
#' \dontrun{
#' define_keywords(n = "Nb. Obs.")
#' } 
#' 
#' @keywords utilities
#' @importFrom utils read.delim edit write.csv
#' @importFrom tcltk tclvalue tk_messageBox tkgetSaveFile
#' @importFrom checkmate check_path_for_output
#' @export
define_keywords <- function(..., ask = FALSE, file = NA) {
  mc <- match.call()
  kw <- names(mc[setdiff(names(mc), names(formals()))])[-1]
  if (length(kw) == 0 && !isTRUE(interactive())) {
    stop("R session not interactive; use arguments to define keywords, ",
         "or see ?use_custom_lang to use an external file to define all ",
         "keywords at once")
  }
  if (length(kw) == 0 && isFALSE(capabilities("tcltk"))) {
    message("Window dialogs not allowed; use arguments to ",
            "redefine specific keywords (see ?define_keywords), or turn to the ",
            "use_custom_lang() function which allows redefining all keywords at ",
            "once using a csv file")
    return(invisible())
  }
  
  if (st_options("lang") == "custom") {
    tr <- as.data.frame(t(.st_env$custom_lang), stringsAsFactors = FALSE)
  } else {
    tr <- as.data.frame(t(.translations[st_options("lang"),]), 
                        stringsAsFactors = FALSE)
  }
  
  tr <- merge(tr, .keywords_context, by = 0, sort = FALSE)
  class(tr$Row.names) <- "character"
  tr$item <- tr$Row.names
  tr$Row.names <- NULL
  tr <- tr[,c(3,1,2)]
  colnames(tr)[2] <- "custom"
  
  if (length(kw) == 0) {
    message("Instructions: \n  - Modify entries in the second column only \n",
            "  - leave column names unchanged\n",
            "  - expanding the window reveals context information stored in ",
            "third column\n",
            "  - Close the editing window when finished")
    tr.copy <- tr
    tr <- try(edit(tr), silent = TRUE)
    if (class(tr) == "try-error") {
      stop("Window dialogs not allowed; use arguments to ",
           "redefine specific keywords (see ?define_keywords), or turn to the ",
           "use_custom_lang() function which allows redefining all keywords at ",
           "once using a csv file")
    }
    if (identical(tr[[2]], tr.copy[[2]])) {
      message("No changes were registered")
      return(invisible())
    }
  } else {
    for (it in kw) {
      ind <- which(tr$item == it)
      if (length(ind) == 0) {
        stop("'", it, "' is not a recognized keyword; see ?define_keywords ",
             "for a list of valid keywords")
      }
      if (inherits(mc[[it]], c("call", "name"))) {
       mc[[it]] <- eval(mc[[it]], parent.frame())
      } 
      tr$custom[ind] <- mc[[it]]
    }
  }

  use_custom_lang(tr)

  if (!is.na(file)) {
    filename <- normalizePath(file, mustWork = FALSE)
    if (isTRUE(check_path_for_output(filename, overwrite = TRUE))) {
    write.csv(x = tr,
              file = filename,
              row.names = FALSE, 
              fileEncoding = "utf-8")
    message("Custom language file written: ", filename)
    } else {
      warning("file name or path is invalid. Custom language is in effect; ",
              "call define_keywords() without arguments to save file")
    }
  } else if (isTRUE(ask)) {
    filename <- ""
    filename_ok <- FALSE
    
    if (interactive() && length(kw) == 1) {
      
      if (isTRUE(capabilities("tcltk")) && isTRUE(ask)) {
        # tcltk capabilities: yes
        resp <- try(tk_messageBox(type = "yesno", 
                                  message = "Export language file for later use?",
                                  caption = "Keywords Successfully Updated"),
                    silent = TRUE)
        if (class(resp) == "try-error") {
          tcltk_error <- TRUE
        } else {
          if (resp == "yes") {
            while (!filename_ok) {
              filename <- tclvalue(
                tkgetSaveFile(initialfile = "custom_lang.csv", 
                              initialdir = "~",
                              filetypes = "{{csv files} {*.csv}}")
              )
              
              if (filename != "") {
                filename <- sub("(.csv)+$", "\\1", paste0(filename, ".csv"))
                filename <- normalizePath(filename, mustWork = FALSE)
                if (!isTRUE(check_path_for_output(filename, overwrite = TRUE))) {
                  rv <- tk_messageBox(
                    type = "okcancel", 
                    message = "Invalid file name or location"
                  )
                  if (rv == "cancel") {
                    filename <- ""
                    filename_ok <- TRUE
                  }
                } else {
                  # Filename is valid
                  filename_ok <- TRUE
                }
              } else {
                # dialog "Save as..." was cancelled
                filename_ok <- TRUE
              }
            }
          }
        }
      }
      
      # tcltk capabilities: no, or attempt failed
      if (isFALSE(capabilities("tcltk")) || exists("tcltk_error")) {
        resp <- " "
        while (!resp %in% c("Y", "N", "")) {
          resp <- toupper(
            readline(prompt = "Export language file for later use? [y/N] ")
          )
        }
        
        if (resp == "Y") {
          while (!filename_ok) {
            filename <- readline(prompt = "Full path to csv file (ESC to cancel): ")
            # Remove surrounding quotes if any
            filename <- sub('^"(.+)"$|^\'(.+)\'$', "\\1\\2", filename)
            if (filename != "" && 
                (!isTRUE(check_path_for_output(filename, overwrite = TRUE)) || 
                 !grepl("\\.csv$", filename))) {
              message("Invalid file location or extension (must be .csv)")
            } else {
              filename_ok <- TRUE # filename either "" or valid
            }
          }
        }
      }
    }
    
    if (filename != "") {
      write.csv(x = tr, file = filename, row.names = FALSE, 
                fileEncoding = "utf-8")
      message("Custom language file written: ", normalizePath(filename))
    }
  }
}
