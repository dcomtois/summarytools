#' Query and set summarytools global options
#'
#' To list all \code{summarytools} global options, call without arguments. To
#' display the value of one or several options, enter the name(s) of the
#' option(s) in a character vector as sole argument. To \strong{reset} all 
#' options, use single unnamed argument \sQuote{reset} or \code{0}.
#'
#' @param option option(s) name(s) to query (optional). Can be a single string
#'   or a vector of strings to query multiple values.
#' @param value The value you wish to assign to the option specified in the
#'   first argument. This is for backward-compatibility, as all options can now 
#'   be set via their own parameter. That is, instead of 
#'   \code{st_options('plain.ascii', FALSE))}, use
#'   \code{st_options(plain.ascii = FALSE)}.
#' @param style Character. One of \dQuote{simple} (default), \dQuote{rmarkdown},
#'   or \dQuote{grid}. Does not apply to \code{\link{dfSummary}}.
#' @param plain.ascii Logical. \code{\link[pander]{pander}} argument; when
#'   \code{TRUE}, no markup characters will be used (useful when printing to
#'   console). \code{TRUE} by default, but when \code{style = 'rmarkdown'},
#'   it is automatically set to \code{FALSE}. To override this behavior,
#'   \code{plain.ascii = TRUE} must be specified in the function call.
#' @param round.digits Numeric. Defaults to \code{2}.
#' @param headings Logical. Set to \code{FALSE} to remove all headings from
#'   outputs. Only the tables will be printed out, except when \code{\link{by}}
#'   or \code{\link{lapply}} are used. In that case, the variable or the group
#'   will still appear before each table. \code{TRUE} by default.
#' @param footnote Character. When the default value \dQuote{default} is used,
#'   the package name & version, as well as the R version number are displayed
#'   below \emph{html} outputs. Set no \code{NA} to omit the footnote, or 
#'   provide a custom string. Applies only to \emph{html} outputs.
#' @param display.labels Logical. \code{TRUE} by default. Set to \code{FALSE} to
#'   omit data frame and variable labels in the headings section.
#' @param na.val Character. For factors and character vectors, consider this
#'   value as \code{NA}. Ignored if there are actual NA values or if it matches
#'   no value / factor level in the data. \code{NULL} by default.
#' @param bootstrap.css Logical. Specifies whether to include 
#'   \emph{Bootstrap css} in \emph{html} reports' \emph{head} section.
#'   Defaults to \code{TRUE}. Set to \code{FALSE} when using the \dQuote{render}
#'   method inside a \code{shiny} app to avoid interacting with the app's 
#'   layout.
#' @param custom.css Character. Path to an additional, user-provided, CSS file.
#'   \code{NA} by default.
#' @param escape.pipe Logical. Set to \code{TRUE} if Pandoc conversion is your
#'   goal and you have unsatisfying results with grid or multiline tables.
#'   \code{FALSE} by default.
#' @param char.split Numeric. Maximum number of characters allowed in a column
#'   heading for \code{\link{descr}} and \code{\link{ctable}} \emph{html} 
#'   outputs. Any variable name having more than this number of characters
#'   will be split on two or more lines. Defaults to 12.
#' @param freq.cumul Logical. Corresponds to the \code{cumul} parameter of
#'   \code{\link{freq}}. \code{TRUE} by default.
#' @param freq.totals Logical. Corresponds to the \code{totals} parameter of
#'   \code{\link{freq}}. \code{TRUE} by default.
#' @param freq.report.nas Logical. Corresponds to the \code{display.nas}
#'   parameter of \code{\link{freq}}. \code{TRUE} by default.
#' @param freq.ignore.threshold Numeric. Number of distinct values above which
#'   numerical variables are ignored when calling \code{\link{freq}} with a
#'   whole data frame as main argument. Defaults to \code{25}.
#' @param freq.silent Logical. Hide console messages. \code{FALSE} by default.
#' @param ctable.prop Character. Corresponds to the \code{prop} parameter of
#'   \code{\link{ctable}}. Defaults to \dQuote{r} (\emph{r}ow).
#' @param ctable.totals Logical. Corresponds to the \code{totals} parameter of
#'   \code{\link{ctable}}. \code{TRUE} by default.
#' @param ctable.round.digits Numeric. Defaults to \code{1}.
#' @param ctable.silent Logical. Hide console messages. \code{FALSE} by default.
#' @param descr.stats Character. Corresponds to the \code{stats} parameter of
#'   \code{\link{descr}}. Defaults to \dQuote{all}.
#' @param descr.transpose Logical. Corresponds to the \code{transpose} parameter
#'   of \code{\link{descr}}. \code{FALSE} by default.
#' @param descr.silent Logical. Hide console messages. \code{FALSE} by default.
#' @param dfSummary.style Character. \dQuote{multiline} by default. Set to 
#'   \dQuote{grid} for \emph{R Markdown} documents.
#' @param dfSummary.varnumbers Logical. In \code{\link{dfSummary}}, display
#'   variable numbers in the first column. Defaults to \code{TRUE}.
#' @param dfSummary.class Logical. Show data classes in Name column.
#'   \code{TRUE} by default.
#'   variable numbers in the first column. Defaults to \code{TRUE}.
#' @param dfSummary.labels.col Logical. In \code{\link{dfSummary}}, display
#'   variable labels Defaults to \code{TRUE}.
#' @param dfSummary.valid.col Logical. In \code{\link{dfSummary}}, include
#'   column indicating count and proportion of valid (non-missing). \code{TRUE}
#'   by default.
#' @param dfSummary.na.col Logical. In \code{\link{dfSummary}}, include column
#'   indicating count and proportion of missing (NA) values. \code{TRUE} by
#'   default.
#' @param dfSummary.graph.col Logical. Display barplots / histograms column in
#'   \code{\link{dfSummary}} \emph{html} reports. \code{TRUE} by default.
#' @param dfSummary.graph.magnif Numeric. Magnification factor, useful if
#'   \code{\link{dfSummary}} graphs show up too large (then use a value between
#'   0 and 1) or too small (use a value > 1). Must be positive. Default to
#'   \code{1}.
#' @param dfSummary.silent Logical. Hide console messages. \code{FALSE} by 
#'   default.
#' @param dfSummary.custom.1 Expression. First of two optional expressions
#'   which once evaluated will populate lines 3+ of the `Stats / Values` 
#'   cell when column data is numerical and has more distinct values than 
#'   allowed by the \code{max.distinct.values} parameter. By default, it
#'   contains the expression which generates the `IQR (CV) : ...` line. To reset it back to
#'   this default value, use \code{st_options(dfSummary.custom.1 = "default")}.
#'   See \emph{Details} and \emph{Examples} sections for more.
#' @param dfSummary.custom.2 Expression. Second the two optional expressions
#'   which once evaluated will populate lines 3+ of the `Stats / Values` 
#'   cell when the column data is numerical and has more distinct values than 
#'   allowed by the `max.distinct.values` parameter. \code{NA} by default.
#'   See \emph{Details} and \emph{Examples} sections for more.
#' @param tmp.img.dir Character. Directory used to store temporary images. See
#'   \emph{Details} section of \code{\link{dfSummary}}. \code{NA} by default.
#' @param subtitle.emphasis Logical. Controls the formatting of the 
#'  \dQuote{subtitle} (the \emph{data frame} or \emph{variable} name, depending 
#'  on context. When \code{TRUE} (default), \dQuote{h4} is used, while with
#'  \code{FALSE}, \dQuote{bold} / \dQuote{strong} is used. Hence the default
#'  value gives it stronger emphasis.
#' @param lang Character. A 2-letter code for the language to use in the
#'   produced outputs. Currently available languages are: \sQuote{en}, 
#'   \sQuote{es}, \sQuote{fr}, \sQuote{pt}, \sQuote{ru}, and \sQuote{tr}.
#' @param use.x11 Logical. TRUE by default. In console-only environments,
#'   setting this to \code{FALSE} will prevent errors occurring when
#'   \code{\link{dfSummary}}  tries to generate \emph{html} 
#'   \dQuote{Base64-encoded} graphs.
#' 
#' @details The \code{dfSummary.custom.1} and \code{dfSummary.custom.2} options
#'   must be defined as expressions. In the expression, use the
#'   \code{culumn_data} variable name to refer to data. Assume the type to be
#'   numerical (real or integer). The expression must paste together both the
#'   labels (short name for the statistic(s) being displayed) and the 
#'   statistics themselves. Although \code{\link[base]{round}} can be used, a
#'   better alternative is to call the internal \code{\link{format_number}},
#'   which uses \code{\link[base]{format}} to apply all relevant formatting
#'   that is active within the call to \code{\link{dfSummary}}. For keywords
#'   having a translated term, the \code{trs()} internal function can be
#'   used (see \emph{Examples}). 
#'   
#' @examples
#' 
#' # show all summarytools global options
#' st_options()
#' 
#' # show a specific option
#' st_options("round.digits")
#' 
#' # show two (or more) options
#' st_options(c("plain.ascii", "style", "footnote"))
#' 
#' \dontrun{
#' # set one option
#' st_options(plain.ascii = FALSE)
#' 
#' # set one options, legacy way
#' st_options("plain.ascii", FALSE)
#' 
#' # set several options
#' st_options(plain.ascii = FALSE,
#'            style       = "rmarkdown",
#'            footnote    = NA)
#'
#' # reset all
#' st_options('reset')
#' # ... or
#' st_options(0)
#' 
#' # Define custom dfSummary stats
#' st_options(dfSummary.custom.1 = expression(
#'   paste(
#'     "Q1 - Q3 :",
#'     format_number(
#'       quantile(column_data, probs = .25, type = 2, 
#'                names = FALSE, na.rm = TRUE), round.digits
#'     ),
#'     "-",
#'     format_number(
#'       quantile(column_data, probs = .75, type = 2, 
#'                names = FALSE, na.rm = TRUE), round.digits
#'     ),
#'     collapse = ""
#'   )
#' ))
#' 
#' dfSummary(iris)
#' 
#' # Set back to default value
#' st_options(dfSummary.custom.1 = "default")
#' }
#'  
#' @note 
#' To learn more about summarytools options, see
#' \code{vignette("introduction", "summarytools")}.
#' 
#' @keywords utilities
#' 
#' 
#' @export
st_options <- function(option                 = NULL,
                       value                  = NULL,
                       style                  = "simple",
                       plain.ascii            = TRUE,
                       round.digits           = 2,
                       headings               = TRUE,
                       footnote               = "default",
                       display.labels         = TRUE,
                       na.val                 = NULL,
                       bootstrap.css          = TRUE,
                       custom.css             = NA_character_,
                       escape.pipe            = FALSE,
                       char.split             = 12,
                       freq.cumul             = TRUE,
                       freq.totals            = TRUE,
                       freq.report.nas        = TRUE,
                       freq.ignore.threshold  = 25,
                       freq.silent            = FALSE,
                       ctable.prop            = "r",
                       ctable.totals          = TRUE,
                       ctable.round.digits    = 1,
                       ctable.silent          = FALSE,
                       descr.stats            = "all",
                       descr.transpose        = FALSE,
                       descr.silent           = FALSE,
                       dfSummary.style        = "multiline",
                       dfSummary.varnumbers   = TRUE,
                       dfSummary.class        = TRUE,
                       dfSummary.labels.col   = TRUE,
                       dfSummary.valid.col    = TRUE,
                       dfSummary.na.col       = TRUE,
                       dfSummary.graph.col    = TRUE,
                       dfSummary.graph.magnif = 1,
                       dfSummary.silent       = FALSE,
                       dfSummary.custom.1     = 
                         expression(
                           paste(
                             paste0(
                               trs("iqr"), " (", trs("cv"), ") : "
                             ),
                             format_number(
                               IQR(column_data, na.rm = TRUE), round.digits
                             ),
                             " (",
                             format_number(
                               sd(column_data, na.rm = TRUE) /
                                 mean(column_data, na.rm = TRUE), round.digits
                             ),
                             ")",
                             collapse = "", sep = ""
                           )
                         ),
                       dfSummary.custom.2     = NA,
                       tmp.img.dir            = NA_character_,
                       subtitle.emphasis      = TRUE,
                       lang                   = "en",
                       use.x11                = TRUE) {
  
  allOpts <- getOption("summarytools")
  
  # Validate arguments
  mc <- match.call()
  errmsg <- check_args_st_options(mc = mc)
  
  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "), "\n No options have been modified")
  }

  # Querying all
  if (is.null(names(mc))) {
    return(allOpts)
  }
  
  # Querying one or several
  if (length(mc) == 2 && "option" %in% names(mc) && 
      !identical(option, "reset") && !identical(option, 0)
      && !identical(option, 0L)) {
    # Check that option is among the existing ones
    for (o in option) {
      if (!o %in% c(names(allOpts), 0)) {
        stop("Option ", o, " not recognized / not available")
      }
    }
    
    if (length(option) == 1) {
      return(allOpts[[option]])
    } else {
      return(allOpts[option])
    }
  }

  if (isTRUE(option == "reset" || option == 0)) {
    if (length(mc) > 2) {
      stop("Cannot reset options and set them at the same time")
    }
    
    options("summarytools" = 
              list("style"                  = "simple",
                   "plain.ascii"            = TRUE,
                   "round.digits"           = 2,
                   "headings"               = TRUE,
                   "footnote"               = "default",
                   "display.labels"         = TRUE,
                   "na.val"                 = NULL,
                   "bootstrap.css"          = TRUE,
                   "custom.css"             = NA_character_,
                   "escape.pipe"            = FALSE,
                   "char.split"             = 12,
                   "freq.cumul"             = TRUE,
                   "freq.totals"            = TRUE,
                   "freq.report.nas"        = TRUE,
                   "freq.ignore.threshold"  = 25,
                   "freq.silent"            = FALSE,
                   "ctable.prop"            = "r",
                   "ctable.totals"          = TRUE,
                   "ctable.round.digits"    = 1,
                   "ctable.silent"          = FALSE,
                   "descr.stats"            = "all",
                   "descr.transpose"        = FALSE,
                   "descr.silent"           = FALSE,
                   "dfSummary.style"        = "multiline",
                   "dfSummary.varnumbers"   = TRUE,
                   "dfSummary.class"        = TRUE,
                   "dfSummary.labels.col"   = TRUE,
                   "dfSummary.graph.col"    = TRUE,
                   "dfSummary.valid.col"    = TRUE,
                   "dfSummary.na.col"       = TRUE,
                   "dfSummary.graph.magnif" = 1,
                   "dfSummary.silent"       = FALSE,
                   "dfSummary.custom.1"     = 
                     expression(
                       paste(
                         paste0(
                           trs("iqr"), " (", trs("cv"), ") : "
                         ),
                         format_number(
                           IQR(column_data, na.rm = TRUE), round.digits
                         ),
                         " (",
                         format_number(
                           sd(column_data, na.rm = TRUE) /
                             mean(column_data, na.rm = TRUE), round.digits
                         ),
                         ")",
                         collapse = "", sep = ""
                       )
                     ),
                   "dfSummary.custom.2"     = NA,
                   "tmp.img.dir"            = NA_character_,
                   "subtitle.emphasis"      = TRUE,
                   "lang"                   = "en",
                   "use.x11"                = TRUE))
    
    message("summarytools options have been reset")
    return(invisible())
  }

  # Legacy way of setting of options
  if (length(names(mc)) == 3 && 
      identical(sort(names(mc)), c("", "option", "value"))) {
    if (length(option) > 1) {
      stop("Cannot set more than one option at a time in the legacy way; ",
           "Use separate arguments for each option instead")
    }
    if (!option %in% names(allOpts)) {
      stop("Option ", option, "not recognized / not available")
    } else {
      allOpts[[option]] <- value
    }
    options("summarytools" = allOpts)
    return(invisible())
  }
  
  # Regular way of setting options    
  for (o in setdiff(names(mc), c("", "option", "value",
                                 "dfSummary.custom.1", "dfSummary.custom.2"))) {
    allOpts[[o]] <- get(o)
  }
  
  if ("dfSummary.custom.1" %in% names(mc)) {
    val <- get("dfSummary.custom.1")
    if (is.expression(val) || is.na(val)) {
      allOpts[["dfSummary.custom.1"]] <- val
    } else if (val %in% c("default", "reset")) {
      allOpts[["dfSummary.custom.1"]] <-
        expression(
          paste(
            paste0(
              trs("iqr"), " (", trs("cv"), ") : "
            ),
            format_number(
              IQR(column_data, na.rm = TRUE), round.digits
            ),
            " (",
            format_number(
              sd(column_data, na.rm = TRUE) /
                mean(column_data, na.rm = TRUE), round.digits
            ),
            ")",
            collapse = "", sep = ""
          )
        )
    }
  }
    
  if ("dfSummary.custom.2" %in% names(mc)) {
    val <- get("dfSummary.custom.2")
    if (is.expression(val) || is.na(val)) {
      allOpts[["dfSummary.custom.2"]] <- val
    }
  }
  
  options("summarytools" = allOpts)
  
  return(invisible())
}
