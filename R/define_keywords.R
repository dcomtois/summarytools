#' Modify Keywords Used In Outputs
#'
#' As an alternative to \code{\link{use_custom_lang}}, this allows temporarily
#' modifying the keywords used in the outputs.
#'
#' A window will pop-up, allowing the modification of the \emph{custom} column.
#' The changes will be active as long as the package is loaded.
#'
#' @keywords utilities
#' @importFrom utils read.delim edit write.csv
#' @importFrom tcltk tclvalue tkgetSaveFile
#' @importFrom checkmate check_path_for_output
#' @export
define_keywords <- function() {
  if (!isTRUE(interactive())) {
    stop("R session not interactive; see ?use_custom_lang")
  }
  message("please modify entries in the second column only and leave the column",
          " names unchanged; third column gives context information; close the",
          " window when finished")
  tr <- edit(read.delim(
    system.file("includes/language_template.tab", package = "summarytools"),
    stringsAsFactors = FALSE))
  colnames(tr)[2] <- "custom"
  use_custom_lang(tr)
  message("keywords successfully modified")
  resp <- " "
  while (!resp %in% c("Y", "N", "")) {
    resp <- toupper(
      readline(prompt = "Save language file for later use? [y/N] ")
      )
  }
  if (resp == "Y") {
    filename <- character()
    if (.st_env$system == "Windows") {
      filename <- tclvalue(tkgetSaveFile(initialfile = "custom_lang.csv", 
                                         initialdir = "~",
                                         filetypes = "{{csv files} {*.csv}}"))
      filename <- normalizePath(filename, mustWork = FALSE)
    } else {
      filename_ok <- FALSE
      while (!filename_ok) {
        filename <- readline(prompt = "Path to csv file (ESC to cancel): ")
        filename <- sub('^"(.+)"$', "\\1", filename)
        if (filename != "" && 
            (!isTRUE(check_path_for_output(filename, overwrite = TRUE)) || 
             !grepl("\\.csv$", filename))) {
          message("Invalid file location or extension (must be .csv)")
        } else {
          filename_ok <- TRUE
        }
      }
    }
    
    if (filename != "") {
      filename <- sub("(.csv)+$", "\\1", paste0(filename, ".csv"))
      write.csv(x = tr, file = filename, row.names = FALSE, 
                fileEncoding = "utf-8")
    }
  }
}
