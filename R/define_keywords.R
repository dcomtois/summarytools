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
#' @export
define_keywords <- function() {
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
    fn <- character()
    fn <- tclvalue(tkgetSaveFile(initialfile = "custom_lang.csv", 
                                 initialdir = "~",
                                 filetypes = "{{csv files} {*.csv}}"))
    if (fn != "") {
      fn <- sub(".csv.csv", ".csv", paste0(fn, ".csv"), fixed = TRUE)
      write.csv(x = tr, file = fn, row.names = FALSE, fileEncoding = "utf-8")
    }
  }
}
