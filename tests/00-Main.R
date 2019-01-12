# Setup
# rm(list=ls())
setwd("~/Github/summarytools")
(orig.dir <- getwd())
(newdir <- paste0("~/GitHub/summarytools/tests/Outputs/", 
                  format(Sys.time(), format = "%Y-%m-%d (%Hh%M)")))
dir.create(newdir)
refdir <- "~/GitHub/summarytools/tests/Outputs/ref"

(testfiles <- grep(dir("~/GitHub/summarytools/tests/"), pattern = "\\d(?!0)\\d",
                  perl = TRUE, value = TRUE))
setwd(newdir)

eval_with_feedback <- function(filename, lang, outdir) {
  
  setwd(newdir)
  dir.create(outdir, showWarnings = FALSE)
  setwd(outdir)
  
  library(summarytools)
  st_options(lang = lang)
  
  cat("reading ", filename, "...\n")
  contents <- readLines(paste0("../../../",filename), encoding = 'UTF-8')
  Encoding(contents) <- "UTF-8"
  path_out <- paste0(basename(tools::file_path_sans_ext(filename)), ".txt")
  
  # delete output file if already exists
  unlink(path_out)
  
  cat("writing to ", path_out, "...\n")
  
  outfile <- file(description = path_out, open = "w")
  
  for (line in contents) {
    # Ignore empty lines, as well as "#library(..." and "#detach(..." lines
    if (grepl("(^cat\\(|^\\s*$|^#\\s*library|^#detach)", line))
      next
    
    # Show comments in the console and write them to outfile
    if (grepl("^#", line)) {
      cat("\n", file = outfile, append = TRUE)
      line <- paste(line, strrep("-", max(0, 79 - nchar(line))), collapse = " ")
      cat(line, "\n\n", file = outfile, append = TRUE)
      message("  ", line, appendLF = TRUE)
      next
    }
    
    # Store the execute line's output
    res <- capture.output(eval(parse(text=line)), type = "output")
    if (is.null(res))
      res <- character()
    # Store the executed line's generated messages / warnings
    msg <- capture.output(eval(parse(text=line)), type = "message")
    msg <- sub("(Output file written|Output file appended).+", "\\1", msg)

    # Write out the line that was executed, its messages + results
    if (length(msg) == 0 && length(res) == 0) {
      #cat("\n", file = outfile, append = TRUE)
      cat(">", line, "\n", file = outfile, append = TRUE)
    } else if (length(msg) > 0 && length(res) == 0) {
      cat(">", line, "\n-- ", file = outfile, append = TRUE)
      cat(msg, sep = "\n-- ", file = outfile, append = TRUE)
      cat("\n", file = outfile, append = TRUE)
    } else if (length(msg) == 0) {
      cat(">", line, "\n\n", file = outfile, append = TRUE)
      cat(res, sep = "\n", file = outfile, append = TRUE)
      cat("\n", file = outfile, append = TRUE)
    } else {
      cat(">", line, "\n-- ", file = outfile, append = TRUE)
      cat(msg, sep = "\n-- ", file = outfile, append = TRUE)
      cat("\n", file = outfile, append = TRUE)
      cat(res, sep = "\n", file = outfile, append = TRUE)
      cat("\n", file = outfile, append = TRUE)
    }
  }
  
  cat("Closing", path_out, "...\n")
  close(outfile)
  detach("package:summarytools", unload = TRUE)
  #browser()
  #shell.exec(path_out)
  test_dir <- normalizePath(paste(newdir, outdir, sep = "/"))
  ref_dir  <- normalizePath(paste(refdir, outdir, sep = "/"))
  #shell.exec(test_dir)
  #shell.exec(ref_dir)
  system(paste0('compare "', ref_dir, '" "', test_dir, '"'))
}

eval_with_feedback(testfiles[1],  lang = "fr", outdir = "st_options (fr)") # st_options
eval_with_feedback(testfiles[2],  lang = "fr", outdir = "freq (fr)") # freq
eval_with_feedback(testfiles[3],  lang = "fr", outdir = "ctable (fr)") # ctable
eval_with_feedback(testfiles[4],  lang = "fr", outdir = "descr (fr)") # descr
eval_with_feedback(testfiles[5],  lang = "fr", outdir = "dfSummary (fr)") # dfSummary
#eval_with_feedback(testfiles[6],  lang = "fr", outdir = overrides (fr)) # overrides
#eval_with_feedback(testfiles[7],  lang = "fr", outdir = with-by (fr)) # with/by
#eval_with_feedback(testfiles[8],  lang = "fr", outdir = lapply (fr)) # lapply
#eval_with_feedback(testfiles[9],  lang = "fr", outdir = view-print (fr)) # view/print
#eval_with_feedback(testfiles[10], lang = "fr", outdir = misc (fr)) # 11-misc
#eval_with_feedback(testfiles[11], lang = "fr", outdir = special cases (fr)) # 12-special cases
#
#setwd(orig.dir)
