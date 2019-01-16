# Setup
# rm(list=ls())
# setwd("~/GitHub/summarytools")
(orig_dir <- getwd())
(ref_dir <- paste(orig_dir, "tests/ref", sep = "/"))

(date_dir <- paste(orig_dir, "tests/output", 
                   format(Sys.time(), format = "%Y-%m-%d (%Hh%M)"),
                   sep = "/"))

(dir.create(date_dir, recursive = TRUE))

(testfiles <- grep(dir("~/GitHub/summarytools/tests/"), pattern = "\\d(?!0)\\d",
                  perl = TRUE, value = TRUE))

eval_with_feedback <- function(filename, lang, out_dir) {
  on.exit(setwd(orig_dir))
  setwd(date_dir)
  dir.create(paste(date_dir, out_dir, sep = "/"), showWarnings = FALSE)
  setwd(paste(date_dir, out_dir, sep = "/"))
  
  try(detach("package:summarytools", unload = TRUE), silent = TRUE)
  library(summarytools)
  st_options(lang = lang)
  
  cat("reading ", filename, "...\n")
  contents <- readLines(paste(orig_dir, "tests", filename, sep = "/"), 
                        encoding = 'UTF-8')
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
  test_dir <- normalizePath(paste(date_dir, out_dir, sep = "/"))
  ref_dir  <- normalizePath(paste(ref_dir, out_dir, sep = "/"))
  if (Sys.info()[['sysname']] == "Linux") {
    system(paste0('meld "', ref_dir, '" "', test_dir, '"'), wait = FALSE)
  } else {
    system(paste0('compare "', ref_dir, '" "', test_dir, '"'))
  }
}

eval_with_feedback(testfiles[1],  lang = "fr", out_dir = "parse_args (fr)") # st_options
eval_with_feedback(testfiles[2],  lang = "fr", out_dir = "freq (fr)") # freq
eval_with_feedback(testfiles[3],  lang = "fr", out_dir = "ctable (fr)") # ctable
eval_with_feedback(testfiles[4],  lang = "fr", out_dir = "descr (fr)") # descr
eval_with_feedback(testfiles[5],  lang = "fr", out_dir = "dfSummary (fr)") # dfSummary
#eval_with_feedback(testfiles[6],  lang = "fr", out_dir = overrides (fr)) # overrides
#eval_with_feedback(testfiles[7],  lang = "fr", out_dir = with-by (fr)) # with/by
#eval_with_feedback(testfiles[8],  lang = "fr", out_dir = lapply (fr)) # lapply
#eval_with_feedback(testfiles[9],  lang = "fr", out_dir = view-print (fr)) # view/print
#eval_with_feedback(testfiles[10], lang = "fr", out_dir = misc (fr)) # 11-misc
#eval_with_feedback(testfiles[11], lang = "fr", out_dir = special cases (fr)) # 12-special cases
#
#setwd(orig.dir)
