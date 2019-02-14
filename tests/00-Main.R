# setwd("~/github/summarytools")
rm(list=ls())
(orig_dir <- getwd())
(ref_dir <- paste(orig_dir, "tests/ref", sep = "/"))

(date_dir <- paste(orig_dir, "tests/output", 
                   format(Sys.time(), format = "%Y-%m-%d (%Hh%M)"),
                   sep = "/"))
save(date_dir, file = "last_date_dir.Rdata")

(dir.create(date_dir, recursive = TRUE))

(testfiles <- grep(dir(paste0(orig_dir, "/tests")), pattern = "\\d(?!0)\\d",
                   perl = TRUE, value = TRUE))

eval_with_feedback <- function(filename, lang, compare = TRUE) {
  on.exit(setwd(orig_dir))
  out_dir <- paste(date_dir, lang, sub("\\.R", "", filename), sep = "/")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  setwd(out_dir)
  
  try(detach("package:summarytools", unload = TRUE), silent = TRUE)
  library(summarytools)
  st_options(lang = lang, footnote = "Placeholder footnote")
  
  cat("reading ", filename, "...\n")
  contents <- readLines(paste(orig_dir, "tests", filename, sep = "/"), 
                        encoding = 'UTF-8')
  Encoding(contents) <- "UTF-8"
  
  path_out <- paste0(basename(tools::file_path_sans_ext(filename)), ".txt")
  
  # delete output file if it already exists
  unlink(path_out)
  
  cat("writing to ", path_out, "...\n")
  
  outfile <- file(description = path_out, open = "w")
  
  for (line in contents) {
    # Ignore empty lines
    if (grepl("^\\s*$", line))
      next
    
    # Show comments in the console and write them to outfile
    if (grepl("^#", line)) {
      cat("\n", file = outfile, append = TRUE)
      line <- paste(line, strrep("-", max(0, 79 - nchar(line))), collapse = " ")
      cat(line, "\n\n", file = outfile, append = TRUE)
      message(line, appendLF = TRUE)
      next
    }
    
    message("> ", line, appendLF = TRUE)
    
    # Store the cuurent executing line's output
    res <- capture.output(eval(parse(text=line)), type = "output")
    if (is.null(res))
      res <- character()
    # Store the executed line's generated messages / warnings
    msg <- capture.output(eval(parse(text=line)), type = "message")
    
    # Truncate the file name to avoid having meaningless diffs in the outputs
    msg <- sub("(Output file written|Output file appended).+", "\\1", msg)

    # Write out the line that was executed, its messages + results
    if (length(msg) == 0 && length(res) == 0) {
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
  
  if (isTRUE(compare)) {
    ref_dir  <- normalizePath(paste(ref_dir, lang, sub("\\.R", "", filename),
                                    sep = "/"), mustWork = FALSE)
    if (!dir.exists(ref_dir)) {
      dir.create(ref_dir, recursive = TRUE)
      return(paste("No ref files exist in", ref_dir))
    }
    if (Sys.info()[['sysname']] == "Linux") {
      system(paste0('meld "', ref_dir, '" "', out_dir, '"'), wait = FALSE)
    } else {
      system(paste0('compare "', ref_dir, '" "', out_dir, '"'))
    }
  }
}

compare_dirs <- function(lang) {
  ref_dir  <- normalizePath(paste(ref_dir, lang, sep = "/"), mustWork = FALSE)
  out_dir <- paste(date_dir, lang, sep = "/")
  if (!dir.exists(ref_dir)) {
    dir.create(ref_dir, recursive = TRUE)
    return(paste("No ref files exist in", ref_dir))
  }
  if (Sys.info()[['sysname']] == "Linux") {
    system(paste0('meld "', ref_dir, '" "', out_dir, '"'), wait = FALSE)
  } else {
    system(paste0('"C:\\Program Files\\Araxis\\Araxis Merge\\compare"', 
                 ' "', ref_dir, '" "', out_dir, '"'))
  }
}

eval_with_feedback(testfiles[1],  lang = "en", compare = FALSE) # parse-args
eval_with_feedback(testfiles[2],  lang = "en", compare = FALSE) # freq
eval_with_feedback(testfiles[3],  lang = "en", compare = FALSE) # ctable
eval_with_feedback(testfiles[4],  lang = "en", compare = FALSE) # descr
eval_with_feedback(testfiles[5],  lang = "en", compare = FALSE) # dfSummary
eval_with_feedback(testfiles[6],  lang = "en", compare = FALSE) # overrides
eval_with_feedback(testfiles[7],  lang = "en", compare = FALSE) # lapply
eval_with_feedback(testfiles[8],  lang = "en", compare = FALSE) # with/by
eval_with_feedback(testfiles[9],  lang = "en", compare = FALSE) # st_options
compare_dirs('en')

eval_with_feedback(testfiles[1],  lang = "fr", compare = FALSE) # parse-args
eval_with_feedback(testfiles[2],  lang = "fr", compare = FALSE) # freq
eval_with_feedback(testfiles[3],  lang = "fr", compare = FALSE) # ctable
eval_with_feedback(testfiles[4],  lang = "fr", compare = FALSE) # descr
eval_with_feedback(testfiles[5],  lang = "fr", compare = FALSE) # dfSummary
eval_with_feedback(testfiles[6],  lang = "fr", compare = FALSE) # overrides
eval_with_feedback(testfiles[7],  lang = "fr", compare = FALSE) # lapply
eval_with_feedback(testfiles[8],  lang = "fr", compare = FALSE) # with/by
eval_with_feedback(testfiles[9],  lang = "fr", compare = FALSE) # st_options
compare_dirs('fr')

