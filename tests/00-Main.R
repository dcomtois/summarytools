rm(list=ls())
setwd("~/Github/summarytools")
(orig_dir <- getwd())
if (Sys.info()[["sysname"]] == "Windows") {
  (ref_dir <- paste(orig_dir, "tests/ref", sep = "/"))
} else {
  (ref_dir <- paste(orig_dir, "tests/ref-utf8", sep = "/"))
}

(date_dir <- paste(orig_dir, "tests/output", 
                   paste(format(Sys.time(), format = "%Y-%m-%d-%Hh%M"),
                         git2r::repository_head()$name, sep = "-"),
                   sep = "/"))
# date_dir <- "D:/Documents/GitHub/summarytools/tests/output/2020-05-21-22h51-dev-current"
save(date_dir, file = paste0(orig_dir, "/tests/last_date_dir.Rdata"))
load(file = paste0(orig_dir, "/tests/last_date_dir.Rdata"))

(dir.create(date_dir, recursive = TRUE, showWarnings = FALSE))

(testfiles <- grep(dir(paste0(orig_dir, "/tests")), pattern = "^\\d{2}\\-",
                   perl = TRUE, value = TRUE)[-1])

reset <- function() {
  base_pkgs <- c("compiler", "magrittr", "graphics", "htmltools", "tools", "utils", "yaml", "grDevices", 
                 "Rcpp", "stats", "datasets", "rmarkdown", "knitr", "methods", "xfun", "digest", "packrat",
                 "rlang", "base", "evaluate")
  for (p in setdiff(loadedNamespaces(), base_pkgs)) {
    pk <- paste0("package:",p)
    try(detach(pk, character.only = TRUE, unload = TRUE), silent = TRUE)
  }
    #try(detach("package:dplyr", character.only = TRUE, unload = TRUE), silent = TRUE)
    #try(detach("package:magrittr", character.only = TRUE, unload = TRUE))
    #try(detach("package:summarytools", character.only = TRUE, unload = TRUE))
  suppressWarnings(rm(tobacco, tabagisme, examens, exams, pr_number))
}
#reset()
#l = 1
#f = 1
base_content <- c("date_dir", "f", "l", "orig_dir", "ref_dir", "reset", 
                  "testfiles", "base_content", "lang", "compare_dirs")
l=1
f=10
for (l in 1:6) {
  lang <- c("en", "fr", "es", "pt", "tr", "ru")[l]
  for (f in 1:11) {
    filename <- testfiles[f]
    (out_dir <- paste(date_dir, lang, sub("\\.R", "", filename), sep = "/"))
    (dir.create(out_dir, recursive = TRUE, showWarnings = FALSE))
    reset()
    suppressPackageStartupMessages(library(summarytools))
    st_options(lang = lang)
    if (lang == "ru" && Sys.info()[["sysname"]] == "Windows") {
      Sys.setlocale("LC_CTYPE", "russian")
    }
    (setwd(out_dir))
    (outfilename <- paste0(out_dir, "/", sub("\\.R", "", filename), ".txt"))
    outfile <- file(outfilename, open = "wt")
    sink(outfile)
    sink(outfile, type = "message")
    source(file = paste0(orig_dir, "/tests/", filename), local=FALSE, echo=TRUE, spaced=TRUE, 
           prompt.echo="> ", chdir=FALSE, encoding="UTF-8", continue.echo=">", max.deparse.length = 200,
           width.cutoff=200, keep.source=TRUE, print.eval=TRUE)
    sink(type = "message")
    sink()
    close(outfile)
    
    fff <- file(outfilename, "r")
    content <- readLines(fff)
    close(fff)
    
    content <- sub("^> #(.+)$", "# ----------------- \\1", content)
    content <- sub("^(Output file (written|appended)).+$", "\\1", content)
    
    fff <- file(outfilename, "w")
    writeLines(content, fff)
    close(fff)
    
    if (lang == "ru" && Sys.info()[["sysname"]] == "Windows") {
      Sys.setlocale("LC_CTYPE", "")
      st_options(lang = "en")
    }
    
    rm(list=setdiff(ls(), base_content))
    setwd(orig_dir)
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
    cat(ref_dir)
    cat("\n")
    cat(out_dir)
    #system(paste0('"C:\\Program Files\\Araxis\\Araxis Merge\\compare"', 
    system(paste0('"compare"', 
                  ' "', ref_dir, '" "', out_dir, '"'))
  }
}

compare_dirs("en")
compare_dirs("fr")
compare_dirs("es")
compare_dirs("pt")
compare_dirs("tr")
compare_dirs("ru")

