rm(list=ls())
setwd("~/GitHub/summarytools")
(orig_dir <- getwd())
if (!git2r::repository_head()$name %in% dir("tests/ref")) {
  cat("********** Using dev-current references as no exist for current branch ************\n")
  cat("********** branch name: ", git2r::repository_head()$name, "\n")
  cat("********** Ref dirs available: ", paste(dir("tests/ref"), collapse = ", "))
  cat("********** Ref dir used: tests/ref/dev-current\n")
  ref_dir <- paste0(orig_dir, "/tests/ref/dev-current")
  # ref_dir <- paste0(orig_dir, "/tests/ref/master")
} else {
  (ref_dir <- paste(orig_dir, "tests/ref", git2r::repository_head()$name, sep = "/"))
}
  
load(file = paste0(orig_dir, "/tests/date_dirs.Rdata"))

# To use last saved directory, skip this step ------------------------------------
(date_dir <- paste(orig_dir, "tests/output", 
                   paste(format(Sys.time(), format = "%Y-%m-%d-%Hh%M"),
                         git2r::repository_head()$name, sep = "-"),
                   sep = "/"))
date_dirs[nrow(date_dirs) + 1,] <- list(lubridate::today(), date_dir, ref_dir)
save(date_dirs, file = paste0(orig_dir, "/tests/date_dirs.Rdata"))
# end skip -----------------------------------------------------------------------

#date_dirs <- data.frame(date=lubridate::today(), dir=date_dir, ref=ref_dir, stringsAsFactors=F)

(date_dir <- tail(date_dirs$dir, 1))
(dir.create(date_dir, recursive = TRUE, showWarnings = FALSE))
(testfiles <- grep(dir(paste0(orig_dir, "/tests")), pattern = "^\\d{2}\\-",
                   perl = TRUE, value = TRUE)[-1])

# Following objects will not be deleted after each iteration; all others will.
base_content <- c("date_dir", "f", "l", "orig_dir", "ref_dir", "reset", 
                  "testfiles", "base_content", "lang", "compare_dirs")

cleanup <- function() {
  closeAllConnections()
  cat("\nSink has been stopped for results and messages\n")
}

l=1
f=2
for (l in 1:6) {
  lang <- c("en", "fr", "es", "pt", "tr", "ru")[l]
  for (f in 1:11) {
    options(width = 200)
    options(tibble.print_max = 200)
    options(tibble.width = 200)
    
    filename <- testfiles[f]
    (out_dir <- paste(date_dir, lang, sub("\\.R", "", filename), sep = "/"))
    (dir.create(out_dir, recursive = TRUE, showWarnings = FALSE))
    suppressWarnings(rm(tobacco, tabagisme, examens, exams, pr_number))
    suppressPackageStartupMessages(library(summarytools))
    st_options(lang = lang)
    if (lang == "ru" && Sys.info()[["sysname"]] == "Windows") {
      Sys.setlocale("LC_CTYPE", "russian")
    } else {
      Sys.setlocale(category = "LC_ALL", locale = "")
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

    if (lang == "ru" && Sys.info()[["sysname"]] == "Windows") {
      content_utf8 <- iconv(content, "1251", "UTF-8")
    } else {
      content_utf8 <- enc2utf8(content) #iconv(content, "1252", "UTF-8") #enc2utf8(content) 
    }

    content_utf8 <- sub("^> #(.+)$", "# ----------------- \\1", content_utf8)
    content_utf8 <- sub("^(Output file (written|appended)).+$", "\\1", content_utf8)
    fff <- file(outfilename, "w", encoding = "UTF-8")
    writeLines(content_utf8, fff)
    close(fff)
    
    if (lang == "ru" && Sys.info()[["sysname"]] == "Windows") {
      Sys.setlocale("LC_CTYPE", "")
      library(summarytools)
      st_options(lang = "en")
    }
    
    rm(list=setdiff(ls(), base_content))
    #getwd()
    setwd(orig_dir)
  }
}

compare_dirs <- function(lang) {
  ref_dir  <- normalizePath(
    paste(ref_dir, lang, sep = "/"), 
    mustWork = FALSE)
  out_dir <- paste(date_dir, lang, sep = "/")
  if (!dir.exists(ref_dir)) {
    #dir.create(ref_dir, recursive = TRUE)
    return(paste("No ref files exist in", ref_dir))
  }
  if (Sys.info()[['sysname']] == "Linux") {
    system(paste0('meld "', ref_dir, '" "', out_dir, '"'), wait = FALSE)
    library(summarytools)
    st_options(lang="en")
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

