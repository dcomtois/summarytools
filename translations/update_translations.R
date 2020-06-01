# Update package translations
# Import english file first - the others will be merged to it.
setwd("~/GitHub/summarytools")
tr <- read.csv("translations/en.csv", strip.white = TRUE, 
               stringsAsFactors = FALSE, encoding = "UTF-8")
items <- tr$item
tr <- t(as.matrix(tr[,-1:-2]))
colnames(tr) <- items
tr <- as.data.frame(tr, stringsAsFactors = FALSE)
rownames(tr) <- "en"
.translations <- tr

for (f in list.files("translations")) {
  if (f == "en.csv" || !grepl("^\\w{2}\\.csv$", f)) {
    next
  }
  tr <- read.csv(paste("translations", f, sep = "/"), strip.white = TRUE, 
                 stringsAsFactors = FALSE, encoding = "UTF-8")
  items <- tr$item
  tr <- t(as.matrix(tr[,-1:-2]))
  colnames(tr) <- items
  tr <- as.data.frame(tr, stringsAsFactors = FALSE)
  rownames(tr) <- substr(f, 1, 2)
  .translations[nrow(.translations) + 1, ] <- tr
}


.keywords_context <- read.csv("translations/language_template.csv", 
                              encoding = "UTF-8", strip.white = TRUE, 
                              stringsAsFactors = FALSE)[,-3]
rownames(.keywords_context) <- .keywords_context$item
.keywords_context$item <- NULL

usethis::use_data(.translations, .keywords_context, 
                  internal = TRUE, overwrite = TRUE)

rm(tr, f, items)

# Check that translations is in the package's environment (after build)
# ls(loadNamespace("summarytools"))
# View(summarytools:::.translations)
