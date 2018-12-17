# Update package translations
tr <- read.csv2("translations/translations.csv", strip.white = TRUE, stringsAsFactors = F,
                fileEncoding = "UTF-8")
#apply(tr, c(1,2), conv_non_ascii)
items <- tr$item
tr <- t(as.matrix(tr[,-1:-2]))
colnames(tr) <- items
tr <- as.data.frame(tr, stringsAsFactors = FALSE)

for (cname in colnames(tr)) {
  Encoding(tr[,cname]) <- "UTF-8"
}

translations <- tr
usethis::use_data(translations, internal = TRUE, overwrite = TRUE)

# Check that translations is in the package's environment
#ls(loadNamespace("summarytools")) 
#View(translations)
