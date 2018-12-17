# Update package translations
tr <- read.csv("translations/translations.csv", strip.white = TRUE, 
               stringsAsFactors = F, fileEncoding = "UTF-8")
items <- tr$item
tr <- t(as.matrix(tr[,-1:-2]))
colnames(tr) <- items
tr <- as.data.frame(tr, stringsAsFactors = FALSE)
for (cname in colnames(tr)) {
  tr[,cname] <- enc2utf8(tr[,cname])
}
translations <- tr
usethis::use_data(translations, internal = TRUE, overwrite = TRUE)

# Check that translations is in the package's environment (after build)
# ls(loadNamespace("summarytools")) 
# View(translations)
