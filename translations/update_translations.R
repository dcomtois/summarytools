# Update package translations
tr <- read.csv2("translations.txt", strip.white = TRUE, stringsAsFactors = F,
                fileEncoding = "UTF-8",)
#apply(tr, c(1,2), conv_non_ascii)
items <- tr$item
tr <- t(as.matrix(tr[,-1:-2]))
colnames(tr) <- items
tr <- as.data.frame(tr, stringsAsFactors = FALSE)
save(tr, file = "data/translations.RData")

#ls(loadNamespace("summarytools")) 