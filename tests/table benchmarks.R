# sudo apt install libgsl-dev
install.packages("Rfast")

suppressPackageStartupMessages(library(summarytools))
suppressPackageStartupMessages(library(microbenchmark))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(Rfast))

load("~/GitHub/summarytools/tests/data/barcodes.RData")
#barcodes$straatnaam[grep("s‐Gravenpark", barcode$straatnaam)] <- NA
#barcodes$woonplaats[grep("S‐GRAVENHAGE", barcode$woonplaats)] <- NA
#save(barcodes, file = "tests/data/barcodes.RData") 

bcclas <- lapply(barcodes, class)
unique(bcclas)
bcchr <- barcodes[which(bcclas == "character")]
bcnum <- barcodes[which(bcclas != "character")]

dfsc <- dfSummary(bcchr)

dfsn <- dfSummary(bcnum)

mbm <- microbenchmark(table(bcchr$straatnaam),
                      plyr::count(bcchr$straatnaam),
                      plyr::count(barcodes, "straatnaam"),
                      barcodes %>% dplyr::count(straatnaam), 
                      barcodes %>% group_by(straatnaam) %>% mutate(F = n()),
                      barcodes.dt[, .N ,by = straatnaam],
                      as.data.table(barcodes)[ , .N, by=straatnaam],
                      Table(barcodes$straatnaam, useNA = TRUE),
                      times=5)
autoplot(mbm)

install.packages("Rfast")
barcodes.dt <- as.data.table(barcodes)
barcodes.dt[, .N ,by = ]

freq()