# --------------------------- 5.dfSummary.R  -----------------------------------
# library(summarytools)

data(tobacco)
tobacco <- tibble::as_tibble(tobacco)

label(tobacco) <- "A Study on Tobacco Use and Health"
label(tobacco$gender) <- "Subject's Gender"
label(tobacco$BMI) <- "Body Mass Index"
label(tobacco$smoker) <- "Smoking Status"
label(tobacco$samp.wgts) <- "Sampling Weights"

# Bare-bones
(dfs1 <- dfSummary(tobacco))
print(dfs1, headings = FALSE)

# Omit columns
print(dfs1, varnumbers = FALSE)
print(dfs1, labels.col = FALSE)
print(dfs1, valid.col = FALSE)
print(dfs1, na.col = FALSE)
print(dfs1, graph.col = FALSE)

# Minimal results using print overrides
print(dfs1, headings = FALSE, labels.col = FALSE, varnumbers = FALSE, valid.col = FALSE, na.col = FALSE, graph.col = FALSE)

# Print to file
print(dfs1, file = "dfs1.html")

# Test global options (1/2)
st_options('reset')
st_options(dfSummary.varnumbers = F, dfSummary.labels.col = F, dfSummary.valid.col = F)
print(dfSummary(tobacco), file = "01 - basic.html")

# Test global options (2/2)
st_options('reset')
st_options(dfSummary.varnumbers = F, dfSummary.labels.col = F, dfSummary.graph.col = F, dfSummary.valid.col = F, dfSummary.na.col = F)
(dfs2 <- dfSummary(tobacco))
view(dfs2, method="browser", col.widths = c(240, 240, 240), footnote = "3 equal width cols", file = "02 - equal widths.html")
st_options('reset')

tobacco$disease.f <- as.factor(tobacco$disease)
(dfs3 <- dfSummary(tobacco, round.digits = 2, max.distinct.values = 4, varnumbers = FALSE, labels.col = TRUE, valid.col = FALSE, na.col = FALSE, max.string.width = 20))
print(dfs3, footnote = "4 distinct vals.", report.title = "DFS - 4 distinct values", file = "03 - 4 distinct val.html")

data(cars)
(dfs4 <- dfSummary(cars))
view(dfs4, method="browser", footnote = "cars", file = "04 - cars.html")

# Test special variables (ean, binary, ternary, na's, etc)
load("../../../data/special_vars.RData")
(dfs_special <- dfSummary(special_vars))
view(dfs_special, method = "browser", file = "02 - special vars.html")

# One variable only
data(iris)
dfSummary(iris$Sepal.Length)
dfSummary(iris['Sepal.Length'])
dfSummary(iris[['Sepal.Length']])
dfSummary(iris[["Sepal.Length"]])
data("AirPassengers")
view(dfSummary(AirPassengers), file = "03 - AirPassengers.html")

# subsetting
dfSummary(tobacco[1:100,])
print(dfSummary(tobacco[1:100,1:4]), footnote = "subset = [1:100, 1:4]", file = "04 - tobacco subset.html")


# round.digits and frequencies
tobacco$samp.wgts.3 <- round(tobacco$samp.wgts, 3)
tobacco$samp.wgts.3[tobacco$samp.wgts.3 == 0.861] <- 0
tobacco$samp.wgts.4 <- round(tobacco$samp.wgts, 4)
tobacco$samp.wgts.4 <- round(tobacco$samp.wgts, 1)
dfSummary(tobacco, round.digits = 2)
dfSummary(tobacco, round.digits = 3)
dfSummary(tobacco, round.digits = 4)

# st-small
print(dfSummary(tobacco, graph.magnif = 0.8, table.classes = 'st-small'), footnote = "st_small", file = "05 - st_small.html")

# render
print(dfSummary(tobacco), method = "render")
