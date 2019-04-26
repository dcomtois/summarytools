# Create special_vars data frame
emails <- read.table("tests/data/random_email.txt", stringsAsFactors = FALSE)[[1]]

ean_values <- c(8716892700004, 8716892720002, 8716892750009, 8716874000009, 
                8716892000005, 8716892710003, 8716921000006, 8716886000004, 
                8716925000002, 8716946000005, 8716924000003, 8716892740000)
probs <- numeric()
for (i in 1:12) {
  probs[i] <- 100/(i * 310.3211)
}

set.seed(25245)

bin          <- sample(c(2,248,NA), size = 1000, prob = c(.04, .9, .06), replace = TRUE)
tri          <- sample(c(15, 20, 25, NA), size = 1000, prob = c(.03, .7, .24, .03), replace = TRUE)
ean_num      <- sample(ean_values, size = 1000, prob = probs, replace = TRUE)
empty_na     <- sample(c("", NA), size = 1000, prob = c(.65, .35), replace = TRUE)
date1        <- seq(from = as.Date("2015-01-01"), to = as.Date("2029-12-31"),
                    length.out = 1000)
date2        <- sample(c(as.Date("2019-01-18"), as.Date("2019-01-19"), as.Date("2019-01-20"), NA), 
                       replace = TRUE, size = 1000, prob = c(.4, .4, .16, .04))
miss         <- unique(floor(runif(100, 1,1000)))
emails[miss] <- NA
inval         <- unique(floor(runif(150, 1,1000)))
emails[inval] <- sub("@", " ", emails[inval])
dups              <- unique(floor(runif(100, 1,1000)))
emails[dups + 1]  <- emails[dups]
some_empty_str    <- sample(c("", "  ", NA, "A", "B", "C"), size = 1000, replace = TRUE,
                            prob = c(.03,.04, .03, .6,.25,.05))

special_vars <- data.frame(bin         = bin, 
                           bin_char    = as.character(bin),
                           bin_fact    = as.factor(bin),
                           tri         = tri,
                           tri_char    = as.character(tri),
                           tri_fact    = as.factor(tri),
                           nans        = NaN,
                           nas_logi    = NA,
                           nas_real    = NA_real_,
                           nas_int     = NA_integer_,
                           nas_compl   = NA_complex_,
                           nas_char    = NA_character_,
                           nas_fact    = as.factor(NA_character_),
                           nas_fact_2  = factor(NA_character_, levels = c("a", "b")),
                           empty_char  = "",
                           empty_fact  = as.factor(""),
                           empty_na_chr = empty_na,
                           empty_na_f  = as.factor(empty_na),
                           ean_num     = ean_num,
                           ean_char    = as.character(ean_num),
                           ean_fact    = as.factor(ean_num),
                           date        = date1,
                           date_posxct = as.POSIXct(date1),
                           date_diff   = date1 - as.Date("2019-01-01"),
                           date_2      = date2,
                           date_2_posxct = as.POSIXct(date2),
                           emails      = emails,
                           emails_fact = as.factor(emails),
                           some_empty_str = some_empty_str,
                           some_empty_str_f = as.factor(some_empty_str), 
                           stringsAsFactors = FALSE
)
rm(bin, date1, date2, ean_num, ean_values, empty_na, i, probs, tri, emails, miss, inval)
save(special_vars, file = "tests/data/special_vars.RData")
