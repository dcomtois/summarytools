# Create special_vars data frame
ean_values <- c(8716892700004, 8716892720002, 8716892750009, 8716874000009, 
                8716892000005, 8716892710003, 8716921000006, 8716886000004, 
                8716925000002, 8716946000005, 8716924000003, 8716892740000)
probs <- numeric()
for (i in 1:12) {
  probs[i] <- 100/(i * 310.3211)
}

set.seed(25245)
bin      <- sample(c(2,248,NA), size = 1000, prob = c(.04, .9, .06), replace = TRUE)
tri      <- sample(c(15, 20, 25, NA), size = 1000, prob = c(.03, .7, .24, .03), replace = TRUE)
ean_num  <- sample(ean_values, size = 1000, prob = probs, replace = TRUE)
empty_na <- sample(c("", NA), size = 1000, prob = c(.65, .35), replace = TRUE)

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
                           empty_char  = "",
                           empty_fact  = as.factor(""),
                           empty_na    = empty_na,
                           empty_na_f  = as.factor(empty_na),
                           ean_num     = ean_num,
                           ean_char    = as.character(ean_num),
                           ean_fact    = as.factor(ean_num),
                           stringsAsFactors = FALSE
)

save(special_vars, file = "tests/data/special_vars.RData")
