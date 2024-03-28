source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")
if (!requiet("correlation")) exit_file("correlation package")

x <- mtcars[, 1:4]
co <- correlation(x)
tab <- datasummary_correlation(co, output = "dataframe", stars = TRUE)
expect_equivalent(tab[[2]], c("1", "-.85***", "-.85***", "-.78***"))

# basics
tab <- datasummary_correlation(x, output = "dataframe")
expect_equivalent(tab[[3]], c(".", "1", ".90", ".83"))

dat <- mtcars[, c("mpg", "hp")]
colnames(dat) <- c("Miles / Gallon", "Horse Power")

# alternative methods
tab <- datasummary_correlation(dat, method = "pearspear", output = "dataframe")
expect_equivalent(tab[[2]], c("1", "-.89"))

# custom function
cor_fun <- function(x) cor(x, method = "kendall")
table <- datasummary_correlation(dat, method = cor_fun, output = "dataframe")
expect_equivalent(table[[3]], c("-.74", "1.00"))


