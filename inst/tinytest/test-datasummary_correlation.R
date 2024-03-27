source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")


x <- mtcars[, 1:4]

# basics
expect_snapshot_print(datasummary_correlation(x, output = "latex"), label= "datasummary_correlation-basics")

# easycorrelation
if (!require(correlation)) {
  install.packages(correlation, dependencies = TRUE)
  library(correlation, character.only = TRUE)
}
x <- correlation(x)
expect_snapshot_print(datasummary_correlation(x, output = "latex"), label= "datasummary_correlation-easycorrelation")

dat <- mtcars[, c("mpg", "hp")]
colnames(dat) <- c("Miles / Gallon", "Horse Power")

# alternative methods
expect_snapshot_print(datasummary_correlation(dat, method = "pearspear", output = "latex"), label= "datasummary_correlation-alternative_methods")

# custom function
cor_fun <- function(x) cor(x, method = "kendall")
expect_snapshot_print(datasummary_correlation(dat, method = cor_fun, output = "latex"), label= "datasummary_correlation-custom_function")

# rename columns alphabetically and include a footnote for reference
note <- sprintf("(%s) %s", letters[1:ncol(dat)], colnames(dat))
note <- paste(note, collapse = "; ")
colnames(dat) <- sprintf("(%s)", letters[1:ncol(dat)])
expect_snapshot_print(datasummary_correlation(dat, notes = note, output = "latex"), label= "datasummary_correlation-alphabetical_columns_and_footnotes")


