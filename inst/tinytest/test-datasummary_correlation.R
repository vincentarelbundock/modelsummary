source("helpers.R")
requiet("tinysnapshot")
requiet("data.table")
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

# issue #772: escape
x <- setNames(iris[, 1:3], c("blah_blah", "hello_world", "foo_bar"))
escape_false <- datasummary_correlation(x, output = "latex", escape = FALSE)
escape_true <- datasummary_correlation(x, output = "latex", escape = TRUE)
expect_false(grepl("foo_bar", escape_true))
expect_true(grepl("foo_bar", escape_false))
expect_true(grepl("foo\\\\_bar", escape_true))

# issue #771: data.table support
x <- setNames(iris[, 1:3], c("blah blah", "hello world", "foo bar"))
x <- data.table(x)
x <- datasummary_correlation(x)
expect_inherits(x, "tinytable")


# issue #799: Including a call to datasummary_correlation as an argument to datasummary fails with an uninformative error message #799
requiet("car") # Prestige data
k <- subset(Prestige, select = c(income, education, women, prestige))
tab <- datasummary(
  All(k) ~ Mean + SD,
  data = k,
  add_columns = datasummary_correlation(k, output = "data.frame")[, -1]
)
expect_inherits(tab, "tinytable")


# Stars don't appear when stars=FALSE
res <- datasummary_correlation(
  correlation::correlation(mtcars[, 1:3]),
  stars = FALSE,
  output = "data.frame"
)
res <- grepl("*", res[3, 3], fixed = TRUE)
expect_false(res)

# Issue #860: custom stars
res <- datasummary_correlation(
  correlation::correlation(mtcars[, 1:3]),
  stars = c("[zzz]" = .1),
  output = "data.frame"
)
expect_true(grepl("[zzz]", res[2, 2], fixed = TRUE))
expect_true(grepl("[zzz]", res[3, 2], fixed = TRUE))
expect_true(grepl("[zzz]", res[3, 3], fixed = TRUE))
