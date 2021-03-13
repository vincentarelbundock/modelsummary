test_that("diagonal rounding", {
  k <- datasummary_correlation(mtcars, output = "data.frame", fmt = 4)
  k[[1]] <- NULL
  k <- as.matrix(k)
  expect_true(all(diag(k) == "1"))
})

test_that("pearson equals pearson", {
  a <- datasummary_correlation(mtcars, method = "pearson")
  b <- datasummary_correlation(mtcars, method = function(x) cor(x, use = "pairwise.complete.obs", method = "pearson"))
  expect_equal(a, b)
})

test_that("pearson, kendall, spearman, pearspear", {
  dat <- mtcars[, c("mpg", "hp")]

  # pearson
  truth <- structure(list(` ` = c("mpg", "hp"), mpg = c("1", "-.78"), hp = c(".", "1")), row.names = c(NA, -2L), class = "data.frame", align = "lrr", output_format = "dataframe")
  tab <- datasummary_correlation(dat, output = "data.frame", method = "pearson")
  expect_equal(truth, tab)

  # kendall
  truth <- structure(list(` ` = c("mpg", "hp"), mpg = c("1", "-.74"), hp = c(".", "1")), row.names = c(NA, -2L), class = "data.frame", align = "lrr", output_format = "dataframe")
  tab <- datasummary_correlation(dat, output = "data.frame", method = "kendall")
  expect_equal(truth, tab)

  # spearman
  tab <- datasummary_correlation(dat, output = "data.frame", method = "spearman")
  truth <- structure(list(` ` = c("mpg", "hp"), mpg = c("1", "-.89"), hp = c(".", "1")), row.names = c(NA, -2L), class = "data.frame", align = "lrr", output_format = "dataframe")
  expect_equal(truth, tab)

  # pearspear
  tab <- datasummary_correlation(dat, output = "data.frame", method = "pearspear")
  truth <- structure(list(` ` = c("mpg", "hp"), mpg = c("1", "-.89"), hp = c("-.78", "1")), row.names = c(NA, -2L), class = "data.frame", align = "lrr", output_format = "dataframe")
  expect_equal(truth, tab)

})

