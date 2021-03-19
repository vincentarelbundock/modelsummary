skip_if(getRversion() < '4.0.0')

test_that("different rows and columns", {
  dat <- mtcars[, c("mpg", "hp", "vs")]
  cor_fun <- function(x) {
      out <- cor(x)
      row.names(out) <- sprintf("(%s) %s", 1:nrow(out), row.names(out))
      colnames(out) <- sprintf("(%s)", 1:nrow(out))
      return(out)
  }
  tab <- datasummary_correlation(dat, method = cor_fun, output = "data.frame")
  truth <- structure(list(` ` = c("(1) mpg", "(2) hp", "(3) vs"), `(1)` = c("1.00", "-.78", ".66"), `(2)` = c("-.78", "1.00", "-.72"), `(3)` = c(".66", "-.72", "1.00")), class = "data.frame", row.names = c(NA, -3L ), align = "lrrr", output_format = "dataframe")
  expect_equal(tab, truth)
})
    
    
test_that("diagonal rounding", {
  k <- datasummary_correlation(mtcars, output = "data.frame", fmt = 4)
  k[[1]] <- NULL
  k <- as.matrix(k)
  expect_true(all(diag(k) == "1"))
})

test_that("pearson equals pearson", {
  f <- function(x) {
    out <- cor(x, use = "pairwise.complete.obs", method = "pearson")
    out <- datasummary_correlation_format(out, fmt = 2,
                                          upper_triangle = ".",
                                          diagonal = "1")
    out <- as.matrix(out)
  }
  a <- datasummary_correlation(mtcars, method = "pearson", output = "dataframe")
  b <- datasummary_correlation(mtcars, method = f, output = "dataframe")
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
