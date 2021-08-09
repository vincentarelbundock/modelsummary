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
  expect_s3_class(tab, "data.frame")
  expect_equal(dim(tab), c(3, 4))

  expect_known_output(dput(tab),
                      file = "known_output/datasummary_correlation_1.R",
                      update = TRUE,
                      print = TRUE)
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
  tab <- datasummary_correlation(dat, output = "data.frame", method = "pearson")

  expect_known_output(dput(tab),
                      file = "known_output/datasummary_correlation_2.R",
                      update = TRUE,
                      print = TRUE)

  # kendall
  tab <- datasummary_correlation(dat, output = "data.frame", method = "kendall")

  expect_known_output(dput(tab),
                      file = "known_output/datasummary_correlation_3.R",
                      update = TRUE,
                      print = TRUE)

  # spearman
  tab <- datasummary_correlation(dat, output = "data.frame", method = "spearman")

  expect_known_output(dput(tab),
                      file = "known_output/datasummary_correlation_4.R",
                      update = TRUE,
                      print = TRUE)

  # pearspear
  tab <- datasummary_correlation(dat, output = "data.frame", method = "pearspear")
  expect_known_output(dput(tab),
                      file = "known_output/datasummary_correlation_5.R",
                      update = TRUE,
                      print = TRUE)
})
