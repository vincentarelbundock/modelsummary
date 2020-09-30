context("modelsummary_wide")

library(nnet)

test_that("nnet::multinom", {
  var1 <- sample(c('A', 'B', 'C'), replace = T, size=100)
  var2 <- sample(c(0,1), size=100, replace=T)
  var3 <- rnorm(100, mean=10, sd=2)
  df <- data.frame(var1, var2, var3)
  invisible(capture.output(mod <- nnet::multinom(var1~var2, data=df)))
  tmp <- modelsummary_wide(mod, output="data.frame")
  expect_is(tmp, "data.frame")
  expect_equal(dim(tmp), c(7, 5))
})
