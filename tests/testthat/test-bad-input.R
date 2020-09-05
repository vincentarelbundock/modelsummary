context("bad input")

library(modelsummary)

test_that("coef_map: two variables with the same name within or between models", {

  set.seed(1)

  # within one model produces an error
  x <- rnorm(100)
  y <- rnorm(100)
  z <- rnorm(100)
  mod <- lm(y ~ x + z)
  expect_error(msummary(mod, "data.frame", coef_map = c('x' = 'X', 'z' = 'X')))

  # across models works well
  mod <- list()
  mod[[1]] <- lm(y ~ x)
  mod[[2]] <- lm(y ~ z)
  tab <- msummary(mod, output = "gt", coef_map = c('x' = 'X', 'z' = 'X'))
  expect_is(tab, 'gt_tbl')

})
