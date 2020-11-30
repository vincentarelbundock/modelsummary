context('ellipses')

library(modelsummary)

test_that("exponentiate logit coefficients", {

  mod <- glm(am ~ mpg, mtcars, family = binomial)
  raw <- modelsummary(mod, output="dataframe", exponentiate=TRUE)

  truth <- c("0.001", "(2.351)", "1.359", "(0.115)", "32", "33.7", "36.6", "-14.838")
  expect_equal(truth, unname(raw[[4]]))

})
