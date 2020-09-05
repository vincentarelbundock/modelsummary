context('coef_map')

library(modelsummary)

test_that("bad estimate name throws error", {
  mod <- lm(am ~ drat, data = mtcars)
  expect_error(modelsummary:::extract_estimates(mod, estimate = "junk"))
})

test_that("bad statistic name throws error", {
  mod <- lm(am ~ drat, data = mtcars)
  expect_error(modelsummary:::extract_estimates(
    mod,
    statistic = "junk"))
  expect_error(modelsummary:::extract_estimates(models, statistic = "junk", statistic_override = vcov))
})

test_that("horizontal statistics", {
  mod <- lm(am ~ drat, data = mtcars)
  raw <- modelsummary:::extract_estimates(
    mod,
    statistic_vertical = FALSE)
  expect_equal(dim(raw), c(2, 3))
})
