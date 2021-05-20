test_that("first time stars=TRUE produces a warning (run first)", {
  mod <- lm(mpg ~ hp, mtcars)
  expect_warning(modelsummary(mod, "data.frame", stars = TRUE))
})
