context("gt")

test_that("gof_omit='.*' used to produce an error", {
  mod <- lm(mpg~wt, mtcars)
  expect_error(msummary(mod, output="gt", gof_omit=".*"), NA)
})
