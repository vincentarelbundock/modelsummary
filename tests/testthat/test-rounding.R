test_that("rounding gives expected results", {
  expect_equal("blah", modelsummary:::rounding("blah"))
  expect_equal("3.142", modelsummary:::rounding(pi))
  expect_equal(c("1", "2"), modelsummary:::rounding(factor(1:2)))
  expect_equal("2", modelsummary:::rounding(2, TRUE))
})

test_that("rounding cleans up NaN inside \\num", {
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    expect_snapshot(datasummary(cyl + mpg ~ SD + N, data = dat, output = "latex"))
})

test_that("siunitx works with empty cells", {
  skip_if_not_installed("lme4")
  dat <- mtcars
  dat$cyl <- factor(dat$cyl)
  mod <- lme4::lmer(mpg ~ hp + (1 | cyl), data = dat)
  expect_snapshot(modelsummary(
      mod,
      output = "latex",
      estimate = "{estimate} [{conf.low}, {conf.high}]"))
})
