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
