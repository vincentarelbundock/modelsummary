test_that("rounding gives expected results", {
  expect_equal("blah", modelsummary:::rounding("blah"))
  expect_equal("3.142", modelsummary:::rounding(pi))
  expect_equal(c("1", "2"), modelsummary:::rounding(factor(1:2)))
  expect_equal("2", modelsummary:::rounding(2, TRUE))
})
