test_that("errors", {
  # + not allowed
  expect_error(datasummary_crosstab(var1 + var2 ~ var3, data = mtcars))
  # formula needs to be two-sided
  expect_error(datasummary_crosstab(~ var3, data = mtcars))
  # left-hand side of `statistic` must either be empty of 1
  expect_error(datasummary_crosstab(var1 ~ var2, statistic = N ~ N, data = mtcars))
  # right-hand side of `statistic` may only contain allowed valued
  expect_error(datasummary_crosstab(var1 ~ var2, statistic = ~ Mean + N + Percent(), data = mtcars))
  # `statistic` may not contain interactions
  expect_error(datasummary_crosstab(var1 ~ var2, statistic = ~ N * Percent(), data = mtcars))
})

test_that("basic functionality", {
  # just one statistic, no totals
  tab1 <- datasummary_crosstab(cyl ~ gear, statistic = ~ N,
      data = mtcars, output = "dataframe")
  expect_equal(dim(tab1), c(3, 5))
  expect_equal(tab1[, 2], rep("N", 3))

  # just one statistic and row totals
  tab2 <- datasummary_crosstab(cyl ~ gear, statistic = ~ 1 + N,
      data = mtcars, output = "dataframe")
  expect_equal(dim(tab2), c(3, 6))
  expect_equal(utils::tail(names(tab2), 1), "All")

  # just one statistic and row totals and col totals
  tab3 <- datasummary_crosstab(cyl ~ gear, statistic = 1 ~ 1 + N,
      data = mtcars, output = "dataframe")
  expect_equal(dim(tab3), c(4, 6))
  expect_equal(utils::tail(names(tab3), 1), "All")
  expect_equal(utils::tail(tab3[, 1], 1), "All")

  # another statistic, fmt argument
  tab4 <- datasummary_crosstab(cyl ~ gear, statistic = 1 ~ 1 + Percent(),
      data = mtcars, output = "dataframe", fmt = 2)
  expect_equal(dim(tab4), c(4, 6))
  expect_equal(utils::tail(names(tab4), 1), "All")
  expect_equal(utils::tail(tab4[, 1], 1), "All")
  expect_equal(tab4[4, 6], "100.00")

  # default statistic
  tab5 <- datasummary_crosstab(cyl ~ gear,
      data = mtcars, output = "dataframe")
  expect_equal(dim(tab5), c(8, 6))
  expect_equal(tab5[, 2], rep(c("N", "% row"), 4))

  # switch order
  tab6 <- datasummary_crosstab(cyl ~ gear, statistic = 1 ~ 1 + Percent("row") + N,
      data = mtcars, output = "dataframe")
  expect_equal(dim(tab6), c(8, 6))
  expect_equal(tab6[, 2], rep(c("% row", "N"), 4))

  # all stats
  tab7 <- datasummary_crosstab(cyl ~ gear, statistic = 1 ~ 1 + N + Percent() + Percent("col") + Percent("row"),
      data = mtcars, output = "dataframe")
  expect_equal(dim(tab7), c(16, 6))
  expect_equal(tab7[, 2], rep(c("N", "%", "% col", "% row"), 4))

  # all stats - single quotes
  tab8 <- datasummary_crosstab(cyl ~ gear, statistic = 1 ~ 1 + N + Percent() + Percent('col') + Percent('row'),
      data = mtcars, output = "dataframe")
  expect_equal(dim(tab8), c(16, 6))
  expect_equal(tab8[, 2], rep(c("N", "%", "% col", "% row"), 4))
})

test_that("interactions", {
  # left-hand side interaction
  tab1 <- datasummary_crosstab(am * cyl ~ gear, statistic = 1 ~ 1 + N + Percent(),
      data = mtcars, output = "dataframe")
  expect_equal(dim(tab1), c(14, 7))

  # right-hand side interaction
  tab2 <- datasummary_crosstab(cyl ~ am * gear, statistic = 1 ~ 1 + N + Percent(),
      data = mtcars, output = "dataframe")
  expect_equal(dim(tab2), c(8, 9))

  # both side interaction
  tab3 <- datasummary_crosstab(vs * cyl ~ am * gear, statistic = 1 ~ 1 + N + Percent(),
      data = mtcars, output = "dataframe")
  expect_equal(dim(tab3), c(14, 10))
})
