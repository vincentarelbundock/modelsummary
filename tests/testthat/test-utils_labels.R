test_that("get_labs works", {
  data(trees)
  # give vars some random label
  trees$Height <- haven::labelled(trees$Height, label = "Height (in feet)")
  trees$Volume <- haven::labelled(trees$Volume, label = "Volume (in liters)")

  models <- list(
    "Bivariate" = lm(Girth ~ Height, data = trees),
    "Multivariate" = lm(Girth ~ Height + Volume, data = trees)
  )
  expect_equal(
    get_labs(models[[1]]),
    list(
      "old" = "Height",
      "new" = "Height (in feet)"
    )
  )
  expect_equal(
    get_labs(models[[2]]),
    list(
      "old" = c("Height", "Volume"),
      "new" = c("Height (in feet)", "Volume (in liters)")
    )
  )
})
