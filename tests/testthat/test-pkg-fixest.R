skip_if_not_installed("fixest")
requiet("fixest")

test_that("gof_map standard errors with `vcov.type`", {
  gm <- list(
    list("raw" = "FE: gear", "clean" = "FE: Gear", fmt = 0),
    list("raw" = "vcov.type", "clean" = "Uncertainty", fmt = 0))
  mod <- feols(mpg ~hp | gear, data = mtcars)
  tab <- modelsummary(mod,
                      gof_map = gm,
                      output = "data.frame")
  expect_true("Uncertainty" %in% tab$term)
  expect_true("FE: Gear" %in% tab$term)
})
