mod <- lm(hp ~ mpg, mtcars)

test_that("hard deprecation", {
  expect_warning(
    modelsummary(mod, statistic_vertical = TRUE),
    regexp = "deprecated")
  expect_warning(
    modelsummary(mod, statistic_vertical = FALSE),
    regexp = "deprecated")
})


test_that("soft deprecation", {
  tab1 = modelsummary(mod, statistic_override = "robust")
  tab2 = modelsummary(mod, vcov = "robust")
  expect_identical(tab1, tab2)
  expect_error(
    modelsummary(mod, vcov = "robust", statistic_override = "robust"),
    regexp = "deprecated")
})
