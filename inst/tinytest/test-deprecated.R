mod <- lm(hp ~ mpg, mtcars)

# hard deprecation
expect_warning(
  modelsummary(mod, statistic_vertical = TRUE),
  pattern = "deprecated")
expect_warning(
  modelsummary(mod, statistic_vertical = FALSE),
  pattern = "deprecated")

# soft deprecation
tab1 = modelsummary(mod, statistic_override = "robust")
tab2 = modelsummary(mod, vcov = "robust")
expect_identical(tab1, tab2)
expect_error(
  modelsummary(mod, vcov = "robust", statistic_override = "robust"),
  pattern = "deprecated")