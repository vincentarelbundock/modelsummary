# bad estimate name throws error
mod <- lm(am ~ drat, data = mtcars)
expect_error(modelsummary:::format_estimates(mod, estimate = "junk"))


# bad statistic name throws error
mod <- lm(am ~ drat, data = mtcars)
expect_error(
  modelsummary(mod, output = "dataframe", statistic = "junk")
)

expect_error(
  modelsummary(mod,
    statistic = "junk",
    output = "dataframe",
    statistic_override = vcov)
)