context('coef_map')

library(modelsummary)

test_that(": in interactions become x", {
  mod <- lm(am ~ drat * mpg * vs, mtcars)
  expect_known_output(modelsummary(mod, "markdown"),
                      file="known_output/msummary_interaction.md",
                      print=TRUE,
                      update=FALSE)
})
