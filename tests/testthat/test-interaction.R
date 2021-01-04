# CRAN win-devel does not play well with fancy unicode x
skip_on_cran()

test_that(": in interactions become x", {
  mod <- lm(am ~ drat * mpg * vs, mtcars)
  expect_known_output(modelsummary(mod, "markdown"),
                      file="known_output/msummary_interaction.md",
                      print=TRUE,
                      update=FALSE)
})
