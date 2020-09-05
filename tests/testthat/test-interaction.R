context('coef_map')

library(modelsummary)

test_that(": in interactions become x", {

  mod <- lm(am ~ drat * mpg * vs, mtcars)

  old <- readLines('known_output/interaction_markdown.md')
  new <- as.character(msummary(mod, 'markdown'))
  expect_identical(old, new)

})
