context('fmt')

library(modelsummary)

test_that("exponentiate logit coefficients", {

    mod <- glm(am ~ mpg, mtcars, family = binomial)
    raw <- modelsummary:::extract(mod, exponentiate = TRUE)

    truth <- c("353.65253", "(76.04873)", "-57.54523", "(20.92205)", "", "")
    expect_equal('1.359' , raw[[4]][3])

})
