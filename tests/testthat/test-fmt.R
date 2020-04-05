context('fmt')

library(modelsummary)

mod <- list()
mod$OLS <- lm(hp ~ drat, data = mtcars)
mod$Logit <- glm(am ~ drat + hp, data = mtcars, family = binomial())

test_that("rounding 5 decimals", {

    raw <- modelsummary::extract(mod, fmt = '%.5f')

    truth <- c("353.65253", "(76.04873)", "-57.54523", "(20.92205)", "", "")
    expect_equal(truth, raw[[4]][1:6])
    
    truth <- c("-29.07608", "(12.41692)", "7.30978", "(3.04660)", "0.01079", "(0.00933)")
    expect_equal(truth, raw[[5]][1:6])

})
