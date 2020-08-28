context('coef_omit')

library(modelsummary)

test_that("omit coefficients using regular expressions", {

    mod <- list()
    mod$OLS <- lm(am ~ drat, data = mtcars)
    mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

    raw <- modelsummary:::extract_models(mod, coef_omit = c('drat|qsec'))

    truth <- c('(Intercept)', '(Intercept)', 'Num.Obs.')
    expect_equal(unname(raw[[2]][1:3]), truth)

})
