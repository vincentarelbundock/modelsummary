context('coef_map')

library(modelsummary)

test_that("combine different regressors and collapse rows", {

    cmap <- c('(Intercept)' = 'Constant', 'drat' = 'Combined', 'qsec' = 'Combined')
    mod <- list()
    mod$OLS <- lm(am ~ drat, data = mtcars)
    mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

    raw <- modelsummary::extract(mod, coef_map = cmap)

    truth <- c('Constant', 'Constant', 'Combined', 'Combined', 'Num.Obs.')
    expect_equal(unname(raw[[2]][1:5]), truth)

})

test_that("reorder and omit", {

    cmap <- c('qsec' = 'qsec', 'drat' = 'drat')
    mod <- list()
    mod$OLS <- lm(am ~ drat, data = mtcars)
    mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

    raw <- modelsummary::extract(mod, coef_map = cmap)

    truth <- c('qsec', 'qsec', 'drat', 'drat', 'Num.Obs.')
    expect_equal(unname(raw[[2]][1:5]), truth)

})
