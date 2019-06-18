context("Test bad input")

if ('gt' %in% rownames(utils::installed.packages())) {

    library(gt)
    library(MASS)
    library(dplyr)
    library(sandwich)
    library(modelsummary)

    test_that("coef_map: two variables with the same name", {

        set.seed(1)

        # within one model produces an error
        x <- rnorm(100)
        y <- rnorm(100)
        z <- rnorm(100)
        mod <- lm(y ~ x + z)
        expect_error(msummary(mod, coef_map = c('x' = 'X', 'z' = 'X')))

        # across models works well
        mod <- list()
        mod[[1]] <- lm(y ~ x)
        mod[[2]] <- lm(y ~ z)
        tab <- msummary(mod, coef_map = c('x' = 'X', 'z' = 'X'))
        checkmate::expect_class(tab, 'gt_tbl')

    })

}
