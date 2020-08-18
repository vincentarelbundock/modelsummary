context('add_rows')

library(modelsummary)
library(tibble)

mod <- list()
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod$OLS <- lm(am ~ cyl, data = dat)
mod$Logit <- glm(am ~ cyl, data = dat, family = binomial())

test_that("tibble", {

    rows <- tibble::tribble(~term,       ~OLS, ~Logit,
                            'cyl4',      '-',  '-',
                            'NEW GOF 1', '?',  '?',
                            'NEW GOF 2', 'X',  'X',
                            'NEW GOF 3', 'Y',  'Y')
    attr(rows, 'position') <- c(1, 2, 9, 10)

    raw <- modelsummary(mod, add_rows = rows, output = 'dataframe')

	truth <- c("cyl4", "NEW GOF 1", "(Intercept)", "", "cyl6", "", "cyl8", "", "NEW GOF 2", "NEW GOF 3", "Num.Obs.", "R2", "R2 Adj.", "AIC", "BIC", "Log.Lik.", "F")

    expect_equal(truth, unname(raw[[1]]))

})

test_that("error: bad number of cols", {
    rows <- tibble::tribble(~term,       ~OLS, ~Logit, ~blah,
                            'cyl4',      '-',  '-',    1,
                            'NEW GOF 1', '?',  '?',    2,
                            'NEW GOF 2', 'X',  'X',    3,
                            'NEW GOF 3', 'Y',  'Y',    4)
    expect_error(modelsummary(mod, add_rows = rows, output = 'dataframe'))
})

test_that('add_rows numeric are formatted by fmt', {

    tmp <- data.frame(a = 1:2, b = 2:3, c = 3:4)
    tab <- datasummary(hp + mpg ~ mean + sd, data = mtcars, add_rows = tmp, 
                       fmt = "%.0f", output = 'dataframe')

    expect_equal(tab$sd, c('69', '6', '3', '4'))

})
