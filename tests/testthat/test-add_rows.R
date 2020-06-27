context('add_rows')

library(modelsummary)
library(tibble)

mod <- list()
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod$OLS <- lm(am ~ cyl, data = dat)
mod$Logit <- glm(am ~ cyl, data = dat, family = binomial())

test_that("tibble", {
    rows <- tibble::tribble(~term,       ~OLS, ~Logit, ~position, ~section,
                            'cyl4',      '-',  '-',    2,         'middle',
                            'NEW GOF 1', '?',  '?',    2,         'bottom',
                            'NEW GOF 2', 'X',  'X',    3,         'bottom',
                            'NEW GOF 3', 'Y',  'Y',    6,         'bottom')
    raw <- modelsummary:::extract(mod, add_rows = rows)
	truth <- c("(Intercept)", "cyl4", "(Intercept)", "cyl6", "cyl6", "cyl8", "cyl8", "Num.Obs.", "NEW GOF 1", "NEW GOF 2", "R2", "R2 Adj.", "NEW GOF 3", "AIC", "BIC", "Log.Lik.")
    expect_equal(truth, unname(raw$term))
})

test_that("tibble with model missing", {
    rows <- tibble::tribble(~term,       ~OLS, ~position, ~section,
                            'cyl4',      '-',  2,         'middle',
                            'NEW GOF 1', '?',  2,         'bottom',
                            'NEW GOF 2', 'X',  3,         'bottom',
                            'NEW GOF 3', 'Y',  6,         'bottom')
    raw <- modelsummary:::extract(mod, add_rows = rows)
	truth <- c("0.981", "", "(0.677)", "-1.269", "(1.021)", "-2.773", "(1.021)", "32", "", "", "", "", "", "39.9", "44.3", "-16.967")
    expect_equal(truth, unname(raw$Logit))
})

test_that("list of vectors", {
    rows <- list(c('cyl4', '-', '-'),
                 c('NEW GOF1', 'X', 'X'))
    raw <- modelsummary:::extract(mod, add_rows = rows)
	truth <- c("(Intercept)", "(Intercept)", "cyl6", "cyl6", "cyl8", "cyl8", "Num.Obs.", "R2", "R2 Adj.", "AIC", "BIC", "Log.Lik.", "cyl4", "NEW GOF1")
    expect_equal(truth, unname(raw$term))
})


test_that("expect errors", {

    # no position
    rows <- tibble::tribble(~term,       ~OLS, ~Logit2, ~section,
                            'cyl4',      '-',  '-',    'middle',
                            'NEW GOF 1', '?',  '?',    'bottom',
                            'NEW GOF 2', 'X',  'X',    'bottom',
                            'NEW GOF 3', 'Y',  'Y',    'bottom')
    expect_error(modelsummary(mod, add_rows = rows))

    # no section
    rows <- tibble::tribble(~term,       ~OLS, ~Logit2, ~position, 
                            'cyl4',      '-',  '-',    2,
                            'NEW GOF 1', '?',  '?',    2,
                            'NEW GOF 2', 'X',  'X',    3,
                            'NEW GOF 3', 'Y',  'Y',    6)
    expect_error(modelsummary(mod, add_rows = rows))

    # bad model name
    rows <- tibble::tribble(~term,       ~OLS, ~Logit2, ~position, ~section,
                            'cyl4',      '-',  '-',    2,         'middle',
                            'NEW GOF 1', '?',  '?',    2,         'bottom',
                            'NEW GOF 2', 'X',  'X',    3,         'bottom',
                            'NEW GOF 3', 'Y',  'Y',    6,         'bottom')
    expect_error(modelsummary(mod, add_rows = rows))

    # list of bad length
    rows <- list(c(''))
    expect_error(modelsummary(mod, add_rows = rows))

})
