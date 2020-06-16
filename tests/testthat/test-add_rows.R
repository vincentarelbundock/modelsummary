context('add_rows')

library(modelsummary)

test_that("insert rows between estimates", {

    mod <- list()
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    mod$OLS <- lm(am ~ cyl, data = dat)
    mod$Logit <- glm(am ~ cyl, data = dat, family = binomial())

    rows <- list(c('cyl4', '-', '-'),
                 c('blank', '', ''))
    attr(rows, 'section') <- 'middle'

    raw <- modelsummary::extract(mod, add_rows = rows, add_rows_location = 3)


    truth <- c("(Intercept)", "(Intercept)", "cyl4", "blank", "cyl6", "cyl6",
               "cyl8", "cyl8", "Num.Obs.", "R2", "R2 Adj.", "AIC", "BIC",
               "Log.Lik.")
    expect_equal(unname(raw[[2]]), truth)

})
