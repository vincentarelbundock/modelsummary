requiet("broom")

fit <- glm(am ~ mpg + factor(cyl), data = mtcars, family = binomial)
tid <- tidy(fit, conf.int = TRUE)

test_that("exponentiate is a flag", {
    expect_error(modelsummary(fit, exponentiate = "yes"))
    expect_error(modelplot(fit, exponentiate = "yes"))
})

test_that("logit coefficients exponentiate", {

    tab <- modelsummary(fit, gof_omit = ".*", estimate = "estimate", statistic = NULL, output = "dataframe", exponentiate = TRUE)
    expect_equal(tab[[4]], sprintf("%.3f", exp(tid$estimate)))

    tab <- modelsummary(fit, gof_omit = ".*", estimate = "conf.low", statistic = NULL, output = "dataframe", exponentiate = TRUE)
    expect_equal(tab[[4]], sprintf("%.3f", exp(tid$conf.low)))

    tab <- modelsummary(fit, gof_omit = ".*", estimate = "conf.high", statistic = NULL, output = "dataframe", exponentiate = TRUE)
    expect_equal(tab[[4]], sprintf("%.3f", exp(tid$conf.high)))

    tab <- modelsummary(fit, gof_omit = ".*", estimate = "std.error", statistic = NULL, output = "dataframe", exponentiate = TRUE)
    expect_equal(tab[[4]], sprintf("%.3f", exp(tid$estimate) * tid$std.error))

})

test_that("vcov", {
    requiet("sandwich")
    b <- coef(fit)
    se  <- sqrt(diag(vcovCL(fit, ~cyl)))
    tab <- modelsummary(fit, vcov = ~cyl, gof_omit = ".*", estimate = "estimate",
                        statistic = NULL, output = "dataframe",
                        exponentiate = TRUE)
    expect_equal(tab[[4]], sprintf("%.3f", exp(b)))
    tab <- modelsummary(fit, vcov = ~cyl, gof_omit = ".*", estimate = "std.error",
                        statistic = NULL, output = "dataframe",
                        exponentiate = TRUE)
    expect_equal(tab[[4]], sprintf("%.3f", exp(b) * se))
})
