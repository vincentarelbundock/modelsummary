requiet("broom")

fit <- glm(am ~ mpg + factor(cyl), data = mtcars, family = binomial)
tid <- tidy(fit, conf.int = TRUE)

test_that("exponentiate is a flag", {
    expect_error(modelsummary(fit, exponentiate = "yes"))
    expect_error(modelplot(fit, exponentiate = "yes"))
})

test_that("logit coefficients exponentiate", {

    tab <- modelsummary(fit, gof_omit = ".*", estimate = "estimate", statistic = NULL, output = "dataframe", exponentiate = TRUE)
    x <- c("0.0002", "1.448", "2.078", "2.017")
    expect_equal(tab[[4]], x)

    tab <- modelsummary(fit, gof_omit = ".*", estimate = "conf.low", statistic = NULL, output = "dataframe", exponentiate = TRUE)
    x <- c("2e-09", "1.026", "0.130", "0.044")
    expect_equal(tab[[4]], x)

    tab <- modelsummary(fit, gof_omit = ".*", estimate = "conf.high", statistic = NULL, output = "dataframe", exponentiate = TRUE)
    expect_equal(tab[[4]], sprintf("%.3f", exp(tid$conf.high)))

    tab <- modelsummary(fit, gof_omit = ".*", estimate = "std.error", statistic = NULL, output = "dataframe", exponentiate = TRUE)
    expect_equal(tab[[4]], sprintf("%.3f", exp(tid$estimate) * tid$std.error))

})

test_that("vcov", {
    requiet("sandwich")
    b <- coef(fit)
    se  <- sqrt(diag(vcovCL(fit, ~cyl)))
    x <- c("0.0002", "1.448", "2.078", "2.017")
    tab <- modelsummary(fit, vcov = ~cyl, gof_omit = ".*", estimate = "estimate",
                        statistic = NULL, output = "dataframe",
                        exponentiate = TRUE)
    expect_equal(tab[[4]], x)
    tab <- modelsummary(fit, vcov = ~cyl, gof_omit = ".*", estimate = "std.error",
                        statistic = NULL, output = "dataframe",
                        exponentiate = TRUE)
    expect_equal(tab[[4]], sprintf("%.3f", exp(b) * se))
})


test_that("exponentiate vector", {
    mod <- glm(am ~ mpg, family = binomial, data = mtcars)
    mod <- list(mod, mod)
    b <- coef(mod[[1]])
    se <- sqrt(diag(stats::vcov(mod[[1]])))

    # coefficients
    tab <- modelsummary(mod, exponentiate = FALSE,
                        output = "data.frame", statistic = NULL, fmt = identity)
    expect_equal(b, as.numeric(tab[["Model 1"]][1:2]), ignore_attr = TRUE)
    expect_equal(b, as.numeric(tab[["Model 2"]][1:2]), ignore_attr = TRUE)

    tab <- modelsummary(mod, exponentiate = TRUE,
                        output = "data.frame", statistic = NULL, fmt = identity)
    expect_equal(exp(b), as.numeric(tab[["Model 1"]][1:2]), ignore_attr = TRUE)
    expect_equal(exp(b), as.numeric(tab[["Model 2"]][1:2]), ignore_attr = TRUE)

    # standard error
    tab <- modelsummary(mod, exponentiate = TRUE, output = "data.frame",
                        estimate = "std.error", statistic = NULL, fmt = identity)
    expect_equal(exp(b) * se, as.numeric(tab[["Model 1"]][1:2]), ignore_attr = TRUE)
    expect_equal(exp(b) * se, as.numeric(tab[["Model 2"]][1:2]), ignore_attr = TRUE)

    # vector
    tab <- modelsummary(mod, exponentiate = c(TRUE, FALSE),
                        output = "data.frame", statistic = NULL, fmt = identity)
    expect_equal(exp(b), as.numeric(tab[["Model 1"]][1:2]), ignore_attr = TRUE)
    expect_equal(b, as.numeric(tab[["Model 2"]][1:2]), ignore_attr = TRUE)
})
