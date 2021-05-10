# replicability: this gets reverted at the end of the file 
options(modelsummary_get = "easystats")

library(gamlss)
data(trees)
models <- list()
models[['Bivariate']] <- lm(Girth ~ Height, data = trees)
models[['Multivariate']] <- lm(Girth ~ Height + Volume, data = trees)
models[["GAMLSS"]] <- gamlss(y~pb(x),sigma.fo=~pb(x),family=BCT, data=abdom, method=mixed(1,20), trace=FALSE)


test_that("flipped table (no groups)", {

    mod = list(
    lm(hp ~ mpg, mtcars),
    lm(hp ~ mpg + drat, mtcars))
    tab = modelsummary(mod,
                    output = "data.frame",
                    group = model ~ term)
    expect_true("model" %in% colnames(tab))
})


test_that("group: nnet::multinom", {
    skip_if_not_installed("nnet")

    library(nnet)
    dat_multinom <- mtcars
    dat_multinom$cyl <- as.factor(dat_multinom$cyl)

    mod <- list(
        nnet::multinom(cyl ~ mpg, data = dat_multinom, trace = FALSE),
        nnet::multinom(cyl ~ mpg + drat, data = dat_multinom, trace = FALSE))

    tab <- modelsummary(mod, "data.frame", group = response + term ~ model)
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "group", "term", "statistic", "Model 1", "Model 2"))

    tab <- modelsummary(mod, "data.frame", group = model + term ~ response)
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "model", "term", "statistic", "6", "8"))

    tab <- modelsummary(mod, "data.frame", group = term ~ model + response)
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "term", "statistic", "Model 1 / 6", "Model 2 / 6",
                   "Model 1 / 8", "Model 2 / 8"))

})


test_that("grouped coefficients: gamlss", {
    skip_if_not_installed("gamlss")
    library(gamlss)

    data(abdom)
    mod <- list(
        gamlss(y ~ pb(x),
            sigma.fo = ~ pb(x), trace = FALSE,
            family = BCT, data = abdom, method = mixed(1, 20)),
        gamlss(y ~ x,
            sigma.fo = ~ pb(x), trace = FALSE,
            family = BCT, data = abdom, method = mixed(1, 20)))

    tab <- modelsummary(mod, "data.frame", group = term + component ~ model)
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "term", "group", "statistic", "Model 1", "Model 2"))

    tab <- modelsummary(mod, "data.frame", group = component + term ~ model)
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "group", "term", "statistic", "Model 1", "Model 2"))

    tab <- modelsummary(mod, "data.frame", group = term ~ model + component)
    expect_s3_class(tab, "data.frame")

    tab <- modelsummary(mod, "data.frame", group = term ~ component + model)
    expect_s3_class(tab, "data.frame")

    
    tab <- modelsummary(mod, "data.frame", group = term + model ~ component)
    expect_s3_class(tab, "data.frame")

    tab <- modelsummary(mod, "data.frame", group = model + term ~ component)
    expect_s3_class(tab, "data.frame")

})


test_that("model names are preserved", {
    skip_if_not_installed("gamlss")
    library(gamlss)
    dat <- rgamma(100, shape=1, scale=10)
    models <- list()
    models[["GA"]] <- gamlss(dat ~ 1, family = GA, trace = FALSE)
    models[["GA 2"]] <- gamlss(dat ~ 1, family = GA, trace = FALSE)
    tab <- modelsummary(models, output = "data.frame",
                 group = component + term ~ model)
    expect_true(all(c("GA", "GA 2") %in% colnames(tab)))
})



options(modelsummary_get = NULL)
