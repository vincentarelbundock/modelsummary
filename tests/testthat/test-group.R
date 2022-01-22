# replicability: this gets reverted at the end of the file 
options(modelsummary_get = "easystats")
skip_if_not_installed("gamlss")
requiet("gamlss")

test_that("Michael E Flynn ultra-niche bug check", {
    skip_if_not_installed("nnet")
    requiet("nnet")
    dat_multinom <- mtcars
    dat_multinom$cyl <- as.factor(dat_multinom$cyl)
    dat_multinom$under_score <- dat_multinom$mpg
    mod <- list(
        "a" = nnet::multinom(cyl ~ under_score, data = dat_multinom, trace = FALSE),
        "b" = nnet::multinom(cyl ~ under_score + drat, data = dat_multinom, trace = FALSE))
    coef_list = c("under_score" = "Under Score")
    void <- capture.output(
    tab <- modelsummary(mod,
                        output = "latex",
                        coef_map = coef_list,
                        group = term ~ model + response))
    expect_snapshot(cat(tab))
})


test_that("flipped table (no groups)", {
    mod <- list(
    lm(hp ~ mpg, mtcars),
    lm(hp ~ mpg + drat, mtcars))
    tab <- modelsummary(mod,
                        output = "data.frame",
                        group = model ~ term)
    expect_true("model" %in% colnames(tab))
})


test_that("nnet::multinom: order of rows determined by formula terms", {
    skip_if_not_installed("nnet")
    requiet("nnet")
    dat_multinom <- mtcars
    dat_multinom$cyl <- as.factor(dat_multinom$cyl)

    mod <- list(
        nnet::multinom(cyl ~ mpg, data = dat_multinom, trace = FALSE),
        nnet::multinom(cyl ~ mpg + drat, data = dat_multinom, trace = FALSE))

    ## order of rows determined by order of formula terms
    trash <- capture.output(tab <- modelsummary(mod, "data.frame", group = response + term ~ model))
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "group", "term", "statistic", "Model 1", "Model 2"))
    expect_equal(tab$term[1:4], c("(Intercept)", "(Intercept)", "mpg", "mpg"))
    expect_equal(tab$group[1:12], c(rep("6", 6), rep("8", 6)))

    ## order of rows determined by order of formula terms
    trash <- capture.output(tab <- modelsummary(mod, "data.frame", group = term + response ~ model))
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "term", "group", "statistic", "Model 1", "Model 2"))
    expect_equal(tab$term[1:4], rep("(Intercept)", 4))
    expect_equal(tab$group[1:4], c("6", "6", "8", "8"))

    ## order of rows determined by order of formula terms
    trash <- capture.output(tab <- modelsummary(mod, "data.frame", group = model + term ~ response))
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "model", "term", "statistic", "6", "8"))
    expect_equal(tab$model[1:10], c(rep("Model 1", 4), rep("Model 2", 6)))

    ## order of rows determined by order of formula terms
    trash <- capture.output(tab <- modelsummary(mod, "data.frame", group = term + model ~ response))
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "term", "model", "statistic", "6", "8"))
    expect_equal(tab$model[1:3], c("Model 1", "Model 1", "Model 2"))

})

test_that("group ~ model + term", {
    dat_multinom <- mtcars
    dat_multinom$cyl <- as.factor(dat_multinom$cyl)
    mod <- list(
        nnet::multinom(cyl ~ mpg, data = dat_multinom, trace = FALSE),
        nnet::multinom(cyl ~ mpg + drat, data = dat_multinom, trace = FALSE))

    tab <- modelsummary(mod,
                        output = "data.frame",
                        group = response ~ model + term,
                        metrics = "RMSE")
    known <- c("part", "group", "statistic", "Model 1 / (Intercept)", "Model 1 / mpg", "Model 2 / (Intercept)", "Model 2 / drat", "Model 2 / mpg")
    expect_equal(colnames(tab), known)
    expect_equal(dim(tab), c(4, 8))

    tab <- modelsummary(mod, 
                        output = "data.frame",
                        group = response ~ term + model,
                        metrics = "RMSE")
    known <- c("part", "group", "statistic", "(Intercept) / Model 1", "(Intercept) / Model 2", "drat / Model 2", "mpg / Model 1", "mpg / Model 2")
    expect_equal(colnames(tab), known)
    expect_equal(dim(tab), c(4, 8))
})

test_that("nnet::multinom: order of columns determined by formula terms", {
    skip_if_not_installed("nnet")

    library(nnet)
    dat_multinom <- mtcars
    dat_multinom$cyl <- as.factor(dat_multinom$cyl)

    mod <- list(
        nnet::multinom(cyl ~ mpg, data = dat_multinom, trace = FALSE),
        nnet::multinom(cyl ~ mpg + drat, data = dat_multinom, trace = FALSE))

    ## term ~ model + response
    trash <- capture.output(
        tab <- modelsummary(mod, "data.frame", group = term ~ model + response))
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "term", "statistic", "Model 1 / 6", "Model 1 / 8", "Model 2 / 6", "Model 2 / 8"))

    ## term ~ response + model
    trash <- capture.output(tab <- modelsummary(mod, "data.frame", group = term ~ response + model))
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "term", "statistic", "6 / Model 1", "6 / Model 2", "8 / Model 1", "8 / Model 2"))

    ## model ~ term + response
    trash <- capture.output(tab <- modelsummary(mod, "data.frame", group = model ~ term + response))
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "statistic", "model", "(Intercept) / 6",
                 "(Intercept) / 8", "mpg / 6", "mpg / 8", "drat / 6",
                 "drat / 8"))

    ## model ~ response + term
    trash <- capture.output(tab <- modelsummary(mod, "data.frame", group = model ~ response + term))
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "statistic", "model", "6 / (Intercept)", "6 / mpg",
                   "6 / drat", "8 / (Intercept)", "8 / mpg", "8 / drat"))

})


test_that("grouped coefficients: gamlss", {
    skip_if_not_installed("gamlss")
    requiet("gamlss")

    data(abdom)
    mod <- list(gamlss(y ~ pb(x),
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
    requiet("gamlss")
    dat <- rgamma(100, shape=1, scale=10)
    models <- list()
    models[["GA"]] <- gamlss(dat ~ 1, family = GA, trace = FALSE)
    models[["GA 2"]] <- gamlss(dat ~ 1, family = GA, trace = FALSE)
    tab <- modelsummary(models, output = "data.frame", group = component + term ~ model)
    expect_true(all(c("GA", "GA 2") %in% colnames(tab)))
})


options(modelsummary_get = NULL)
