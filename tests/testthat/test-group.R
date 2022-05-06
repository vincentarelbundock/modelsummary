requiet("gamlss")


test_that("group -> shape", {
    mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
    tab1 <- modelsummary(list(mod, mod), statistic = "conf.int", group = ~ statistic, output = "data.frame")
    tab2 <- modelsummary(list(mod, mod), statistic = "conf.int", shape = ~ statistic, output = "data.frame")
    expect_true("Model 1 / 2.5 %" %in% colnames(tab1))
    expect_equal(tab1, tab2)
    expect_error(modelsummary(mod, shape = ~ statistic, group = ~ statistic))
})


test_that("gof merge on partial column match", {
    options(modelsummary_factory_default = "data.frame")
    mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
    tab <- modelsummary(list(mod, mod), group = ~ model + statistic)
    expect_equal(dim(tab), c(11, 6))
    expect_true("gof" %in% tab$part)
})

test_that("partial formulas", {
    options(modelsummary_factory_default = "data.frame")
    mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)

    # term + statistic ~ model
    tab1 <- modelsummary(mod, group = ~ model, gof_map = NA)
    tab2 <- modelsummary(mod, group = statistic ~ model, gof_map = NA)
    tab3 <- modelsummary(mod, group = term + statistic ~ model, gof_map = NA)
    expect_equal(dim(tab1), c(8, 4))
    expect_equal(tab1, tab2)
    expect_equal(tab1, tab3)

    # model + statistic ~ term
    tab1 <- modelsummary(list(mod, mod), group = model + statistic ~ term, gof_map = NA)
    tab2 <- modelsummary(list(mod, mod), group = model ~ term, gof_map = NA)
    expect_equal(tab1, tab2)
    expect_equal(dim(tab1), c(4, 7))

    # term + response + statistic ~ model
    mod <- nnet::multinom(factor(cyl) ~ mpg, data = mtcars, trace = FALSE)
    mod <- list(mod, mod)
    tab1 <- modelsummary(mod, group = term + response + statistic ~ model, gof_map = NA)
    tab2 <- modelsummary(mod, group = response ~ model, gof_map = NA)
    tab3 <- modelsummary(mod, group = response + statistic ~ model, gof_map = NA)
    tab4 <- modelsummary(mod, group = term + response ~ model, gof_map = NA)
    expect_equal(dim(tab1), c(8, 6))
    expect_equal(tab1, tab2)
    expect_equal(tab1, tab3)
    expect_equal(tab1, tab4)

    options(modelsummary_factory_default = NULL)
})


test_that("horizontal statistics: dim only", {
    mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
    tab <- modelsummary(mod, group = model + term ~ statistic, output = "data.frame", gof_map = NA)
    expect_equal(dim(tab), c(4, 5))
    tab <- modelsummary(list(mod, mod), group = model + term ~ statistic,
                        output = "data.frame", gof_map = NA)
    expect_equal(dim(tab), c(8, 5))
    tab <- modelsummary(list(mod, mod), group = term ~ model + statistic,
                        output = "data.frame", gof_map = NA)
    expect_equal(dim(tab), c(4, 6))


    tab <- modelsummary(mod,
                        statistic = c("std.error", "conf.int"),
                        group = model + term ~ statistic,
                        fmt = 2,
                        conf_level = .99,
                        output = "data.frame",
                        gof_map = NA)
    expect_equal(dim(tab), c(4, 7))
    expect_true(all(c("Est.", "S.E.", "0.5 %", "99.5 %") %in% colnames(tab)))
})


test_that("Michael E Flynn ultra-niche bug check", {
    requiet("nnet")
    dat_multinom <<- mtcars
    dat_multinom$cyl <- as.factor(dat_multinom$cyl)
    dat_multinom$under_score <- dat_multinom$mpg
    mod <- list(
        "a" = nnet::multinom(cyl ~ under_score, data = dat_multinom, trace = FALSE),
        "b" = nnet::multinom(cyl ~ under_score + drat, data = dat_multinom, trace = FALSE))
    coef_list = c("under_score" = "Under Score")
    void <- capture.output(
    tab <- modelsummary(mod,
                        output = "latex",
                        ccoef_map = coef_list,
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
    known <- c("part", "group", "statistic", "Model 1 / (Intercept)", "Model 1 / mpg",
"Model 2 / (Intercept)", "Model 2 / mpg", "Model 2 / drat")
    expect_equal(colnames(tab), known)
    expect_equal(dim(tab), c(6, 8))

    tab <- modelsummary(mod,
                        output = "data.frame",
                        group = response ~ term + model,
                        metrics = "RMSE")
    known <- c("part", "group", "statistic", "(Intercept) / Model 1", "(Intercept) / Model 2",
"mpg / Model 1", "mpg / Model 2", "drat / Model 2")
    expect_equal(colnames(tab), known)
    expect_equal(dim(tab), c(6, 8))
})

test_that("nnet::multinom: order of columns determined by formula terms", {
    requiet("nnet")
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
                 c("part", "model", "statistic", "(Intercept) / 6",
                 "(Intercept) / 8", "mpg / 6", "mpg / 8", "drat / 6",
                 "drat / 8"))

    ## model ~ response + term
    trash <- capture.output(tab <- modelsummary(mod, "data.frame", group = model ~ response + term))
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
        c("part", "model", "statistic", "6 / (Intercept)", "6 / mpg", "6 / drat",
          "8 / (Intercept)", "8 / mpg", "8 / drat"))
})


test_that("grouped coefficients: gamlss", {
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
    requiet("gamlss")
    dat <- rgamma(100, shape=1, scale=10)
    models <- list()
    models[["GA"]] <- gamlss(dat ~ 1, family = GA, trace = FALSE)
    models[["GA 2"]] <- gamlss(dat ~ 1, family = GA, trace = FALSE)
    tab <- modelsummary(models, output = "data.frame", group = component + term ~ model)
    expect_true(all(c("GA", "GA 2") %in% colnames(tab)))
})


test_that("group_map reorder rename", {
    requiet("pscl")
    data("bioChemists", package = "pscl")
    mod <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    tab <- modelsummary(mod,
                        group = component + term ~ model,
                        group_map = c("zero_inflated" = "Zero", "conditional" = "Count"),
                        output = "data.frame")
    expect_equal(tab$group[1], "Zero")
})

