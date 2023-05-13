source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")

requiet("gamlss")
requiet("nnet")
requiet("marginaleffects")

# combine columns with :
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
mfx <- suppressWarnings(marginaleffects(mod))
tab1 <- modelsummary(mfx, output = "dataframe", shape = term:contrast ~ model)
tab2 <- modelsummary(mfx, output = "dataframe", shape = term:contrast + statistic ~ model)
tab3 <- modelsummary(mfx, output = "dataframe", shape = term + contrast + statistic ~ model)
tab4 <- modelsummary(
    mfx,
    output = "dataframe",
    coef_rename = function(x) gsub(" dY/dX", " (Slope)", x),
    shape = term : contrast ~ model)
expect_equivalent(tab1, tab2)
expect_equivalent(nrow(tab1), nrow(tab3))
expect_equivalent(ncol(tab1) + 1, ncol(tab3))
expect_error(modelsummary(mfx, shape = term * contrast + statistic ~ model), pattern = "character")
tab <- modelsummary(mfx, output = "dataframe", shape = term:contrast + statistic ~ model)
expect_inherits(tab, "data.frame")


# gof merge on partial column match
options(modelsummary_factory_default = "data.frame")
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
tab <- modelsummary(list(mod, mod), shape = ~ model + statistic)
expect_equivalent(dim(tab), c(12, 6))
expect_true("gof" %in% tab$part)
options(modelsummary_factory_default = NULL)


# partial formulas
options(modelsummary_factory_default = "data.frame")
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)

# term + statistic ~ model
tab1 <- modelsummary(mod, shape = ~ model, gof_map = NA)
tab2 <- modelsummary(mod, shape = statistic ~ model, gof_map = NA)
tab3 <- modelsummary(mod, shape = term + statistic ~ model, gof_map = NA)
expect_equivalent(dim(tab1), c(8, 4))
expect_equivalent(tab1, tab2)
expect_equivalent(tab1, tab3)

# model + statistic ~ term
tab1 <- modelsummary(list(mod, mod), shape = model + statistic ~ term, gof_map = NA)
tab2 <- modelsummary(list(mod, mod), shape = model ~ term, gof_map = NA)
expect_equivalent(tab1, tab2)
expect_equivalent(dim(tab1), c(4, 7))

# term + response + statistic ~ model
mod <- nnet::multinom(factor(cyl) ~ mpg, data = mtcars, trace = FALSE)
mod <- list(mod, mod)
tab1 <- modelsummary(mod, shape = term + response + statistic ~ model, gof_map = NA)
tab2 <- modelsummary(mod, shape = response ~ model, gof_map = NA)
tab3 <- modelsummary(mod, shape = response + statistic ~ model, gof_map = NA)
tab4 <- modelsummary(mod, shape = term + response ~ model, gof_map = NA)
expect_equivalent(dim(tab1), c(8, 6))
expect_equivalent(tab1, tab2)
expect_equivalent(tab1, tab3)
expect_equivalent(tab1, tab4)

options(modelsummary_factory_default = NULL)



# horizontal statistics: dim only
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
tab <- modelsummary(mod, shape = model + term ~ statistic, output = "data.frame", gof_map = NA)
expect_equivalent(dim(tab), c(4, 5))
tab <- modelsummary(list(mod, mod), shape = model + term ~ statistic,
                    output = "data.frame", gof_map = NA)
expect_equivalent(dim(tab), c(8, 5))
tab <- modelsummary(list(mod, mod), shape = term ~ model + statistic,
                    output = "data.frame", gof_map = NA)
expect_equivalent(dim(tab), c(4, 6))


tab <- modelsummary(mod,
                    statistic = c("std.error", "conf.int"),
                    shape = model + term ~ statistic,
                    fmt = 2,
                    conf_level = .99,
                    output = "data.frame",
                    gof_map = NA)
expect_equivalent(dim(tab), c(4, 7))
expect_true(all(c("Est.", "S.E.", "0.5 %", "99.5 %") %in% colnames(tab)))



# Michael E Flynn ultra-niche bug check
options(modelsummary_get = "easystats")
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
                        shape = term ~ model + response)
)
expect_snapshot_print(tab, "shape-multinom_wide")



# flipped table (no groups)
mod <- list(
lm(hp ~ mpg, mtcars),
lm(hp ~ mpg + drat, mtcars))
tab <- modelsummary(mod,
                    output = "data.frame",
                    shape = model ~ term)
expect_true("model" %in% colnames(tab))



# nnet::multinom: order of rows determined by formula terms
requiet("nnet")
dat_multinom <- mtcars
dat_multinom$cyl <- as.factor(dat_multinom$cyl)

mod <- list(
    nnet::multinom(cyl ~ mpg, data = dat_multinom, trace = FALSE),
    nnet::multinom(cyl ~ mpg + drat, data = dat_multinom, trace = FALSE))

## order of rows determined by order of formula terms
trash <- capture.output(tab <- modelsummary(mod, "data.frame", shape = response + term ~ model))
expect_inherits(tab, "data.frame")
expect_equivalent(colnames(tab),
             c("part", "response", "term", "statistic", "(1)", "(2)"))
expect_equivalent(tab$term[1:4], c("(Intercept)", "(Intercept)", "mpg", "mpg"))
expect_equivalent(tab$response[1:12], c(rep("6", 6), rep("8", 6)))

## order of rows determined by order of formula terms
trash <- capture.output(tab <- modelsummary(mod, "data.frame", shape = term + response ~ model))
expect_inherits(tab, "data.frame")
expect_equivalent(colnames(tab),
             c("part", "term", "response", "statistic", "(1)", "(2)"))
expect_equivalent(tab$term[1:4], rep("(Intercept)", 4))
expect_equivalent(tab$response[1:4], c("6", "6", "8", "8"))

## order of rows determined by order of formula terms
trash <- capture.output(tab <- modelsummary(mod, "data.frame", shape = model + term ~ response))
expect_inherits(tab, "data.frame")
expect_equivalent(colnames(tab),
             c("part", "model", "term", "statistic", "6", "8"))
expect_equivalent(tab$model[1:10], c(rep("(1)", 4), rep("(2)", 6)))

## order of rows determined by order of formula terms
trash <- capture.output(tab <- modelsummary(mod, "data.frame", shape = term + model ~ response))
expect_inherits(tab, "data.frame")
expect_equivalent(colnames(tab),
             c("part", "term", "model", "statistic", "6", "8"))
expect_equivalent(tab$model[1:3], c("(1)", "(1)", "(2)"))



# group ~ model + term
dat_multinom <- mtcars
dat_multinom$cyl <- as.factor(dat_multinom$cyl)
mod <- list(
    nnet::multinom(cyl ~ mpg, data = dat_multinom, trace = FALSE),
    nnet::multinom(cyl ~ mpg + drat, data = dat_multinom, trace = FALSE))

tab <- modelsummary(mod,
                    output = "data.frame",
                    shape = response ~ model + term,
                    metrics = "RMSE")
known <- c("part", "response", "statistic", "(1) / (Intercept)", "(1) / mpg",
"(2) / (Intercept)", "(2) / mpg", "(2) / drat")
expect_equivalent(colnames(tab), known)
expect_equivalent(dim(tab), c(6, 8))

tab <- modelsummary(mod,
                    output = "data.frame",
                    shape = response ~ term + model,
                    metrics = "RMSE")
known <- c("part", "response", "statistic", "(Intercept) / (1)", "(Intercept) / (2)",
"mpg / (1)", "mpg / (2)", "drat / (2)")
expect_equivalent(colnames(tab), known)
expect_equivalent(dim(tab), c(6, 8))


# nnet::multinom: order of columns determined by formula terms
requiet("nnet")
dat_multinom$cyl <- as.factor(dat_multinom$cyl)

mod <- list(
    nnet::multinom(cyl ~ mpg, data = dat_multinom, trace = FALSE),
    nnet::multinom(cyl ~ mpg + drat, data = dat_multinom, trace = FALSE))

## term ~ model + response
trash <- capture.output(
    tab <- modelsummary(mod, "data.frame", shape = term ~ model + response))
expect_inherits(tab, "data.frame")
expect_equivalent(colnames(tab),
             c("part", "term", "statistic", "(1) / 6", "(1) / 8", "(2) / 6", "(2) / 8"))

## term ~ response + model
trash <- capture.output(tab <- modelsummary(mod, "data.frame", shape = term ~ response + model))
expect_inherits(tab, "data.frame")
expect_equivalent(colnames(tab),
             c("part", "term", "statistic", "6 / (1)", "6 / (2)", "8 / (1)", "8 / (2)"))

## model ~ term + response
trash <- capture.output(tab <- modelsummary(mod, "data.frame", shape = model ~ term + response))
expect_inherits(tab, "data.frame")
expect_equivalent(colnames(tab),
             c("part", "model", "statistic", "(Intercept) / 6",
             "(Intercept) / 8", "mpg / 6", "mpg / 8", "drat / 6",
             "drat / 8"))

## model ~ response + term
trash <- capture.output(tab <- modelsummary(mod, "data.frame", shape = model ~ response + term))
expect_inherits(tab, "data.frame")
expect_equivalent(colnames(tab),
    c("part", "model", "statistic", "6 / (Intercept)", "6 / mpg", "6 / drat",
      "8 / (Intercept)", "8 / mpg", "8 / drat"))



# grouped coefficients: gamlss
requiet("gamlss")

data(abdom)
mod <- list(gamlss(y ~ pb(x),
                   sigma.fo = ~ pb(x), trace = FALSE,
                   family = BCT, data = abdom, method = mixed(1, 20)),
            gamlss(y ~ x,
                   sigma.fo = ~ pb(x), trace = FALSE,
                   family = BCT, data = abdom, method = mixed(1, 20)))

tab <- modelsummary(mod, "data.frame", shape = term + component ~ model)
expect_inherits(tab, "data.frame")
expect_equivalent(colnames(tab),
             c("part", "term", "component", "statistic", "(1)", "(2)"))

tab <- modelsummary(mod, "data.frame", shape = component + term ~ model)
expect_inherits(tab, "data.frame")
expect_equivalent(colnames(tab),
             c("part", "component", "term", "statistic", "(1)", "(2)"))

tab <- modelsummary(mod, "data.frame", shape = term ~ model + component)
expect_inherits(tab, "data.frame")

tab <- modelsummary(mod, "data.frame", shape = term ~ component + model)
expect_inherits(tab, "data.frame")

tab <- modelsummary(mod, "data.frame", shape = term + model ~ component)
expect_inherits(tab, "data.frame")

tab <- modelsummary(mod, "data.frame", shape = model + term ~ component)
expect_inherits(tab, "data.frame")



# model names are preserved
requiet("gamlss")
dat <- rgamma(100, shape=1, scale=10)
models <- list()
models[["GA"]] <- gamlss(dat ~ 1, family = GA, trace = FALSE)
models[["GA 2"]] <- gamlss(dat ~ 1, family = GA, trace = FALSE)
tab <- modelsummary(models, output = "data.frame", shape = component + term ~ model)
expect_true(all(c("GA", "GA 2") %in% colnames(tab)))



# group_map reorder rename
requiet("pscl")
data("bioChemists", package = "pscl")
mod <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
tab <- modelsummary(mod,
                    shape = component + term ~ model,
                    group_map = c("zero_inflated" = "Zero", "conditional" = "Count"),
                    output = "data.frame")
expect_equivalent(tab$component[1], "Zero")



# Issue #531
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
tab <- modelsummary(
    mod,
    output = "dataframe",
    shape = term + model ~ statistic,
    statistic = c("std.error", "{p.value}{stars}", "{estimate} ({statistic})"),
    fmt = fmt_statistic(estimate = 3, p.value = 2))
expect_equivalent(
    colnames(tab),
    c("part", "term", "model", "Est.", "S.E.", "p", "Est.  (t)"))


# Issue #631: bad group column
requiet("nnet")
dat_multinom <- mtcars
dat_multinom$cyl <- sprintf("Cyl: %s", dat_multinom$cyl)
mod <- list(
    nnet::multinom(cyl ~ mpg, data = dat_multinom, trace = FALSE),
    nnet::multinom(cyl ~ mpg + drat, data = dat_multinom, trace = FALSE))
tab <- modelsummary(mod, shape = model + term + response ~ statistic, output = "dataframe")
expect_true(all(tab$model %in% c("(1)", "(2)")))


# Issue #631: coef_rename
dat_multinom <- mtcars
dat_multinom <- transform(mtcars, vs = as.factor(vs), carb = as.factor(carb))
mod <- list(
    nnet::multinom(cyl ~ vs, data = dat_multinom, trace = FALSE),
    nnet::multinom(cyl ~ vs + carb, data = dat_multinom, trace = FALSE))

tab <- modelsummary(
    mod, shape = model + term + response ~ statistic,
    output = "dataframe", coef_rename = TRUE)
known <- sort(unique(tab$term))
unknown <- c("(Intercept)", "carb [2]", "carb [3]", "carb [4]", "carb [6]", "carb [8]", "vs [1]")
expect_equal(known, unknown)