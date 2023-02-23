source("helpers.R")
requiet("lme4")

# random components are displayed together
mod <- lmer(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + (1 | Species), data = iris)
tab <- modelsummary(mod, output = "data.frame", gof_map = NA)
expect_equivalent(
    tab$term,
    c("(Intercept)", "(Intercept)", "Sepal.Width", "Sepal.Width", "Petal.Length", "Petal.Length", "Petal.Width", "Petal.Width", "SD (Intercept Species)", "SD (Observations)")
)

# Issue #505
mod <- lme4::lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris)
tab <- modelsummary(mod, output = "dataframe")
expect_inherits(tab, "data.frame")
tab <- modelsummary(mod, ci_random = TRUE, output = "dataframe")
expect_inherits(tab, "data.frame")
tab <- modelsummary(mod, statistic = "conf.int", ci_random = TRUE, output = "dataframe")
expect_inherits(tab, "data.frame")
tab <- modelsummary(mod, output = "data.frame", statistic = "conf.int", ci_random = TRUE)
expect_inherits(tab, "data.frame")
tab <- modelsummary(mod, output = "data.frame", statistic = "conf.int", ci_random = TRUE)

# 4 confidence intervals includes the random terms
# TODO: Did that change upstream?
# expect_equivalent(sum(grepl("\\[", tab[["(1)"]])), 4)

# Issue #501
mod <- lme4::lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris)
tab <- modelsummary(mod, "data.frame")
expect_true("AIC" %in% tab$term)
expect_false("aicc" %in% tab$term)

# Issue #494 comment
models <- modelsummary:::hush(list(
    lme4::lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris),
    lme4::lmer(Sepal.Width ~ Petal.Length + (1 + Petal.Length | Species), data = iris),
    lme4::lmer(Sepal.Width ~ Petal.Length + Petal.Width + (1 + Petal.Length | Species), data = iris)
))
tab1 <- modelsummary(
    models[[3]],
    estimate = "{estimate} [{conf.low}, {conf.high}]",
    statistic = NULL,
    gof_map = NA,
    output = "dataframe")
tab2 <- suppressMessages(data.frame(parameters::parameters(models[[3]], effects = "all")))
expect_equivalent(nrow(tab1), nrow(tab2))

# Issue #496: multiple models keeps random/fixed grouped together
models <- modelsummary:::hush(list(
    lm(Sepal.Width ~ Petal.Length + Petal.Width, data = iris),
    lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris),
    lmer(Sepal.Width ~ Petal.Length + (1 + Petal.Length | Species), data = iris),
    lmer(Sepal.Width ~ Petal.Length + Petal.Width + (1 + Petal.Length | Species), data = iris)
))
tab <- modelsummary(
    models,
    output = "data.frame",
    statistic = NULL)
expect_equivalent(
    tab$term[1:7],
    c("(Intercept)", "Petal.Length", "Petal.Width", "SD (Intercept Species)", "SD (Petal.Length Species)", "Cor (Intercept~Petal.Length Species)", "SD (Observations)"))

# Issue #494: glue-related partial breakage
mod <- lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris)
tab <- modelsummary(
    mod,
    output = "dataframe",
    estimate = "{estimate} [{conf.low}, {conf.high}] ({p.value})",
    statistic = NULL,
    gof_map = NA)
expect_equivalent(nrow(tab), 4) # a lot of rows used to be omitted

# better lme4 printout
data(sleepstudy)
set.seed(12345)
sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
mod <- lmer(
    Reaction ~ (Days + 1 | grp) + (1 | Subject),
    data = sleepstudy)
tab <- msummary(mod, "dataframe")
expect_true("SD (Days grp)" %in% tab$term)

mod <- modelsummary:::hush(lmer(
    Reaction ~ Days + (1 | grp) + (1 + Days | Subject),
    data = sleepstudy))
tab <- modelsummary(mod, "dataframe") # no warning

# random effects variance components do not have standard errors and produce "empty"
mod <- lmer(mpg ~ hp + (1 | gear), mtcars)
tab <- modelsummary(mod, output = "data.frame", metrics = "RMSE")
known <- c("(Intercept)", "(Intercept)", "hp", "hp", "SD (Intercept gear)", "SD (Observations)", "Num.Obs.", "RMSE")
expect_equivalent(tab$term, known)

# performance metrics
N <- 1e4
dat <- data.frame(
    x = rnorm(N),
    y = rnorm(N),
    k = factor(sample(1:50, N, replace = TRUE)),
    m = factor(sample(1:1000, N, replace = TRUE)))
mod <- suppressMessages(lmer(y ~ x + (1 | k) + (1 | m), data = dat))
tab1 <- modelsummary(mod,
    output = "data.frame",
    group = term + group ~ model)
tab2 <- modelsummary(
    mod,
    output = "data.frame",
    group = term + group ~ model,
    metrics = c("RMSE", "BIC"))
expect_true("RMSE" %in% tab1$term)
expect_false("R2" %in% tab1$term)
expect_true(all(c("RMSE", "BIC") %in% tab2$term))

# lme4
d <- as.data.frame(ChickWeight)
colnames(d) <- c("y", "x", "subj", "tx")
mod <- lmer(y ~ tx * x + (x | subj), data = d)
tab <- modelsummary(mod, output = "dataframe")
expect_inherits(tab, "data.frame")
expect_true(nrow(tab) > 21)

# sandwich does not support lmer
expect_error(suppressWarnings(modelsummary(mod, vcov = "robust")), pattern = "Unable to extract")
expect_error(suppressWarnings(modelsummary(mod, vcov = ~subj)), pattern = "Unable to extract")

# lme4 with 2 random effects
mod <- lmer(mpg ~ hp + (1 | am) + (1 | cyl), data = mtcars)
tab <- modelsummary(mod, output = "data.frame", gof_omit = ".*") # no longer raises warning
tab <- suppressWarnings(modelsummary(mod, output = "data.frame", gof_omit = ".*"))
expect_inherits(tab, "data.frame")
tab <- modelsummary(mod,
    output = "data.frame", gof_omit = ".*",
    group = group + term ~ model)
expect_inherits(tab, "data.frame")
expect_equivalent(dim(tab), c(7, 5))

# lme4 with parameter's effects argument
d <- as.data.frame(ChickWeight)
colnames(d) <- c("y", "x", "subj", "tx")
mod <- lmer(y ~ tx * x + (x | subj), data = d)

# all effects implicit
tab <- modelsummary(mod, output = "dataframe")
tab <- tab[tab$part == "estimates", ]
expect_equivalent(nrow(tab), 20)

# all effects explicit
tab <- modelsummary(mod, output = "dataframe", effects = "all")
tab <- tab[tab$part == "estimates", ]
expect_equivalent(nrow(tab), 20)

# fixed effects explicit
tab <- modelsummary(mod, output = "dataframe", effects = "fixed")
tab <- tab[tab$part == "estimates", ]
expect_equivalent(nrow(tab), 16)

# random effects explicit
tab <- modelsummary(mod, output = "dataframe", effects = "random")
tab <- tab[tab$part == "estimates", ]
expect_equivalent(nrow(tab), 4)

# Issue #566
data(Orthodont, package = "nlme")
Orthodont$nsex <- as.numeric(Orthodont$Sex == "Male")
m1 <- lm(distance ~ age * nsex, data = Orthodont)
m2 <- lmer(distance ~ age * nsex + (1 | Subject), data = Orthodont)
tab <- modelsummary(list(m1, m2), output = "dataframe")
expect_false("agensex" %in% tab$term)