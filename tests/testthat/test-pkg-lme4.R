requiet("lme4")


test_that("Issue #505", {
    skip_if_not_installed("parameters", minimum_version = "0.18.2")
    mod <- lme4::lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris)
    expect_error(modelsummary(mod, output = "dataframe"), NA)
    expect_error(modelsummary(mod, ci_random = TRUE, output = "dataframe"), NA)
    expect_error(modelsummary(mod, statistic = "conf.int", ci_random = TRUE, output = "dataframe"), NA)
    expect_error(modelsummary(mod, output = "data.frame", statistic = "conf.int", ci_random = TRUE), NA)
    tab <- modelsummary(mod, output = "data.frame", statistic = "conf.int", ci_random = TRUE)

    skip("TODO: not sure why this doesn't work on some platforms")
    # 4 confidence intervals includes the random terms
    expect_equal(sum(grepl("\\[", tab[["Model 1"]])), 4)
})


test_that("Issue #501", {
    mod <- lme4::lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris)
    tab <- modelsummary(mod, "data.frame")
    expect_true("AIC" %in% tab$term)
    expect_false("aicc" %in% tab$term)
})


test_that("Issue #494 comment", {
    skip_if_not_installed("parameters", minimum_version = "0.18.1.7")
    models <- modelsummary:::hush(list(
        lme4::lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris),
        lme4::lmer(Sepal.Width ~ Petal.Length + (1 + Petal.Length |Species), data = iris),
        lme4::lmer(Sepal.Width ~ Petal.Length + Petal.Width + (1 + Petal.Length |Species), data = iris)
    ))
    tab1 <- modelsummary(
        models[[3]],
        estimate = "{estimate} [{conf.low}, {conf.high}]",
        statistic = NULL,
        gof_map = NA,
        output = "dataframe")
    tab2 <- suppressMessages(data.frame(parameters::parameters(models[[3]], effects = "all")))
    expect_equal(nrow(tab1), nrow(tab2))
})


test_that("Issue #496: multiple models keeps random/fixed grouped together", {
    models <- modelsummary:::hush(list(
        lm(Sepal.Width ~ Petal.Length, data = iris),
        lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris),
        lmer(Sepal.Width ~ Petal.Length + (1 + Petal.Length |Species), data = iris),
        lmer(Sepal.Width ~ Petal.Length + Petal.Width + (1 + Petal.Length | Species), data = iris)
    ))
    tab <- modelsummary(
        models,
        output = "data.frame",
        statistic = NULL)
    expect_equal(
        tab$term[1:7],
        c("(Intercept)", "Petal.Length", "Petal.Width", "SD (Intercept Species)",
        "SD (Petal.Length Species)", "Cor (Intercept~Petal.Length Species)", "SD (Observations)"))
})


test_that("Issue #494: glue-related partial breakage", {
    skip_if_not_installed("parameters", minimum_version = "0.18.1.7")
    mod <- lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris)
    tab <- modelsummary(
        mod,
        output = "dataframe",
        estimate = "{estimate} [{conf.low}, {conf.high}] ({p.value})",
        statistic = NULL,
        gof_map = NA)
    expect_equal(nrow(tab), 4) # a lot of rows used to be omitted
})


test_that("better lme4 printout", {
    data(sleepstudy)
    set.seed(12345)
    sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
    mod <- lmer(
      Reaction ~ (Days + 1 | grp ) + (1 | Subject),
      data = sleepstudy)
    expect_warning(tab <- msummary(mod, "dataframe"), NA)
    expect_true("SD (Days grp)" %in% tab$term)

    mod <- modelsummary:::hush(lmer(
      Reaction ~ Days + (1 | grp ) + (1 + Days | Subject),
      data = sleepstudy))
    expect_warning(modelsummary(mod, "dataframe"), NA)
})


test_that('random effects variance components do not have standard errors and produce "empty"', {
    mod <- lmer(mpg ~ hp + (1 | gear), mtcars)
    tab <- modelsummary(mod, output = "data.frame", metrics = "RMSE")
    known <- c("(Intercept)", "(Intercept)", "hp", "hp", "SD (Intercept gear)", "SD (Observations)", "Num.Obs.", "RMSE")
    expect_equal(tab$term, known)
})


test_that("performance metrics", {
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
})




test_that("lme4", {
  d <- as.data.frame(ChickWeight)
  colnames(d) <- c("y", "x", "subj", "tx")
  mod <- lmer(y ~ tx * x + (x | subj), data = d)
  tab <- modelsummary(mod, output="dataframe")
  expect_s3_class(tab, "data.frame")
  expect_true(nrow(tab) > 21)

  # sandwich does not support lmer
  expect_error(suppressWarnings(modelsummary(mod, vcov = "robust")), regexp = "Unable to extract")
  expect_error(suppressWarnings(modelsummary(mod, vcov =~ subj)), regexp = "Unable to extract")
})


test_that("lme4 with 2 random effects", {
  mod <- lmer(mpg ~ hp + (1|am) + (1|cyl), data = mtcars)
  expect_warning(modelsummary(mod, output = "data.frame", gof_omit = ".*"), NA) # no longer raises warning
  tab <- suppressWarnings(modelsummary(mod, output = "data.frame", gof_omit = ".*"))
  expect_s3_class(tab, "data.frame")
  tab <- modelsummary(mod, output = "data.frame", gof_omit = ".*",
                      group = group + term ~ model)
  expect_s3_class(tab, "data.frame")
  expect_equal(dim(tab), c(7, 5))
})


test_that("lme4 with parameter's effects argument", {
  d <- as.data.frame(ChickWeight)
  colnames(d) <- c("y", "x", "subj", "tx")
  mod <- lmer(y ~ tx * x + (x | subj), data = d)

  # all effects implicit
  tab <- modelsummary(mod, output="dataframe")
  tab <- tab[tab$part == "estimates",]
  expect_equal(nrow(tab), 20)

  # all effects explicit
  tab <- modelsummary(mod, output="dataframe", effects = "all")
  tab <- tab[tab$part == "estimates",]
  expect_equal(nrow(tab), 20)

  # fixed effects explicit
  tab <- modelsummary(mod, output="dataframe", effects = "fixed")
  tab <- tab[tab$part == "estimates",]
  expect_equal(nrow(tab), 16)

  # random effects explicit
  tab <- modelsummary(mod, output="dataframe", effects = "random")
  tab <- tab[tab$part == "estimates",]
  expect_equal(nrow(tab), 4)
})


test_that("Issue #566", {
  data(Orthodont, package = "nlme")
  Orthodont$nsex <- as.numeric(Orthodont$Sex == "Male")
  m1 <- lm(distance ~ age * nsex, data = Orthodont)
  m2 <- lmer(distance ~ age * nsex + (1 | Subject), data = Orthodont)
  tab <- modelsummary(list(m1, m2), output = "dataframe")
  expect_false("agensex" %in% tab$term)
})
