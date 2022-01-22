requiet("lme4")

test_that("first call raises a warning about `performance` metrics.", {
    mod <- lmer(mpg ~ hp + (1 | gear), data = mtcars)
    expect_warning(modelsummary(mod))
})

test_that('random effects variance components do not have standard errors and produce "empty"', {
    mod <- lmer(mpg ~ hp + (1 | gear), mtcars)
    tab <- modelsummary(mod, output = "data.frame", metrics = "RMSE")
    known <- c("(Intercept)", "(Intercept)", "hp", "hp", "SD (Intercept)", "SD (Observations)", "Num.Obs.", "RMSE")
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

    modelsummary(mod, group = term + group ~ model)
    tab1 <- modelsummary(mod, output = "data.frame", group = term + group ~ model)
    tab2 <- modelsummary(mod, output = "data.frame", group = term + group ~ model, metrics = c("RMSE", "BIC"))
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
  expect_error(modelsummary(mod, vcov = "robust"), regexp = "Unable to extract")
  expect_error(modelsummary(mod, vcov =~ subj), regexp = "Unable to extract")
})


test_that("lme4 with 2 random effects", {
  mod <- lmer(mpg ~ hp + (1|am) + (1|cyl), data = mtcars)
  expect_warning(modelsummary(mod, output = "data.frame", gof_omit = ".*"),
                 regexp = "duplicate")
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
