skip_if(getRversion() < '3.6.6') # change in .Rng
skip_if_not_installed("fixest", minimum_version = "0.10.5")
requiet("fixest")


test_that("fixest built in SE label", {
    mod <- feols(Euros ~ dist_km | Product + Destination + Origin, data = trade)
    tab <- modelsummary(mod, "data.frame")
    expect_true("by: Product" %in% tab[["Model 1"]])
})


test_that("multi: after 0.10.5", {
  skip_if_not_installed("fixest", "0.10.5")
  mod <- feols(mpg ~ hp, split = ~cyl, data = mtcars)
  tab <- modelsummary(mod, "data.frame")
  expect_true(all(c("cyl: 4", "cyl: 6", "cyl: 8") %in% colnames(tab)))

  v <- c("mpg", "wt", "drat")
  mod <- feols(.[v] ~ hp, data = mtcars)
  tab <- modelsummary(mod, "data.frame")
  expect_true(all(c("mpg", "wt", "drat") %in% colnames(tab)))
})


test_that("simple model", {
  mod <- feols(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris)
  raw <- modelsummary(mod, "data.frame")
  expect_s3_class(raw, "data.frame")
  expect_equal(ncol(raw), 4)
  expect_true("by: Species" %in% raw[["Model 1"]])
})


test_that("gof_map standard errors with `vcov.type`", {
  gm <- list(
    list("raw" = "FE: gear", "clean" = "FE: Gear", fmt = 0),
    list("raw" = "vcov.type", "clean" = "Uncertainty", fmt = 0))
  mod <- feols(mpg ~hp | gear, data = mtcars)
  tab <- modelsummary(mod,
                      gof_map = gm,
                      output = "data.frame")
  expect_true("Uncertainty" %in% tab$term)
  expect_true("FE: Gear" %in% tab$term)
})


test_that("fixest std.error labels", {
  mod <- feols(hp ~ mpg + drat, mtcars, cluster = "vs")
  tab <- modelsummary(mod, output = "data.frame")
  expect_equal(tab[tab$term == "Std.Errors", "Model 1"], "by: vs")
  tab <- modelsummary(mod, vcov = list(NULL), output = "data.frame")
  expect_equal(tab[tab$term == "Std.Errors", "Model 1"], "by: vs")
  tab <- modelsummary(mod, vcov = list(NULL, "iid"), output = "data.frame")
  expect_equal(tab[tab$term == "Std.Errors", "Model 1"], "by: vs")
  expect_equal(tab[tab$term == "Std.Errors", "Model 2"], "by: vs")
  # unnamed function includes no label
  tab1 <- modelsummary(mod,
                       vcov = vcov(mod, se = "standard"),
                       output = "data.frame")
  tab2 <- modelsummary(mod,
                       vcov = list(vcov(mod, se = "standard")),
                       output = "data.frame")
  tab3 <- modelsummary(mod,
                       output = "data.frame",
                       vcov = list("test " = stats::vcov(mod, se = "standard")))
  expect_true("Custom" %in% tab1[["Model 1"]])
  expect_true("Custom" %in% tab2[["Model 1"]])
  expect_true("test " %in% tab3[["Model 1"]])
})


test_that("regression: issue #450", {
    requiet("sandwich")
    mod <- feols(mpg ~ wt, data = mtcars)

    se1 <- sqrt(diag(vcovHC(mod, type = "HC3")))
    tab <- modelsummary(mod,
        vcov = "HC3",
        gof_map = NA,
        fmt = 10,
        estimate = "std.error",
        statistic = NULL,
        output = "dataframe")
    expect_equal(as.numeric(tab[["Model 1"]]), se1, ignore_attr = TRUE)

    se1 <- sqrt(diag(vcovHC(mod, type = "HC1")))
    se2 <- get_estimates(mod, vcov = "HC1")$std.error
    expect_equal(se2, se1, ignore_attr = TRUE)
})


test_that("Issue #551", {
    mod <- suppressMessages(feols(mpg ~ hp * i(cyl), data = mtcars))
    cm <- c(
      "cyl::6" = "Cylinders 6",
      "cyl::8" = "Cylinders 8",
      "hp:cyl::4" = "HP x Cylinders 4",
      "hp:cyl::6" = "HP x Cylinders 6")
    tab <- modelsummary(mod, coef_map = cm, output = "dataframe")
    expect_true("HP x Cylinders 4" %in% tab$term)
})




