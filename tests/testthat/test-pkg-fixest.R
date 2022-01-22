skip_if(getRversion() < '3.6.6') # change in .Rng
requiet("fixest")

test_that("simple model", {
  mod <- feols(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris)
  raw <- modelsummary(mod, "data.frame")
  expect_s3_class(raw, "data.frame")
  expect_equal(dim(raw), c(14, 4))
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
  expect_equal(tab[tab$term == "Std.Errors", "Model 2"], "IID")
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
  expect_false("Std.Errors" %in% tab1$term)
  expect_false("Std.Errors" %in% tab2$term)
  expect_true("Std.Errors" %in% tab3$term)
})
