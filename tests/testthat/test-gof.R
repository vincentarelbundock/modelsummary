mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())


test_that('gof_omit omits everything', {
  tab <- modelsummary(mod, gof_omit = ".*", output = "data.frame")
  expect_equal(dim(tab), c(6, 5))
})


test_that('custom gof_map omits everything by default', {
  # error about factor levels, not clear why
  skip_if(getRversion() < '4.0.0')
  gm <- read.csv(text =
"raw,clean,fmt
nobs,Num.Obs,0
r.squared,R2,2")
  tab <- modelsummary(mod, gof_map = gm, output = "dataframe")
  expect_equal(dim(tab), c(8, 5))
})


test_that('list of lists', {
  f1 <- function(x) sprintf("%.0f", x)
  f2 <- function(x) sprintf("%.1f", x)
  gm <- list(
    list("raw" = "nobs", "clean" = "N", "fmt" = f1),
    list("raw" = "aic", "clean" = "AIC", "fmt" = f2))
  tab <- modelsummary(mod, output = "data.frame", gof_map = gm)
  tab <- tab[tab$part == "gof",]
  expect_equal(tab$OLS, c("32", "28.6"))
  expect_equal(tab$Logit, c("32", "45.5"))
})


test_that('omit all gof', {
  gof_map_custom <- modelsummary::gof_map
  gof_map_custom$omit <- TRUE
  raw <- modelsummary(mod, gof_map = list(), output="dataframe")
  truth <- c("-1.986", "(0.434)", "0.665", "(0.120)", "", "")
  expect_equal(truth, unname(raw[[4]]))
  truth <- c("4.739", "(4.045)", "", "", "-0.288", "(0.228)")
  expect_equal(truth, unname(raw[[5]]))
})


test_that('omit all gof from only a few models', {
  mod1 <- glm(vs ~ mpg, data = mtcars, family = "binomial")
  mod2 <- lm(vs ~ mpg, data = mtcars)
  gm <- modelsummary::gof_map
  gm <- gm[gm$raw %in% c("r2", "r.squared"),]
  tmp <- modelsummary(list(mod1, mod2), gof_map = gm, output="dataframe")
  expect_equal(nrow(tmp), 5)
  expect_equal(ncol(tmp), 5)
})


test_that("F statistic included for both `lm` and `glm` objects", {
  mod <- list(
    "OLS"   = lm(am ~ drat, data = mtcars),
    "Logit" = glm(am ~ qsec, data = mtcars, family = binomial())
  )
  tab <- modelsummary(mod, output = "data.frame")
  expect_true(tab$OLS[tab$term == "F"] != "")
  expect_true(tab$Logit[tab$term == "F"] != "")
})
