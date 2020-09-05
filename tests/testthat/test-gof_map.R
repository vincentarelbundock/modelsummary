context('gof_map')

library(modelsummary)

mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

test_that('omit all gof', {

  gof_map_custom <- modelsummary::gof_map
  gof_map_custom$omit <- TRUE
  raw <- modelsummary:::extract_models(mod, gof_map = gof_map_custom)

  truth <- c("-1.986", "(0.434)", "0.665", "(0.120)", "", "")
  expect_equal(truth, unname(raw[[4]]))

  truth <- c("4.739", "(4.045)", "", "", "-0.288", "(0.228)")
  expect_equal(truth, unname(raw[[5]]))

})


test_that('omit all gof from only a few models', {

  mod1 <- glm(vs ~ mpg, data = mtcars, family = "binomial")
  mod2 <- lm(vs ~ mpg, data = mtcars)
  gof_map <- modelsummary::gof_map
  gof_map$omit <- TRUE
  gof_map$omit[gof_map$raw == "r.squared"] <- FALSE

  tmp <- modelsummary:::extract_models(list(mod1, mod2), gof_map = gof_map)

  expect_equal(nrow(tmp), 5)
  expect_equal(ncol(tmp), 5)

})


test_that("F statistic omitted by default except for lm objects", {

  mod <- list()
  mod$OLS <- lm(am ~ drat, data = mtcars)
  mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())
  unknown <- paste(msummary(mod, "markdown"), collapse = "\n")
  known <-
    "|            |OLS     |Logit   |
|:-----------|:-------|:-------|
|(Intercept) |-1.986  |4.739   |
|            |(0.434) |(4.045) |
|drat        |0.665   |        |
|            |(0.120) |        |
|qsec        |        |-0.288  |
|            |        |(0.228) |
|Num.Obs.    |32      |32      |
|R2          |0.508   |        |
|R2 Adj.     |0.492   |        |
|AIC         |28.6    |45.5    |
|BIC         |33.0    |48.4    |
|Log.Lik.    |-11.306 |-20.733 |
|F           |30.970  |        |"
  expect_equal(known, unknown)

})
