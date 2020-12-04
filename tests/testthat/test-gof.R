context('gof_map')

library(modelsummary)

mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

test_that('gof_omit omits everything', {
  tab = modelsummary(mod, gof_omit=".*", output="data.frame")
  expect_equal(dim(tab), c(6, 5))
})

test_that('custom gof_map omits everything by default', {
  gm = tibble::tribble(~raw,        ~clean, ~fmt, ~omit,
                       "nobs",      "Num.Obs", 0, omit=FALSE,
                       "r.squared", "R2", 2, omit=FALSE,
                       "r2", "R2", 2, omit=FALSE)
  tab = modelsummary(mod, gof_map=gm, output="data.frame")
  expect_equal(dim(tab), c(8, 5))
})


test_that('omit all gof', {
  gof_map_custom <- modelsummary::gof_map
  gof_map_custom$omit <- TRUE
  raw <- modelsummary(mod, gof_map = gof_map_custom, output="dataframe")
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
  gof_map$omit[gof_map$raw == "r2"] <- FALSE
  tmp <- modelsummary(list(mod1, mod2), gof_map = gof_map, output="dataframe")
  expect_equal(nrow(tmp), 5)
  expect_equal(ncol(tmp), 5)
})

test_that("F statistic omitted by default except for lm objects", {
  mod <- list(
    "OLS"   = lm(am ~ drat, data = mtcars),
    "Logit" = glm(am ~ qsec, data = mtcars, family = binomial())
  )
  tab = modelsummary(mod, output="data.frame")  
  expect_false(tab$OLS[nrow(tab)] == "")
  expect_true(tab$Logit[nrow(tab)] == "")
})
