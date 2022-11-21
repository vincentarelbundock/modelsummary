library(modelsummary)

test_that("perl=TRUE allows lookbehind", {
  # omit vs except when it is preceded by mpg
  mod <- lm(hp ~ mpg * vs, mtcars)
  out <- modelsummary(mod, 
                      coef_omit="^(?!mpg).*vs",
                      output="data.frame")
  expect_equal(out$term[1:7],
               c("(Intercept)", "(Intercept)", "mpg", "mpg", "mpg × vs", "mpg × vs", "Num.Obs."))
})

test_that("omit coefficients using regular expressions", {

  mod <- list()
  mod$OLS <- lm(am ~ drat, data = mtcars)
  mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

  raw <- modelsummary(mod, coef_omit = c('drat|qsec'), output="dataframe")

  truth <- c('(Intercept)', '(Intercept)', 'Num.Obs.')
  expect_equal(unname(raw[[2]][1:3]), truth)

})


# numeric indices
test_that("numeric indices", {
  mod <- list(
    lm(mpg ~ hp + factor(cyl) + drat + factor(am), mtcars),
    lm(mpg ~ factor(cyl) + drat, mtcars))
  tab <- modelsummary(mod, "data.frame", coef_omit = 1:2)
  expect_false("(Intercept)" %in% tab$term)
  tab <- modelsummary(mod, "data.frame", gof_map = NA, coef_omit = 3)
  expect_equal(nrow(tab), 10)
  tab <- modelsummary(mod, "data.frame", gof_map = NA, coef_omit = 2:3)
  expect_equal(nrow(tab), 8)
  expect_error(
    modelsummary(mod, shape = model ~ term, coef_omit = 3),
    regexp = "shape")
  tab <- modelsummary(mod, "data.frame", coef_omit = -1, gof_map = NA)
  expect_equal(nrow(tab), 2)
  tab <- modelsummary(mod, "data.frame", coef_omit = -c(1, 3), gof_map = NA)
  expect_equal(nrow(tab), 4)
  expect_error(
    modelsummary(mod, "data.frame", coef_omit = -1:3, gof_map = NA),
    regexp = "sign")
})
