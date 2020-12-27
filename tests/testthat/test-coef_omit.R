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
