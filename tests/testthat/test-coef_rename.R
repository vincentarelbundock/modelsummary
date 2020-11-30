context('coef_rename')

library(modelsummary)

mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

test_that("rename 2 out of 3 coefficients", {
  cmap <- c('(Intercept)'='Constant', 'drat'='Rear axle ratio')
  mod <- modelsummary(mod, "dataframe", coef_rename=cmap)
  known <- c("Constant", "Constant", "Rear axle ratio", "Rear axle ratio", "qsec")
  expect_equal(mod$term[1:5], known)
})

test_that("coef_rename and coef_map are incompatible", {
  cmap <- c('(Intercept)'='Constant', 'drat'='Rear axle ratio')
  expect_error(modelsummary(mod, coef_rename=cmap, coef_map=cmap))
})
