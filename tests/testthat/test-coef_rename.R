library(modelsummary)

mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

test_that("unique names", {
  cmap <- c('drat'='Constant', 'drat'='Rear axle ratio')
  expect_error(modelsummary(mod, "data.frame", coef_rename = cmap),
               regexp = "duplicated")

  cmap <- c('Constant', 'Rear axle ratio')
  expect_error(modelsummary(mod, "data.frame", coef_rename = cmap),
               regexp = "Must have names")
})

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


test_that("regression test: coef_rename() function", {
    x <- list(
      lm(mpg ~ factor(cyl) + drat + disp, data = mtcars),
      lm(hp ~ factor(cyl) + drat + disp, data = mtcars))
    expect_error(modelsummary(dvnames(x), coef_rename = coef_rename), NA)
})
