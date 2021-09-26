library(modelsummary)

mod <- list()
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod$OLS <- lm(am ~ cyl, data = dat)
mod$Logit <- glm(am ~ cyl, data = dat, family = binomial())

test_that("data.frame", {
  rows = read.csv(text = 
    "term      , OLS , Logit
     cyl4      , -   , - 
     NEW GOF 1 , ?   , ?
     NEW GOF 2 , X   , X
     NEW GOF 3 , Y   , Y")
  attr(rows, 'position') <- c(3, 8, 9, 12)
  expect_snapshot(modelsummary(mod, add_rows = rows, output = "markdown"))
})

test_that('add_rows numeric are formatted by fmt', {
  tmp <- data.frame(a = 1:2, b = 2:3, c = 3:4)
  tab <- datasummary(hp + mpg ~ mean + sd, data = mtcars, add_rows = tmp,
                     fmt = "%.0f", output = 'dataframe')
  expect_equal(tab$sd, c('69', '6', '3', '4'))
})
