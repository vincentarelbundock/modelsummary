context('stars')

library(modelsummary)

mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

test_that("same stars with different statistics", {

  m <- lm(dist ~ speed, data = cars)
  tab1 <- modelsummary:::extract_models(m, stars = TRUE)
  tab2 <- modelsummary:::extract_models(m, statistic = 'p.value',
    stars = TRUE)
  tab3 <- modelsummary:::extract_models(m,
    statistic = c('p.value', 'conf.int'),
    stars = TRUE)

  expect_equal(tab1[c(1, 3), 4], tab2[c(1, 3), 4])
  expect_equal(tab1[c(1, 3), 4], tab3[c(1, 4), 4])

})


test_that("stars = FALSE", {

  raw <- modelsummary:::extract_models(mod, stars = FALSE)

  truth <- c("-1.986", "(0.434)", "0.665", "(0.120)")
  expect_equal(truth, unname(raw[[4]][1:4]))

  truth <- c("4.739", "(4.045)", "", "", "-0.288", "(0.228)")
  expect_equal(truth, unname(raw[[5]][1:6]))

})

test_that("stars = TRUE", {

  raw <- modelsummary:::extract_models(mod, stars = TRUE)

  truth <- c("-1.986***", "(0.434)", "0.665***", "(0.120)")
  expect_equal(truth, unname(raw[[4]][1:4]))

  truth <- c("4.739", "(4.045)", "", "", "-0.288", "(0.228)")
  expect_equal(truth, unname(raw[[5]][1:6]))

})

test_that("custom stars", {

  raw <- modelsummary:::extract_models(mod, stars = c('+' = .8, '*' = .1))

  truth <- c("-1.986*", "(0.434)", "0.665*", "(0.120)")
  expect_equal(truth, unname(raw[[4]][1:4]))

  truth <- c("4.739+", "(4.045)", "", "", "-0.288+", "(0.228)")
  expect_equal(truth, unname(raw[[5]][1:6]))

})
