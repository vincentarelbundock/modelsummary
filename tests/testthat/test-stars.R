mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

test_that("clean_stars unknown (we should never reach this)", {
  expect_null(modelsummary:::clean_stars(FALSE))
})


test_that("glue stars", {
  tab <- modelsummary(
    mod, 
    stars = FALSE,
    output = "data.frame",
    gof_omit = ".*",
    estimate = c("{estimate} ({std.error}){stars}",
                 "{estimate} [{conf.low}, {conf.high}]"),
    statistic = c("({std.error}){stars}", 
                  "[{conf.low}, {conf.high}]"))
  truth <- c("-1.986 (0.434)***", "(0.434)***", "[-2.873, -1.099]", 
             "0.665 (0.120)***", "(0.120)***", "[0.421, 0.909]", 
             "", "", "")
  expect_equal(truth, tab$OLS)
  truth <- c("4.739 [-2.760, 13.501]", "(4.045)", "[-2.760, 13.501]", "", "",
             "", "-0.288 [-0.784, 0.131]", "(0.228)", "[-0.784, 0.131]")
  expect_equal(truth, tab$Logit)
})


test_that("bug: make stars before rounding", {
  m <- lm(vs ~ hp + mpg + factor(cyl), data = mtcars)
  st <- c("*"=.49)
  tab1 <- modelsummary(m, stars=st, output="data.frame",
          statistic=NULL, fmt=1, gof_omit=".*")
  tab2 <- modelsummary(m, stars=st, output="data.frame",
          statistic=NULL, fmt=3, gof_omit=".*")
  tab1 <- grepl("\\*", tab1[[4]])
  tab2 <- grepl("\\*", tab2[[4]])
  expect_equal(tab1, tab2)
})


test_that("same stars with different statistics", {
  m <- lm(dist ~ speed, data = cars)
  tab1 <- modelsummary(m, stars=TRUE, output="dataframe")
  tab2 <- modelsummary(m, 
    statistic="p.value", 
    stars=TRUE, 
    output="dataframe")
  tab3 <- modelsummary(m, 
    statistic=c("p.value", "conf.int"), 
    stars=TRUE,
    output="dataframe")
  tab1 <- tab1[[4]]
  tab2 <- tab2[[4]]
  tab3 <- tab3[[4]]
  expect_equal(tab1[c(1, 3)], tab2[c(1, 3)])
  expect_equal(tab1[c(1, 3)], tab3[c(1, 4)])
})


test_that("stars = FALSE", {
  raw <- modelsummary(mod, stars=FALSE, output="dataframe")
  truth <- c("-1.986", "(0.434)", "0.665", "(0.120)")
  expect_equal(truth, unname(raw[[4]][1:4]))
  truth <- c("4.739", "(4.045)", "", "", "-0.288", "(0.228)")
  expect_equal(truth, unname(raw[[5]][1:6]))
})

test_that("stars = TRUE", {

  raw <- modelsummary(mod, stars = TRUE, output="dataframe")

  truth <- c("-1.986***", "(0.434)", "0.665***", "(0.120)")
  expect_equal(truth, unname(raw[[4]][1:4]))

  truth <- c("4.739", "(4.045)", "", "", "-0.288", "(0.228)")
  expect_equal(truth, unname(raw[[5]][1:6]))

})

test_that("custom stars", {

  raw <- modelsummary(mod, stars = c('+' = .8, '*' = .1), output="dataframe")

  truth <- c("-1.986*", "(0.434)", "0.665*", "(0.120)")
  expect_equal(truth, unname(raw[[4]][1:4]))

  truth <- c("4.739+", "(4.045)", "", "", "-0.288+", "(0.228)")
  expect_equal(truth, unname(raw[[5]][1:6]))

})
