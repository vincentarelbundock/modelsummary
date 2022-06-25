test_that("rounding gives expected results", {
    expect_equal("blah", modelsummary:::rounding("blah"))
    expect_equal("3.142", modelsummary:::rounding(pi))
    expect_equal(c("1", "2"), modelsummary:::rounding(factor(1:2)))
    expect_equal("2", modelsummary:::rounding(2, TRUE))
})


test_that("rounding cleans up NaN inside \\num", {
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    expect_snapshot(datasummary(cyl + mpg ~ SD + N, data = dat, output = "latex"))
})


test_that("siunitx works with empty cells", {
  skip("new versions of parameters return CI, so this is no longer a usefult est")
  requiet("lme4")
  dat <- mtcars
  dat$cyl <- factor(dat$cyl)
  mod <- lme4::lmer(mpg ~ hp + (1 | cyl), data = dat)
  expect_snapshot(modelsummary(
      mod,
      output = "latex",
      estimate = "{estimate} [{conf.low}, {conf.high}]"))
})


test_that("named list", {
    mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
    tab <- modelsummary(
        mod,
        statistic = c("SE: {std.error}", "conf.int"),
        fmt = list(estimate = 0, std.error = 4, r.squared = 7, rmse = 5),
        output = "data.frame")
    expect_equal(
        tab[["Model 1"]][1:3],
        c("29", "SE: 1.5878", "[25.398, 31.903]"))
    expect_equal(
        tab[["Model 1"]][tab$term == "R2"],
        "0.7538578")
    expect_equal(
        tab[["Model 1"]][tab$term == "RMSE"],
        "2.94304")
})


test_that("glue function", {
    m <- glm(am ~ mpg, data = mtcars, family = binomial)
    tab <- modelsummary(
        m,
        output = "data.frame",
        fmt = NULL,
        estimate = "{round(exp(estimate), 5)}",
        statistic = "{round(exp(estimate) * std.error, 3)}")
    known <- c("0.00136", "0.003", "1.35938", "0.156")
    expect_equal(tab[["Model 1"]][1:4], known)
})

