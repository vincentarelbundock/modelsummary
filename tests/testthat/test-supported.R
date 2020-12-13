context('supported models')

library(modelsummary)

test_that("fixest", {
  testthat::skip_if_not_installed("fixest")
  testthat::skip_if(getRversion() < '3.6.6') # change in .Rng
  library(fixest)

  # simple model
  mod <- feols(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris)
  raw <- modelsummary(mod, "data.frame")
  expect_s3_class(raw, "data.frame")
  expect_equal(dim(raw), c(14, 4))

  # glance custom
  mod <- feols(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris)
  raw <- glance_custom(mod)
  expect_is(raw, "data.frame")
  expect_equal(dim(raw), c(1, 2))
})


test_that("mice", {
  testthat::skip_if_not_installed("mice")
  testthat::skip_if(getRversion() < '4.0.0') # change in .Rng
  
  library(mice)

  # impute
  set.seed(10393983)
  dat <- mtcars
  dat$wt[sample(1:nrow(dat), 4)] <- NA
  dat$hp[sample(1:nrow(dat), 3)] <- NA
  dat <- mice(dat, m = 5, printFlag = FALSE, seed = 10)

  # fit
  f <- am ~ wt + hp
  mod <- list()
  mod$OLS <- with(dat, lm(am ~ wt + hp + vs))
  mod$Logit <- with(dat, glm(am ~ wt + vs, family = binomial()))
  mod <- lapply(mod, mice::pool)

  # ols and logit
  raw <- modelsummary(mod, output="dataframe")

  truth <- c("1.772", "(0.329)", "-0.499", "(0.092)", "0.002", "(0.002)",
             "-0.108", "(0.182)", "32", "5", "0.564", "0.517")
  expect_equal(truth, unname(raw[[4]]))

  truth <- c("19.428", "(7.833)", "-5.986", "(2.299)", "", "", "-3.498",
             "(2.301)", "32", "5", "", "")
  expect_equal(truth, unname(raw[[5]]))
})


test_that("lme4", {
  testthat::skip_if_not_installed("lme4")
  library(lme4)
  d <- as.data.frame(ChickWeight)
  colnames(d) <- c("y", "x", "subj", "tx")
  mod <- lmer(y ~ tx * x + (x | subj), data = d)
  tab <- modelsummary(mod, output="dataframe")
  expect_s3_class(tab, "data.frame")
  expect_equal(dim(tab), c(23, 4))
})


#################### sandwich
test_that("sandwich website vignette", {
  testthat::skip_if_not_installed("sandwich")
  testthat::skip_if_not_installed("lmtest")
  data("PetersenCL", package = "sandwich")
  m <- lm(y ~ x, data = PetersenCL)
  vc <- list(
    "Standard"              = vcov(m),
    "Sandwich (basic)"      = sandwich::sandwich(m),
    "Clustered"             = sandwich::vcovCL(m, cluster = ~ firm),
    "Clustered (two-way)"   = sandwich::vcovCL(m, cluster = ~ firm + year),
    "HC3"                   = sandwich::vcovHC(m),
    "Andrews' kernel HAC"   = sandwich::kernHAC(m),
    "Newey-West"            = sandwich::NeweyWest(m),
    "Bootstrap"             = sandwich::vcovBS(m),
    "Bootstrap (clustered)" = sandwich::vcovBS(m, cluster = ~ firm))
  tab <- modelsummary(
    m, 
    output = "data.frame",
    statistic_override = vc["Clustered"], 
    stars = TRUE)
  expect_s3_class(tab, "data.frame")
  expect_equal(dim(tab), c(11, 4))
})
