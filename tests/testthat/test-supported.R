# several tests adapted from `parameters` package under GPL3
skip_on_cran()

test_that("supported_models() returns a long character vector", {
  x <- supported_models()
  expect_gt(length(x), 100)
  expect_true(is.character(x))
})


test_that("margins", {
  testthat::skip_if_not_installed("margins")
  suppressMessages(library(margins))
  mod = glm(vs ~ hp + drat, data = mtcars, family = binomial)
  mfx = margins(mod)
  tab = modelsummary(mfx, "data.frame")
  expect_s3_class(tab, "data.frame")
  expect_equal(dim(tab), c(8, 4))
})


test_that("nnet::multinom with `y.level` column", {
  testthat::skip_if_not_installed("nnet")
  library(nnet)
  make_data <- function(response = c("A", "B", "C")) {
    var1 <- sample(response, replace = T, size=100)
    var2 <- sample(c(0,1), size=100, replace=T)
    var3 <- rnorm(100, mean=10, sd=2)
    var1 <- factor(var1)
    df1 <- data.frame(var1, var2, var3)
    df1
  }
  dat <- make_data()
  invisible(capture.output(mod <- nnet::multinom(var1~var2, data=dat)))
  tab <- modelsummary(mod, output="dataframe")
  expect_s3_class(tab, "data.frame")
  expect_equal(dim(tab), c(11, 4))
})


test_that("MASS", {
  testthat::skip_if_not_installed("MASS")
  suppressMessages(library(MASS))

  # broom::tidy requires p.values=TRUE 
  fit <- polr(Sat ~ Freq, weights = Freq, data = housing)
  expect_error(suppressMessages(modelsummary(fit, statistic="p.value")))
  expect_error(suppressMessages(modelsummary(fit, p.values=TRUE,
                                             statistic="p.value")), NA)
  expect_error(suppressMessages(modelsummary(fit, stars=TRUE)))
})


test_that("survival", {
  testthat::skip_if_not_installed("survival")
  library(survival)
  data("lung")
  lung <- subset(lung, subset = ph.ecog %in% 0:2)
  lung$sex <- factor(lung$sex, labels = c("male", "female"))
  lung$ph.ecog <- factor(lung$ph.ecog, labels = c("good", "ok", "limited"))
  mod <- survival::coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung)
  tab <- modelsummary(mod, output="data.frame") 
  expect_is(tab, "data.frame")
  expect_true(nrow(tab) > 11)
})


test_that("mgcv::gam", {
  testthat::skip_if_not_installed("mgcv")
  mod <- mgcv::gam(
    formula = mpg ~ s(hp) + s(wt) + factor(cyl) + am + qsec,
    family = stats::quasi(),
    data = mtcars)
  tab <- modelsummary(mod, estimate = "edf", output="data.frame", statistic="p.value") 
  expect_is(tab, "data.frame")
  expect_true(nrow(tab) > 5)
})


test_that("betareg::betareg", {
  testthat::skip_if_not_installed("betareg")
  data("GasolineYield", package="betareg")
  data("FoodExpenditure", package="betareg")
  m1 <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
  m2 <- betareg::betareg(I(food / income) ~ income + persons, data = FoodExpenditure)
  tab <- modelsummary(list(m1, m2), output="data.frame")
  expect_is(tab, "data.frame")
  expect_true(nrow(tab) > 30)
})
 

test_that("ivreg::ivreg", {
  testthat::skip_if_not_installed("ivreg")
  testthat::skip_if_not_installed("AER")
  data(CigarettesSW, package="AER")
  mod <- ivreg::ivreg(
    log(packs) ~ log(price) + log(income) | log(income) + I(tax / cpi),
    data = CigarettesSW)
  tab <- modelsummary(mod, output="data.frame", diagnostic=TRUE)
  expect_is(tab, "data.frame")
  expect_true(nrow(tab) > 5)
})


test_that("pscl::hurdle", {
  testthat::skip_if_not_installed("pscl")
  set.seed(123)
  data("bioChemists", package = "pscl")
  mod<- pscl::hurdle(formula = art ~ .,
               data = bioChemists, zero = "geometric")
  tab <- modelsummary(mod, output="data.frame")
  expect_is(tab, "data.frame")
  expect_true(nrow(tab) > 25)
})


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
  expect_true(nrow(tab) > 22)

  # sandwich does not support lmer
  expect_error(modelsummary(mod, vcov="robust"),
               regexp = "Unable to extract")
  expect_error(modelsummary(mod, vcov=~subj),
               regexp = "Unable to extract")
})


test_that("lme4 with parameter's effects argument", {
  testthat::skip_if_not_installed("lme4")
  library(lme4)
  d <- as.data.frame(ChickWeight)
  colnames(d) <- c("y", "x", "subj", "tx")
  mod <- lmer(y ~ tx * x + (x | subj), data = d)

  # all effects implicit
  tab <- modelsummary(mod, output="dataframe")
  tab <- tab[tab$part == "estimates",]
  expect_equal(nrow(tab), 20)

  # all effects explicit
  tab <- modelsummary(mod, output="dataframe", effects = "all")
  tab <- tab[tab$part == "estimates",]
  expect_equal(nrow(tab), 20)

  # fixed effects explicit
  tab <- modelsummary(mod, output="dataframe", effects = "fixed")
  tab <- tab[tab$part == "estimates",]
  expect_equal(nrow(tab), 16)

  # random effects explicit
  tab <- modelsummary(mod, output="dataframe", effects = "random")
  tab <- tab[tab$part == "estimates",]
  expect_equal(nrow(tab), 4)
})
 

test_that("sandwich vignette", {
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



