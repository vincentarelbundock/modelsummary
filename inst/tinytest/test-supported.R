# several tests adapted from `parameters` package under GPL3
source("helpers.R")

# supported_models() returns a long character vector
x <- supported_models()
expect_true(length(x) > 100)
expect_true(is.character(x))

# margins
if (isTRUE(requiet("margins"))) {
  mod = glm(vs ~ hp + drat, data = mtcars, family = binomial)
  mfx = margins(mod)
  tab = modelsummary(mfx, "data.frame")
  expect_inherits(tab, "data.frame")
  expect_equivalent(dim(tab), c(7, 4))
}


# MASS
requiet("MASS")
# broom::tidy requires p.values=TRUE, but we now use easystats, so no need to error checking
fit <- polr(Sat ~ Freq, weights = Freq, data = housing)
tab <- modelsummary(fit, output = 'dataframe', statistic = "p.value")
expect_inherits(tab, "data.frame")

# survival
requiet("survival")
lung <- survival::lung
lung <- subset(lung, subset = ph.ecog %in% 0:2)
lung$sex <- factor(lung$sex, labels = c("male", "female"))
lung$ph.ecog <- factor(lung$ph.ecog, labels = c("good", "ok", "limited"))
mod <- survival::coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung)
tab <- modelsummary(mod, output="data.frame")
expect_inherits(tab, "data.frame")
expect_true(nrow(tab) > 11)

# mgcv::gam
requiet("mgcv")
mod <- mgcv::gam(
  formula = mpg ~ s(hp) + s(wt) + factor(cyl) + am + qsec,
  family = stats::quasi(),
  data = mtcars)
tab <- modelsummary(mod, output="data.frame", statistic="p.value")
expect_inherits(tab, "data.frame")
expect_true(nrow(tab) > 5)

# betareg::betareg
requiet("betareg")
data("GasolineYield", package = "betareg")
data("FoodExpenditure", package = "betareg")
m1 <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
m2 <- betareg::betareg(I(food / income) ~ income + persons, data = FoodExpenditure)
tab <- modelsummary(list(m1, m2), output="data.frame")
expect_inherits(tab, "data.frame")
expect_true(nrow(tab) > 30)

# ivreg::ivreg
# these two packages depend on `car`, which depends on `rio`, which depends
# on `foreign`, which cannot be installed on R<4.0.0
requiet("AER")
requiet("ivreg")
data(CigarettesSW, package = "AER")
mod <- ivreg::ivreg(
  log(packs) ~ log(price) + log(income) | log(income) + I(tax / cpi),
  data = CigarettesSW)
tab <- modelsummary(mod, output="data.frame", diagnostic=TRUE)
expect_inherits(tab, "data.frame")
expect_true(nrow(tab) > 5)

# pscl::hurdle
requiet("pscl")
set.seed(123)
data("bioChemists", package = "pscl")
mod<- pscl::hurdle(formula = art ~ .,
             data = bioChemists, zero = "geometric")
tab <- modelsummary(mod, output="data.frame")
expect_inherits(tab, "data.frame")
expect_true(nrow(tab) > 25)

# mice
if (getRversion() < "4.0.0") exit_file("old R")
requiet("mice")

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
expect_equivalent(truth, unname(raw[[4]]))

truth <- c("19.428", "(7.833)", "-5.986", "(2.299)", "", "", "-3.498",
           "(2.301)", "32", "5", "", "")
expect_equivalent(truth, unname(raw[[5]]))

# sandwich vignette
requiet("sandwich")
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
  vcov = vc["Clustered"],
  stars = TRUE)
expect_inherits(tab, "data.frame")
expect_true("Clustered" %in% tab[["(1)"]])

# nnet::multinom with `response` column
requiet("nnet")
make_data <- function(response = c("A", "B", "C")) {
  var1 <- sample(response, replace = T, size=100)
  var2 <- sample(c(0,1), size=100, replace=T)
  var3 <- rnorm(100, mean=10, sd=2)
  var1 <- factor(var1)
  df1 <- data.frame(var1, var2, var3)
  df1
}
dat <- make_data()
invisible(capture.output(mod <- nnet::multinom(var1 ~ var2, data = dat)))
expect_warning(modelsummary(mod, output = "dataframe"), pattern = "duplicate")
tab <- suppressWarnings(modelsummary(mod, shape = response + term ~ model, output = "dataframe"))
expect_inherits(tab, "data.frame")
expect_true(nrow(tab) > 11)


# did
requiet("did")
data(mpdta, package = 'did')
dat <- mpdta
dat$newvar <- substr(mpdta$countyreal, 1, 2)
mod1 <- att_gt(yname = "lemp", gname = "first.treat", idname = "countyreal",
               tname = "year", xformla = ~1, data = dat)
mod2 <- att_gt(yname = "lemp", gname = "first.treat", idname = "countyreal",
               tname = "year", xformla = ~1, data = dat,
               clustervars = c('countyreal', 'newvar'))
mods <- list('mod1' = mod1, 'mod2' = mod2)
tab <- msummary(mods, gof_omit = 'Num|ngroup|ntime|control|method', output = 'data.frame')
expect_equivalent(tab$mod1[nrow(tab)], "by: countyreal")
expect_equivalent(tab$mod2[nrow(tab)], "by: countyreal & newvar")
