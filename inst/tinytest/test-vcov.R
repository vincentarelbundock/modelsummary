source("helpers.R")
requiet("sandwich")
requiet("lmtest")

url <- "https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv"
dat <- read.csv(url)
dat$Clergy <- ifelse(dat$Clergy > 40, 1, 0) # binary variable for logit model
dat <- dat[!is.na(dat$Region), ]
models <- list()
models[["OLS 1"]] <- lm(Literacy ~ Crime_prop + Infants, dat)
models[["Poisson 1"]] <- glm(
  Literacy ~ Crime_prop + Donations,
  dat,
  family = poisson()
)
models[["OLS 2"]] <- lm(Desertion ~ Crime_prop + Infants, dat)
models[["Poisson 2"]] <- glm(
  Desertion ~ Crime_prop + Donations,
  dat,
  family = poisson()
)
models[["Logit 1"]] <- glm(
  Clergy ~ Crime_prop + Infants,
  dat,
  family = binomial()
)

# manual sandwich
# HC1
for (m in models) {
  tab <- modelsummary(
    m,
    output = "dataframe",
    vcov = "stata",
    estimate = "std.error",
    fmt = NULL,
    statistic = NULL,
    gof_map = NA
  )
  tab <- as.numeric(tab[["(1)"]])
  man <- sqrt(diag(sandwich::vcovHC(m, type = "HC1")))
  expect_equivalent(tab, man, ignore_attr = TRUE)
}
# HC4
for (m in models) {
  tab <- modelsummary(
    m,
    output = "dataframe",
    vcov = "HC4",
    estimate = "std.error",
    fmt = NULL,
    statistic = NULL,
    gof_map = NA
  )
  tab <- as.numeric(tab[["(1)"]])
  man <- sqrt(diag(sandwich::vcovHC(m, type = "HC4")))
  expect_equivalent(tab, man, ignore_attr = TRUE)
}


# user-supplied vcov_type in gof section
mod <- lm(hp ~ mpg, data = mtcars)
vc <- list(
  "Rob" = "robust",
  "Stata Corp" = "stata",
  "Newey Lewis & the News" = "NeweyWest"
)
tab <- modelsummary(mod, output = "data.frame", vcov = vc)
row <- unname(unlist(tab[nrow(tab), 4:6]))
expect_equivalent(row, c("Rob", "Stata Corp", "Newey Lewis & the News"))

# sandwich arguments in ellipsis
data(PetersenCL)
mod <- lm(y ~ x, PetersenCL)
tab <- modelsummary(
  mod,
  output = "dataframe",
  fmt = 7,
  estimate = "std.error",
  gof_omit = ".*",
  statistic = NULL,
  vcov = "panel-corrected",
  cluster = "firm"
)
v <- vcovPC(mod, cluster = "firm")
fun <- fmt_decimal(7)
v <- fun(sqrt(diag(v)))
expect_equivalent(unname(v), tab[["(1)"]])

# multi vcov with model recycling
mod <- lm(hp ~ 1, mtcars)
vc <- list("HC0", "classical", ~cyl)
tab <- modelsummary(
  mod,
  output = "data.frame",
  estimate = "std.error",
  statistic = NULL,
  vcov = vc,
  gof_omit = ".*"
)
expect_equivalent(tab[["(1)"]], "11.929")
expect_equivalent(tab[["(2)"]], "12.120")
expect_equivalent(tab[["(3)"]], "43.502")

# character vector
tab1 = modelsummary(models, vcov = "robust", output = "data.frame")
tab2 = modelsummary(models, vcov = rep("robust", 5), output = "data.frame")
tab3 = modelsummary(
  models,
  vcov = list("robust", "robust", "robust", "robust", "robust"),
  output = "data.frame"
)
tab4 = modelsummary(models, vcov = sandwich::vcovHC, output = "data.frame")
expect_identical(tab1, tab2)
expect_identical(tab1, tab3)
# bad input
expect_error(modelsummary(models, vcov = "bad", output = "data.frame"))

# robust character shortcuts
requiet("estimatr")

mod = lm(hp ~ mpg, mtcars)
mod_estimatr = estimatr::lm_robust(hp ~ mpg, mtcars)
x = modelsummary(mod, vcov = "HC1", output = "dataframe", gof_omit = ".*")
y = modelsummary(mod, vcov = "stata", output = "dataframe", gof_omit = ".*")
expect_equivalent(x[[4]], y[[4]])

x = modelsummary(mod, vcov = "HC3", output = "dataframe", gof_omit = ".*")
y = modelsummary(mod, vcov = "robust", output = "dataframe", gof_omit = ".*")
expect_equivalent(x[[4]], y[[4]])

x = modelsummary(mod, vcov = "classical", output = "dataframe", gof_omit = ".*")
y = modelsummary(mod, output = "dataframe", gof_omit = ".*")
expect_equivalent(x[[4]], y[[4]])

x = modelsummary(mod, vcov = "HC2", output = "dataframe", gof_omit = ".*")
y = modelsummary(mod_estimatr, output = "dataframe", gof_omit = ".*")
expect_equivalent(x[[4]], y[[4]])

# single model
mod <- lm(hp ~ mpg + drat, mtcars)
x <- modelsummary(mod, vcov = vcov, output = "data.frame")
y <- modelsummary(mod, vcov = vcov(mod), output = "data.frame")
z <- modelsummary(
  mod,
  vcov = list(sqrt(diag(vcov(mod)))),
  output = "data.frame"
)
expect_equivalent(x, y)
expect_equivalent(y, z)


# reference tables
results <- list()

results[["one sandwich"]] <- modelsummary(
  models,
  output = "data.frame",
  vcov = vcov,
  statistic = "p.value",
  fmt = "%.7f"
)

results[["many sandwiches"]] <- modelsummary(
  models,
  output = "data.frame",
  vcov = list(vcov, vcov, vcov, vcov, vcov),
  fmt = "%.7f"
)

results[["list of matrices"]] <- modelsummary(
  models,
  output = "data.frame",
  vcov = lapply(models, stats::vcov),
  fmt = "%.7f"
)

results[["hardcoded numerical"]] <- modelsummary(
  models,
  output = "data.frame",
  fmt = "%.7f",
  vcov = list(
    `OLS 1` = c("(Intercept)" = 2, Crime_prop = 3, Infants = 4),
    `NBin 1` = c("(Intercept)" = 3, Crime_prop = -5, Donations = 3),
    `OLS 2` = c("(Intercept)" = 7, Crime_prop = -6, Infants = 9),
    `NBin 2` = c("(Intercept)" = 4, Crime_prop = -7, Donations = -9),
    `Logit 1` = c("(Intercept)" = 1, Crime_prop = -5, Infants = -2)
  )
)

results[["hardcoded arbitrary"]] <- modelsummary(
  models,
  output = "data.frame",
  fmt = "%.7f",
  vcov = list(
    `OLS 1` = c("(Intercept)" = "!!", Crime_prop = "!!!", Infants = "!!!!"),
    `NBin 1` = c(
      "(Intercept)" = "[1, 5]",
      Crime_prop = "[-5, +5]",
      Donations = "95% CI[-2, -1]"
    ),
    `OLS 2` = c("(Intercept)" = "7", Crime_prop = "-6", Infants = "9"),
    `NBin 2` = c(
      "(Intercept)" = "\U03B5",
      Crime_prop = "\U2135",
      Donations = "\U0414"
    ),
    `Logit 1` = c("(Intercept)" = 1, Crime_prop = -5, Infants = -2)
  )
)


# we are not interested in GOFs in this test
for (i in seq_along(results)) {
  results[[i]] <- results[[i]][results[[i]]$part == "estimates", , drop = FALSE]
  results[[i]]$part <- results[[i]]$statistic <- NULL
}

# # save known values (comment out until new manual check)
# saveRDS(results, file="known_output/statistic-override.rds")

# load reference values (comment out when updating)
reference <- readRDS(file = "known_output/statistic-override.rds")

# bad formula
expect_error(modelsummary(models, vcov = ~bad))

# bad function
expect_error(modelsummary(models, vcov = stats::na.omit))


# vector must be named
vec <- as.numeric(1:3)
expect_error(modelsummary(
  models[[1]],
  estimate = c("estimate", "std.error"),
  vcov = vec
))

# vcov content
expect_equivalent(
  results[["one sandwich"]],
  reference[["one sandwich"]],
  ignore_attr = TRUE
)
expect_equivalent(
  results[["many sandwiches"]],
  reference[["many sandwiches"]],
  ignore_attr = TRUE
)
expect_equivalent(
  results[["list of matrices"]],
  reference[["list of matrices"]],
  ignore_attr = TRUE
)
expect_equivalent(
  results[["hardcoded numerical"]],
  reference[["hardcoded numerical"]],
  ignore_attr = TRUE
)
expect_equivalent(
  results[["hardcoded arbitrary"]],
  reference[["hardcoded arbitrary"]],
  ignore_attr = TRUE
)

# useless: function but no ci needed
tab <- modelsummary(
  models,
  output = "dataframe",
  vcov = vcov,
  conf_level = NULL
)
expect_inherits(tab, "data.frame")

# sanity checks
expect_error(
  modelsummary(models, vcov = "panel-corrected"),
  pattern = "cluster"
)

# lme4
requiet("lme4")
requiet("clubSandwich")
mod <- lmer(mpg ~ hp + drat + (1 | gear), data = mtcars)
tab <- modelsummary(mod, vcov = "CR0", output = "data.frame")
expect_inherits(tab, "data.frame")

# fixest
requiet("fixest")
mod_lm = lm(hp ~ mpg + drat, mtcars)
if (utils::packageVersion("fixest") >= "0.10.0") {
  mod_feols = feols(hp ~ mpg + drat, mtcars, vcov = ~vs)
} else {
  mod_feols = feols(hp ~ mpg + drat, mtcars, cluster = ~vs)
}
models = list("lm" = mod_lm, "feols" = mod_feols)

# no longer true since lm produces RMSE but not fixest
# tab = msummary(models, vcov = 'iid', gof_omit = 'R2|IC|Log|F', output = "data.frame")
# expect_equivalent(tab$lm, tab$feols)
tab = msummary(
  models,
  vcov = "HC1",
  gof_omit = "R2|IC|Log|F",
  output = "data.frame"
)
expect_equivalent(tab$lm, tab$feols)

tab = msummary(
  models,
  vcov = ~vs,
  gof_omit = "R2|IC|Log|F",
  output = "data.frame"
)
expect_equivalent(tab$lm, tab$feols)

tab = msummary(
  models,
  vcov = ~cyl,
  gof_omit = "R2|IC|Log|F",
  output = "data.frame"
)
expect_equivalent(tab$lm, tab$feols)

# warning for non-iid hardcoded vcov
requiet("lfe")
requiet("estimatr")

mod <- felm(hp ~ mpg + drat | 0 | 0 | vs, mtcars)
expect_warning(
  modelsummary(mod, vcov = "iid", output = "data.frame"),
  pattern = "IID"
)

mod <- lm_robust(hp ~ mpg + drat, mtcars)
expect_warning(
  modelsummary(mod, vcov = "iid", output = "data.frame"),
  pattern = "IID"
)

mod <- felm(hp ~ mpg + drat | cyl, mtcars)
expect_warning(
  modelsummary(mod, vcov = "iid", output = "data.frame"),
  pattern = "IID"
)

mod <- felm(hp ~ mpg | cyl ~ gear, data = mtcars)

# sandwich does not work with lfe::felm
expect_error(modelsummary(
  mod,
  vcov = list(NULL, "robust"),
  output = "data.frame"
))

# # assume user knows that they are doing
# modelsummary(mod, vcov = list('Robust' = mod$robustvcv), output = "data.frame")
#
# # no explicit vcov names, so we raise the warning
# expect_warning(modelsummary(mod, vcov = list(NULL, "classical", vcov(mod, se = "hetero")), output = "data.frame"),
#                pattern = "IID")

# hardcoded numerical
mod <- lm(mpg ~ hp + drat, mtcars)
tab <- modelsummary(
  mod,
  gof_map = NA,
  estimate = "std.error",
  statistic = NULL,
  output = "data.frame",
  vcov = c(
    "(Intercept)" = 9,
    "drat" = 3,
    "hp" = 2
  )
)
expect_equivalent(tab[["(1)"]], c("9.000", "2.000", "3.000"))


# sublist (sandwich vignette)
models <- lm(hp ~ mpg + drat, mtcars)
tab <- modelsummary(
  models,
  output = "data.frame",
  vcov = list(vcov)
)
expect_inherits(tab, "data.frame")
expect_equivalent(dim(tab), c(14, 4))


# clustered standard errors
# checked manually against fixest clusters
# se = fixest::feols(f, mtcars) %>%
#      fixest::se(cluster=~cyl)
f = hp ~ mpg + drat + vs
mod = lm(f, mtcars)
tmp = modelsummary(mod, gof_omit = ".*", vcov = ~cyl, output = "dataframe")
truth = c(
  "247.053",
  "(73.564)",
  "-7.138",
  "(3.162)",
  "18.064",
  "(40.237)",
  "-50.124",
  "(13.268)"
)
expect_equivalent(truth, tmp[[4]])

# lme4 and various warnings
requiet("lme4")
models = list(
  lm(hp ~ mpg, mtcars),
  lmer(hp ~ mpg + (1 | cyl), mtcars)
)
tab = modelsummary(
  models,
  output = "dataframe",
  vcov = list("robust", "classical")
)
expect_inherits(tab, "data.frame")
expect_equivalent(ncol(tab), 5)
expect_error(
  modelsummary(models, output = "dataframe", vcov = "robust"),
  pattern = "Unable to extract"
)
expect_inherits(
  modelsummary(models[[2]], output = "dataframe", vcov = stats::vcov),
  "data.frame"
)


# Issue #605: get_estimates supports vcov shortcuts
mod <- lm(mpg ~ hp, data = mtcars)
expect_equivalent(
  get_estimates(mod, vcov = "stata")$std.error,
  sqrt(diag(sandwich::vcovHC(mod, "HC1")))
)
expect_equivalent(
  get_estimates(mod, vcov = "robust")$std.error,
  sqrt(diag(sandwich::vcovHC(mod, "HC3")))
)
expect_equivalent(
  get_estimates(mod, vcov = ~cyl)$std.error,
  sqrt(diag(sandwich::vcovCL(mod, ~cyl)))
)


# Issue 640
mlm <- lm(mpg ~ wt, data = mtcars)
mglm <- glm(mpg ~ wt, data = mtcars)

a <- modelsummary(
  mlm,
  vcov = vcovHC,
  estimate = "conf.int",
  statistic = NULL,
  output = "dataframe",
  gof_map = NA
)
b <- modelsummary(
  mlm,
  estimate = "conf.int",
  statistic = NULL,
  output = "dataframe",
  gof_map = NA
)
expect_false(any(a[["(1)"]] == b[["(1)"]]))

a <- modelsummary(
  mglm,
  vcov = sandwich::vcovHC,
  estimate = "conf.int",
  statistic = NULL,
  output = "dataframe",
  gof_map = NA
)
b <- modelsummary(
  mglm,
  estimate = "conf.int",
  statistic = NULL,
  output = "dataframe",
  gof_map = NA
)
expect_false(any(a[["(1)"]] == b[["(1)"]]))
