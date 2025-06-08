source("helpers.R")
requiet("broom")


fit <- glm(am ~ mpg + factor(cyl), data = mtcars, family = binomial)
tid <- tidy(fit, conf.int = TRUE)

# exponentiate is a flag
expect_error(modelsummary(fit, exponentiate = "yes"))
expect_error(modelplot(fit, exponentiate = "yes"))

# logit coefficients exponentiate

tab <- modelsummary(
  fit,
  gof_omit = ".*",
  estimate = "estimate",
  statistic = NULL,
  output = "dataframe",
  exponentiate = TRUE
)
x <- c("0.000", "1.448", "2.078", "2.017")
expect_equivalent(tab[[4]], x)
tab <- modelsummary(
  fit,
  gof_omit = ".*",
  estimate = "estimate",
  statistic = NULL,
  output = "dataframe",
  exponentiate = TRUE
)

tab <- modelsummary(
  fit,
  gof_omit = ".*",
  estimate = "conf.low",
  statistic = NULL,
  output = "dataframe",
  exponentiate = TRUE
)
x <- c("0.000", "1.026", "0.130", "0.044")
expect_equivalent(tab[[4]], x)

tab <- modelsummary(
  fit,
  gof_omit = ".*",
  estimate = "conf.high",
  statistic = NULL,
  output = "dataframe",
  exponentiate = TRUE
)
expect_equivalent(tab[[4]], sprintf("%.3f", exp(tid$conf.high)))

tab <- modelsummary(
  fit,
  gof_omit = ".*",
  estimate = "std.error",
  statistic = NULL,
  output = "dataframe",
  exponentiate = TRUE
)
expect_equivalent(tab[[4]], sprintf("%.3f", exp(tid$estimate) * tid$std.error))

# vcov
requiet("sandwich")
b <- coef(fit)
se <- sqrt(diag(vcovCL(fit, ~cyl)))
x <- c("0.000", "1.448", "2.078", "2.017")
tab <- modelsummary(
  fit,
  vcov = ~cyl,
  gof_omit = ".*",
  estimate = "estimate",
  statistic = NULL,
  output = "dataframe",
  exponentiate = TRUE
)
expect_equivalent(tab[[4]], x)
tab <- modelsummary(
  fit,
  vcov = ~cyl,
  gof_omit = ".*",
  estimate = "std.error",
  statistic = NULL,
  output = "dataframe",
  exponentiate = TRUE
)

# not sure why this no longer works. My results seem to match parameters::parameters()
expect_equivalent(tab[[4]], sprintf("%.3f", exp(b) * se))

# exponentiate vector
mod <- glm(am ~ mpg, family = binomial, data = mtcars)
mod <- list(mod, mod)
b <- coef(mod[[1]])
se <- sqrt(diag(stats::vcov(mod[[1]])))

# coefficients
tab <- modelsummary(
  mod,
  exponentiate = FALSE,
  output = "data.frame",
  statistic = NULL,
  fmt = identity
)
expect_equivalent(b, as.numeric(tab[["(1)"]][1:2]), ignore_attr = TRUE)
expect_equivalent(b, as.numeric(tab[["(2)"]][1:2]), ignore_attr = TRUE)

tab <- modelsummary(
  mod,
  exponentiate = TRUE,
  output = "data.frame",
  statistic = NULL,
  fmt = identity
)
expect_equivalent(exp(b), as.numeric(tab[["(1)"]][1:2]), ignore_attr = TRUE)
expect_equivalent(exp(b), as.numeric(tab[["(2)"]][1:2]), ignore_attr = TRUE)

# standard error
tab <- modelsummary(
  mod,
  exponentiate = TRUE,
  output = "data.frame",
  estimate = "std.error",
  statistic = NULL,
  fmt = identity
)
expect_equivalent(
  exp(b) * se,
  as.numeric(tab[["(1)"]][1:2]),
  ignore_attr = TRUE
)
expect_equivalent(
  exp(b) * se,
  as.numeric(tab[["(2)"]][1:2]),
  ignore_attr = TRUE
)

# vector
tab <- modelsummary(
  mod,
  exponentiate = c(TRUE, FALSE),
  output = "data.frame",
  statistic = NULL,
  fmt = identity
)
expect_equivalent(exp(b), as.numeric(tab[["(1)"]][1:2]), ignore_attr = TRUE)
expect_equivalent(b, as.numeric(tab[["(2)"]][1:2]), ignore_attr = TRUE)


# Issue #878: do not exponentiate random effects parameters
requiet("lme4")
gm1a <- glmer(
  cbind(incidence, size - incidence) ~ period + (1 | herd),
  data = cbpp,
  binomial,
  verbose = 0,
  nAGQ = 9
)
e1 <- get_estimates(gm1a, exponentiate = TRUE)
e2 <- get_estimates(gm1a, exponentiate = FALSE)
expect_equal(e1$estimate[nrow(e1)], e2$estimate[nrow(e2)])
expect_equal(e1$estimate[1:3], exp(e2$estimate[1:3]))

mod <- lmer(Sepal.Length ~ (1 + Sepal.Width | Species), data = iris)
e1 <- get_estimates(mod, exponentiate = FALSE)
e2 <- get_estimates(mod, exponentiate = TRUE)
expect_equal(exp(e1$estimate[1]), e2$estimate[1])
expect_equal(e1$estimate[2:5], e2$estimate[2:5])
