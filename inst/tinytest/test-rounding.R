source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")

# rounding cleans up NaN inside \\num
dat <- mtcars
dat$cyl <- factor(dat$cyl)
expect_snapshot_print(
  datasummary(cyl + mpg ~ SD + N, data = dat, output = "latex"),
  "rounding-datasummary_latex"
)

# named list
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
tab <- modelsummary(
  mod,
  statistic = c("SE: {std.error}", "conf.int"),
  fmt = fmt_statistic(estimate = 0, std.error = 4),
  output = "data.frame"
)
expect_equivalent(
  tab[["(1)"]][1:3],
  c("29", "SE: 1.5878", "[25.398, 31.903]")
)

# glue function
m <- glm(am ~ mpg, data = mtcars, family = binomial)
tab <- modelsummary(
  m,
  output = "data.frame",
  fmt = NULL,
  estimate = "{round(exp(estimate), 5)}",
  statistic = "{round(exp(estimate) * std.error, 3)}"
)
known <- c("0.00136", "0.003", "1.35938", "0.156")
expect_equivalent(tab[["(1)"]][1:4], known)

# bugfix: format() is not vectorized (in fact, it is vectorized and vincent just didn't understand the function)
mod <- lm(mpg ~ I(hp * 1000) + drat, data = mtcars)
f <- function(x) format(x, digits = 3, nsmall = 2, scientific = FALSE)
tab <- modelsummary(
  mod,
  fmt = f,
  statistic = NULL,
  gof_map = NA,
  output = "data.frame"
)
expect_equivalent(tab[["(1)"]], c("10.7898612", "-0.0000518", "4.6981578"))

# very small numbers
mod <- lm(mpg ~ I(hp * 1000) + drat, data = mtcars)
tab <- modelsummary(mod, fmt = 2, output = "dataframe")
expect_true("10.79" %in% tab[["(1)"]])
tab <- modelsummary(mod, fmt = fmt_significant(2), output = "dataframe")
expect_true("-5.2e-05" %in% tab[["(1)"]])

# per term rounding
mod <- lm(mpg ~ qsec + factor(cyl), data = mtcars)
tab <- modelsummary(
  mod,
  output = "dataframe",
  statistic = NULL,
  gof_map = NA,
  fmt = fmt_term("(Intercept)" = 0, "qsec" = 2)
)
expect_equivalent(c("35", "-0.44", "-7.431", "-12.603"), tab[["(1)"]])
